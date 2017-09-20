module Exitify ( exitifyProgram ) where

{-
Note [Exitification]
~~~~~~~~~~~~~~~~~~~~

This module implements Exitification. The goal is to pull as much code out of
recursive functions as possible, as the simplifier is better at inlining into
call-sites that are not in non-recursive positions.

Example:

  let t = foo bar
  joinrec go 0     x y = t (x*x)
          go (n-1) x y = call go (n-1) (x+y)
  in …

We’d like to inline `t`, but that does not happen: Because t is a thunk and is
used in a recursive function, doing so might lose sharing in general. In
this case, however, `t` is on the _exit path_ of `go`, so called at most once.
How do we make this clearly visible to the simplifier?

We can take a code path (expression in tail-recursive position) recursive
function that is an exit path (does not contain a recursive call) and bind it
outside the recursive function as a join-point.

Example result:

  let t = foo bar
  join exit x = t (x*x)
  joinrec go 0     x y = call exit x
          go (n-1) x y = call go (n-1) (x+y)
  in …

Now `t` is no longer in a recursive function, and good things happen!
-}

import GhcPrelude
import Var
import Id
import IdInfo
import CoreSyn
import CoreUtils
import State
import Unique
import VarSet
import VarEnv
import CoreFVs
import FastString
import Type

import Data.Bifunctor
import Control.Monad

-- | Traverses the AST, simply to find all joinrecs and call 'exitify' on them.
exitifyProgram :: CoreProgram -> CoreProgram
exitifyProgram binds = map goTopLvl binds
  where
    goTopLvl (NonRec v e) = NonRec v (go in_scope_toplvl e)
    goTopLvl (Rec pairs) = Rec (map (second (go in_scope_toplvl)) pairs)

    in_scope_toplvl = emptyInScopeSet `extendInScopeSetList` bindersOfBinds binds

    go :: InScopeSet -> CoreExpr -> CoreExpr
    go _ e@(Var{})       = e
    go _ e@(Lit {})      = e
    go _ e@(Type {})     = e
    go _ e@(Coercion {}) = e

    go in_scope (Lam v e')  = Lam v (go in_scope' e')
      where in_scope' = in_scope `extendInScopeSet` v
    go in_scope (App e1 e2) = App (go in_scope e1) (go in_scope e2)
    go in_scope (Case scrut bndr ty alts)
        = Case (go in_scope scrut) bndr ty (map (goAlt in_scope') alts)
      where in_scope' = in_scope `extendInScopeSet` bndr
    go in_scope (Cast e' c) = Cast (go in_scope e') c
    go in_scope (Tick t e') = Tick t (go in_scope e')
    go in_scope (Let bind body) = goBind in_scope bind (go in_scope' body)
      where in_scope' = in_scope `extendInScopeSetList` bindersOf bind

    goAlt :: InScopeSet -> CoreAlt -> CoreAlt
    goAlt in_scope (dc, pats, rhs) = (dc, pats, go in_scope' rhs)
      where in_scope' = in_scope `extendInScopeSetList` pats

    goBind :: InScopeSet -> CoreBind -> (CoreExpr -> CoreExpr)
    goBind in_scope (NonRec v rhs) = Let (NonRec v (go in_scope rhs))
    goBind in_scope (Rec pairs)
        | is_join_rec = exitify in_scope' pairs'
        | otherwise   = Let (Rec pairs')
      where pairs' = map (second (go in_scope')) pairs
            is_join_rec = any (isJoinId . fst) pairs
            in_scope' = in_scope `extendInScopeSetList` bindersOf (Rec pairs)

-- | Given a recursive group of a joinrec, identifies “exit paths” and binds them as
--   join-points outside the joinrec.
exitify :: InScopeSet -> [(Var,CoreExpr)] -> (CoreExpr -> CoreExpr)
exitify in_scope pairs =
    \body ->mkExitLets exits (mkLetRec pairs' body)
  where
    mkExitLets ((exitId, exitRhs):exits') = mkLetNonRec exitId exitRhs . mkExitLets exits'
    mkExitLets [] = id

    -- We need the set of free variables of many subexpressions here, so
    -- annotate the AST with them
    ann_pairs = map (second freeVars) pairs

    -- Which are the recursive calls?
    recursive_calls = mkVarSet $ map fst pairs

    (pairs',exits) = (`runState` []) $ do
        forM ann_pairs $ \(x,rhs) -> do
            -- go past the lambdas of the join point
            let (args, body) = collectNAnnBndrs (idJoinArity x) rhs
            body' <- go args body
            let rhs' = mkLams args body'
            return (x, rhs')

    -- main working function. Goes through the RHS (tail-call positions only),
    -- checks if there are no more recursive calls, if so, abstracts over
    -- variables bound on the way and lifts it out as a join point.
    --
    -- It uses a state monad to keep track of floated binds
    go :: [Var]           -- ^ variables to abstract over
       -> CoreExprWithFVs -- ^ current expression in tail position
       -> State [(Id, CoreExpr)] CoreExpr

    go captured ann_e
        -- Do not touch an expression that is already a join call with no free
        -- variables in the arguments. See Note [Idempotency]
        | (Var f, args) <- collectArgs e
        , isJoinId f
        , isEmptyVarSet (exprsFreeVars args `minusVarSet` mkVarSet captured)
        = return e

        -- Do not touch a boring expression
        -- See Note [Interesting expression]
        | is_exit, not is_interesting = return e

        -- Cannot float out if local join points are used, as
        -- we cannot abstract over them
        | is_exit, captures_join_points = return e

        -- We have something to float out!
        | is_exit = do
            -- Assemble the RHS of the exit join point
            let rhs = mkLams args e
                ty = exprType rhs
            let avoid = in_scope `extendInScopeSetList` captured
            -- Remember this binding under a suitable name
            v <- addExit avoid ty (length args) rhs
            -- And call it from here
            return $ mkVarApps (Var v) args
      where
        -- An exit expression has no recursive calls
        is_exit = disjointVarSet fvs recursive_calls

        -- An interesting exit expression has free variables from
        -- outside the recursive group
        -- See Note [Interesting expression]
        is_interesting = not (isEmptyVarSet (fvs `minusVarSet` mkVarSet captured))

        -- The possible arguments of this exit join point
        args = filter (`elemVarSet` fvs) captured

        -- We cannot abstract over join points
        captures_join_points = any isJoinId args

        e = deAnnotate ann_e
        fvs = dVarSetToVarSet (freeVarsOf ann_e)


    -- Case right hand sides are in tail-call position
    go captured (_, AnnCase scrut bndr ty alts) = do
        alts' <- forM alts $ \(dc, pats, rhs) -> do
            rhs' <- go (pats ++ bndr : captured) rhs
            return (dc, pats, rhs')
        return $ Case (deAnnotate scrut) bndr ty alts'

    go captured (_, AnnLet ann_bind body)
        -- join point, RHS and body are in tail-call position
        | AnnNonRec j rhs <- ann_bind
        , Just join_arity <- isJoinId_maybe j
        = do let (params, join_body) = collectNAnnBndrs join_arity rhs
             join_body' <- go (params ++ captured) join_body
             let rhs' = mkLams params join_body'
             body' <- go (j : captured) body
             return $ Let (NonRec j rhs') body'

        -- rec join point, RHSs and body are in tail-call position
        | AnnRec pairs <- ann_bind
        , isJoinId (fst (head pairs))
        = do let js = map fst pairs
             pairs' <- forM pairs $ \(j,rhs) -> do
                 let join_arity = idJoinArity j
                     (params, join_body) = collectNAnnBndrs join_arity rhs
                 join_body' <- go (params ++ js ++ captured) join_body
                 let rhs' = mkLams params join_body'
                 return (j, rhs')
             body' <- go (js ++ captured) body
             return $ Let (Rec pairs') body'

        -- normal Let, only the body is in tail-call position
        | otherwise
        = do body' <- go (bindersOf bind ++ captured) body
             return $ Let bind body'
      where bind = deAnnBind ann_bind

    go _ ann_e = return (deAnnotate ann_e)


-- Picks a new unique, which is disjoint from
--  * the free variables of the whole joinrec
--  * any bound variables (captured)
--  * any exit join points created so far.
mkExitJoinId :: InScopeSet -> Type -> JoinArity -> ExitifyM JoinId
mkExitJoinId in_scope ty join_arity = do
    fs <- get
    let avoid = in_scope `extendInScopeSetList` (map fst fs)
                         `extendInScopeSet` exit_id_tmpl -- just cosmetics
    return (uniqAway avoid exit_id_tmpl)
  where
    exit_id_tmpl = mkSysLocal (fsLit "exit") initExitJoinUnique ty
                    `asJoinId` join_arity
                    `setIdOccInfo` exit_occ_info

    -- See Note [Do not inline exit join points]
    exit_occ_info =
        OneOcc { occ_in_lam = True
               , occ_one_br = True
               , occ_int_cxt = False
               , occ_tail = AlwaysTailCalled join_arity }

addExit :: InScopeSet -> Type -> JoinArity -> CoreExpr -> ExitifyM JoinId
addExit in_scope ty join_arity rhs = do
    -- Pick a suitable name
    v <- mkExitJoinId in_scope ty join_arity
    fs <- get
    put ((v,rhs):fs)
    return v


type ExitifyM =  State [(JoinId, CoreExpr)]

{-
Note [Interesting expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We do not want this to happen:

  joinrec go 0     x y = x
          go (n-1) x y = call go (n-1) (x+y)
  in …
==>
  join exit x = x
  joinrec go 0     x y = call exit x
          go (n-1) x y = call go (n-1) (x+y)
  in …

or

  joinrec go 0     x y = x+x
          go (n-1) x y = call go (n-1) (x+y)
  in …
==>
  join exit x = x+x
  joinrec go 0     x y = call exit x
          go (n-1) x y = call go (n-1) (x+y)
  in …

So we only hoist an exit expression out if it contains at least one free
non-exported variable.


Note [Idempotency]
~~~~~~~~~~~~~~~~~~

We do not want this to happen:

  join exit x = t (x*x)
  joinrec go 0     x y = call exit x
          go (n-1) x y = call go (n-1) (x+y)
  in …
==>
  join exit x = t (x*x)
  join exit' x = t (x*x)
  joinrec go 0     x y = call exit' x
          go (n-1) x y = call go (n-1) (x+y)
  in …

So when the RHS is a join call, and none of  the arguments mention a variables
bound outside of `go`, we leave it in place.

Note [Do not inline exit join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When we have

  let t = foo bar
  join exit x = t (x*x)
  joinrec go 0     x y = call exit x
          go (n-1) x y = call go (n-1) (x+y)
  in …

we do not want the simplifier to simply inline `exit` back in (which it happily
would).

To prevent this, we need to recognize exit join points, and then disable
inlining.

Exit join points are join points with an occurence in a recursive group. The
latter is visible as `occ_in_lam (idOccinfo id) == True`. This logic is encoded in
`isExitJoinId` in `Id`.

We create exit join point ids with such an `OccInfo`, see `exit_occ_info`.

To prevent inlining, we check for that in `preInlineUnconditionally` directly.
For `postInlineUnconditionally` and unfolding-based inlining, the function
`simplLetUnfolding` simply gives exit join points no unfolding, which prevents
this kind of inlining.

-}
