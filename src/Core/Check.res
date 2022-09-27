open Tree
open Value
open Unify
open Eval
open Context

exception CannotFindVariable(string, Location.range)
exception CannotInferLambda(Location.range)
exception CannotApply(Location.range, Term.term)

let rec check = (ctx, expr, ty) =>
    switch (expr, ty) {
    | (Lam(r', name, b), VPi(r, _, t, b')) =>
        let var   = VStuck(r, Rigid(ctx.level), list{})
        let evalB = check(bind(ctx, name.iVal, var, t), b, b'(var))
        Term.Lam(r', name.iVal, evalB)
    | (expr, expected) =>
        let (elab, infered) = infer(ctx, expr)
        unify(ctx, get_range(expr), infered, expected)
        elab
    }

and infer = (ctx, expr) => 
    switch expr {
    | Type(r) => (Term.Type(r), VType(r))
    | Var(n) => 
        switch Belt.Map.String.get(ctx.types, n.iVal) {
        | Some((ty, lvl)) => (Term.Var(n.iPos, ctx.level - lvl - 1), ty)
        | None => raise(CannotFindVariable(n.iVal, n.iPos))
        }
    | Lam(r, _, _) => raise(CannotInferLambda(r))
    | Ann(r, f, t) => {
        let elabA = check(ctx, t, VType(get_range(t)))
        let runT = eval(ctx, elabA)
        let elabF = check(ctx, f, runT)
        (Ann(r, elabF, elabA), runT)
    }
    | App(r, f, a) => {
        let (elabF, fTy) = infer(ctx, f)
        switch fTy {
        | VPi(_, _, t, b') => {
            let elabA = check(ctx, a, t)
            (Term.App(r, elabF, elabA), b'(eval(ctx, elabA)))
        }
        | e => raise(CannotApply(get_range(f), quote(ctx.level, e)))
        }
    }
    | Pi(r, n, t, b) => {
        let elabT = check(ctx, t, VType(r))
        let var   = VStuck(r, Rigid(ctx.level), list{})
        let elabB = check(bind(ctx, n.iVal, var, eval(ctx, elabT)), b, VType(r))
        (Term.Pi(r, n.iVal, elabT, elabB), VType(r))
    }
    | Let(r, n, t, v, b) => {
        let elabT = check(ctx, t, VType(r))
        let elabV = check(ctx, v, eval(ctx, elabT))
        print_endline("Quoted '" ++ Term.print_term(list{}, elabT) ++ "'")
        let (elabB, resTy) = infer(bind(ctx, n.iVal, eval(ctx, elabV), eval(ctx, elabT)), b)
        (Term.Let(r, n.iVal, elabT, elabV, elabB), resTy)
    }
    }
