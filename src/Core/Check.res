open Tree
open Value
open Unify
open Eval
open Context

exception CannotFindVariable(string, Location.range)
exception CannotInferLambda

let rec check = (ctx, expr, ty) =>
    switch (expr, ty) {
    | (Lam(name, b), VPi(_, t, b')) =>
        let var   = VStuck(Rigid(ctx.level), list{})
        let evalB = check(bind(ctx, name.iVal, var, t), b, b'(var))
        Term.Lam(name.iVal, evalB)
    | (expr, expected) =>
        let (elab, infered) = infer(ctx, expr)
        unify(ctx, infered, expected)
        elab
    }

and infer = (ctx, expr) => 
    switch expr {
    | Type => (Term.Type, VType)
    | Var(n) => 
        switch Belt.Map.String.get(ctx.types, n.iVal) {
        | Some((ty, lvl)) => (Term.Var(ctx.level - lvl - 1), ty)
        | None => raise(CannotFindVariable(n.iVal, n.iPos))
        }
    | Lam(_, _) => failwith("Cant infer lambda")
    | Ann(f, t) => {
        let elabA = check(ctx, t, VType)
        let runT = eval(ctx, elabA)
        let elabF = check(ctx, f, runT)
        (Ann(elabF, elabA), runT)
    }
    | App(f, a) => {
        let (elabF, fTy) = infer(ctx, f)
        switch fTy {
        | VPi(_, t, b') => {
            let elabA = check(ctx, a, t)
            (Term.App(elabF, elabA), b'(eval(ctx, elabA)))
        }
        | _ => failwith("Not a function to be applied")
        }
    }
    | Pi(n, t, b) => {
        let elabT = check(ctx, t, VType)
        let var   = VStuck(Rigid(ctx.level), list{})
        let elabB = check(bind(ctx, n.iVal, var, eval(ctx, elabT)), b, VType)
        (Term.Pi(n.iVal, elabT, elabB), VType)
    }
    | Let(n, t, v, b) => {
        let elabT = check(ctx, t, VType)
        print_endline(Term.print_term(list{}, elabT) ++ " | " ++ print_expr(t))
        let elabV = check(ctx, v, eval(ctx, elabT))
        let (elabB, resTy) = infer(bind(ctx, n.iVal, eval(ctx, elabV), eval(ctx, elabT)), b)
        (Term.Let(n.iVal, elabT, elabV, elabB), resTy)
    }
    }
