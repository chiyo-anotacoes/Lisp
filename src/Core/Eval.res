open Term
open Context
open Value

let app = (fn, arg) =>
    switch fn {
    | VStuck(st, spine) => VStuck(st, Belt.List.concat(spine, list{arg}))
    | VLam(_, f)        => f(arg)
    | _                 => failwith("Impossible D:")
    }

let eval = (ctx, term) => {
    let rec loop = (ctx, term) =>
        switch term {
        | Var(n)      => 
            switch Belt.List.get(ctx.values, n) {
            | Some(x) => x
            | None => failwith("Error in de bruijin indices:  " ++ string_of_int(n))
            }
        | Lam(n, v)   => VLam(n, arg => loop(bind_val(ctx, arg), v))
        | App(f, a)   => app(loop(ctx, f), loop(ctx, a))
        | Ann(f, _)   => loop(ctx, f)
        | Pi(n, t, b) => 
            let ty = loop(ctx, t)
            VPi(n, ty, arg => loop(bind_val(ctx, arg), b))
        | Let(_, _, v, b) => loop(bind_val(ctx, loop(ctx, v)), b)
        | Type => VType
    }
    loop(ctx, term)
}
let quote_stuck = (depth, stuck) => 
    switch stuck {
    | Rigid(lvl) => Var(depth - lvl - 1)
    }

let rec quote = (depth, value) =>
    switch value {
    | VLam(n, b)        => Lam(n, quote(depth + 1, b(VStuck(Rigid(depth), list{}))))
    | VPi(n, t, b)      => Pi(n, quote(depth, t), quote(depth + 1, b(VStuck(Rigid(depth), list{}))))
    | VLet(n, v, t, b)  => Let(n, quote(depth, t), quote(depth, v), quote(depth + 1, b(VStuck(Rigid(depth), list{}))))
    | VStuck(st, spine) => Belt.List.reduce(List.map(quote(depth), spine), quote_stuck(depth, st), (x, y) => App(x,y))
    | VType => Type
    }
    