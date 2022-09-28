open Term
open Context
open Value

let app = (fn, arg) =>
    switch fn {
    | VStuck(r, Top("+"), spine) => {
        if Belt.List.length(spine) == 1 {
            let t = Belt.List.reduce(Belt.List.concat(spine, list{arg}), 0, (a, b) => {
                switch b {
                | VNum(_, n1) => a + n1
                | _ => a
                }
            });
            VNum(r, t)
        } else {
            VStuck(r, Top("+"), Belt.List.concat(spine, list{arg}))
        }
    }
    | VStuck(r, st, spine) => VStuck(r, st, Belt.List.concat(spine, list{arg}))
    | VLam(_, _, f)        => f(arg)
    | _                    => failwith("Impossible D:")
    }

let eval = (ctx, term) => {
    let rec loop = (ctx, term) =>
        switch term {
        | Num(r, t) => VNum(r, t)
        | Var(_, n)      => 
            switch Belt.List.get(ctx.values, n) {
            | Some(x) => x
            | None => failwith("Error in de bruijin indices:  " ++ string_of_int(n))
            }
        | Top(r, n) => VStuck(r, Top(n), list{})
        | Lam(r, n, v)   => VLam(r, n, arg => loop(bind_val(ctx, arg), v))
        | App(_, f, a)   => app(loop(ctx, f), loop(ctx, a))
        | Ann(_, f, _)   => loop(ctx, f)
        | Pi(r, n, t, b) => 
            let ty = loop(ctx, t)
            VPi(r, n, ty, arg => loop(bind_val(ctx, arg), b))
        | Let(_, _, _, v, b) => loop(bind_val(ctx, loop(ctx, v)), b)
        | Type(r) => VType(r)
    }
    loop(ctx, term)
}
let quote_stuck = (range, depth, stuck) => 
    switch stuck {
    | Rigid(lvl) => Var(range, depth - lvl - 1)
    | Top(res) => Term.Top(range, res)
    }

let rec quote = (depth, value) =>
    switch value {
    | VNum(r, t) => Num(r, t)
    | VLam(r, n, b)        => Lam(r, n, quote(depth + 1, b(VStuck(r, Rigid(depth), list{}))))
    | VPi(r, n, t, b)      => Pi(r, n, quote(depth, t), quote(depth + 1, b(VStuck(r, Rigid(depth), list{}))))
    | VLet(r, n, v, t, b)  => Let(r, n, quote(depth, t), quote(depth, v), quote(depth + 1, b(VStuck(r, Rigid(depth), list{}))))
    | VStuck(r, st, spine) => Belt.List.reduce(List.map(quote(depth), spine), quote_stuck(r, depth, st), (x, y) => App(Location.mix(Term.get_range(x), Term.get_range(y)), x, y))
    | VType(r)             => Type(r)
    }
    