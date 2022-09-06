open Term
open Value

let app = (fn, arg) =>
    switch fn {
    | VStuck(st, spine) => VStuck(st, Belt.List.concat(spine, list{arg}))
    | VLam(_, f)        => f(arg)
    | _                 => failwith("Impossible D:")
    }

let rec eval = (ctx, term) =>
    switch term {
    | Var(n)      => 
        switch Belt.List.get(ctx, n) {
        | Some(x) => x
        | None => failwith("LOOOOL  " ++ string_of_int(n))
        }
    | Lam(n, v)   => VLam(n, arg => eval(list{arg, ...ctx}, v))
    | App(f, a)   => app(eval(ctx, f), eval(ctx, a))
    | Pi(n, t, b) => VPi(n, eval(ctx, t), arg => eval(list{arg, ...ctx}, b))
    | Let(_, _, v, b) => eval(list{eval(ctx, v), ...ctx}, b)
    | Type => VType
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
    