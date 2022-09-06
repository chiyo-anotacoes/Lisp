open Value

let rec unify_spine = (ctx, a, b) => 
    switch (a,b) {
    | (list{}, list{}) => ()
    | (list{x, ...xs}, list{y, ...ys}) => unify(ctx, x, y); unify_spine(ctx, xs, ys)
    | _ => failwith("Unify mismatch on spine lol")
    }

and unify = (depth, left, right) => {
    switch (left, right) {
    | (VType, VType) => ()
    | (VPi(_, t, v), VPi(_, t', v')) => 
        unify(depth, t, t');
        unify(depth + 1, v(VStuck(Rigid(depth), list{})), v'(VStuck(Rigid(depth), list{})))
    | (VLam(_, v), VLam(_, v')) => 
        unify(depth + 1, v(VStuck(Rigid(depth), list{})), v'(VStuck(Rigid(depth), list{})))
    | (VStuck(Rigid(x), s), VStuck(Rigid(x'), s')) if x == x' =>
        unify_spine(depth, s, s')
    | _ => 
        failwith("Unify Mismatch!")
    }
}