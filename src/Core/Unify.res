open Value
open Context

exception SurfaceUnifyMismatch()
exception UnifyMismatch(Context.context, value, value)

let rec unify_spine = (ctx, a, b) => 
    switch (a,b) {
    | (list{}, list{}) => ()
    | (list{x, ...xs}, list{y, ...ys}) => loop_unify(ctx, x, y); unify_spine(ctx, xs, ys)
    | _ => raise(SurfaceUnifyMismatch)
    }

and loop_unify = (depth, left, right) => {
    switch (left, right) {
    | (VType, VType) => ()
    | (VPi(_, t, v), VPi(_, t', v')) => 
        loop_unify(depth, t, t');
        loop_unify(depth + 1, v(VStuck(Rigid(depth), list{})), v'(VStuck(Rigid(depth), list{})))
    | (VLam(_, v), VLam(_, v')) => 
        loop_unify(depth + 1, v(VStuck(Rigid(depth), list{})), v'(VStuck(Rigid(depth), list{})))
    | (VStuck(Rigid(x), s), VStuck(Rigid(x'), s')) if x == x' =>
        unify_spine(depth, s, s')
    | _ => 
        raise(SurfaceUnifyMismatch)
    }
}
and unify = (ctx, left, right) => {
    try {
        loop_unify(ctx.level, left, right)
    } catch {
    | SurfaceUnifyMismatch => raise(UnifyMismatch(ctx, left, right))
    }
}