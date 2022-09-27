open Value
open Context

exception SurfaceUnifyMismatch()
exception UnifyMismatch(Context.context, Location.range, value, value)

let rec unify_spine = (ctx, a, b) => 
    switch (a,b) {
    | (list{}, list{}) => ()
    | (list{x, ...xs}, list{y, ...ys}) => loop_unify(ctx, x, y); unify_spine(ctx, xs, ys)
    | _ => raise(SurfaceUnifyMismatch)
    }

and loop_unify = (depth, left, right) => {
    switch (left,  right) {
    | (VType(_), VType(_)) => ()
    | (VPi(r, _, t, v), VPi(_, _, t', v')) => 
        loop_unify(depth, t, t');
        loop_unify(depth + 1, v(VStuck(r, Rigid(depth), list{})), v'(VStuck(r, Rigid(depth), list{})))
    | (VLam(r, _, v), VLam(_, _, v')) => 
        loop_unify(depth + 1, v(VStuck(r, Rigid(depth), list{})), v'(VStuck(r, Rigid(depth), list{})))
    | (VStuck(_, Rigid(x), s), VStuck(_, Rigid(x'), s')) if x == x' =>
        unify_spine(depth, s, s')
    | (VStuck(_, Top(x), s), VStuck(_, Top(x'), s')) if x == x' =>
        unify_spine(depth, s, s')
    | (VStuck(_, Top(_), _s), t) =>
        loop_unify(depth, failwith("Unimplemented"), t)
    | (t, VStuck(_, Top(_), _s)) =>
        loop_unify(depth, t, failwith("Unimplemented"))
    | _ => 
        raise(SurfaceUnifyMismatch)
    }
}
and unify = (ctx, r, left, right) => {
    try {
        loop_unify(ctx.level, left, right)
    } catch {
    | SurfaceUnifyMismatch => raise(UnifyMismatch(ctx, r, left, right))
    }
}