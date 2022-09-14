open Value

type context = {
    names: Belt.Map.String.t<(value, int)>,
    env: list<value>,
    level: int
}

let empty_ctx = {names: Belt.Map.String.empty, env: list{}, level: 0}

let bind = (ctx, name, val) => 
    { names: Belt.Map.String.set(ctx.names, name, (val, ctx.level))
    , env: list{VStuck(Rigid(ctx.level), list{}), ...ctx.env}
    , level: ctx.level + 1
    }

let bind_val = (ctx, val) => 
    { ...ctx
    , env: list{val, ...ctx.env}
    , level: ctx.level + 1
    }
