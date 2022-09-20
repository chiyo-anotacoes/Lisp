open Value

type context = {
    types: Belt.Map.String.t<(value, int)>,
    values: list<value>,
    level: int
}

let empty_ctx = {types: Belt.Map.String.empty, values: list{}, level: 0}

let bind = (ctx, name, val, ty) => 
    { types: Belt.Map.String.set(ctx.types, name, (ty, ctx.level))
    , values: list{val, ...ctx.values}
    , level: ctx.level + 1
    }

let bind_val = (ctx, val) => 
    { ... ctx 
    , values: list{val, ...ctx.values}
    , level: ctx.level + 1
    }