open Value
open Term

type context = {
    types: Belt.Map.String.t<(value, int)>,
    top: Belt.Map.String.t<(term, value)>,
    values: list<value>,
    level: int
}

let empty_ctx = {types: Belt.Map.String.empty, top: Belt.Map.String.empty, values: list{}, level: 0}

let bind = (ctx, name, val, ty) => 
    { ...ctx
    , types: Belt.Map.String.set(ctx.types, name, (ty, ctx.level))
    , values: list{val, ...ctx.values}
    , level: ctx.level + 1
    }

let bind_val = (ctx, val) => 
    { ... ctx 
    , values: list{val, ...ctx.values}
    , level: ctx.level + 1
    }

let add_top = (ctx, name, val, ty) => 
    { ... ctx
    , top: Belt.Map.String.set(ctx.top, name, (val, ty))
    }