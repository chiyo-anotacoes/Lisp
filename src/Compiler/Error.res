open Location

type syntax_error_kind =
    | UnexpectedToken

type syntax_error = {
    location: range,
    kind: syntax_error_kind
}
