exception NoRoot

Js.log(Belt.Map.String.new())

switch ReactDOM.querySelector("#root_react_element") {
| Some(root) => ReactDOM.render(<Demo />, root)
| None => raise(NoRoot)
}