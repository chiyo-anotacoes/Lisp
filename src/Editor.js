Split(['#split-0', '#split-1'])

/*

let ℕ : ★     
 = Π nat: ★. (nat → nat) → nat → nat
in

let z : ℕ     
 = λt. λs. λz. z
in

let s : ℕ → ℕ 
 = λn. λt. λs. λz. s (n t s z)
in

let a : ★ → Nat 
  = λx. 2
in

(a 3123)

*/
let changeEvent;

let editor = CodeMirror.fromTextArea(document.getElementById("code-block"), {
    lineNumbers: true,
    matchBrackets: true,
    height: 'auto',
    lineWrapping: true,
    indentUnit: 4,
    mode: "application/x-aspx",
    autoCloseBrackets: true
});

editor.execCommand("closeBrackets");

let flushed = false

let markers = []

editor.on('change', res => {
    markers.forEach(x => x.clear())
    if (changeEvent && !flushed) {
        let rest = editor.getSearchCursor(/\\(?<name>(?:lambda|star|pi|arr))/)
        let resp = rest.findNext()
        if (resp){
            switch(resp.groups.name){
                case "lambda": rest.replace("λ"); break
                case "star":   rest.replace("★"); break
                case "pi":     rest.replace("Π"); break
                case "arr":    rest.replace("→"); break
            }
        }
        changeEvent(res.getValue())
    } else {
        flushed = false
    }
})

export const setOnChange = (f) => { 
    editor.setValue(localStorage.getItem('ablablabla'))
    changeEvent = f
    f(localStorage.getItem('ablablabla'))
 }

let doc = document.getElementById("result")

export const setRes = (text) => {
    doc.innerHTML = text
}

export const mark = (ranges) => {
    markers = ranges.map(range => 
        editor.getDoc().markText({line:range.start.line-1,ch:range.start.column-1},{line:range.end.line-1,ch:range.end.column-1}, {
            css: "border-bottom: 2px solid red"
        })
    )
}


setInterval(() => {
    localStorage.setItem('ablablabla', editor.getValue());
}, 3000)