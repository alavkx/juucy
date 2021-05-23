open Belt

Js.log("Hello, World!")

let input = `
  state enabled {
    toggle => disabled
  }

  initial state disabled {
    toggle => enabled
  }
`
type bracket = Open | Close
type keyword = State | Enabled | Arrow | Initial
type token =
  Word(string) | Number(string) | Curly(bracket) | Keyword(keyword) | Unid(string) | Space
let rec lexer = (chars, tokens: array<token>) => {
  let tail = Array.sliceToEnd(chars, 1)
  switch chars[0] {
  | None => tokens
  | Some("") => tokens
  | Some("{") => lexer(tail, Array.concat(tokens, [Curly(Open)]))
  | Some("}") => lexer(tail, Array.concat(tokens, [Curly(Close)]))
  | Some("\n") => lexer(tail, Array.concat(tokens, [Space]))
  | Some(" ") => lexer(tail, Array.concat(tokens, [Space]))
  | Some(char) => lexer(tail, Array.concat(tokens, [Unid(char)]))
  }
}
let rec parser = (tokens: array<token>) => {
  let head = Array.split(tokens, Space)
  let tail = Array.sliceToEnd(tokens, Array.length(head))
  switch head {
  | None => tsb
  | Some(Space) => parser(tail)
  | Some(Unid("state")) => Keyword(State)
  | Some(Unid("enabled")) => Keyword(Enabled)
  | Some(Unid("initial")) => Keyword(Initial)
  | Some(Unid("=>")) => Keyword(Arrow)
  | Some(Unid(sumn)) => Word(word)
  }
}
