open Belt

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
let rec scan = (source, tokens: array<token>) => {
  let tail = Js.Array.sliceFrom(1, source)
  switch source[0] {
  | None => tokens
  | Some("") => tokens
  | Some("{") => scan(tail, Array.concat(tokens, [Curly(Open)]))
  | Some("}") => scan(tail, Array.concat(tokens, [Curly(Close)]))
  | Some("\n") => scan(tail, Array.concat(tokens, [Space]))
  | Some(" ") => scan(tail, Array.concat(tokens, [Space]))
  | Some(char) => scan(tail, Array.concat(tokens, [Unid(char)]))
  }
}
// let rec parse = (tokens: array<token>) => {
//   let firstSpaceIndex = Array.getIndexBy(tokens, x => x === Space)
//   let head = Array.slice(0, firstSliceIndex)
//   let tail = Array.sliceToEnd(tokens, Array.length(head))
//   switch head {
//   | None => tsb
//   | Some(Space) => parse(tail)
//   | Some(Unid("state")) => Keyword(State)
//   | Some(Unid("enabled")) => Keyword(Enabled)
//   | Some(Unid("initial")) => Keyword(Initial)
//   | Some(Unid("=>")) => Keyword(Arrow)
//   | Some(Unid(sumn)) => Word(word)
//   }
// }
let output = Js.String.split("", input)->scan([])
Js.log(output)
