open Belt

module Token = {
  type t =
    | OpenCurly
    | CloseCurly
    | State
    | Arrow
    | Initial
    | Identifier(string)
    | Unexpected(string)
  let fromString = str =>
    switch str {
    | "{" => OpenCurly
    | "}" => CloseCurly
    | "state" => State
    | "initial" => Initial
    | "=>" => Arrow
    | ","
    | "<"
    | "."
    | "/"
    | "?"
    | ":"
    | ";"
    | "\""
    | "\'"
    | "["
    | "]"
    | "\\"
    | "|"
    | "+"
    | "-"
    | "_"
    | "("
    | ")"
    | "*"
    | "&"
    | "^"
    | "%"
    | "$"
    | "#"
    | "@"
    | "!" =>
      Unexpected(str)
    | s => Identifier(s)
    }
  let toString = token =>
    switch token {
    | OpenCurly => "{"
    | CloseCurly => "}"
    | State => "state"
    | Initial => "initial"
    | Arrow => "arrow"
    | Identifier(str)
    | Unexpected(str) => str
    }
  let length = token => token->toString->String.length
}
module PositionToken = {
  type t = {
    value: Token.t,
    line: int,
    column: int,
  }
  let make = (~column, ~line, ~value) => {
    value: value,
    line: line,
    column: column,
  }
}
module Cursor = {
  type t = {
    source: array<string>,
    tokens: array<PositionToken.t>,
    index: int,
    line: int,
    column: int,
    word: option<string>,
  }
  let make = source => {
    source: source,
    tokens: [],
    index: 0,
    line: 1,
    column: 1,
    word: None,
  }
  let fromString = str => Js.String.split("", str)->make
  let appendToken = (cursor, token) =>
    Array.concat(
      cursor.tokens,
      [
        PositionToken.make(
          ~value=token,
          ~line=cursor.line - Token.length(token),
          ~column=cursor.column,
        ),
      ],
    )
  let appendWord = cursor =>
    switch cursor.word {
    | None => cursor.tokens
    | Some(word) => appendToken(cursor, Identifier(word))
    }
  let commitToken = (cursor, token) => {
    ...cursor,
    tokens: appendToken(cursor, token),
    index: cursor.index + Token.length(token),
    line: cursor.line + Token.length(token),
    column: cursor.column + Token.length(token),
    word: None,
  }
  let nextLine = cursor => {
    ...cursor,
    tokens: appendWord(cursor),
    index: cursor.index + 2,
    line: cursor.line + 1,
    column: 1,
    word: None,
  }
  let advance = cursor => {
    ...cursor,
    tokens: appendWord(cursor),
    index: cursor.index + 1,
    column: cursor.column + 1,
    word: None,
  }
  let lookAhead = (cursor, word) => {
    ...cursor,
    index: cursor.index + 1,
    column: cursor.column + 1,
    word: Some(word),
  }
}
let rec scan = (cursor: Cursor.t) => {
  let word = switch cursor.word {
  | None => cursor.source[cursor.index]
  | Some(w) =>
    Some(
      cursor.source
      ->Array.slice(~offset=cursor.index - String.length(w), ~len=String.length(w) + 1)
      ->Array.joinWith("", x => x),
    )
  }
  switch word {
  | None => cursor.tokens
  | Some(" " | "\t") => cursor->Cursor.advance->scan
  | Some("\n" | "\r") => cursor->Cursor.nextLine->scan
  | Some(x) =>
    switch Token.fromString(x) {
    | Identifier(word) => cursor->Cursor.lookAhead(word)->scan
    | token => cursor->Cursor.commitToken(token)->scan
    }
  }
}
let report = (line, where, message) => {
  Js.log(`[line: ${line}] Error ${where}: ${message}`)
}
let input = `
  state enabled {
    toggle => disabled
  }

  initial state disabled {
    toggle => enabled
  }
`
let output = input->Cursor.fromString->scan
type debugToken = {
  value: string,
  line: int,
  column: int,
}
output
->Array.map(pt => {
  value: Token.toString(pt.value),
  line: pt.line,
  column: pt.column,
})
->Js.log
