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
    source: string,
    tokens: array<PositionToken.t>,
    index: int,
    wordOffset: int,
    line: int,
    column: int,
  }
  let make = source => {
    source: source,
    tokens: [],
    index: 0,
    wordOffset: 0,
    line: 1,
    column: 1,
  }
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
  let toWord = cursor =>
    Js.String.substrAtMost(~from=cursor.index, ~length=cursor.wordOffset, cursor.source)
  let appendWord = cursor =>
    switch toWord(cursor) {
    | "" => cursor.tokens
    | word => appendToken(cursor, Identifier(word))
    }
  let commitToken = (cursor, token) => {
    ...cursor,
    tokens: appendToken(cursor, token),
    index: cursor.index + cursor.wordOffset,
    wordOffset: 0,
  }
  let nextLine = cursor => {
    ...cursor,
    tokens: appendWord(cursor),
    index: cursor.index + 1,
    wordOffset: 0,
    line: cursor.line + 1,
    column: 1,
  }
  let advance = cursor => {
    ...cursor,
    index: cursor.index + 1,
    wordOffset: 0,
    column: cursor.column + 1,
  }
  let lookAhead = cursor => {
    ...cursor,
    index: cursor.index + 1,
    wordOffset: cursor.wordOffset + 1,
    column: cursor.column + 1,
  }
}
let rec scan = (cursor: Cursor.t) => {
  switch Cursor.toWord(cursor) {
  | "" => cursor.tokens
  | " " | "\t" => cursor->Cursor.advance->scan
  | "\n" | "\r" => cursor->Cursor.nextLine->scan
  | x =>
    switch Token.fromString(x) {
    | Identifier(_) => cursor->Cursor.lookAhead->scan
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
let output = input->Cursor.make->scan
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
