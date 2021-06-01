open Belt

let debug = true
module Token = {
  type timeUnit = Seconds | Milliseconds | Minutes
  type specialEvent = Entry | Exit
  type t =
    | OpenCurly
    | CloseCurly
    | OpenParens
    | CloseParens
    | State
    | Arrow
    | Initial
    | Machine
    | Use
    | Delay
    | Invoke
    | Assign
    | On
    | Action
    | Guard
    | Equals
    | Spawn
    | Send
    | Timeframe(timeUnit, int)
    | SpecialEvent(specialEvent)
    | String(string)
    | Word(string)
    | Symbol(string)
    | Unexpected(string)
    | Number(int)
  let fromString = str =>
    switch str {
    | "{" => OpenCurly
    | "}" => CloseCurly
    | "(" => OpenParens
    | ")" => CloseParens
    | "=" => Equals
    | "=>" => Arrow
    | "on" => On
    | "use" => Use
    | "delay" => Delay
    | "invoke" => Invoke
    | "assign" => Assign
    | "state" => State
    | "action" => Action
    | "guard" => Guard
    | "spawn" => Spawn
    | "send" => Send
    | "initial" => Initial
    | "machine" => Machine
    | "@entry" => SpecialEvent(Entry)
    | "@exit" => SpecialEvent(Exit)
    | ""
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
    | "*"
    | "&"
    | "^"
    | "%"
    | "$"
    | "#"
    | "@"
    | "!" =>
      Unexpected(str)
    | s => Word(s)
    }
  let toString = token =>
    switch token {
    | OpenCurly => "OpenCurly"
    | CloseCurly => "CloseCurly"
    | OpenParens => "OpenParens"
    | CloseParens => "CloseParens"
    | Machine => "Machine"
    | Use => "Use"
    | Delay => "Delay"
    | Invoke => "Invoke"
    | Assign => "Assign"
    | On => "On"
    | Action => "Action"
    | Guard => "Guard"
    | Equals => "Equals"
    | Spawn => "Spawn"
    | Send => "Send"
    | State => "State"
    | Initial => "Initial"
    | Arrow => "Arrow"
    | Timeframe(Milliseconds, ms) => `Timeframe(MS, ${ms->Int.toString}]`
    | Timeframe(Seconds, s) => `Timeframe(S, ${s->Int.toString}]`
    | Timeframe(Minutes, min) => `Timeframe(MIN, ${min->Int.toString}]`
    | SpecialEvent(Entry) => "SpecialEvent(Entry)"
    | SpecialEvent(Exit) => "SpecialEvent(Exit)"
    | String(str) => `String(${str})`
    | Symbol(symb) => `Symbol(${symb})`
    | Number(num) => `Number(${num->Int.toString})`
    | Word(str) => `Word(${str})`
    | Unexpected(str) => `Unexpected(${str})`
    }
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
module Grammar = {
  type t
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
  let toWord = cursor =>
    Js.String.substrAtMost(~from=cursor.index, ~length=cursor.wordOffset + 1, cursor.source)
  let toCharacter = cursor =>
    Js.String.substrAtMost(~from=cursor.index + cursor.wordOffset, ~length=1, cursor.source)
  let trace = (name, fn, cursor) => {
    let res = fn(cursor)
    if debug {
      Js.log(`[${name}]`)
    }
    res
  }
  let trace2 = (name, fn, cursor, cursorToToken: t => Token.t) => {
    let token = cursorToToken(cursor)
    let res = fn(cursor, token)
    if debug {
      Js.log(`[${name}] ${Token.toString(token)}`)
      Js.log(`Current character: ${toCharacter(cursor)}`)
      Js.log(`Current word: ${toWord(cursor)}`)
      Js.log(
        `index: ${Int.toString(cursor.index)} — wordOffset: ${Int.toString(cursor.wordOffset)}`,
      )
      Js.log(`------------------------------------`)
    }
    res
  }
  let commitWith = (cursor, cursorToToken: t => Token.t) => {
    ...cursor,
    tokens: Array.concat(
      cursor.tokens,
      [PositionToken.make(~value=cursorToToken(cursor), ~line=cursor.line, ~column=cursor.column)],
    ),
    wordOffset: 0,
    index: cursor.index + cursor.wordOffset + 1,
    column: cursor.column + cursor.wordOffset + 1,
  }
  let nextLine = trace("nextLine", cursor => {
    ...cursor,
    index: cursor.index + 1,
    wordOffset: 0,
    line: cursor.line + 1,
    column: 1,
  })
  let advance = trace("advance", cursor => {
    ...cursor,
    index: cursor.index + 1,
    wordOffset: 0,
    column: cursor.column + 1,
  })
  let lookahead = trace("lookahead", cursor => {
    ...cursor,
    wordOffset: cursor.wordOffset + 1,
  })
}
let alpha = str => Js.Re.test_(%re("/^[A-Z_]+$/i"), str)
let alphanumeric = str => Js.Re.test_(%re("/^\w+$/"), str)
let numeric = str => Js.Re.test_(%re("/^\d+$/"), str)
let rec scanWhile = (cursor, predicate) =>
  if cursor->Cursor.lookahead->Cursor.toCharacter->predicate {
    cursor->Cursor.lookahead->scanWhile(predicate)
  } else {
    cursor
  }
let rec scan = cursor => {
  switch Cursor.toCharacter(cursor) {
  | "" => cursor.tokens
  | " " | "\t" => cursor->Cursor.advance->scan
  | "\n" | "\r" => cursor->Cursor.nextLine->scan
  | "=" as lexeme =>
    switch cursor->Cursor.lookahead->Cursor.toCharacter {
    | ">" => cursor->Cursor.lookahead->Cursor.commitWith(_c => Arrow)->scan
    | _ => cursor->Cursor.commitWith(_c => Unexpected(lexeme))->scan
    }
  | "\"" as quote | "'" as quote =>
    cursor
    ->scanWhile(char => char !== quote)
    ->Cursor.commitWith(c => String(c->Cursor.toWord))
    ->scan
  | "@" as char
  | char if alpha(char) =>
    cursor
    ->scanWhile(alphanumeric)
    ->Cursor.commitWith(c => c->Cursor.toWord->Token.fromString)
    ->scan
  | char if numeric(char) =>
    cursor->scanWhile(numeric)->Cursor.commitWith(c => c->Cursor.toWord->Token.fromString)->scan
  | _ => cursor->Cursor.commitWith(c => c->Cursor.toWord->Token.fromString)->scan
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
if debug {
  Js.log(`-------------[Tokens]---------------`)
  Js.log(output)
  Js.log(`------------------------------------`)
}
