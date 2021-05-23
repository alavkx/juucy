// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Array = require("rescript/lib/js/belt_Array.js");

var input = "\n  state enabled {\n    toggle => disabled\n  }\n\n  initial state disabled {\n    toggle => enabled\n  }\n";

function scan(_source, _tokens) {
  while(true) {
    var tokens = _tokens;
    var source = _source;
    var tail = source.slice(1);
    var $$char = Belt_Array.get(source, 0);
    if ($$char === undefined) {
      return tokens;
    }
    switch ($$char) {
      case "" :
          return tokens;
      case "\n" :
      case " " :
          _tokens = Belt_Array.concat(tokens, [/* Space */0]);
          _source = tail;
          continue ;
      case "{" :
          _tokens = Belt_Array.concat(tokens, [{
                  TAG: /* Curly */2,
                  _0: /* Open */0
                }]);
          _source = tail;
          continue ;
      case "}" :
          _tokens = Belt_Array.concat(tokens, [{
                  TAG: /* Curly */2,
                  _0: /* Close */1
                }]);
          _source = tail;
          continue ;
      default:
        _tokens = Belt_Array.concat(tokens, [{
                TAG: /* Unid */4,
                _0: $$char
              }]);
        _source = tail;
        continue ;
    }
  };
}

var output = scan(input.split(""), []);

console.log(output);

exports.input = input;
exports.scan = scan;
exports.output = output;
/* output Not a pure module */
