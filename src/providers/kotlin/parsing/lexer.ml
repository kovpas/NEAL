(* Copyright (c) 2019 Uber Technologies, Inc. *)
(* *)
(* Permission is hereby granted, free of charge, to any person obtaining a copy *)
(* of this software and associated documentation files (the "Software"), to deal *)
(* in the Software without restriction, including without limitation the rights *)
(* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell *)
(* copies of the Software, and to permit persons to whom the Software is *)
(* furnished to do so, subject to the following conditions: *)
(* *)
(* The above copyright notice and this permission notice shall be included in *)
(* all copies or substantial portions of the Software. *)
(* *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE *)
(* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, *)
(* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN *)
(* THE SOFTWARE. *)

open Angstrom
open Combinators
open Neal.Absyn
open Unicode

let rec lexer = ()

(*| SECTION: lexicalGeneral |*)

(*| ShebangLine             |*)
(*|     : '#!' ~[\r\n]*     |*)
(*|     ;                   |*)
and shebangLine' _ =
  mkNode "ShebangLine"
  <* string "#!"
  <:> mkProp "Value" (
    pos >>= fun pos ->
      many1(fun () -> satisfy(function
        | '\r' | '\n' -> false
        | _ -> true
      )) >>| fun chars ->
        NodeHolder(pos, string_of_chars chars)
    )

(*| NL: '\n' | '\r' '\n'?; |*)
and nl = linebreak

(*| SECTION: separatorsAndOperations |*)

(*| RESERVED: '...';                                          |*)
(*| DOT: '.';                                                 |*)
and dot = wchar '.'
(*| COMMA: ',';                                               |*)
and comma = wchar ','
(*| LPAREN: '(' -> pushMode(Inside);                          |*)
and lparen = wchar '('
(*| RPAREN: ')';                                              |*)
and rparen = wchar ')'
(*| LSQUARE: '[' -> pushMode(Inside);                         |*)
and lsquare = wchar '['
(*| RSQUARE: ']';                                             |*)
and rsquare = wchar ']'
(*| LCURL: '{' -> pushMode(DEFAULT_MODE);                     |*)
(*| RCURL: '}' { if (!_modeStack.isEmpty()) { popMode(); } }; |*)
(*| MULT: '*';                                                |*)
(*| MOD: '%';                                                 |*)
(*| DIV: '/';                                                 |*)
(*| ADD: '+';                                                 |*)
(*| SUB: '-';                                                 |*)
(*| INCR: '++';                                               |*)
(*| DECR: '--';                                               |*)
(*| CONJ: '&&';                                               |*)
(*| DISJ: '||';                                               |*)
(*| EXCL_WS: '!' Hidden;                                      |*)
(*| EXCL_NO_WS: '!';                                          |*)
(*| COLON: ':';                                               |*)
and colon = wchar ':'
(*| SEMICOLON: ';';                                           |*)
(*| ASSIGNMENT: '=';                                          |*)
(*| ADD_ASSIGNMENT: '+=';                                     |*)
(*| SUB_ASSIGNMENT: '-=';                                     |*)
(*| MULT_ASSIGNMENT: '*=';                                    |*)
(*| DIV_ASSIGNMENT: '/=';                                     |*)
(*| MOD_ASSIGNMENT: '%=';                                     |*)
(*| ARROW: '->';                                              |*)
and arrow = wstring "->"
(*| DOUBLE_ARROW: '=>';                                       |*)
(*| RANGE: '..';                                              |*)
(*| COLONCOLON: '::';                                         |*)
(*| DOUBLE_SEMICOLON: ';;';                                   |*)
(*| HASH: '#';                                                |*)
(*| AT: '@';                                                  |*)
(*| QUEST_WS: '?' Hidden;                                     |*)
(*| QUEST_NO_WS: '?';                                         |*)
and quest = wchar '?'
(*| LANGLE: '<';                                              |*)
and langle = wchar '<'
(*| RANGLE: '>';                                              |*)
and rangle = wchar '>'
(*| LE: '<=';                                                 |*)
(*| GE: '>=';                                                 |*)
(*| EXCL_EQ: '!=';                                            |*)
(*| EXCL_EQEQ: '!==';                                         |*)
(*| AS_SAFE: 'as?';                                           |*)
(*| EQEQ: '==';                                               |*)
(*| EQEQEQ: '===';                                            |*)
(*| SINGLE_QUOTE: '\'';                                       |*)

(*| SECTION: keywords |*)

(*| ANNOTATION_USE_SITE_TARGET_FILE: '@file' NL* COLON; |*)
and annotation_use_site_target_file () =
  mkNode "AnnotationFile"
  <* string "@file"
  <* skip_many nl
  <* colon

(*| PACKAGE: 'package'; |*)
and package = wstring "package"
(*| IMPORT: 'import'; |*)
and import = wstring "import"
(*| CLASS: 'class';             |*)
and class' = wstring "class"
(*| INTERFACE: 'interface';     |*)
and interface = wstring "interface"
(*| FUN: 'fun';                 |*)
(*| OBJECT: 'object';           |*)
(*| VAL: 'val';                 |*)
(*| VAR: 'var';                 |*)
(*| TYPE_ALIAS: 'typealias';    |*)
(*| CONSTRUCTOR: 'constructor'; |*)
(*| BY: 'by';                   |*)
(*| COMPANION: 'companion';     |*)
(*| INIT: 'init';               |*)
(*| THIS: 'this';               |*)
(*| SUPER: 'super';             |*)
(*| TYPEOF: 'typeof';           |*)
(*| WHERE: 'where';             |*)
(*| IF: 'if';                   |*)
(*| ELSE: 'else';               |*)
(*| WHEN: 'when';               |*)
(*| TRY: 'try';                 |*)
(*| CATCH: 'catch';             |*)
(*| FINALLY: 'finally';         |*)
(*| FOR: 'for';                 |*)
(*| DO: 'do';                   |*)
(*| WHILE: 'while';             |*)
(*| THROW: 'throw';             |*)
(*| RETURN: 'return';           |*)
(*| CONTINUE: 'continue';       |*)
(*| BREAK: 'break';             |*)
(*| AS: 'as'; |*)
and as' = wstring "as"
(*| IS: 'is';                   |*)
(*| IN: 'in';                   |*)
and in' = wstring "in"
(*| NOT_IS: '!is' (Hidden | NL);|*)
(*| NOT_IN: '!in' (Hidden | NL);|*)
(*| OUT: 'out';                 |*)
and out = wstring "out"
(*| GETTER: 'get';              |*)
(*| SETTER: 'set';              |*)
(*| DYNAMIC: 'dynamic';         |*)
and dynamic = wstring "dynamic"

(*| SECTION: lexicalModifiers |*)

(*| PUBLIC: 'public';           |*)
(*| PRIVATE: 'private';         |*)
(*| PROTECTED: 'protected';     |*)
(*| INTERNAL: 'internal';       |*)
(*| ENUM: 'enum';               |*)
(*| SEALED: 'sealed';           |*)
(*| ANNOTATION: 'annotation';   |*)
(*| DATA: 'data';               |*)
(*| INNER: 'inner';             |*)
(*| TAILREC: 'tailrec';         |*)
(*| OPERATOR: 'operator';       |*)
(*| INLINE: 'inline';           |*)
(*| INFIX: 'infix';             |*)
(*| EXTERNAL: 'external';       |*)
(*| SUSPEND: 'suspend';         |*)
and suspend = wstring "suspend"
(*| OVERRIDE: 'override';       |*)
(*| ABSTRACT: 'abstract';       |*)
(*| FINAL: 'final';             |*)
(*| OPEN: 'open';               |*)
(*| CONST: 'const';             |*)
(*| LATEINIT: 'lateinit';       |*)
(*| VARARG: 'vararg';           |*)
(*| NOINLINE: 'noinline';       |*)
(*| CROSSINLINE: 'crossinline'; |*)
(*| REIFIED: 'reified';         |*)
and reified = wstring "reified"
(*| EXPECT: 'expect';           |*)
(*| ACTUAL: 'actual';           |*)

(*| SECTION: lexicalIdentifiers |*)

(*| fragment UnicodeDigit: UNICODE_CLASS_ND; |*)
and unicodeDigit () = unicode_class_nd ()

(*| Identifier                                                                            |*)
(*|     : (Letter | '_') (Letter | '_' | UnicodeDigit)*                                   |*)
(*|     | '`' ~([\r\n] | '`' | '.' | ';' | ':' | '\\' | '/' | '[' | ']' | '<' | '>')+ '`' |*)
(*|     ;                                                                                 |*)
and identifier' pos =
  (
    List.cons <$> (letter' () <|> char '_')
      <*> (option [] (many1
        (fun () ->
          letter' () <|> char '_' <|> unicodeDigit ()
        )
      )
    )
  ) >>= fun str ->
    return (NodeHolder (pos, Node ("Identifier", Off pos, [("Value", string_of_chars str)])))

(*| SECTION: characters |*)

and letter' () =
  unicode_class_ll ()
  <!> unicode_class_lm
  <!> unicode_class_lo
  <!> unicode_class_lt
  <!> unicode_class_lu
  <!> unicode_class_nl


