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
and dot = char '.'
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
and lcurl = wchar '{'
(*| RCURL: '}' { if (!_modeStack.isEmpty()) { popMode(); } }; |*)
and rcurl = wchar '}'
(*| MULT: '*';                                                |*)
and mult = wfstring "*"
(*| MOD: '%';                                                 |*)
and mod' = wfstring "%"
(*| DIV: '/';                                                 |*)
and div = wfstring "/"
(*| ADD: '+';                                                 |*)
and add = wfstring "+"
(*| SUB: '-';                                                 |*)
and sub = wfstring "-"
(*| INCR: '++';                                               |*)
and incr = wfstring "++"
(*| DECR: '--';                                               |*)
and decr = wfstring "--"
(*| CONJ: '&&';                                               |*)
and conj = wfstring "&&"
(*| DISJ: '||';                                               |*)
and disj = wfstring "||"
(*| EXCL_WS: '!' Hidden;                                      |*)
and exclWs = string "!" <* anyspace
(*| EXCL_NO_WS: '!';                                          |*)
and exclNoWs = string "!"
(*| COLON: ':';                                               |*)
and colon = char ':'
(*| SEMICOLON: ';';                                           |*)
and semicolon = wchar ';'
(*| ASSIGNMENT: '=';                                          |*)
and assignment = wfstring "="
(*| ADD_ASSIGNMENT: '+=';                                     |*)
and addAssignment = wfstring "+="
(*| SUB_ASSIGNMENT: '-=';                                     |*)
and subAssignment = wfstring "-="
(*| MULT_ASSIGNMENT: '*=';                                    |*)
and multAssignment = wfstring "*="
(*| DIV_ASSIGNMENT: '/=';                                     |*)
and divAssignment = wfstring "/="
(*| MOD_ASSIGNMENT: '%=';                                     |*)
and modAssignment = wfstring "%="
(*| ARROW: '->';                                              |*)
and arrow = wfstring "->"
(*| DOUBLE_ARROW: '=>';                                       |*)
(*| RANGE: '..';                                              |*)
and range = wfstring ".."
(*| COLONCOLON: '::';                                         |*)
and colonColon = wfstring "::"
(*| DOUBLE_SEMICOLON: ';;';                                   |*)
(*| HASH: '#';                                                |*)
(*| AT: '@';                                                  |*)
(*| QUEST_WS: '?' Hidden;                                     |*)
and questWs = string "?" <* anyspace
(*| QUEST_NO_WS: '?';                                         |*)
and questNoWs = string "?"
(*| LANGLE: '<';                                              |*)
and langle = wchar '<'
(*| RANGLE: '>';                                              |*)
and rangle = wchar '>'
(*| LE: '<=';                                                 |*)
and le = wfstring "<="
(*| GE: '>=';                                                 |*)
and ge = wfstring ">="
(*| EXCL_EQ: '!=';                                            |*)
and exclEq = wfstring "!="
(*| EXCL_EQEQ: '!==';                                         |*)
and exclEqEq = wfstring "!=="
(*| AS_SAFE: 'as?';                                           |*)
and asSafe = wfstring "as?"
(*| EQEQ: '==';                                               |*)
and eqEq = wfstring "=="
(*| EQEQEQ: '===';                                            |*)
and eqEqEq = wfstring "==="
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
and val' = wstring "val"
(*| VAR: 'var';                 |*)
and var = wstring "var"
(*| TYPE_ALIAS: 'typealias';    |*)
(*| CONSTRUCTOR: 'constructor'; |*)
and constructor = wstring "constructor"
(*| BY: 'by';                   |*)
and by = wstring "by"
(*| COMPANION: 'companion';     |*)
(*| INIT: 'init';               |*)
(*| THIS: 'this';               |*)
(*| SUPER: 'super';             |*)
(*| TYPEOF: 'typeof';           |*)
(*| WHERE: 'where';             |*)
and where = wstring "where"
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
(*| AS: 'as';                   |*)
and as' = wstring "as"
(*| IS: 'is';                   |*)
and is = wstring "is"
(*| IN: 'in';                   |*)
and in' = wstring "in"
(*| NOT_IS: '!is' (Hidden | NL);|*)
and notIs = wstring "!is"
(*| NOT_IN: '!in' (Hidden | NL);|*)
and notIn = wstring "!in"
(*| OUT: 'out';                 |*)
and out = wstring "out"
(*| GETTER: 'get';              |*)
(*| SETTER: 'set';              |*)
(*| DYNAMIC: 'dynamic';         |*)
and dynamic = wstring "dynamic"

(*| SECTION: lexicalModifiers |*)

(*| PUBLIC: 'public';           |*)
and public = wstring "public"
(*| PRIVATE: 'private';         |*)
and private' = wstring "private"
(*| PROTECTED: 'protected';     |*)
and protected = wstring "protected"
(*| INTERNAL: 'internal';       |*)
and internal = wstring "internal"
(*| ENUM: 'enum';               |*)
and enum = wstring "enum"
(*| SEALED: 'sealed';           |*)
and sealed = wstring "sealed"
(*| ANNOTATION: 'annotation';   |*)
and annotation = wstring "annotation"
(*| DATA: 'data';               |*)
and data = wstring "data"
(*| INNER: 'inner';             |*)
and inner = wstring "inner"
(*| TAILREC: 'tailrec';         |*)
and tailrec = wstring "tailrec"
(*| OPERATOR: 'operator';       |*)
and operator = wstring "operator"
(*| INLINE: 'inline';           |*)
and inline = wstring "inline"
(*| INFIX: 'infix';             |*)
and infix = wstring "infix"
(*| EXTERNAL: 'external';       |*)
and external' = wstring "external"
(*| SUSPEND: 'suspend';         |*)
and suspend = wstring "suspend"
(*| OVERRIDE: 'override';       |*)
and override = wstring "override"
(*| ABSTRACT: 'abstract';       |*)
and abstract = wstring "abstract"
(*| FINAL: 'final';             |*)
and final = wstring "final"
(*| OPEN: 'open';               |*)
and open' = wstring "open"
(*| CONST: 'const';             |*)
and const = wstring "const"
(*| LATEINIT: 'lateinit';       |*)
and lateinit = wstring "lateinit"
(*| VARARG: 'vararg';           |*)
and vararg = wstring "vararg"
(*| NOINLINE: 'noinline';       |*)
and noinline = wstring "noinline"
(*| CROSSINLINE: 'crossinline'; |*)
and crossinline = wstring "crossinline"
(*| REIFIED: 'reified';         |*)
and reified = wstring "reified"
(*| EXPECT: 'expect';           |*)
and expect = wstring "expect"
(*| ACTUAL: 'actual';           |*)
and actual = wstring "actual"

(*| SECTION: lexicalIdentifiers |*)

(*| fragment UnicodeDigit: UNICODE_CLASS_ND; |*)
and unicodeDigit () =
  unicode_class_nd ()

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

(*| IdentifierOrSoftKey       |*)
(*|     : Identifier          |*)
(*|     /* Soft keywords */   |*)
(*|     | ABSTRACT            |*)
(*|     | ANNOTATION          |*)
(*|     | BY                  |*)
(*|     | CATCH               |*)
(*|     | COMPANION           |*)
(*|     | CONSTRUCTOR         |*)
(*|     | CROSSINLINE         |*)
(*|     | DATA                |*)
(*|     | DYNAMIC             |*)
(*|     | ENUM                |*)
(*|     | EXTERNAL            |*)
(*|     | FINAL               |*)
(*|     | FINALLY             |*)
(*|     | GETTER              |*)
(*|     | IMPORT              |*)
(*|     | INFIX               |*)
(*|     | INIT                |*)
(*|     | INLINE              |*)
(*|     | INNER               |*)
(*|     | INTERNAL            |*)
(*|     | LATEINIT            |*)
(*|     | NOINLINE            |*)
(*|     | OPEN                |*)
(*|     | OPERATOR            |*)
(*|     | OUT                 |*)
(*|     | OVERRIDE            |*)
(*|     | PRIVATE             |*)
(*|     | PROTECTED           |*)
(*|     | PUBLIC              |*)
(*|     | REIFIED             |*)
(*|     | SEALED              |*)
(*|     | TAILREC             |*)
(*|     | SETTER              |*)
(*|     | VARARG              |*)
(*|     | WHERE               |*)
(*|     | EXPECT              |*)
(*|     | ACTUAL              |*)
(*|     /* Strong keywords */ |*)
(*|     | CONST               |*)
(*|     | SUSPEND             |*)
(*|     ;                     |*)
and identifierOrSoftKey () = (* TODO *)
  pos >>= fun pos -> identifier' pos

(*| IdentifierAt                  |*)
(*|     : IdentifierOrSoftKey '@' |*)
(*|     ;                         |*)
and identifierAt () =
  identifierOrSoftKey () <* char '@'

(*| SECTION: characters |*)

and letter' () =
  unicode_class_ll ()
  <!> unicode_class_lm
  <!> unicode_class_lo
  <!> unicode_class_lt
  <!> unicode_class_lu
  <!> unicode_class_nl
