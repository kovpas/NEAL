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

(*| PACKAGE: 'package'; |*)
let _package = "package"
let package = wstring _package
(*| IMPORT: 'import'; |*)
let _import = "import"
let import = wstring _import
(*| CLASS: 'class';             |*)
let _class' = "class"
let class' = wstring _class'
(*| INTERFACE: 'interface';     |*)
let _interface = "interface"
let interface = wstring _interface
(*| FUN: 'fun';                 |*)
let _fun' = "fun"
let fun' = wstring _fun'
(*| OBJECT: 'object';           |*)
let _object' = "object"
let object' = wstring _object'
(*| VAL: 'val';                 |*)
let _val' = "val"
let val' = wstring _val'
(*| VAR: 'var';                 |*)
let _var = "var"
let var = wstring _var
(*| TYPE_ALIAS: 'typealias';    |*)
let _type_alias = "typealias"
let type_alias = wstring _type_alias
(*| CONSTRUCTOR: 'constructor'; |*)
let _constructor = "constructor"
let constructor = wstring _constructor
(*| BY: 'by';                   |*)
let _by = "by"
let by = wstring _by
(*| COMPANION: 'companion';     |*)
let _companion = "companion"
let companion = wstring _companion
(*| INIT: 'init';               |*)
let _init = "init"
let init = wstring _init
(*| THIS: 'this';               |*)
let _this = "this"
let this = wstring _this
(*| SUPER: 'super';             |*)
let _super = "super"
let super = wstring _super
(*| TYPEOF: 'typeof';           |*)
let _typeof = "typeof"
let typeof = wstring _typeof
(*| WHERE: 'where';             |*)
let _where = "where"
let where = wstring _where
(*| IF: 'if';                   |*)
let _if' = "if"
let if' = wstring _if'
(*| ELSE: 'else';               |*)
let _else' = "else"
let else' = wstring _else'
(*| WHEN: 'when';               |*)
let _when' = "when"
let when' = wstring _when'
(*| TRY: 'try';                 |*)
let _try' = "try"
let try' = wstring _try'
(*| CATCH: 'catch';             |*)
let _catch = "catch"
let catch = wstring _catch
(*| FINALLY: 'finally';         |*)
let _finally = "finally"
let finally = wstring _finally
(*| FOR: 'for';                 |*)
let _for' = "for"
let for' = wstring _for'
(*| DO: 'do';                   |*)
let _do' = "do"
let do' = wstring _do'
(*| WHILE: 'while';             |*)
let _while' = "while"
let while' = wstring _while'
(*| THROW: 'throw';             |*)
let _throw = "throw"
let throw = wstring _throw
(*| RETURN: 'return';           |*)
let _return' = "return"
let return' = wstring _return'
(*| CONTINUE: 'continue';       |*)
let _continue = "continue"
let continue = wstring _continue
(*| BREAK: 'break';             |*)
let _break = "break"
let break = wstring _break
(*| AS: 'as';                   |*)
let _as' = "as"
let as' = wstring _as'
(*| IS: 'is';                   |*)
let _is = "is"
let is = wstring _is
(*| IN: 'in';                   |*)
let _in' = "in"
let in' = wstring _in'
(*| NOT_IS: '!is' (Hidden | NL);|*)
let _notIs = "!is"
let notIs = wstring _notIs
(*| NOT_IN: '!in' (Hidden | NL);|*)
let _notIn = "!in"
let notIn = wstring _notIn
(*| OUT: 'out';                 |*)
let _out = "out"
let out = wstring _out
(*| GETTER: 'get';              |*)
let _getter = "get"
let getter = wstring _getter
(*| SETTER: 'set';              |*)
let _setter = "set"
let setter = wstring _setter
(*| DYNAMIC: 'dynamic';         |*)
let _dynamic = "dynamic"
let dynamic = wstring _dynamic

(*| SECTION: lexicalModifiers |*)

(*| PUBLIC: 'public';           |*)
let _public = "public"
let public = wstring _public
(*| PRIVATE: 'private';         |*)
let _private' = "private"
let private' = wstring _private'
(*| PROTECTED: 'protected';     |*)
let _protected = "protected"
let protected = wstring _protected
(*| INTERNAL: 'internal';       |*)
let _internal = "internal"
let internal = wstring _internal
(*| ENUM: 'enum';               |*)
let _enum = "enum"
let enum = wstring _enum
(*| SEALED: 'sealed';           |*)
let _sealed = "sealed"
let sealed = wstring _sealed
(*| ANNOTATION: 'annotation';   |*)
let _annotation = "annotation"
let annotation = wstring _annotation
(*| DATA: 'data';               |*)
let _data = "data"
let data = wstring _data
(*| INNER: 'inner';             |*)
let _inner = "inner"
let inner = wstring _inner
(*| TAILREC: 'tailrec';         |*)
let _tailrec = "tailrec"
let tailrec = wstring _tailrec
(*| OPERATOR: 'operator';       |*)
let _operator = "operator"
let operator = wstring _operator
(*| INLINE: 'inline';           |*)
let _inline = "inline"
let inline = wstring _inline
(*| INFIX: 'infix';             |*)
let _infix = "infix"
let infix = wstring _infix
(*| EXTERNAL: 'external';       |*)
let _external' = "external"
let external' = wstring _external'
(*| SUSPEND: 'suspend';         |*)
let _suspend = "suspend"
let suspend = wstring _suspend
(*| OVERRIDE: 'override';       |*)
let _override = "override"
let override = wstring _override
(*| ABSTRACT: 'abstract';       |*)
let _abstract = "abstract"
let abstract = wstring _abstract
(*| FINAL: 'final';             |*)
let _final = "final"
let final = wstring _final
(*| OPEN: 'open';               |*)
let _open' = "open"
let open' = wstring _open'
(*| CONST: 'const';             |*)
let _const = "const"
let const = wstring _const
(*| LATEINIT: 'lateinit';       |*)
let _lateinit = "lateinit"
let lateinit = wstring _lateinit
(*| VARARG: 'vararg';           |*)
let _vararg = "vararg"
let vararg = wstring _vararg
(*| NOINLINE: 'noinline';       |*)
let _noinline = "noinline"
let noinline = wstring _noinline
(*| CROSSINLINE: 'crossinline'; |*)
let _crossinline = "crossinline"
let crossinline = wstring _crossinline
(*| REIFIED: 'reified';         |*)
let _reified = "reified"
let reified = wstring _reified
(*| EXPECT: 'expect';           |*)
let _expect = "expect"
let expect = wstring _expect
(*| ACTUAL: 'actual';           |*)
let _actual = "actual"
let actual = wstring _actual

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
and semicolon = char ';'
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
  <* colon

(*| SECTION: lexicalIdentifiers |*)

(*| fragment UnicodeDigit: UNICODE_CLASS_ND; |*)
and unicodeDigit () =
  unicode_class_nd ()

and nonReservedIdentifier pos keywords =
  (
    List.cons <$> (letter' () <|> char '_')
      <*> (option [] (many1
        (fun () ->
          letter' () <|> char '_' <|> unicodeDigit ()
        )
      )
    )
  ) >>= fun chars ->
    match string_of_chars chars with
    | String l when List.mem l keywords ->
        fail "Keyword can't be used as identifier"
    | str ->
        return (NodeHolder (pos, Node ("Identifier", Off pos, [("Value", str)])))

and hardKeywords = [
  _package;
  _import;
  _class';
  _interface;
  _fun';
  _object';
  _val';
  _var;
  _type_alias;
  _constructor;
  _by;
  _companion;
  _init;
  _this;
  _super;
  _typeof;
  _where;
  _if';
  _else';
  _when';
  _try';
  _catch;
  _finally;
  _for';
  _do';
  _while';
  _throw;
  _return';
  _continue;
  _break;
  _as';
  _is;
  _in';
  _notIs;
  _notIn;
  _out;
  _getter;
  _setter;
  _dynamic;
  _public;
  _private';
  _protected;
  _internal;
  _enum;
  _sealed;
  _annotation;
  _data;
  _inner;
  _tailrec;
  _operator;
  _inline;
  _infix;
  _external';
  _suspend;
  _override;
  _abstract;
  _final;
  _open';
  _const;
  _lateinit;
  _vararg;
  _noinline;
  _crossinline;
  _reified;
  _expect;
  _actual;
]

(*| Identifier                                                                            |*)
(*|     : (Letter | '_') (Letter | '_' | UnicodeDigit)*                                   |*)
(*|     | '`' ~([\r\n] | '`' | '.' | ';' | ':' | '\\' | '/' | '[' | ']' | '<' | '>')+ '`' |*)
(*|     ;                                                                                 |*)
and identifier' pos =
  nonReservedIdentifier pos hardKeywords

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