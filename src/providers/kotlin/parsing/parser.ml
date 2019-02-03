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
open Lexer
open Neal.Absyn

let rec grammar = ()

(*| SECTION: general |*)

(*| kotlinFile                                                                          |*)
(*|     : shebangLine? NL* fileAnnotation* packageHeader importList topLevelObject* EOF |*)
(*|     ;                                                                               |*)
and kotlinFile () = 
  mkList1 (fun () -> (* TODO - how to parse in order? *)
    fix shebangLine (* TODO - should be first line in the file and should have exactly one of these*)
    <|> fix fileAnnotation
    <|> fix packageHeader (* TODO - exactly one? *)
    <|> fix importList
    <|> fix topLevelObject
  ) >>= fun stm ->
    pos >>= fun pos ->
      (
        anyspace *> end_of_input <?> (" at offset " ^ string_of_int pos ^ ", expected")
      ) *> return stm

(*| script                                                                                |*)
(*|     : shebangLine? NL* fileAnnotation* packageHeader importList (statement semi)* EOF |*)
(*|     ;                                                                                 |*)
(* and script () =  *)

(*| shebangLine                                                                                             |*)
(*|     : ShebangLine NL+ |*)
(*|     ;                                                                                                      |*)
and shebangLine _ = 
  shebangLine' ()
  <* skip_many1 (fix nl)

(*| fileAnnotation                                                                                             |*)
(*|     : ANNOTATION_USE_SITE_TARGET_FILE NL* (LSQUARE unescapedAnnotation+ RSQUARE | unescapedAnnotation) NL* |*)
(*|     ;                                                                                                      |*)
and fileAnnotation _ = 
  annotation_use_site_target_file ()
  <* skip_many (fix nl)
  <:> (
    (lsquare *> mkProp "Annotations" (mkList1 (fun () -> unescapedAnnotation () <* anyspace)) <* rsquare)
    <|> mkPropE "Annotation" unescapedAnnotation (* TODO - make a list *)
  )
  <* skip_many (fix nl)

(*| packageHeader                     |*)
(*|     : (PACKAGE identifier semi?)? |*)
(*|     ;                             |*)
and packageHeader _ =
  mkNode "PackageHeader"
  <* package
  <:> mkPropE "Name" identifier
  <* optSemi

(*| importList          |*)
(*|     : importHeader* |*)
(*|     ;               |*)
and importList _ =
  mkNode "ImportList"
  <:> mkPropE "Value" (fun () -> mkList1 importHeader)

(*| importHeader                                            |*)
(*|     : IMPORT identifier (DOT MULT | importAlias)? semi? |*)
(*|     ;                                                   |*)
and importHeader () =
  mkNode "ImportHeader"
  <* import
  <:> mkPropE "Name" identifier
  <:> mkOptPropE "Alias" importAlias
  <* optSemi

(*| importAlias               |*)
(*|     : AS simpleIdentifier |*)
(*|     ;                     |*)
and importAlias () =
  as' *> identifier ()

(*| topLevelObject           |*)
(*|     : declaration semis? |*)
(*|     ;                    |*)
and topLevelObject () =
  declaration ()
  <* optSemi

(*| declaration               |*)
(*|     : classDeclaration    |*)
(*|     | objectDeclaration   |*)
(*|     | functionDeclaration |*)
(*|     | propertyDeclaration |*)
(*|     | typeAlias           |*)
(*|     ;                     |*)

(*| SECTION: classes |*)

(*| SECTION: classMembers |*)

(*| SECTION: enumClasses |*)

(*| SECTION: types |*)

(*| userType                                           |*)
(*|     : simpleUserType (NL* DOT NL* simpleUserType)* |*)
(*|     ;                                              |*)
and userType () =
  mkList1 (simpleUserType) (* TODO *)

(*| simpleUserType                              |*)
(*|     : simpleIdentifier (NL* typeArguments)? |*)
(*|     ;                                       |*)
and simpleUserType () = 
  identifier ()  (* TODO *)

(*| SECTION: statements |*)

(*| valueArguments                                                           |*)
(*|     : LPAREN NL* RPAREN                                                  |*)
(*|     | LPAREN NL* valueArgument (NL* COMMA NL* valueArgument)* NL* RPAREN |*)
(*|     ;                                                                    |*)
and valueArguments () =
  lparen 
  *> skip_many (fix nl) 
  *> mkOpt (commaSep valueArgument) 
  <* skip_many (fix nl) 
  <* rparen

(*| valueArgument                                                                      |*)
(*|     : annotation? NL* (simpleIdentifier NL* ASSIGNMENT NL* )? MULT? NL* expression |*)
(*|     ;                                                                              |*)
and valueArgument () = 
  identifier ()  (* TODO *)

(*| SECTION: expressions |*)

(*| SECTION: modifiers |*)

(*| SECTION: annotations |*)

(*| annotation                                                             |*)
(*|     : (singleAnnotation | multiAnnotation) NL*                         |*)
(*|     ;                                                                  |*)

(*| singleAnnotation                                                       |*)
(*|     : annotationUseSiteTarget NL* unescapedAnnotation                  |*)
(*|     | AT unescapedAnnotation                                           |*)
(*|     ;                                                                  |*)

(*| multiAnnotation                                                        |*)
(*|     : annotationUseSiteTarget NL* LSQUARE unescapedAnnotation+ RSQUARE |*)
(*|     | AT LSQUARE unescapedAnnotation+ RSQUARE                          |*)
(*|     ;                                                                  |*)

(*| annotationUseSiteTarget                                                |*)
(*|     : ANNOTATION_USE_SITE_TARGET_FIELD                                 |*)
(*|     | ANNOTATION_USE_SITE_TARGET_PROPERTY                              |*)
(*|     | ANNOTATION_USE_SITE_TARGET_GET                                   |*)
(*|     | ANNOTATION_USE_SITE_TARGET_SET                                   |*)
(*|     | ANNOTATION_USE_SITE_TARGET_RECEIVER                              |*)
(*|     | ANNOTATION_USE_SITE_TARGET_PARAM                                 |*)
(*|     | ANNOTATION_USE_SITE_TARGET_SETPARAM                              |*)
(*|     | ANNOTATION_USE_SITE_TARGET_DELEGATE                              |*)
(*|     ;                                                                  |*)

(*| unescapedAnnotation        |*)
(*|    : constructorInvocation |*)
(*|    | userType              |*)
(*|    ;                       |*)
and unescapedAnnotation () = 
  constructorInvocation ()
  <!> userType

(*| constructorInvocation         |*)
(*|     : userType valueArguments |*)
(*|     ;                         |*)
and constructorInvocation () = 
  mkNode "ConstructorInvocation"
  <:> mkPropE "Identifier" identifier

(*| SECTION: identifiers |*)

(*| simpleIdentifier: Identifier |*)
(*|     | ABSTRACT               |*)
(*|     | ANNOTATION             |*)
(*|     | BY                     |*)
(*|     | CATCH                  |*)
(*|     | COMPANION              |*)
(*|     | CONSTRUCTOR            |*)
(*|     | CROSSINLINE            |*)
(*|     | DATA                   |*)
(*|     | DYNAMIC                |*)
(*|     | ENUM                   |*)
(*|     | EXTERNAL               |*)
(*|     | FINAL                  |*)
(*|     | FINALLY                |*)
(*|     | GETTER                 |*)
(*|     | IMPORT                 |*)
(*|     | INFIX                  |*)
(*|     | INIT                   |*)
(*|     | INLINE                 |*)
(*|     | INNER                  |*)
(*|     | INTERNAL               |*)
(*|     | LATEINIT               |*)
(*|     | NOINLINE               |*)
(*|     | OPEN                   |*)
(*|     | OPERATOR               |*)
(*|     | OUT                    |*)
(*|     | OVERRIDE               |*)
(*|     | PRIVATE                |*)
(*|     | PROTECTED              |*)
(*|     | PUBLIC                 |*)
(*|     | REIFIED                |*)
(*|     | SEALED                 |*)
(*|     | TAILREC                |*)
(*|     | SETTER                 |*)
(*|     | VARARG                 |*)
(*|     | WHERE                  |*)
(*|     | EXPECT                 |*)
(*|     | ACTUAL                 |*)
(*|     | CONST                  |*)
(*|     | SUSPEND                |*)
(*|     ;                        |*)
and simpleIdentifier () =
  anyspace *>
  pos >>= fun pos ->
    (
      identifier' pos (* TODO *)
    )

(*| identifier                                         |*)
(*|     : simpleIdentifier (NL* DOT simpleIdentifier)* |*)
(*|     ;                                              |*)
and identifier () = 
  simpleIdentifier ()

let parse file input =
  isParsingPattern := false;
  patternDepth := 0;
  match parse_only (kotlinFile ()) (`String input) with
  | Ok ast ->
      let program = extractNode ast in
      let comments' = get_comments () in
      List (comments' @ [program])
  | Error e ->
      failwith (Printf.sprintf "%s: SyntaxError%s" file e)
