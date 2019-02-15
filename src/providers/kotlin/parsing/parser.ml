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
  mkNode "KotlinFile"
  <:> mkOptPropE "ShebangLine" shebangLine
  <:> mkOptProp "FileAnnotations" (mkList1 fileAnnotation)
  <:> mkPropE "PackageHeader" packageHeader
  <:> mkPropE "ImportList" importList
  <:> mkOptProp "TopLevelObject" (mkList1 topLevelObject)
    >>= fun stm ->
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
and shebangLine () =
  shebangLine' ()
  <* skip_many1 nl

(*| fileAnnotation                                                                                             |*)
(*|     : ANNOTATION_USE_SITE_TARGET_FILE NL* (LSQUARE unescapedAnnotation+ RSQUARE | unescapedAnnotation) NL* |*)
(*|     ;                                                                                                      |*)
and fileAnnotation () =
  annotation_use_site_target_file ()
  <* skip_many nl
  <:> (
    (lsquare *> mkProp "Annotations" (mkList1 (fun () -> unescapedAnnotation () <* anyspace)) <* rsquare)
    <|> mkPropE "Annotation" unescapedAnnotation (* TODO - make a list *)
  )
  <* skip_many nl

(*| packageHeader                     |*)
(*|     : (PACKAGE identifier semi?)? |*)
(*|     ;                             |*)
and packageHeader () =
  mkOpt (
    mkNode "PackageHeader"
    <* package
    <:> mkPropE "Name" identifier
    <* optSemi
  )

(*| importList          |*)
(*|     : importHeader* |*)
(*|     ;               |*)
and importList () =
  mkNode "ImportList"
  <:> mkOptProp "Value" (mkList1 importHeader)

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
and declaration () =
  classDeclaration ()
  (* <!> objectDeclaration
  <!> functionDeclaration
  <!> propertyDeclaration
  <!> typeAlias *)

(*| SECTION: classes |*)

(*| classDeclaration                                          |*)
(*|     : modifiers? (CLASS | INTERFACE) NL* simpleIdentifier |*)
(*|     (NL* typeParameters)? (NL* primaryConstructor)?       |*)
(*|     (NL* COLON NL* delegationSpecifiers)?                 |*)
(*|     (NL* typeConstraints)?                                |*)
(*|     (NL* classBody | NL* enumClassBody)?                  |*)
(*|     ;                                                     |*)
and classDeclaration () =
  mkNode "ClassDeclaration"
  <* (class' <|> interface)
  <* skip_many nl
  <:> mkPropE "ClassName" simpleIdentifier
  <:> mkOptPropE "TypeParameters" typeParameters

(*| typeParameters                                                         |*)
(*|   : LANGLE NL* typeParameter (NL* COMMA NL* typeParameter)* NL* RANGLE |*)
(*|   ;                                                                    |*)
and typeParameters () =
  langle
  *> (commaSep typeParameter)
  <* skip_many nl
  <* rangle

(*| typeParameter                                                          |*)
(*|   : typeParameterModifiers? NL* simpleIdentifier (NL* COLON NL* type)? |*)
(*|   ;                                                                    |*)
and typeParameter () =
  mkNode "TypeParameter"
  <:> mkOptPropEmptyE typeParameterModifiers
  <* skip_many nl
  <:> mkPropE "Identifier" simpleIdentifier
  <:> mkOptProp "Type" (colon *> (fix type'))

(*| SECTION: classMembers |*)

(*| parameter                                 |*)
(*|     : simpleIdentifier NL* COLON NL* type |*)
(*|     ;                                     |*)
and parameter () =
  mkNode "Parameter"
  <:> mkPropE "Identifier" simpleIdentifier
  <:> mkProp "Type" (colon *> (fix type'))

(*| SECTION: enumClasses |*)

(*| SECTION: types |*)

(*| type                    |*)
(*|     : typeModifiers?    |*)
(*|     ( parenthesizedType |*)
(*|     | nullableType      |*)
(*|     | typeReference     |*)
(*|     | functionType)     |*)
(*|     ;                   |*)
and type' _ =
  mkNode "Type"
  <:> mkOptPropEmptyE typeModifiers
  <:> mkProp "Value" (
    functionType ()
    <!> nullableType
    <!> parenthesizedType
    <!> typeReference
  )

(*| typeReference  |*)
(*|     : userType |*)
(*|     | DYNAMIC  |*)
(*|     ;          |*)
and typeReference () =
  mkNode "TypeReference"
  <:> (
    mkBoolProp "DynamicModifier" dynamic
    <|> mkPropE "Value" userType
  )

(*| nullableType                                         |*)
(*|     : (typeReference | parenthesizedType) NL* quest+ |*)
(*|     ;                                                |*)
and nullableType () =
  parenthesizedType ()
  <!> typeReference
  <:> mkBoolProp "Nullable" (skip_many1 quest)

(*| parenthesizedType                |*)
(*|     : LPAREN NL* type NL* RPAREN |*)
(*|     ;                            |*)
and parenthesizedType () =
  lparen
  *> (fix type')
  <* rparen

(*| receiverType            |*)
(*|     : typeModifiers?    |*)
(*|     ( parenthesizedType |*)
(*|     | nullableType      |*)
(*|     | typeReference)    |*)
(*|     ;                   |*)
and receiverType () =
  mkNode "ReceiverType"
  <:> mkOptPropEmptyE typeModifiers
  <:> mkProp "Value" (
    nullableType ()
    <!> parenthesizedType
    <!> typeReference
  )

(*| userType                                           |*)
(*|     : simpleUserType (NL* DOT NL* simpleUserType)* |*)
(*|     ;                                              |*)
and userType () =
  sep_by1 dot (simpleUserType ())
  >>= concatStringNodes "."

(*| simpleUserType                              |*)
(*|     : simpleIdentifier (NL* typeArguments)? |*)
(*|     ;                                       |*)
and simpleUserType () =
  simpleIdentifier ()  (* TODO *)

(*| functionType                                                                 |*)
(*|     : (receiverType NL* DOT NL* )? functionTypeParameters NL* ARROW NL* type |*)
(*|     ;                                                                        |*)
and functionType () =
  mkNode "FunctionType"
  <:> mkOptProp "ReceiverType" (receiverType () <* dot)
  <:> mkPropE "Parameters" functionTypeParameters
  <* arrow
  <:> mkProp "ReturnType" (fix type')

(*| functionTypeParameters                                                              |*)
(*|     : LPAREN NL* (parameter | type)? (NL* COMMA NL* (parameter | type))* NL* RPAREN |*)
(*|     ;                                                                               |*)
and functionTypeParameters () =
  lparen
  *> mkOpt (commaSep (fun () ->
      parameter () <|> (fix type')
    )
  )
  <* rparen

(*| SECTION: statements |*)

(*| valueArguments                                                           |*)
(*|     : LPAREN NL* RPAREN                                                  |*)
(*|     | LPAREN NL* valueArgument (NL* COMMA NL* valueArgument)* NL* RPAREN |*)
(*|     ;                                                                    |*)
and valueArguments () =
  lparen
  *> mkOpt (commaSep valueArgument)
  <* rparen

(*| valueArgument                                                                      |*)
(*|     : annotation? NL* (simpleIdentifier NL* ASSIGNMENT NL* )? MULT? NL* expression |*)
(*|     ;                                                                              |*)
and valueArgument () =
  identifier ()  (* TODO *)

(*| SECTION: expressions |*)

(*| SECTION: modifiers |*)

(*| typeModifiers       |*)
(*|     : typeModifier+ |*)
(*|     ;               |*)
and typeModifiers () =
  many1 typeModifier >>= fun mods ->
    List.fold_left (fun p m -> p <:> (return m)) mkPropHolder mods

(*| typeModifier                   |*)
(*|     : annotation | SUSPEND NL* |*)
(*|     ;                          |*)
and typeModifier () =
  (* annotation <|> *) (* TODO *)
  mkBoolProp "SuspendModifier" suspend
  <* skip_many nl

(*| varianceModifier |*)
(*|     : IN         |*)
(*|     | OUT        |*)
(*|     ;            |*)
and varianceModifier () =
  mkBoolProp "VarianceModifierIn" in'
  <|> mkBoolProp "VarianceModifierOut" out

(*| typeParameterModifiers       |*)
(*|     : typeParameterModifier+ |*)
(*|     ;                        |*)
and typeParameterModifiers () =
  many1 typeParameterModifier >>= fun mods ->
    List.fold_left (fun p m -> p <:> (return m)) mkPropHolder mods

(*| typeParameterModifier         |*)
(*|     : reificationModifier NL* |*)
(*|     | varianceModifier NL*    |*)
(*|     | annotation              |*)
(*|     ;                         |*)
and typeParameterModifier () =
  reificationModifier () <* skip_many nl
  <|> varianceModifier () <* skip_many nl
  (* <!> annotation *) (* TODO *)

(*| reificationModifier |*)
(*|     : REIFIED       |*)
(*|     ;               |*)
and reificationModifier () = mkBoolProp "ReificationModifier" reified

(*| SECTION: annotations |*)

(*| annotation                                                             |*)
(*|     : (singleAnnotation | multiAnnotation) NL*                         |*)
(*|     ;                                                                  |*)
and annotation () =
  singleAnnotation ()
  <!> multiAnnotation
  <* skip_many nl

(*| singleAnnotation                                                       |*)
(*|     : annotationUseSiteTarget NL* unescapedAnnotation                  |*)
(*|     | AT unescapedAnnotation                                           |*)
(*|     ;                                                                  |*)
and singleAnnotation () =
  unescapedAnnotation () (* TODO *)

(*| multiAnnotation                                                        |*)
(*|     : annotationUseSiteTarget NL* LSQUARE unescapedAnnotation+ RSQUARE |*)
(*|     | AT LSQUARE unescapedAnnotation+ RSQUARE                          |*)
(*|     ;                                                                  |*)
and multiAnnotation () =
  unescapedAnnotation () (* TODO *)

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
  <:> mkPropE "Identifier" userType (* TODO *)

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
  sep_by1 dot (simpleIdentifier ())
  >>= concatStringNodes "."

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
