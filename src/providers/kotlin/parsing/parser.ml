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
    <* mkOptE semi
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
  <:> (mkBoolProp "StarImport" (dot <* mult) <|> mkOptPropE "Alias" importAlias)
  <* mkOptE semi

(*| importAlias               |*)
(*|     : AS simpleIdentifier |*)
(*|     ;                     |*)
and importAlias () =
  as' *> identifier ()

(*| topLevelObject           |*)
(*|     : declaration semis? |*)
(*|     ;                    |*)
and topLevelObject () =
  (fix declaration)
  <* mkOptE semis

(*| declaration               |*)
(*|     : classDeclaration    |*)
(*|     | objectDeclaration   |*)
(*|     | functionDeclaration |*)
(*|     | propertyDeclaration |*)
(*|     | typeAlias           |*)
(*|     ;                     |*)
and declaration _ = (* TODO *)
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
and classDeclaration () = (* TODO *)
  mkOptPropEmptyE modifiers >>= (fun mods ->
      ((class' *> mkNode "ClassDeclaration") <|> (interface *> mkNode "InterfaceDeclaration"))
      <:> (return mods)
      <:> mkPropE "Name" simpleIdentifier
      <:> mkOptPropE "TypeParameters" typeParameters
      <* commit
      <:> mkOptPropE "PrimaryConstructor" primaryConstructor
      <:> mkOptProp "DelegationSpecifiers" (anyspace *> colon *> anyspace *> delegationSpecifiers ())
      <:> mkOptPropE "TypeConstraints" typeConstraints
      <:> mkOptProp "Body" (fix classBody)
    )

(*| primaryConstructor                                   |*)
(*|     : (modifiers? CONSTRUCTOR NL* )? classParameters |*)
(*|     ;                                                |*)
and primaryConstructor () =
  mkNode "PrimaryConstructor"
  <:> ((modifiers () >>= fun mods -> mkOptPropEmpty ((return mods) <* constructor))
       <|> (mkOpt (constructor >>= mkString) *> mkPropHolder))
  <:> mkPropE "ClassParameters" classParameters

(*| classBody                                         |*)
(*|     : LCURL NL* classMemberDeclarations NL* RCURL |*)
(*|     ;                                             |*)
and classBody _ =
  lcurl *> anyspace *> classMemberDeclarations () <* anyspace <* rcurl

(*| classParameters                                                                |*)
(*|     : LPAREN NL* (classParameter (NL* COMMA NL* classParameter)* )? NL* RPAREN |*)
(*|     ;                                                                          |*)
and classParameters () =
  lparen *> anyspace *> mkOpt (commaSep classParameter) <* anyspace <* rparen

(*| classParameter                                                                                     |*)
(*|     : modifiers? (VAL | VAR)? NL* simpleIdentifier COLON NL* type (NL* ASSIGNMENT NL* expression)? |*)
(*|     ;                                                                                              |*)
and classParameter () =
  mkNode "ClassParameter"
  <:> mkOptPropEmptyE modifiers
  <:> mkOptPropEmpty (
    mkBoolProp "Variable" var
    <|> mkBoolProp "Constant" val'
  )
  <:> mkProp "Identifier" (simpleIdentifier () <* colon)
  <:> mkProp "Type" (fix type')
  <:> mkOptPropEmpty (assignment *> mkProp "DefaultValue" (fix expression))

(*| delegationSpecifiers                                                           |*)
(*|   : annotatedDelegationSpecifier (NL* COMMA NL* annotatedDelegationSpecifier)* |*)
(*|   ;                                                                            |*)
and delegationSpecifiers () =
  commaSep annotatedDelegationSpecifier

(*| delegationSpecifier       |*)
(*|   : constructorInvocation |*)
(*|   | explicitDelegation    |*)
(*|   | userType              |*)
(*|   | functionType          |*)
(*|   ;                       |*)
and delegationSpecifier () =
  explicitDelegation ()
  <!> constructorInvocation
  <!> userType
  <!> functionType

(*| constructorInvocation       |*)
(*|   : userType valueArguments |*)
(*|   ;                         |*)

(*| annotatedDelegationSpecifier            |*)
(*|   : annotation* NL* delegationSpecifier |*)
(*|   ;                                     |*)
and annotatedDelegationSpecifier () = (* TODO *)
  delegationSpecifier ()

(*| explicitDelegation                                    |*)
(*|     : (userType | functionType) NL* BY NL* expression |*)
(*|     ;                                                 |*)
and explicitDelegation () =
  mkNode "ExplicitDelegation"
  <:> mkProp "Type" (userType () <!> functionType)
  <:> mkProp "Expression" (by *> fix expression)

(*| typeParameters                                                         |*)
(*|   : LANGLE NL* typeParameter (NL* COMMA NL* typeParameter)* NL* RANGLE |*)
(*|   ;                                                                    |*)
and typeParameters () =
  (langle <* anyspace)
  *> (commaSep typeParameter)
  <* (rangle <* anyspace)

(*| typeParameter                                                          |*)
(*|   : typeParameterModifiers? NL* simpleIdentifier (NL* COLON NL* type)? |*)
(*|   ;                                                                    |*)
and typeParameter () =
  mkNode "TypeParameter"
  <:> mkOptPropEmptyE typeParameterModifiers
  <* anyspace
  <:> mkPropE "Identifier" simpleIdentifier
  <:> mkOptProp "Type" (anyspace *> colon *> anyspace *> (fix type'))

(*| typeConstraints                                                |*)
(*|     : WHERE NL* typeConstraint (NL* COMMA NL* typeConstraint)* |*)
(*|     ;                                                          |*)
and typeConstraints () =
  where *> commaSep typeConstraint

(*| typeConstraint                                        |*)
(*|     : annotation* simpleIdentifier NL* COLON NL* type |*)
(*|     ;                                                 |*)
and typeConstraint () = (* TODO - annotation *)
  mkNode "TypeConstraint"
  <:> mkPropE "SubType" simpleIdentifier
  <* anyspace <* colon <* anyspace
  <:> mkProp "SuperType" (fix type')

(*| SECTION: classMembers |*)

(*| classMemberDeclarations                |*)
(*|     : (classMemberDeclaration semis?)* |*)
(*|     ;                                  |*)
and classMemberDeclarations () =
  mkList1 (fun () -> classMemberDeclaration () <* mkOptE semis)
  |> mkOpt

(*| classMemberDeclaration     |*)
(*|     : declaration          |*)
(*|     | companionObject      |*)
(*|     | anonymousInitializer |*)
(*|     | secondaryConstructor |*)
(*|     ;                      |*)
and classMemberDeclaration () =
  (fix declaration)
  <!> companionObject
  <!> anonymousInitializer
  <!> secondaryConstructor

(*| anonymousInitializer |*)
(*|     : INIT NL* block |*)
(*|     ;                |*)
and anonymousInitializer () =
  mkNode "AnonymousInitializer"
  <* init <* anyspace
  <:> mkPropE "Block" block

(*| companionObject                           |*)
(*|     : modifiers? COMPANION NL* OBJECT     |*)
(*|     (NL* simpleIdentifier)?               |*)
(*|     (NL* COLON NL* delegationSpecifiers)? |*)
(*|     (NL* classBody)?                      |*)
(*|     ;                                     |*)
and companionObject () =
  mkNode "CompanionObject"
  <:> mkOptPropEmpty (modifiers ())
  <* companion <* object'
  <:> mkOptPropE "Name" simpleIdentifier
  <:> mkOptProp "DelegationSpecifiers" (anyspace *> colon *> anyspace *> delegationSpecifiers ())
  <:> mkOptProp "Body" (fix classBody)

(*| functionValueParameters                                                                        |*)
(*|     : LPAREN NL* (functionValueParameter (NL* COMMA NL* functionValueParameter)* )? NL* RPAREN |*)
(*|     ;                                                                                          |*)
and functionValueParameters () =
  lparen *> anyspace
  *> mkOpt (commaSep functionValueParameter)
  <* anyspace <* rparen

(*| functionValueParameter                                      |*)
(*|     : modifiers? parameter (NL* ASSIGNMENT NL* expression)? |*)
(*|     ;                                                       |*)
and functionValueParameter () =
  mkNode "Parameter"
  <:> mkOptPropEmptyE modifiers
  <:> mkPropHolder <:> parameter ()
  <:> mkOptPropEmpty (assignment *> mkProp "DefaultValue" (fix expression))

(*| parameter                                 |*)
(*|     : simpleIdentifier NL* COLON NL* type |*)
(*|     ;                                     |*)
and parameter () =
  mkPropE "Name" simpleIdentifier
  <:> mkProp "Type" (anyspace *> colon *> anyspace *> (fix type'))

(*| secondaryConstructor                                                                                           |*)
(*|     : modifiers? CONSTRUCTOR NL* functionValueParameters (NL* COLON NL* constructorDelegationCall)? NL* block? |*)
(*|     ;                                                                                                          |*)
and secondaryConstructor () =
  mkNode "SecondaryConstructor"
  <:> mkOptPropEmptyE modifiers
  <* constructor
  <:> mkPropE "Parameters" functionValueParameters
  <:> mkOptProp "DelegationCall" (anyspace *> colon *> anyspace *> constructorDelegationCall ())
  <:> mkOptPropE "Block" block

(*| constructorDelegationCall      |*)
(*|     : THIS NL* valueArguments  |*)
(*|     | SUPER NL* valueArguments |*)
(*|     ;                          |*)
and constructorDelegationCall () =
  (this *> mkNode "ThisCall")
  <|> (super *> mkNode "SuperCall")
  <:> mkPropE "Arguments" valueArguments

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
  mkOptPropEmptyE typeModifiers >>= (fun mods ->
      functionType ()
      <!> nullableType
      <!> parenthesizedType
      <!> typeReference
      <:> (return mods)
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
  <:> mkBoolProp "Nullable" (skip_many1 (quest ()))

(*| quest             |*)
(*|     : QUEST_NO_WS |*)
(*|     | QUEST_WS    |*)
(*|     ;             |*)
and quest () =
  questNoWs <|> questWs

(*| userType                                           |*)
(*|     : simpleUserType (NL* DOT NL* simpleUserType)* |*)
(*|     ;                                              |*)
and userType () = (* TODO: join types up to <Generic> *)
  mkNode "UserType"
  <:> mkProp "Types" (sep_by1 (anyspace *> dot <* anyspace) (simpleUserType ()) >>= toList)

(*| simpleUserType                              |*)
(*|     : simpleIdentifier (NL* typeArguments)? |*)
(*|     ;                                       |*)
and simpleUserType () =
  simpleIdentifier ()
  <:> mkOptPropE "TypeArguments" typeArguments

(*| typeProjection                             |*)
(*|     : typeProjectionModifiers? type | MULT |*)
(*|     ;                                      |*)
and typeProjection () =
  (mkNode "StarProjection" <* mult)
  <|>
  (mkNode "TypeProjection"
   <:> (mkOptPropEmptyE typeProjectionModifiers >>= fun mods ->
        mkProp "Type" (
          fix type'
          <:> (return mods)
        )
       )
  )

(*| typeProjectionModifiers       |*)
(*|     : typeProjectionModifier+ |*)
(*|     ;                         |*)
and typeProjectionModifiers () =
  many1 typeProjectionModifier >>= fun mods ->
  List.fold_left (fun p m -> p <:> (return m)) mkPropHolder mods

(*| typeProjectionModifier     |*)
(*|     : varianceModifier NL* |*)
(*|     | annotation           |*)
(*|     ;                      |*)
and typeProjectionModifier () = (* TODO *)
  varianceModifier ()
(* <!> annotation *)

(*| receiverType            |*)
(*|     : typeModifiers?    |*)
(*|     ( parenthesizedType |*)
(*|     | nullableType      |*)
(*|     | typeReference)    |*)
(*|     ;                   |*)
and receiverType () =
  mkOptPropEmptyE typeModifiers >>= (fun mods ->
      nullableType ()
      <!> parenthesizedType
      <!> typeReference
      <:> (return mods)
    )

(*| parenthesizedType                |*)
(*|     : LPAREN NL* type NL* RPAREN |*)
(*|     ;                            |*)
and parenthesizedType () =
  lparen
  *> (fix type')
  <* rparen

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
  *> mkOpt (commaSep (fun () -> parameter () <|> (fix type')))
  <* rparen

(*| SECTION: statements |*)

(*| statements                                   |*)
(*|     : (statement (semis statement)* semis?)? |*)
(*|     ;                                        |*)
and statements () =
  sep_by (semis ()) (statement ()) >>= toList <* mkOpt (semis ())

(*| statement                   |*)
(*|     : (label | annotation)* |*)
(*|     ( declaration           |*)
(*|     | assignment            |*)
(*|     | loopStatement         |*)
(*|     | expression)           |*)
(*|     ;                       |*)
and statement () =  (* TODO *)
  (fix declaration)
  <!> (fix expression)

(*| label                  |*)
(*|     : IdentifierAt NL* |*)
(*|     ;                  |*)
and label () =
  identifierAt ()

(*| controlStructureBody |*)
(*|     : block          |*)
(*|     | statement      |*)
(*|     ;                |*)

(*| block                                |*)
(*|     : LCURL NL* statements NL* RCURL |*)
(*|     ;                                |*)
and block () =
  lcurl *> anyspace
  *> statements ()
  <* anyspace <* rcurl

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

(*| semi                       |*)
(*|     : (SEMICOLON | NL) NL* |*)
(*|     | EOF;                 |*)
and semi () =
  (semicolon >>= fun c -> mkString (String.make 1 c))
  <|> (nl *> mkString "")

(*| semis                   |*)
(*|     : (SEMICOLON | NL)+ |*)
(*|     | EOF               |*)
(*|     ;                   |*)
and semis () =
  mkList1 (fun () ->
      (semicolon >>= fun c -> mkString (String.make 1 c))
      <|> (nl *> mkString "")
    )

(*| SECTION: expressions |*)

(*| expression        |*)
(*|     : disjunction |*)
(*|     ;             |*)
and expression _ =
  disjunction ()

and auxExpression'' name prop expr1 expr2 =
  expr1 () >>= (fun e ->
      let aux = fun pHolder ->
        mkNode name
        <:> mkProp "Lhs" (return e)
        <:> return pHolder
      in
      (
        (mkPropHolder
         <:> prop
         <:> mkPropE "Rhs" expr2
        ) >>= aux
      ) <|> return e
    )

and auxExpression' name prop expr =
  (* auxExpression'' name prop expr expr *)
  let rec aux = fun e ->
    (prop >>= fun opProp ->
     mkNode name
     <:> mkProp "Lhs" (return e)
     <:> (return opProp)
     <:> mkProp "Rhs" (expr () >>= aux))
    <|> (return e)
  in
  expr () >>= aux


and auxExpression name op expr =
  auxExpression' name (mkProp "Operator" (op >>= mkString)) expr

(*| disjunction                                   |*)
(*|     : conjunction (NL* DISJ NL* conjunction)* |*)
(*|     ;                                         |*)
and disjunction () =
  auxExpression "BinaryExpression" disj conjunction

(*| conjunction                             |*)
(*|     : equality (NL* CONJ NL* equality)* |*)
(*|     ;                                   |*)
and conjunction () =
  auxExpression "BinaryExpression" conj equality

(*| equality                                            |*)
(*|     : comparison (equalityOperator NL* comparison)* |*)
(*|     ;                                               |*)
and equality () =
  auxExpression "ComparisonExpression" (equalityOperator ()) comparison

(*| comparison                                                  |*)
(*|   : infixOperation (comparisonOperator NL* infixOperation)? |*)
(*|   ;                                                         |*)
and comparison () =
  auxExpression "ComparisonExpression" (comparisonOperator ()) infixOperation

(*| infixOperation                                                              |*)
(*|   : elvisExpression (inOperator NL* elvisExpression | isOperator NL* type)* |*)
(*|   ;                                                                         |*)
and infixOperation () =
  elvisExpression () >>= fun eExpr ->
  let aux = fun op expr ->
    mkProp "Operator" ((op () <* anyspace) >>= mkString)
    <:> mkProp "Rhs" expr
  in
  mkNode "InfixOperation"
  <:> mkProp "Lhs" (return eExpr)
  <:> (aux inOperator (elvisExpression ())
       <|> aux isOperator (fix type'))
  <|> (return eExpr)

(*| elvisExpression                                          |*)
(*|   : infixFunctionCall (NL* elvis NL* infixFunctionCall)* |*)
(*|   ;                                                      |*)
and elvisExpression () =
  auxExpression' "ElvisExpression" (mkProp "Operator" (elvis () >>= fun _ -> mkString "?:")) infixFunctionCall

(*| elvis                 |*)
(*|   : QUEST_NO_WS COLON |*)
(*|   ;                   |*)
and elvis () =
  anyspace *> questNoWs <* colon

(*| infixFunctionCall                                           |*)
(*|   : rangeExpression (simpleIdentifier NL* rangeExpression)* |*)
(*|   ;                                                         |*)
and infixFunctionCall () =
  auxExpression'
    "InfixFunctionCall"
    (mkPropE "Identifier" simpleIdentifier)
    rangeExpression


(*| rangeExpression                                        |*)
(*|   : additiveExpression (RANGE NL* additiveExpression)* |*)
(*|   ;                                                    |*)
and rangeExpression () =
  auxExpression "RangeExpression" range additiveExpression

(*| additiveExpression                                                            |*)
(*|   : multiplicativeExpression (additiveOperator NL* multiplicativeExpression)* |*)
(*|   ;                                                                           |*)
and additiveExpression () =
  auxExpression "AdditiveExpression" (additiveOperator ()) multiplicativeExpression

(*| multiplicativeExpression                                    |*)
(*|   : asExpression (multiplicativeOperator NL* asExpression)* |*)
(*|   ;                                                         |*)
and multiplicativeExpression () =
  auxExpression "MultiplicativeExpression" (multiplicativeOperator ()) asExpression

(*| asExpression                                         |*)
(*|   : prefixUnaryExpression (NL* asOperator NL* type)? |*)
(*|   ;                                                  |*)
and asExpression () =
  prefixUnaryExpression () >>= fun puEx ->
  (mkNode "AsExpression"
   <:> mkProp "Prefix" (return puEx)
   <:> mkProp "Operator" (asOperator () >>= mkString)
   <:> mkProp "Type" (fix type')
  )
  <|> (return puEx)

(*| prefixUnaryExpression                   |*)
(*|   : unaryPrefix* postfixUnaryExpression |*)
(*|   ;                                     |*)
and prefixUnaryExpression () =
  let aux = fun prfx ->
    mkNode "PrefixUnaryExpression"
    <:> (return prfx)
    <:> mkPropE "Expression" postfixUnaryExpression
  in
  (mkProp "Prefixes" (mkList1 unaryPrefix) >>= aux)
  <!> postfixUnaryExpression

(*| unaryPrefix                 |*)
(*|   : annotation              |*)
(*|   | label                   |*)
(*|   | prefixUnaryOperator NL* |*)
(*|   ;                         |*)
and unaryPrefix () =
  (* annotation () <!>  *)
  label ()
  <!> prefixUnaryOperator

(*| postfixUnaryExpression                    |*)
(*|   : primaryExpression                     |*)
(*|   | primaryExpression postfixUnarySuffix+ |*)
(*|   ;                                       |*)
and postfixUnaryExpression () =
  primaryExpression () >>= fun pExpr ->
  let aux = fun sfxs ->
    mkNode "PostfixUnaryExpression"
    <:> mkProp "Expression" (return pExpr)
    <:> mkProp "Suffixes" (return sfxs)
  in
  (mkList1 postfixUnarySuffix >>= aux)
  <|> (return pExpr)

(*| postfixUnarySuffix       |*)
(*|   : postfixUnaryOperator |*)
(*|   | typeArguments        |*)
(*|   | callSuffix           |*)
(*|   | indexingSuffix       |*)
(*|   | navigationSuffix     |*)
(*|   ;                      |*)
and postfixUnarySuffix () =
  mkNode "PostfixUnarySuffix"
  <:> (mkPropE "UnaryOperator" postfixUnaryOperator
       <|> mkPropE "TypeArguments" typeArguments
       <|> mkPropE "CallSuffix" callSuffix
       <|> mkPropE "IndexingSuffix" indexingSuffix
       <|> mkPropE "NavigationSuffix" navigationSuffix
      )

(*| directlyAssignableExpression                |*)
(*|   : postfixUnaryExpression assignableSuffix |*)
(*|   | simpleIdentifier                        |*)
(*|   ;                                         |*)

(*| assignableExpression      |*)
(*|   : prefixUnaryExpression |*)
(*|   ;                       |*)

(*| assignableSuffix     |*)
(*|   : typeArguments    |*)
(*|   | indexingSuffix   |*)
(*|   | navigationSuffix |*)
(*|   ;                  |*)

(*| indexingSuffix                                                     |*)
(*|   : LSQUARE NL* expression (NL* COMMA NL* expression)* NL* RSQUARE |*)
(*|   ;                                                                |*)
and indexingSuffix () =
  (lsquare <* anyspace)
  *> commaSep (fun () -> fix expression)
  <* (anyspace *> rsquare)

(*| navigationSuffix                                                                      |*)
(*|   : NL* memberAccessOperator NL* (simpleIdentifier | parenthesizedExpression | CLASS) |*)
(*|   ;                                                                                   |*)
and navigationSuffix () =
  memberAccessOperator ()
  <* (
    (class' >>= mkString)
    <!> parenthesizedExpression
    <!> simpleIdentifier
  )

(*| callSuffix                                         |*)
(*|   : typeArguments? valueArguments? annotatedLambda |*)
(*|   | typeArguments? valueArguments                  |*)
(*|   ;                                                |*)
and callSuffix () = (* TODO *)
  mkOptE typeArguments
  <* valueArguments ()

(*| annotatedLambda                          |*)
(*|   : annotation* label? NL* lambdaLiteral |*)
(*|   ;                                      |*)

(*| typeArguments                                                            |*)
(*|   : LANGLE NL* typeProjection (NL* COMMA NL* typeProjection)* NL* RANGLE |*)
(*|   ;                                                                      |*)
and typeArguments () =
  (langle <* anyspace)
  *> (commaSep typeProjection)
  <* (anyspace *> rangle)

(*| valueArguments                                                         |*)
(*|   : LPAREN NL* RPAREN                                                  |*)
(*|   | LPAREN NL* valueArgument (NL* COMMA NL* valueArgument)* NL* RPAREN |*)
(*|   ;                                                                    |*)

(*| valueArgument                                                                    |*)
(*|   : annotation? NL* (simpleIdentifier NL* ASSIGNMENT NL* )? MULT? NL* expression |*)
(*|   ;                                                                              |*)

(*| primaryExpression            |*)
(*|   : parenthesizedExpression  |*)
(*|   | simpleIdentifier         |*)
(*|   | literalConstant          |*)
(*|   | stringLiteral            |*)
(*|   | callableReference        |*)
(*|   | functionLiteral          |*)
(*|   | objectLiteral            |*)
(*|   | collectionLiteral        |*)
(*|   | thisExpression           |*)
(*|   | superExpression          |*)
(*|   | ifExpression             |*)
(*|   | whenExpression           |*)
(*|   | tryExpression            |*)
(*|   | jumpExpression           |*)
(*|   ;                          |*)
and primaryExpression () = (* TODO *)
  parenthesizedExpression ()
  <!> simpleIdentifier

(*| parenthesizedExpression              |*)
(*|   : LPAREN NL* expression NL* RPAREN |*)
(*|   ;                                  |*)
and parenthesizedExpression () =
  (lparen <* anyspace)
  *> (fix expression)
  <* (anyspace *> rparen)

(*| collectionLiteral                                                  |*)
(*|   : LSQUARE NL* expression (NL* COMMA NL* expression)* NL* RSQUARE |*)
(*|   | LSQUARE NL* RSQUARE                                            |*)
(*|   ;                                                                |*)

(*| literalConstant      |*)
(*|   : BooleanLiteral   |*)
(*|   | IntegerLiteral   |*)
(*|   | HexLiteral       |*)
(*|   | BinLiteral       |*)
(*|   | CharacterLiteral |*)
(*|   | RealLiteral      |*)
(*|   | NullLiteral      |*)
(*|   | LongLiteral      |*)
(*|   | UnsignedLiteral  |*)
(*|   ;                  |*)

(*| stringLiteral              |*)
(*|   : lineStringLiteral      |*)
(*|   | multiLineStringLiteral |*)
(*|   ;                        |*)

(*| lineStringLiteral                                                      |*)
(*|   : QUOTE_OPEN (lineStringContent | lineStringExpression)* QUOTE_CLOSE |*)
(*|   ;                                                                    |*)

(*| multiLineStringLiteral                                                                                                |*)
(*|   : TRIPLE_QUOTE_OPEN (multiLineStringContent | multiLineStringExpression | MultiLineStringQuote)* TRIPLE_QUOTE_CLOSE |*)
(*|   ;                                                                                                                   |*)

(*| lineStringContent      |*)
(*|   : LineStrText        |*)
(*|   | LineStrEscapedChar |*)
(*|   | LineStrRef         |*)
(*|   ;                    |*)

(*| lineStringExpression                  |*)
(*|   : LineStrExprStart expression RCURL |*)
(*|   ;                                   |*)

(*| multiLineStringContent   |*)
(*|   : MultiLineStrText     |*)
(*|   | MultiLineStringQuote |*)
(*|   | MultiLineStrRef      |*)
(*|   ;                      |*)

(*| multiLineStringExpression                          |*)
(*|   : MultiLineStrExprStart NL* expression NL* RCURL |*)
(*|   ;                                                |*)

(*| lambdaLiteral                                                      |*)
(*|   : LCURL NL* statements NL* RCURL                                 |*)
(*|   | LCURL NL* lambdaParameters? NL* ARROW NL* statements NL* RCURL |*)
(*|   ;                                                                |*)

(*| lambdaParameters                                     |*)
(*|   : lambdaParameter (NL* COMMA NL* lambdaParameter)* |*)
(*|   ;                                                  |*)

(*| lambdaParameter                                    |*)
(*|   : variableDeclaration                            |*)
(*|   | multiVariableDeclaration (NL* COLON NL* type)? |*)
(*|   ;                                                |*)

(*| anonymousFunction             |*)
(*|   : FUN                       |*)
(*|   (NL* type NL* DOT)?         |*)
(*|   NL* functionValueParameters |*)
(*|   (NL* COLON NL* type)?       |*)
(*|   (NL* typeConstraints)?      |*)
(*|   (NL* functionBody)?         |*)
(*|   ;                           |*)

(*| functionLiteral       |*)
(*|   : lambdaLiteral     |*)
(*|   | anonymousFunction |*)
(*|   ;                   |*)

(*| objectLiteral                                               |*)
(*|   : OBJECT NL* COLON NL* delegationSpecifiers NL* classBody |*)
(*|   | OBJECT NL* classBody                                    |*)
(*|   ;                                                         |*)

(*| thisExpression |*)
(*|   : THIS       |*)
(*|   | THIS_AT    |*)
(*|   ;            |*)

(*| superExpression                                                |*)
(*|   : SUPER (LANGLE NL* type NL* RANGLE)? (AT simpleIdentifier)? |*)
(*|   | SUPER_AT                                                   |*)
(*|   ;                                                            |*)

(*| ifExpression                                                                                                                         |*)
(*|   : IF NL* LPAREN NL* expression NL* RPAREN NL* (controlStructureBody | SEMICOLON)                                                   |*)
(*|   | IF NL* LPAREN NL* expression NL* RPAREN NL* controlStructureBody? NL* SEMICOLON? NL* ELSE NL* (controlStructureBody | SEMICOLON) |*)
(*|   ;                                                                                                                                  |*)

(*| whenSubject                                                                                     |*)
(*|   : LPAREN (annotation* NL* VAL NL* variableDeclaration NL* ASSIGNMENT NL* )? expression RPAREN |*)
(*|   ;                                                                                             |*)

(*| whenExpression                                                      |*)
(*|   : WHEN NL* whenSubject? NL* LCURL NL* (whenEntry NL* )* NL* RCURL |*)
(*|   ;                                                                 |*)

(*| whenEntry                                                                                 |*)
(*|   : whenCondition (NL* COMMA NL* whenCondition)* NL* ARROW NL* controlStructureBody semi? |*)
(*|   | ELSE NL* ARROW NL* controlStructureBody semi?                                         |*)
(*|   ;                                                                                       |*)

(*| whenCondition  |*)
(*|   : expression |*)
(*|   | rangeTest  |*)
(*|   | typeTest   |*)
(*|   ;            |*)

(*| rangeTest                     |*)
(*|   : inOperator NL* expression |*)
(*|   ;                           |*)

(*| typeTest                |*)
(*|   : isOperator NL* type |*)
(*|   ;                     |*)

(*| tryExpression                                                                |*)
(*|   : TRY NL* block ((NL* catchBlock)+ (NL* finallyBlock)? | NL* finallyBlock) |*)
(*|   ;                                                                          |*)

(*| catchBlock                                                                    |*)
(*|   : CATCH NL* LPAREN annotation* simpleIdentifier COLON type RPAREN NL* block |*)
(*|   ;                                                                           |*)

(*| finallyBlock          |*)
(*|   : FINALLY NL* block |*)
(*|   ;                   |*)

(*| jumpExpression                       |*)
(*|   : THROW NL* expression             |*)
(*|   | (RETURN | RETURN_AT) expression? |*)
(*|   | CONTINUE | CONTINUE_AT           |*)
(*|   | BREAK | BREAK_AT                 |*)
(*|   ;                                  |*)

(*| callableReference                                                 |*)
(*|   : (receiverType? NL* COLONCOLON NL* (simpleIdentifier | CLASS)) |*)
(*|   ;                                                               |*)

(*| assignmentAndOperator |*)
(*|   : ADD_ASSIGNMENT    |*)
(*|   | SUB_ASSIGNMENT    |*)
(*|   | MULT_ASSIGNMENT   |*)
(*|   | DIV_ASSIGNMENT    |*)
(*|   | MOD_ASSIGNMENT    |*)
(*|   ;                   |*)
and assignmentAndOperator () =
  addAssignment <|> subAssignment <|> multAssignment <|> divAssignment <|> modAssignment

(*| equalityOperator |*)
(*|     : EXCL_EQ    |*)
(*|     | EXCL_EQEQ  |*)
(*|     | EQEQ       |*)
(*|     | EQEQEQ     |*)
(*|     ;            |*)
and equalityOperator () =
  exclEqEq <|> exclEq <|> eqEqEq <|> eqEq

(*| comparisonOperator |*)
(*|     : LANGLE       |*)
(*|     | RANGLE       |*)
(*|     | LE           |*)
(*|     | GE           |*)
(*|     ;              |*)
and comparisonOperator () = (* TODO *)
  (* langle <|> rangle <|>  *)
  le <|> ge

(*| inOperator        |*)
(*|     : IN | NOT_IN |*)
(*|     ;             |*)
and inOperator () =
  in' <|> notIn

(*| isOperator        |*)
(*|     : IS | NOT_IS |*)
(*|     ;             |*)
and isOperator () =
  is <|> notIs

(*| additiveOperator |*)
(*|     : ADD | SUB  |*)
(*|     ;            |*)
and additiveOperator () =
  add <|> sub

(*| multiplicativeOperator |*)
(*|     : MULT             |*)
(*|     | DIV              |*)
(*|     | MOD              |*)
(*|     ;                  |*)
and multiplicativeOperator () =
  mult <|> div <|> mod'

(*| asOperator    |*)
(*|     : AS      |*)
(*|     | AS_SAFE |*)
(*|     ;         |*)
and asOperator () =
  as' <|> asSafe

(*| prefixUnaryOperator |*)
(*|     : INCR          |*)
(*|     | DECR          |*)
(*|     | SUB           |*)
(*|     | ADD           |*)
(*|     | excl          |*)
(*|     ;               |*)
and prefixUnaryOperator () =
  (incr <|> decr <|> sub <|> add <|> excl ()) >>= mkString

(*| postfixUnaryOperator  |*)
(*|     : INCR            |*)
(*|     | DECR            |*)
(*|     | EXCL_NO_WS excl |*)
(*|     ;                 |*)
and postfixUnaryOperator () =
  (incr <|> decr <|> (exclNoWs <* excl ())) >>= mkString

(*| excl             |*)
(*|     : EXCL_NO_WS |*)
(*|     | EXCL_WS    |*)
(*|     ;            |*)
and excl () =
  exclNoWs <|> exclWs

(*| memberAccessOperator             |*)
(*|     : DOT | safeNav | COLONCOLON |*)
(*|     ;                            |*)
and memberAccessOperator () =
  (dot *> mkString ".")
  <|> safeNav ()
  <|> (colonColon >>= mkString)

(*| safeNav               |*)
(*|     : QUEST_NO_WS DOT |*)
(*|     ;                 |*)
and safeNav () =
  (questNoWs <* dot) >>= mkString

(*| SECTION: modifiers |*)

(*| modifiers                      |*)
(*|     : (annotation | modifier)+ |*)
(*|     ;                          |*)
and modifiers () =
  many1 (fun () ->
      (* annotation () <!>  *)
      modifier ()
    ) >>= fun mods ->
  List.fold_left (fun p m -> p <:> (return m)) mkPropHolder mods

(*| modifier                    |*)
(*|     : (classModifier        |*)
(*|     | memberModifier        |*)
(*|     | visibilityModifier    |*)
(*|     | functionModifier      |*)
(*|     | propertyModifier      |*)
(*|     | inheritanceModifier   |*)
(*|     | parameterModifier     |*)
(*|     | platformModifier) NL* |*)
(*|     ;                       |*)
and modifier () =
  classModifier ()
  <!> memberModifier
  <!> visibilityModifier
  <!> functionModifier
  <!> propertyModifier
  <!> inheritanceModifier
  <!> parameterModifier
  <!> platformModifier

(*| typeModifiers       |*)
(*|     : typeModifier+ |*)
(*|     ;               |*)
and typeModifiers () =
  many1 typeModifier >>= fun mods ->
  List.fold_left (fun p m -> p <:> (return m)) mkPropHolder mods

(*| typeModifier                   |*)
(*|     : annotation | SUSPEND NL* |*)
(*|     ;                          |*)
and typeModifier () = (* TODO *)
  (* mkPropE "Annotation" annotation <|> *)
  mkBoolProp "SuspendModifier" suspend
  <* skip_many nl

(*| classModifier    |*)
(*|     : ENUM       |*)
(*|     | SEALED     |*)
(*|     | ANNOTATION |*)
(*|     | DATA       |*)
(*|     | INNER      |*)
(*|     ;            |*)
and classModifier () = (* TODO *)
  mkBoolProp "ClassModifierEnum" enum
  <|> mkBoolProp "ClassModifierSealed" sealed
  (* <|> mkBoolPropE "ClassModifierAnnotation" annotation *)
  <|> mkBoolProp "ClassModifierData" data
  <|> mkBoolProp "ClassModifierInner" inner

(*| memberModifier |*)
(*|     : OVERRIDE |*)
(*|     | LATEINIT |*)
(*|     ;          |*)
and memberModifier () =
  mkBoolProp "MemberModifierOverride" override
  <|> mkBoolProp "MemberModifierLateinit" lateinit

(*| visibilityModifier |*)
(*|     : PUBLIC       |*)
(*|     | PRIVATE      |*)
(*|     | INTERNAL     |*)
(*|     | PROTECTED    |*)
(*|     ;              |*)
and visibilityModifier () =
  mkBoolProp "VisibilityModifierPublic" public
  <|> mkBoolProp "VisibilityModifierPrivate" private'
  <|> mkBoolProp "VisibilityModifierInternal" internal
  <|> mkBoolProp "VisibilityModifierProtected" protected

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
  reificationModifier ()
  <|> varianceModifier ()
(* <!> annotation *) (* TODO *)

(*| functionModifier |*)
(*|     : TAILREC    |*)
(*|     | OPERATOR   |*)
(*|     | INFIX      |*)
(*|     | INLINE     |*)
(*|     | EXTERNAL   |*)
(*|     | SUSPEND    |*)
(*|     ;            |*)
and functionModifier () =
  mkBoolProp "FunctionModifierTailrec" tailrec
  <|> mkBoolProp "FunctionModifierOperator" operator
  <|> mkBoolProp "FunctionModifierInfix" infix
  <|> mkBoolProp "FunctionModifierInline" inline
  <|> mkBoolProp "FunctionModifierExternal" external'
  <|> mkBoolProp "FunctionModifierSuspend" suspend

(*| propertyModifier |*)
(*|     : CONST      |*)
(*|     ;            |*)
and propertyModifier () = mkBoolProp "PropertyModifierConst" const

(*| inheritanceModifier |*)
(*|     : ABSTRACT      |*)
(*|     | FINAL         |*)
(*|     | OPEN          |*)
(*|     ;               |*)
and inheritanceModifier () =
  mkBoolProp "InheritanceModifierAbstract" abstract
  <|> mkBoolProp "InheritanceModifierFinal" final
  <|> mkBoolProp "InheritanceModifierOpen" open'

(*| parameterModifier |*)
(*|     : VARARG      |*)
(*|     | NOINLINE    |*)
(*|     | CROSSINLINE |*)
(*|     ;             |*)
and parameterModifier () =
  mkBoolProp "ParameterModifierVararg" vararg
  <|> mkBoolProp "ParameterModifierNoinline" noinline
  <|> mkBoolProp "ParameterModifierCrossinline" crossinline

(*| reificationModifier |*)
(*|     : REIFIED       |*)
(*|     ;               |*)
and reificationModifier () =
  mkBoolProp "ReificationModifier" reified

(*| platformModifier |*)
(*|     : EXPECT     |*)
(*|     | ACTUAL     |*)
(*|     ;            |*)
and platformModifier () =
  mkBoolProp "PlatformModifierExpect" expect
  <|> mkBoolProp "PlatformModifierActual" actual

(*| SECTION: annotations |*)

(*| annotation                                                             |*)
(*|     : (singleAnnotation | multiAnnotation) NL*                         |*)
(*|     ;                                                                  |*)
and annotation () =
  mkNode "Annotation"
  <* singleAnnotation ()
  <!> multiAnnotation

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
  <:> mkPropE "Identifier" userType
  <:> mkPropE "Arguments" valueArguments

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