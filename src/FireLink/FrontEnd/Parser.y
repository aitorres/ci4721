{
module FireLink.FrontEnd.Parser (
  parse) where

import qualified Control.Monad.RWS              as RWS
import           Data.List                      (sort)
import           Data.Maybe                     (catMaybes, isJust, fromJust)
import           FireLink.FrontEnd.Errors
import qualified FireLink.FrontEnd.Grammar      as G
import qualified FireLink.FrontEnd.SymTable     as ST
import qualified FireLink.FrontEnd.Tokens       as T
import qualified FireLink.FrontEnd.TypeChecking as T
import           FireLink.Utils
}

%name                                                                     parse
%tokentype                                                              { T.Token }
%error                                                                  { parseErrors }
%monad                                                                  { ST.ParserMonad }


%nonassoc size memAccessor

%left ARRCLOSE

%left and or
%nonassoc lt lte gt gte
%left eq neq
%left plus minus
%left mult div mod


%left colConcat
%left diff
%left union intersect

%left isActive
%left asciiOf
%right not
%left accessor
%left arrOpen arrClose


%token
  programBegin                                                          { T.Token {T.aToken=T.TkProgramBegin} }
  programEnd                                                            { T.Token {T.aToken=T.TkProgramEnd} }

  aliasListBegin                                                        { T.Token {T.aToken=T.TkAliasListBegin} }
  aliasListEnd                                                          { T.Token {T.aToken=T.TkAliasListEnd} }
  alias                                                                 { T.Token {T.aToken=T.TkAlias} }

  id                                                                    { T.Token {T.aToken=T.TkId} }

  ofType                                                                { T.Token {T.aToken=T.TkOfType} }

  paramRequest                                                          { T.Token {T.aToken=T.TkRequesting} }
  parVal                                                                { T.Token {T.aToken=T.TkVal} }
  parRef                                                                { T.Token {T.aToken=T.TkRef} }

  bigInt                                                                { T.Token {T.aToken=T.TkBigInt} }
  smallInt                                                              { T.Token {T.aToken=T.TkSmallInt} }
  float                                                                 { T.Token {T.aToken=T.TkFloat} }
  char                                                                  { T.Token {T.aToken=T.TkChar} }
  bool                                                                  { T.Token {T.aToken=T.TkBool} }
  ltelit                                                                { T.Token {T.aToken=T.TkLteLit} }
  string                                                                { T.Token {T.aToken=T.TkString} }
  array                                                                 { T.Token {T.aToken=T.TkArray} }
  set                                                                   { T.Token {T.aToken=T.TkSet} }
  unionStruct                                                           { T.Token {T.aToken=T.TkUnionStruct} }
  record                                                                { T.Token {T.aToken=T.TkRecord} }
  pointer                                                               { T.Token {T.aToken=T.TkPointer} }

  intLit                                                                { T.Token {T.aToken=T.TkIntLit} }
  floatLit                                                              { T.Token {T.aToken=T.TkFloatLit} }
  charLit                                                               { T.Token {T.aToken=T.TkCharLit} }
  stringLit                                                             { T.Token {T.aToken=T.TkStringLit} }
  trueLit                                                               { T.Token {T.aToken=T.TkLit} }
  falseLit                                                              { T.Token {T.aToken=T.TkUnlit} }
  unknownLit                                                            { T.Token {T.aToken=T.TkUndiscovered} }
  nullLit                                                               { T.Token {T.aToken=T.TkNull} }

  functionBegin                                                         { T.Token {T.aToken=T.TkInvocation} }
  functionType                                                          { T.Token {T.aToken=T.TkInvocationType} }
  functionEnd                                                           { T.Token {T.aToken=T.TkInvocationEnd} }

  procedureBegin                                                        { T.Token {T.aToken=T.TkSpell} }
  procedureEnd                                                          { T.Token {T.aToken=T.TkSpellEnd} }

  comma                                                                 { T.Token {T.aToken=T.TkComma} }
  brOpen                                                                { T.Token {T.aToken=T.TkBraceOpen} }
  brClose                                                               { T.Token {T.aToken=T.TkBraceClose} }

  with                                                                  { T.Token {T.aToken=T.TkWith} }
  declarend                                                             { T.Token {T.aToken=T.TkDeclarationEnd} }

  const                                                                 { T.Token {T.aToken=T.TkConst} }
  var                                                                   { T.Token {T.aToken=T.TkVar} }
  asig                                                                  { T.Token {T.aToken=T.TkAsig} }

  instructionsBegin                                                     { T.Token {T.aToken=T.TkInstructionBegin} }
  instructionsEnd                                                       { T.Token {T.aToken=T.TkInstructionEnd} }
  seq                                                                   { T.Token {T.aToken=T.TkSeq} }

  cast                                                                  { T.Token {T.aToken=T.TkCast} }
  offering                                                              { T.Token {T.aToken=T.TkOffering} }
  toTheKnight                                                           { T.Token {T.aToken=T.TkInvocationParsEnd} }

  summon                                                                { T.Token {T.aToken=T.TkSummon} }
  granting                                                              { T.Token {T.aToken=T.TkGranting} }
  toTheEstusFlask                                                       { T.Token {T.aToken=T.TkSpellParsEnd} }

  return                                                                { T.Token {T.aToken=T.TkReturn} }
  returnWith                                                            { T.Token {T.aToken=T.TkReturnWith} }

  print                                                                 { T.Token {T.aToken=T.TkPrint} }
  read                                                                  { T.Token {T.aToken=T.TkRead} }

  whileBegin                                                            { T.Token {T.aToken=T.TkWhile} }
  whileEnd                                                              { T.Token {T.aToken=T.TkEndWhile} }
  covenantIsActive                                                      { T.Token {T.aToken=T.TkCovenantIsActive} }

  ifBegin                                                               { T.Token {T.aToken=T.TkIf} }
  ifEnd                                                                 { T.Token {T.aToken=T.TkEndIf} }
  colon                                                                 { T.Token {T.aToken=T.TkColon} }
  else                                                                  { T.Token {T.aToken=T.TkElse} }

  switchBegin                                                           { T.Token {T.aToken=T.TkSwitch} }
  switchDefault                                                         { T.Token {T.aToken=T.TkSwitchDefault} }
  switchEnd                                                             { T.Token {T.aToken=T.TkEndSwitch} }

  forBegin                                                              { T.Token {T.aToken=T.TkFor} }
  forEnd                                                                { T.Token {T.aToken=T.TkEndFor} }
  souls                                                                 { T.Token {T.aToken=T.TkSoul} }
  untilLevel                                                            { T.Token {T.aToken=T.TkLevel} }

  forEachBegin                                                          { T.Token {T.aToken=T.TkForEach} }
  forEachEnd                                                            { T.Token {T.aToken=T.TkEndForEach} }
  withTitaniteFrom                                                      { T.Token {T.aToken=T.TkWithTitaniteFrom} }

  parensOpen                                                            { T.Token {T.aToken=T.TkParensOpen} }
  parensClose                                                           { T.Token {T.aToken=T.TkParensClose} }

  plus                                                                  { T.Token {T.aToken=T.TkPlus} }
  minus                                                                 { T.Token {T.aToken=T.TkMinus} }
  mult                                                                  { T.Token {T.aToken=T.TkMult} }
  div                                                                   { T.Token {T.aToken=T.TkDiv} }
  mod                                                                   { T.Token {T.aToken=T.TkMod} }
  lt                                                                    { T.Token {T.aToken=T.TkLt} }
  gt                                                                    { T.Token {T.aToken=T.TkGt} }
  lte                                                                   { T.Token {T.aToken=T.TkLte} }
  gte                                                                   { T.Token {T.aToken=T.TkGte} }
  eq                                                                    { T.Token {T.aToken=T.TkEq} }
  neq                                                                   { T.Token {T.aToken=T.TkNeq} }
  not                                                                   { T.Token {T.aToken=T.TkNot} }
  and                                                                   { T.Token {T.aToken=T.TkAnd} }
  or                                                                    { T.Token {T.aToken=T.TkOr} }
  asciiOf                                                               { T.Token {T.aToken=T.TkAsciiOf} }
  isActive                                                              { T.Token {T.aToken=T.TkIsActive} }
  colConcat                                                             { T.Token {T.aToken=T.TkConcat} }
  union                                                                 { T.Token {T.aToken=T.TkUnion} }
  intersect                                                             { T.Token {T.aToken=T.TkIntersect} }
  diff                                                                  { T.Token {T.aToken=T.TkDiff} }
  size                                                                  { T.Token {T.aToken=T.TkSize} }

  arrOpen                                                               { T.Token {T.aToken=T.TkArrayOpen} }
  arrClose                                                              { T.Token {T.aToken=T.TkArrayClose} }
  setOpen                                                               { T.Token {T.aToken=T.TkSetOpen} }
  setClose                                                              { T.Token {T.aToken=T.TkSetClose} }

  accessor                                                              { T.Token {T.aToken=T.TkAccessor} }
  memAccessor                                                           { T.Token {T.aToken=T.TkAccessMemory} }

  malloc                                                                { T.Token {T.aToken=T.TkRequestMemory} }
  free                                                                  { T.Token {T.aToken=T.TkFreeMemory} }
%%

PROGRAM :: { G.Program }
  : programBegin ALIASES METHODS NON_OPENER_CODEBLOCK PROGRAMEND        {% do
                                                                             checkRecoverableError $1 $5
                                                                             return $ G.Program $4 }

PROGRAMEND :: { Maybe G.RecoverableError }
  : programEnd                                                          { Nothing }
  | error                                                               { Just G.MissingProgramEnd }

NON_OPENER_CODEBLOCK :: { G.CodeBlock }
  : instructionsBegin DECLARS INSTRL NON_OPENER_INSTEND                 {% do
                                                                             let (asigInsts, declarO) = $2
                                                                             checkRecoverableError $1 $4
                                                                             let (blockInsts, blockO) = $3
                                                                             let o = if blockO /= 0 then blockO else declarO
                                                                             return $ G.CodeBlock (asigInsts ++ reverse blockInsts ++ [G.InstReturn]) o }
  | instructionsBegin INSTRL NON_OPENER_INSTEND                         {% do
                                                                             checkRecoverableError $1 $3
                                                                             let (blockInsts, blockO) = $2
                                                                             return $ G.CodeBlock (reverse blockInsts ++ [G.InstReturn]) blockO }

NON_OPENER_INSTEND :: { Maybe G.RecoverableError }
  : instructionsEnd                                                     { Nothing }
  | error                                                               { Just G.MissingInstructionListEnd }

ALIASES :: { () }
  : ALIASLISTBEGIN ALIASL ALIASLISTEND                                  {% do
                                                                            checkRecoverableError $1 $3
                                                                            st <- RWS.get
                                                                            ST.popOffset
                                                                            RWS.put st{ST.stScopeStack=[1, 0]} }
  | {- empty -}                                                         { () }

ALIASLISTBEGIN :: { T.Token }
ALIASLISTBEGIN : aliasListBegin                                         {% ST.pushOffset 0 >> return $1 }

ALIASLISTEND :: { Maybe G.RecoverableError }
 : aliasListEnd                                                         { Nothing }
 | error                                                                { Just G.MissingAliasListEnd }

ALIASL :: { () }
  : ALIASL comma ALIASADD                                               { () }
  | ALIASADD                                                            { () }

ALIASADD :: { () }
  : ALIAS                                                               { }

ALIAS :: { NameDeclaration }
  : alias ID TYPE                                                       {% do
                                                                            let (ST.Simple i) = $3
                                                                            maybeEntry <- ST.dictLookup i
                                                                            let t = $3
                                                                            let extras = (case maybeEntry of
                                                                                            Nothing -> [t]
                                                                                            Just entry -> [t, ST.findWidth entry])
                                                                            return (ST.Type, $2, extras) }

EXPR :: { G.Expr }
  : intLit                                                              {% buildAndCheckExpr $1 $ G.IntLit (read (T.cleanedString $1) :: Int) }
  | floatLit                                                            {% buildAndCheckExpr $1 $ G.FloatLit (read (T.cleanedString $1) :: Float) }
  | charLit                                                             {% buildAndCheckExpr $1 $ G.CharLit $ head (T.cleanedString $1) }
  | stringLit                                                           {% buildAndCheckExpr $1 $ G.StringLit (T.cleanedString $1) }
  | STRUCTLIT                                                           {% do
                                                                            let (tk, stlit) = $1
                                                                            buildAndCheckExpr tk $ G.StructLit $ reverse stlit }
  | trueLit                                                             {% buildAndCheckExpr $1 G.TrueLit }
  | falseLit                                                            {% buildAndCheckExpr $1 G.FalseLit }
  | unknownLit                                                          {% buildAndCheckExpr $1 G.UndiscoveredLit }
  | nullLit                                                             {% buildAndCheckExpr $1 G.NullLit }
  | arrOpen EXPRL arrClose                                              {% buildAndCheckExpr $1 $ G.ArrayLit $ reverse $2 }
  | setOpen EXPRL setClose                                              {% buildAndCheckExpr $1 $ G.SetLit $ reverse $2 }
  | parensOpen EXPR PARENSCLOSE                                         {% do
                                                                            checkRecoverableError $1 $3
                                                                            return $ $2{G.expTok=$1} }
  | minus EXPR                                                          {% buildAndCheckExpr $1 $ G.Op1 G.Negate $2 }
  | not EXPR                                                            {% buildAndCheckExpr $1 $ G.Op1 G.Not $2 }
  | asciiOf EXPR                                                        {% buildAndCheckExpr $1 $ G.AsciiOf $2 }
  | isActive EXPR                                                       {% buildAndCheckExpr $1 $ G.IsActive $2 }
  | size EXPR                                                           {% buildAndCheckExpr $1 $ G.Size $2 }
  | EXPR plus EXPR                                                      {% buildAndCheckExpr $2 $ G.Op2 G.Add $1 $3 }
  | EXPR minus EXPR                                                     {% buildAndCheckExpr $2 $ G.Op2 G.Substract $1 $3 }
  | EXPR mult EXPR                                                      {% buildAndCheckExpr $2 $ G.Op2 G.Multiply $1 $3 }
  | EXPR div EXPR                                                       {% buildAndCheckExpr $2 $ G.Op2 G.Divide $1 $3 }
  | EXPR mod EXPR                                                       {% buildAndCheckExpr $2 $ G.Op2 G.Mod $1 $3 }
  | EXPR lt EXPR                                                        {% buildAndCheckExpr $2 $ G.Op2 G.Lt $1 $3 }
  | EXPR gt EXPR                                                        {% buildAndCheckExpr $2 $ G.Op2 G.Gt $1 $3 }
  | EXPR lte EXPR                                                       {% buildAndCheckExpr $2 $ G.Op2 G.Lte $1 $3 }
  | EXPR gte EXPR                                                       {% buildAndCheckExpr $2 $ G.Op2 G.Gte $1 $3 }
  | EXPR eq EXPR                                                        {% buildAndCheckExpr $2 $ G.Op2 G.Eq $1 $3 }
  | EXPR neq EXPR                                                       {% buildAndCheckExpr $2 $ G.Op2 G.Neq $1 $3 }
  | EXPR and EXPR                                                       {% buildAndCheckExpr $2 $ G.Op2 G.And $1 $3 }
  | EXPR or EXPR                                                        {% buildAndCheckExpr $2 $ G.Op2 G.Or $1 $3 }
  | EXPR colConcat EXPR                                                 {% buildAndCheckExpr $2 $ G.Op2 G.ColConcat $1 $3 }
  | EXPR union EXPR                                                     {% buildAndCheckExpr $2 $ G.Op2 G.SetUnion $1 $3 }
  | EXPR intersect EXPR                                                 {% buildAndCheckExpr $2 $ G.Op2 G.SetIntersect $1 $3 }
  | EXPR diff EXPR                                                      {% buildAndCheckExpr $2 $ G.Op2 G.SetDifference $1 $3 }
  | FUNCALL                                                             {% let (tk, i, params) = $1 in buildAndCheckExpr tk $ G.EvalFunc i params }
  | ID                                                                  {% do
                                                                            let (G.Id tk _) = $1
                                                                            buildAndCheckExpr tk $ G.IdExpr $1 }
  | EXPR accessor ID                                                    {% do
                                                                            let expr = G.Access $1 $3
                                                                            buildAndCheckExpr $2 expr }
  | EXPR arrOpen EXPR arrClose                                          {% buildAndCheckExpr $2 $ G.IndexAccess $1 $3 }
  | memAccessor EXPR                                                    {% buildAndCheckExpr $1 $ G.MemAccess $2 }

STRUCTLIT :: { (T.Token, [(G.Id, G.Expr)]) }
  : brOpen PROPLIST BRCLOSE                                             {% do
                                                                            checkRecoverableError $1 $3
                                                                            return ($1, $2) }

PROPLIST :: { [(G.Id, G.Expr)]}
  : PROP                                                                { [$1] }
  | PROPLIST comma PROP                                                  { $3:$1 }

PROP :: { (G.Id, G.Expr) }
  : ID asig EXPR                                                        { ($1, $3) }

EXPRL :: { [G.Expr] }
  : {- empty -}                                                         { [] }
  | NONEMPTYEXPRL                                                       { $1 }

NONEMPTYEXPRL :: { [G.Expr] }
  : EXPR                                                                { [$1] }
  | NONEMPTYEXPRL comma EXPR                                            { $3:$1 }

METHODS :: { () }
  : {- empty -}                                                         {% ST.pushOffset 0 }
  | METHODL                                                             {% ST.pushOffset 0 }

METHODL :: { () }
  : METHODL METHOD                                                      { () }
  | METHOD                                                              { () }

METHOD :: { () }
  : FUNC                                                                {% do
                                                                          st <- RWS.get
                                                                          ST.popOffset
                                                                          RWS.put st{ST.stScopeStack=[1, 0]} }
  | PROC                                                                {% do
                                                                          st <- RWS.get
                                                                          ST.popOffset
                                                                          RWS.put st{ST.stScopeStack=[1, 0]} }

FUNC :: { () }
  : FUNCPREFIX CODEBLOCK functionEnd                                    {% do
                                                                            ST.popVisitedMethod
                                                                            case $1 of
                                                                              Nothing -> return ()
                                                                              Just i -> updateCodeBlockOfFun i $2 }

FUNCPREFIX :: { Maybe G.Id }
  : FUNCNAME METHODPARS functionType TYPE                               {% do
                                                                          currScope <- ST.getCurrentScope
                                                                          ST.exitScope
                                                                          let extras = [ $4, ST.Fields ST.Callable currScope ]
                                                                          ST.addVisitedMethod $ G.extractIdName $1
                                                                          ST.pushScope currScope

                                                                          return $ Just $1 }

FUNCNAME :: { G.Id }
FUNCNAME : functionBegin ID                                             {% do
                                                                            offset <- getMethodOffset $2
                                                                            ST.enterScope >> ST.pushOffset offset >> return $2 }

PROC :: { () }
  : PROCPREFIX CODEBLOCK procedureEnd                                   {% do
                                                                            ST.popVisitedMethod
                                                                            case $1 of
                                                                              Nothing -> return ()
                                                                              Just i -> updateCodeBlockOfFun i $2 }

PROCPREFIX :: { Maybe G.Id }
  : PROCNAME PROCPARS                                                   {% do
                                                                            currScope <- ST.getCurrentScope
                                                                            ST.exitScope
                                                                            let extras = [ ST.Simple ST.void, ST.Fields ST.Callable currScope ]
                                                                            st <- RWS.get >>= return . ST.stDict

                                                                            ST.addVisitedMethod $ G.extractIdName $1
                                                                            ST.pushScope currScope
                                                                            return $ Just $1 }

PROCNAME :: { G.Id }
PROCNAME : procedureBegin ID                                            {% do
                                                                            offset <- getMethodOffset $2
                                                                            ST.enterScope >> ST.pushOffset offset >> return $2 }

PROCPARS :: { Int }
PROCPARS : paramRequest PARS toTheEstusFlask                            {% ST.resetArgPosition >> return $2 }
  | {- empty -}                                                         { 0 }

METHODPARS :: { Int }
  : paramRequest PARS                                                   {% ST.resetArgPosition >> return $2 }
  | {- empty -}                                                         { 0 }

PARS :: { Int }
  : PARS comma PAR                                                      { 1 + $1 }
  | PAR                                                                 { 1 }

PAR :: { () }
  : PARTYPE ID ofType TYPE                                              { }

PARTYPE :: { ST.Category }
  : parVal                                                              { ST.ValueParam }
  | parRef                                                              { ST.RefParam }

{- TYPE always returns "ST.Simple String" -}
TYPE :: { ST.Extra }
  : ID                                                                  {% do
                                                                          let G.Id t _ = $1
                                                                          maybeEntry <- ST.dictLookup (T.cleanedString t)
                                                                          case maybeEntry of
                                                                            Just _ -> return ()
                                                                            _ -> logSemError ("Type " ++ show t ++ " not found") t
                                                                          return $ ST.Simple (T.cleanedString t) }
  | bigInt                                                              { ST.Simple ST.humanity }
  | smallInt                                                            { ST.Simple ST.smallHumanity }
  | float                                                               { ST.Simple ST.hollow }
  | char                                                                { ST.Simple ST.sign }
  | bool                                                                { ST.Simple ST.bonfire }
  | ltelit EXPR array ofType TYPE                                       {% do
                                                                          aliasName <- ST.genAliasName
                                                                          updateExprSizeForEntry aliasName $2
                                                                          return $ ST.Simple aliasName }
  | ltelit EXPR string                                                  {% do
                                                                          aliasName <- ST.genAliasName
                                                                          updateExprSizeForEntry aliasName $2
                                                                          return $ ST.Simple aliasName }
  | set ofType TYPE                                                     {% fmap ST.Simple ST.genAliasName }
  | pointer TYPE                                                        {% fmap ST.Simple ST.genAliasName }
  | RECORD_OPEN  brOpen STRUCTITS BRCLOSE                               {% do
                                                                            checkRecoverableError $2 $4
                                                                            currScope <- ST.getCurrentScope
                                                                            nextOffset <- ST.getNextOffset
                                                                            ST.exitScope
                                                                            ST.popOffset
                                                                            fmap ST.Simple ST.genAliasName }
  | UNION_OPEN brOpen UNIONITS BRCLOSE                                  {% do
                                                                             checkRecoverableError $2 $4
                                                                             currScope <- ST.getCurrentScope
                                                                             ST.exitScope
                                                                             ST.popUnion
                                                                             ST.popOffset
                                                                             ST.genAliasName >>= \x -> return $ ST.Simple x }

RECORD_OPEN :: { T.Token }
RECORD_OPEN : record                                                    {% ST.enterScope >> ST.pushOffset 0 >> return $1 }

UNION_OPEN :: { T.Token }
UNION_OPEN : unionStruct                                                {% ST.enterScope >> ST.pushOffset 0 >> ST.newUnion >> return $1 }

BRCLOSE :: { Maybe G.RecoverableError }
  : brClose                                                             { Nothing }
  | error                                                               { Just G.MissingClosingBrace }

PARENSCLOSE :: { Maybe G.RecoverableError }
  : parensClose                                                         { Nothing }
  | error                                                               { Just G.MissingClosingParens }

UNIONITS :: { Int }
  : UNIONITS comma UNIONIT                                             { max $1 $3 }
  | UNIONIT                                                            { $1 }

UNIONIT :: { Int }
  : ID ofType TYPE                                                      {% do
                                                                            let ST.Simple t = $3
                                                                            maybeTypeEntry <- ST.dictLookup t
                                                                            case maybeTypeEntry of
                                                                              Nothing -> return 0
                                                                              Just entry -> do
                                                                                let ST.Width w = ST.findWidth entry
                                                                                return w }

STRUCTITS :: { () }
  : STRUCTITS comma STRUCTIT                                            { () }
  | STRUCTIT                                                            { () }

STRUCTIT :: { () }
  : ID ofType TYPE                                                      { }

ID :: { G.Id }
  : id                                                                  {% do
                                                                            currScope <- ST.getCurrentScope
                                                                            return $ G.Id $1 currScope }

CODEBLOCK :: { G.CodeBlock }
  : INSTBEGIN DECLARS INSTRL INSTEND                                    {% do
                                                                             let (asigInsts, declarO) = $2
                                                                             checkRecoverableError $1 $4
                                                                             let (blockInsts, blockO) = $3
                                                                             let o = if blockO /= 0 then blockO else declarO
                                                                             return $ G.CodeBlock (asigInsts ++ reverse blockInsts) o }
  | INSTBEGIN INSTRL INSTEND                                            {% do
                                                                             checkRecoverableError $1 $3
                                                                             let (blockInsts, blockO) = $2
                                                                             nextOffset <- ST.getNextOffset
                                                                             let o = if blockO /= 0 then blockO else nextOffset
                                                                             return $ G.CodeBlock (reverse blockInsts) o }

INSTBEGIN :: { T.Token }
INSTBEGIN : instructionsBegin                                           {% do
                                                                            ST.enterScope
                                                                            nextOffset <- ST.getNextOffset
                                                                            ST.pushOffset nextOffset
                                                                            st <- RWS.get
                                                                            return $1 }

INSTEND :: { Maybe G.RecoverableError }
  : instructionsEnd                                                     {% do
                                                                             ST.exitScope
                                                                             ST.popOffset
                                                                             return Nothing }
  | error                                                               { Just G.MissingInstructionListEnd }

DECLARS :: { ([G.Instruction], Int) }
  : with DECLARSL DECLAREND                                             {% do
                                                                            let (insts, o) = $2
                                                                            checkRecoverableError $1 $3
                                                                            return (insts, o) }

DECLAREND :: { Maybe G.RecoverableError }
  : declarend                                                           { Nothing }
  | error                                                               { Just G.MissingDeclarationListEnd }

DECLARSL :: { ([G.Instruction], Int) }
  : DECLARSL comma DECLARADD                                            {% do
                                                                            let (mints, _) = $1
                                                                            let (mint, o) = $3
                                                                            return (mints ++ mint, o) }
  | DECLARADD                                                           { $1 }

DECLARADD :: { ([G.Instruction], Int) }
  : DECLAR                                                              {% do
                                                                            -- Adds a declaration to the ST as soon as it's parsed
                                                                            let (declar, _) = $1
                                                                            addIdToSymTable declar
                                                                            let ((_, lvalueId, _), maybeRValue) = $1
                                                                            let baseLvalue = G.IdExpr lvalueId
                                                                            let (G.Id idToken _) = lvalueId
                                                                            lvalue <- buildAndCheckExpr idToken baseLvalue
                                                                            mInstr <- case maybeRValue of
                                                                              Nothing -> do
                                                                                assignment <- defaultAssignment lvalue
                                                                                case assignment of
                                                                                  Nothing -> return []
                                                                                  Just asig -> return [asig]
                                                                              Just (token, rvalue) -> do
                                                                                ST.SymTable {ST.stScopeStack = stack} <- RWS.get
                                                                                asig <- checkAssignment lvalue token rvalue True
                                                                                return [asig]
                                                                            offsetW <- do
                                                                              mEntry <- ST.dictLookup $ G.extractIdName lvalueId
                                                                              case mEntry of
                                                                                Nothing -> return 0
                                                                                Just entry@(ST.DictionaryEntry {ST.entryType = Just t}) -> do
                                                                                  mTypeEntry <- ST.dictLookup t
                                                                                  case mTypeEntry of
                                                                                    Nothing -> return 0
                                                                                    Just tEntry -> do
                                                                                      let ST.Offset o = ST.findOffset entry
                                                                                      let ST.Width w = ST.findWidth tEntry
                                                                                      return $ o + w
                                                                            t <- getType lvalue
                                                                            let instInit = (case t of
                                                                                              T.ArrayT _ -> let (G.IdExpr i) = G.expAst lvalue in [G.InstInitArray i]
                                                                                              T.StringT -> let (G.IdExpr i) = G.expAst lvalue in [G.InstInitArray i]
                                                                                              _ -> [])
                                                                            return (instInit ++ mInstr, offsetW) }

DECLAR :: { (NameDeclaration, Maybe (T.Token, G.Expr)) }
  : var ID ofType TYPE                                                  { ((ST.Variable, $2, [$4]), Nothing) }
  | var ID ofType TYPE asig EXPR                                        { ((ST.Variable, $2, [$4]), Just ($5, $6))  }
  | const ID ofType TYPE asig EXPR                                      { ((ST.Constant, $2, [$4]), Just ($5, $6)) }

INSTRL :: { (G.Instructions, Int) }
  : INSTRL seq INSTR                                                    { let (prevInstrs, prevOffset) = $1 in
                                                                          ($3 : prevInstrs, max prevOffset $ G.getInstrOffset $3) }
  | INSTR                                                               { ([$1], G.getInstrOffset $1) }

INSTR :: { G.Instruction }
  : EXPR asig EXPR                                                      {% do
                                                                          checkLvalue $1
                                                                          checkAssignment $1 $2 $3 False }
  | malloc EXPR                                                         {% do
                                                                          checkPointerVariable $2
                                                                          return $ G.InstMalloc $2 }
  | free EXPR                                                           {% do
                                                                          checkPointerVariable $2
                                                                          return $G.InstFreeMem $2 }
  | cast ID PROCARGS                                                    {% do
                                                                          checkIdAvailability $2
                                                                          (gId, _, params) <- methodsCheck $2 $3 $1
                                                                          return $ G.InstCall gId params }
  | FUNCALL                                                             {% let (tk, i, params) = $1 in do
                                                                          (gId, _, params) <- methodsCheck i params tk
                                                                          return $ G.InstCall gId params }
  | return                                                              {% checkReturnScope $1 }
  | returnWith EXPR                                                     {% do
                                                                          retExpr <- checkReturnType $2 $1
                                                                          return $ G.InstReturnWith retExpr }
  | print EXPR                                                          {% do
                                                                            checkIOTypes $2 $1
                                                                            return $ G.InstPrint $2 }
  | read EXPR                                                           {% do
                                                                            validLvalue <- checkLvalue $2
                                                                            RWS.when validLvalue $ checkIOTypes $2 $1
                                                                            return $ G.InstRead $2 }
  | whileBegin EXPR covenantIsActive COLON CODEBLOCK WHILEEND           {% do
                                                                            checkRecoverableError $1 $4
                                                                            checkRecoverableError $1 $6
                                                                            checkBooleanGuard $2
                                                                            return $ G.InstWhile $2 $5 }
  | ifBegin IFCASES ELSECASE IFEND                                      {% do
                                                                          checkRecoverableError $1 $4
                                                                          return $ G.InstIf (reverse ($3 : $2)) }
  | ifBegin IFCASES IFEND                                               {% do
                                                                          checkRecoverableError $1 $3
                                                                          return $ G.InstIf (reverse $2) }
  | SWITCHBEGIN COLON SWITCHCASES DEFAULTCASE SWITCHEND                 {% do
                                                                          let (switchTk, switchExpr) = $1
                                                                          checkRecoverableError switchTk $2
                                                                          checkRecoverableError switchTk $5
                                                                          ST.popSwitchType
                                                                          return $ G.InstSwitch switchExpr (reverse ($4 : $3)) }
  | SWITCHBEGIN COLON SWITCHCASES SWITCHEND                             {% do
                                                                          let (switchTk, switchExpr) = $1
                                                                          checkRecoverableError switchTk $2
                                                                          checkRecoverableError switchTk $4
                                                                          ST.popSwitchType
                                                                          return $ G.InstSwitch switchExpr (reverse $3) }
  | FORBEGIN CODEBLOCK FOREND                                           {% do
                                                                          let (fstTk, iterVar, shouldPopIterVar, startExpr, stopExpr) = $1
                                                                          checkRecoverableError fstTk $3
                                                                          if shouldPopIterVar
                                                                          then do
                                                                            ST.popIteratorVariable
                                                                          else return ()
                                                                          return $ G.InstFor iterVar startExpr stopExpr $2 }
  | FOREACHBEGIN CODEBLOCK FOREACHEND                                   {% do
                                                                          let (fstTk, iterVar, shouldPopIterVar, shouldPopIterableVar, iteredExpr) = $1
                                                                          checkRecoverableError fstTk $3
                                                                          RWS.when shouldPopIterVar ST.popIteratorVariable
                                                                          RWS.when shouldPopIterableVar ST.popIterableVariable
                                                                          return $ G.InstForEach iterVar iteredExpr $2 }

SWITCHBEGIN :: { (T.Token, G.Expr) }
  : switchBegin EXPR                                                    {% do
                                                                          ST.addSwitchType (G.expType $2)
                                                                          return ($1, $2) }

FORBEGIN :: { (T.Token, G.Id, Bool, G.Expr, G.Expr) }
  : forBegin ID with EXPR souls untilLevel EXPR                         {% do
                                                                          t1 <- getType $4
                                                                          RWS.when (not $ isIntegerType t1) $
                                                                            logSemError "Step should be an integer" $ G.expTok $4
                                                                          t2 <- getType $7
                                                                          RWS.when (not $ isIntegerType t2) $
                                                                            logSemError "Stop should be an integer" $ G.expTok $7

                                                                          mid <- checkIdAvailability $2
                                                                          case mid of
                                                                            Just ST.DictionaryEntry {ST.name=varName} -> do
                                                                              checkIterVarType $2
                                                                              ST.addIteratorVariable varName
                                                                              return ($1, $2, True, $4, $7)
                                                                            Nothing -> return ($1, $2, False, $4, $7) }

FOREND :: { Maybe G.RecoverableError }
  : forEnd                                                              { Nothing }
  | error                                                               { Just G.MissingForEnd }

FOREACHBEGIN :: { (T.Token, G.Id, Bool, Bool, G.Expr) }
  : forEachBegin ID withTitaniteFrom EXPR                               {% do
                                                                          iteratorType <- getType $2
                                                                          containerType <- getType $4
                                                                          let isContainerAVariable = (case G.expAst $4 of
                                                                                                        G.IdExpr _ -> True
                                                                                                        _ -> False)
                                                                          let ret = ($1, $2, iteratorType /= T.TypeError, containerType /= T.TypeError && isContainerAVariable, $4)
                                                                          if iteratorType == T.TypeError then do
                                                                            if containerType == T.TypeError then return ()
                                                                            else
                                                                              case T.getTypeFromContainer containerType of
                                                                                Nothing -> do
                                                                                  logSemError "Iterable expression is not a container" $ G.expTok $4
                                                                                _ -> do
                                                                                  return ()
                                                                          else do
                                                                            checkIterVarType $2
                                                                            ST.addIteratorVariable $ G.extractIdName $2
                                                                            if containerType == T.TypeError then return ()
                                                                            else
                                                                              case T.getTypeFromContainer containerType of
                                                                                Nothing -> do
                                                                                  logSemError "Iterable expression is not a container" $ G.expTok $4
                                                                                Just t -> do
                                                                                  let (G.Id tk _) = $2
                                                                                  RWS.when (T.TypeError /= t && iteratorType /= t) $
                                                                                    logSemError "Iterator variable is not of the type of contained elements" tk
                                                                          if containerType /= T.TypeError && isContainerAVariable then do
                                                                            let (G.IdExpr (G.Id tk _)) = G.expAst $4
                                                                            ST.addIterableVariable (T.cleanedString tk)
                                                                          else return ()
                                                                          return ret
                                                                             }

FOREACHEND :: { Maybe G.RecoverableError }
  : forEachEnd                                                          { Nothing }
  | error                                                               { Just G.MissingForEachEnd }

WHILEEND :: { Maybe G.RecoverableError }
  : whileEnd                                                            { Nothing }
  | error                                                               { Just G.MissingWhileEnd }

COLON :: { Maybe G.RecoverableError }
  : colon                                                               { Nothing }
  | error                                                               { Just G.MissingColon }

FUNCALL :: { (T.Token, G.Id, G.Params) }
  : summon ID FUNCPARS                                                  { ($1, $2, $3) }

IFCASES :: { G.IfCases }
  : IFCASES IFCASE                                                      { $2 : $1 }
  | IFCASE                                                              { [$1] }

IFCASE :: { G.IfCase }
  : EXPR COLON CODEBLOCK                                                {% do
                                                                            checkRecoverableError (G.expTok $1) $2
                                                                            checkBooleanGuard $1
                                                                            return $ G.GuardedCase $1 $3 }

ELSECASE :: { G.IfCase }
  : else COLON CODEBLOCK                                                {% do
                                                                            checkRecoverableError $1 $2
                                                                            return $ G.GuardedCase (G.Expr
                                                                                                    { G.expAst = G.TrueLit
                                                                                                    , G.expType = T.TrileanT
                                                                                                    , G.expTok = (T.Token
                                                                                                                    { T.aToken = T.TkLit
                                                                                                                    , T.capturedString = "lit"
                                                                                                                    , T.cleanedString = "lit"})
                                                                                                    }) $3 }

IFEND :: { Maybe G.RecoverableError }
  : ifEnd                                                               { Nothing }
  | error                                                               { Just G.MissingIfEnd }

SWITCHCASES :: { G.SwitchCases }
  : SWITCHCASES SWITCHCASE                                              { $2 : $1 }
  | SWITCHCASE                                                          { [$1] }

SWITCHCASE :: { G.SwitchCase }
  : EXPR COLON CODEBLOCK                                                {% do
                                                                            checkRecoverableError (G.expTok $1) $2
                                                                            expr <- checkSwitchCaseType $1
                                                                            return $ G.Case expr $3 }

SWITCHEND :: { Maybe G.RecoverableError }
  : switchEnd                                                           { Nothing }
  | error                                                               { Just G.MissingSwitchEnd }

DEFAULTCASE :: { G.SwitchCase }
  : switchDefault COLON CODEBLOCK                                       {% do
                                                                            checkRecoverableError $1 $2
                                                                            return $ G.DefaultCase $3 }

FUNCPARS :: { G.Params }
  : granting PARSLIST TOTHEKNIGHT                                       {% do
                                                                             checkRecoverableError $1 $3
                                                                             return $ reverse $2 }
  | {- empty -}                                                         { [] }

TOTHEKNIGHT :: { Maybe G.RecoverableError }
  : toTheKnight                                                         { Nothing }
  | error                                                               { Just G.MissingFunCallEnd }

PROCARGS :: { G.Params }
  : offering PARSLIST TOTHEESTUSFLASK                                   {% do
                                                                             checkRecoverableError $1 $3
                                                                             return $ reverse $2 }
  | {- empty -}                                                         { [] }

TOTHEESTUSFLASK :: { Maybe G.RecoverableError }
  : toTheEstusFlask                                                     { Nothing }
  | error                                                               { Just G.MissingProcCallEnd }

PARSLIST :: { G.Params }
  : PARSLIST comma EXPR                                                 { $3 : $1 }
  | EXPR                                                                { [$1] }

{

type NameDeclaration = (ST.Category, G.Id, [ST.Extra])
type RecordItem = (G.Id, ST.Extra)

getMethodOffset :: G.Id -> ST.ParserMonad (Int)
getMethodOffset (G.Id (T.Token {T.cleanedString=methodName}) _) = do
  mEntry <- ST.dictLookup methodName
  case mEntry of
    Nothing -> return 0
    Just ST.DictionaryEntry {ST.extra = extras } -> do
      let (ST.Fields _ scope) = head $ filter ST.isFieldsExtra extras
      ST.SymTable {ST.stDict=dict} <- RWS.get
      let paramEntries = ST.findAllInScope scope dict
      if length paramEntries == 0 then return 0
      else do
        let lastParam@(ST.DictionaryEntry {ST.entryType=Just typeName, ST.extra=paramExtras, ST.scope=parScope}) = last paramEntries
        ST.pushScope parScope
        mtEntry <- ST.dictLookup typeName
        ST.exitScope
        case mtEntry of
          Nothing -> logRTError ("Param with no type found for alleged type " ++ typeName) >> return 0
          Just tEntry -> do
            let ST.Width typeWidth = ST.findWidth tEntry
            let ST.Offset varOffset = ST.findOffset lastParam
            return $ typeWidth + varOffset

buildAndCheckExpr :: T.Token -> G.BaseExpr -> ST.ParserMonad G.Expr
buildAndCheckExpr tk bExpr = do
  (t, expr) <- buildType bExpr tk
  return G.Expr
    { G.expAst = expr,
      G.expType = t,
      G.expTok = tk
    }

-- | Only used by array types
updateExprSizeForEntry :: String -> G.Expr -> ST.ParserMonad ()
updateExprSizeForEntry typeName expr = do
  mEntry <- ST.dictLookup typeName
  case mEntry of
    Nothing -> return ()
    Just entry -> do
      let t = ST.extractTypeFromExtra entry
      let restExtras = filter (not . ST.isExtraAType) $ ST.extra entry
      case t of
        ST.CompoundRec s _ ex -> updateExtras entry $ (ST.CompoundRec s expr ex) : restExtras
        ST.Compound s _ -> updateExtras entry $ (ST.Compound s expr) : restExtras
  where
    updateExtras :: ST.DictionaryEntry -> [ST.Extra] -> ST.ParserMonad ()
    updateExtras entry extras = do
        let f x = (if and [ST.scope x == ST.scope entry, ST.name x == ST.name entry]
            then x{ST.extra = extras}
            else x) in ST.updateEntry (\ds -> Just $ map f ds) $ ST.name entry


parseErrors :: [T.Token] -> ST.ParserMonad a
parseErrors errors =
  let tk@T.Token {T.aToken=abst, T.position=pn} = errors !! 0
      name = show tk
      line = show $ row pn
      column' = show $ column pn
      header = bold ++ red ++ "YOU DIED!!" ++ nocolor ++ " Parser error: "
      endmsg = "\n\nFix your syntax errors, ashen one."
      position = "line " ++ bold ++ red ++ line ++ nocolor ++ ", column " ++ bold ++ red ++ column' ++ nocolor ++ "."
      msg = header ++ "Unexpected token " ++ bold ++ red ++ name ++ nocolor ++ " at " ++ position ++ endmsg
  in  fail msg

addIdsToSymTable :: [NameDeclaration] -> ST.ParserMonad ()
addIdsToSymTable = RWS.mapM_ addIdToSymTable

addIdToSymTable :: NameDeclaration -> ST.ParserMonad ()
addIdToSymTable d@(c, gId@(G.Id tk@(T.Token {T.aToken=at, T.cleanedString=idName}) _), t) = do
  maybeIdEntry <- ST.dictLookup idName
  let typeEntry = getTypeNameForSymTable $ head $ filter ST.isExtraAType t
  currScope <- ST.getCurrentScope
  ST.SymTable {ST.stIterationVars=iterVars} <- RWS.get
  case maybeIdEntry of
    -- The name doesn't exists on the table, we just add it
    Nothing ->
      assignOffsetAndInsert ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = Just typeEntry
        , ST.extra = t
        }

    -- The name does exists on the table, we just add it depending on the scope
    Just entry -> do
      let scope = ST.scope entry
      let category = ST.category entry
      if category == ST.Type && c /= ST.RecordItem
      then logSemError ("Name " ++ show tk ++ " conflicts with an type alias") tk
      else if category == ST.Procedure
      then logSemError ("Name " ++ show tk ++ " conflicts with a procedure") tk
      else if category == ST.Function
      then logSemError ("Name " ++ show tk ++ " conflicts with a function") tk
      else if category == ST.RefParam || category == ST.ValueParam
      then logSemError ("Name " ++ show tk ++ " conflicts with a formal param") tk
      else if (T.cleanedString tk) `elem` iterVars
      then logSemError ("Name " ++ show tk ++ " conflicts or shadows an iteration variable") tk
      else if currScope /= scope
      then assignOffsetAndInsert ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = Just typeEntry
        , ST.extra = t
        }
      else logSemError ("Name " ++ idName ++ " was already declared on this scope") tk
  where
    getTypeNameForSymTable :: ST.Extra -> String
    getTypeNameForSymTable (ST.Simple s) = s
    getTypeNameForSymTable (ST.Recursive s _) = s
    getTypeNameForSymTable (ST.Compound s _) = s
    getTypeNameForSymTable (ST.CompoundRec s _ _) = s
    getTypeNameForSymTable (ST.Fields ST.Record _) = ST.bezel
    getTypeNameForSymTable (ST.Fields ST.Union _) = ST.link


assignOffsetAndInsert :: ST.DictionaryEntry -> ST.ParserMonad ()
assignOffsetAndInsert entry = do
  let extras = ST.extra entry
  finalExtras <-
    if requiresOffset then do
      let (ST.Simple t) = ST.extractTypeFromExtra entry
      maybeTypeEntry <- ST.dictLookup t
      case maybeTypeEntry of
        Nothing -> return extras
        Just typeEntry -> do
          nextOffset <- ST.getNextOffset
          let ST.Width realWidth = ST.findWidth typeEntry

          -- Ref params occupies 1 wordSize
          let width = if category == ST.RefParam then ST.wordSize else realWidth

          let o = nextOffset `mod` ST.wordSize
          let remainingOffset = ST.wordSize - o
          let finalOffset = if category == ST.UnionItem then ST.wordSize
                            else if o == 0 || width <= remainingOffset then nextOffset
                            else nextOffset + remainingOffset
          ST.putNextOffset $ finalOffset + width
          return $ (ST.Offset finalOffset) : extras
    else return extras
  ST.addEntry entry{ST.extra=finalExtras}
  where
    category :: ST.Category
    category = ST.category entry
    categoriesThatRequireOffset :: [ST.Category]
    categoriesThatRequireOffset =
      [ ST.ValueParam, ST.RefParam, ST.RecordItem
      , ST.Variable, ST.Constant, ST.UnionItem ]

    requiresOffset :: Bool
    requiresOffset = category `elem` categoriesThatRequireOffset

checkConstantReassignment :: G.Expr -> ST.ParserMonad ()
checkConstantReassignment e = case G.expAst e of
  G.IdExpr (G.Id tk@(T.Token {T.cleanedString=idName}) _) -> do
    maybeEntry <- ST.dictLookup idName
    case maybeEntry of
      Nothing -> do
        return ()
      Just e ->
        case (ST.category e) of
          ST.Constant -> do
            logSemError ("Name " ++ idName ++ " is a constant and must not be reassigned") tk
            return ()
          _ ->
            return ()
  G.IndexAccess gId _ -> checkConstantReassignment gId
  _ -> return ()

checkPointerVariable :: G.Expr -> ST.ParserMonad ()
checkPointerVariable G.Expr {G.expType=(T.PointerT _)} = return ()
checkPointerVariable exp@G.Expr {G.expType=(T.AliasT alias)} = do
  t <- getTypeFromAliasName alias
  case t of
    T.PointerT _ -> return ()
    _ -> checkPointerVariable exp {G.expType=T.TypeError}
checkPointerVariable G.Expr {G.expTok=tk} =
  logSemError ("Expresion " ++ show tk ++ " must be a valid pointer") tk

checkReturnScope :: T.Token -> ST.ParserMonad (G.Instruction)
checkReturnScope tk = do
  ST.SymTable {ST.stVisitedMethod=vMethod} <- RWS.get
  case vMethod of
    Nothing -> return (G.InstReturn)
    Just methodName -> do
      currMethod <- ST.dictLookup $ methodName
      case currMethod of
        Just (ST.DictionaryEntry {ST.category=cat}) -> do
          case cat of
            ST.Function -> do
              logSemError ("Returning without an expression is not allowed inside functions") tk
              return (G.InstReturn)
            _ -> return (G.InstReturn)
        _ -> return (G.InstReturn)

checkReturnType :: G.Expr -> T.Token -> ST.ParserMonad (G.Expr)
checkReturnType e tk = do
  let eType = G.expType e
  ST.SymTable {ST.stVisitedMethod=vMethod} <- RWS.get
  case vMethod of
    Nothing -> do
      logSemError ("Returning with an expression outside of a function not allowed") tk
      return e
    Just methodName -> do
      currMethod <- ST.dictLookup $ methodName
      case currMethod of
        Nothing -> do
          logSemError ("Returning with an expression outside of a function not allowed") tk
          return e
        Just method -> do
          let ST.DictionaryEntry {ST.category=cat} = method
          mType <- getType method
          case cat of
            ST.Function -> do
              let T.FunctionT _ fType = mType
              -- If they're equal, or compatible through casting, this raises no error
              if eType `T.canBeConvertedTo` fType
              then let [castedExpr] = addCastToExprs [(e, fType)] in
                return (castedExpr)
              else if not (T.TypeError `elem` [eType, fType]) then do
                logSemError ("Return expression type " ++ show eType ++ " incompatible with function return type " ++ show fType) tk
                return e
              else return e
            _ -> do
              logSemError ("Returning with an expression outside of a function not allowed") tk
              return e

checkIterVariables :: G.Expr -> ST.ParserMonad ()
checkIterVariables e = case G.expAst e of
  G.IdExpr (G.Id tk@(T.Token {T.cleanedString=idName}) _) -> do
    ST.SymTable {ST.stIterationVars=iterVars} <- RWS.get
    let matchesIterVar = idName `elem` iterVars
    if matchesIterVar
    then logSemError ("Iteration variable " ++ show tk ++ " must not be reassigned") tk
    else return ()
  _ -> return ()

checkIterableVariables :: G.Expr -> ST.ParserMonad ()
checkIterableVariables e = case G.expAst e of
  G.IdExpr (G.Id tk@(T.Token {T.cleanedString=idName}) _) -> do
    ST.SymTable {ST.stIterableVars=iterableVars} <- RWS.get
    let matchesIterVar = idName `elem` iterableVars
    if matchesIterVar
    then logSemError ("Iterable container " ++ show tk ++ " must not be reassigned") tk
    else return ()
  _ -> return ()

checkIdAvailability :: G.Id -> ST.ParserMonad (Maybe ST.DictionaryEntry)
checkIdAvailability (G.Id tk@(T.Token {T.cleanedString=idName}) _) = do
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      logSemError ("Name " ++ show tk ++ " is not available on this scope") tk
      return Nothing
    Just e -> return $ Just e

checkIterVarType :: G.Id -> ST.ParserMonad ()
checkIterVarType (G.Id tk@(T.Token {T.cleanedString=idName}) _) = do
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> return ()
    Just (ST.DictionaryEntry {ST.category=varCat}) -> do
      if varCat == ST.Constant
      then do
        logSemError ("Constant " ++ show tk ++ " can not be used as an iteration variable") tk
      else return ()

checkSwitchCaseType :: G.Expr -> ST.ParserMonad (G.Expr)
checkSwitchCaseType expr = do
  ST.SymTable {ST.stSwitchTypes=swTypes} <- RWS.get
  if swTypes == [] then return (expr)
  else do
    let swType = head swTypes
    case swType of
      T.TypeError -> return (expr)
      tp -> do
        let exprType = G.expType expr
        if exprType `T.canBeConvertedTo` tp
        then let [castedExpr] = addCastToExprs [(expr, tp)] in
          return (castedExpr)
        else do
          let tk = G.expTok expr
          RWS.tell [Error ("Expresion type " ++ show exprType ++ " conflicts with switch type (expected: " ++ show tp ++ ")") (T.position tk)]
          return (expr)

checkRecoverableError :: T.Token -> Maybe G.RecoverableError -> ST.ParserMonad ()
checkRecoverableError openTk maybeErr = do
  case maybeErr of
    Nothing -> return ()
    Just err -> do
      let errorName = show err
      logSemError (errorName ++ " (recovered from to continue parsing)") openTk

checkIOTypes :: G.Expr -> T.Token -> ST.ParserMonad ()
checkIOTypes expr tk = do
  t <- getType expr
  if t `elem` T.ioTypes then return ()
  -- Avoid propagation
  else if t == T.TypeError then return ()
  else logSemError (show t ++ " is not an I/O valid type") tk

defaultAssignment :: G.Expr -> ST.ParserMonad (Maybe G.Instruction)
defaultAssignment lvalue = do
  t <- getType lvalue
  if t `elem` T.defaultableTypes then case t of
    -- TODO: Change default type of Trileans to Undiscovered (currently Lit due to undiscovered not supported)
    T.TrileanT -> return $ Just $ G.InstAsig lvalue $ lvalue { G.expAst = G.TrueLit }
    T.BigIntT -> return $ Just $ G.InstAsig lvalue $ lvalue { G.expAst = G.IntLit 0 }
    T.SmallIntT -> return $ Just $ G.InstAsig lvalue $ lvalue { G.expAst = G.IntLit 0 }
    T.FloatT -> return $ Just $ G.InstAsig lvalue $ lvalue { G.expAst = G.FloatLit 0.0 }
    T.CharT -> return $ Just $ G.InstAsig lvalue $ lvalue { G.expAst = G.CharLit '\0' }
    _ -> return Nothing
  else return Nothing

checkAssignment :: G.Expr -> T.Token -> G.Expr -> Bool -> ST.ParserMonad G.Instruction
checkAssignment lvalue token rvalue isInit = do
  RWS.unless isInit $ do
    checkConstantReassignment lvalue
    checkIterVariables lvalue
    checkIterableVariables lvalue
  t <- getType rvalue
  asigRvalue <-
    if t == T.StructLitT then checkStructAssignment lvalue rvalue token
    else checkTypeOfAssignment lvalue rvalue token >> return rvalue
  return $ G.InstAsig lvalue asigRvalue

extractFieldsFromExtra :: [ST.Extra] -> ST.Extra
extractFieldsFromExtra [] = logRTErrorNonMonadic "The `extra` array doesn't have any `Fields` item"
extractFieldsFromExtra (s@ST.Fields{} : _) = s
extractFieldsFromExtra (_:ss) = extractFieldsFromExtra ss

addFunction :: NameDeclaration -> ST.ParserMonad (Maybe (ST.Scope, G.Id))
addFunction d@(_, i@(G.Id tk@(T.Token {T.cleanedString=idName}) _), _) = do
  currScope <- ST.getCurrentScope
  ST.addVisitedMethod idName
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      addIdToSymTable d
      return $ Just (currScope, i)
    Just entry -> do
      logSemError ("Function " ++ idName ++ " was already declared") tk
      return Nothing

updateCodeBlockOfFun ::  G.Id -> G.CodeBlock -> ST.ParserMonad ()
updateCodeBlockOfFun (G.Id tk@(T.Token {T.cleanedString=idName}) _) code = do
  let f x = (if and [ST.scope x == 1, ST.name x == idName, ST.category x `elem` [ST.Function, ST.Procedure]]
            then let e = ST.extra x in x{ST.extra = (ST.CodeBlock code) : e}
            else x)
  ST.updateEntry (\ds -> Just $ map f ds) idName

------------------
-- TYPECHECKING --
------------------

class TypeCheckable a where
  getType :: a -> ST.ParserMonad T.Type

getTypeFromAliasName :: String -> ST.ParserMonad T.Type
getTypeFromAliasName aliasName = do
  maybeEntry <- ST.dictLookup aliasName
  case maybeEntry of
    Nothing -> return T.TypeError
    Just entry -> getType entry

isIntegerType :: T.Type -> Bool
isIntegerType t = t `elem` T.integerTypes

exprsToTypes :: [G.Expr] -> [T.Type]
exprsToTypes = map G.expType

-- this functions assumes that expressions can be casted to the
-- target type if they are different
addCastToExprs :: [(G.Expr, T.Type)] -> [G.Expr]
addCastToExprs = map caster
  where
    caster :: (G.Expr, T.Type) -> G.Expr
    caster (expr, t) = let t' = G.expType expr in
      if t == t' then expr
      else expr{G.expAst = G.Caster expr t'}

containerCheck :: [G.Expr] -> (T.Type -> T.Type) -> ([G.Expr] -> G.BaseExpr) -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
containerCheck [] c c' _ = return (c T.Any, c' [])
containerCheck exprs typeCons exprCons tk = do
  let types = exprsToTypes exprs
  let t = findTypeForContainerLiteral types
  case t of
    -- case when there is already a type error on the list
    (Nothing, T.TypeError) -> return (T.TypeError, exprCons exprs)

    -- a particular expression couldn't be casted to the accumulated type
    (Just x, T.TypeError) -> do
      let tk = G.expTok $ exprs !! x -- not so efficient, but works
      logSemError ("Type of item #" ++ show x ++ " of list mismatch") tk
      return (T.TypeError, exprCons exprs)

    -- when a real type is found, we can safely ignore the first part of the tuple
    (_, t') -> do
      let castedExprs = addCastToExprs $ zip exprs (replicate (length exprs) t')
      return (typeCons t', exprCons castedExprs)

findTypeForContainerLiteral :: [T.Type] -> (Maybe Int, T.Type)
findTypeForContainerLiteral [] = (Nothing, T.Any)
findTypeForContainerLiteral types = findT (zip [0..] types) (head types)
  where
    findT :: [(Int, T.Type)] -> T.Type -> (Maybe Int, T.Type)
    findT [] a = (Nothing, a)
    findT ((i, a) : xs) b =
      if a `T.canBeConvertedTo` b then findT xs b
      else if b `T.canBeConvertedTo` a then findT xs a
      else (Just i, T.TypeError)

checkIndexAccess :: G.Expr -> G.Expr -> T.Token -> ST.ParserMonad T.Type
checkIndexAccess array index tk = do
  arrayType <- getType array
  indextype <- getType index
  case arrayType of
    T.ArrayT t -> if isIntegerType indextype
      then return t
      else return T.TypeError
    T.TypeError -> return T.TypeError
    _ -> do
      logSemError "Left side is not a chest" tk
      return T.TypeError

domainCallCheck :: G.Id -> [T.Type] -> [G.Expr] -> T.Token -> ST.ParserMonad (Maybe [G.Expr])
domainCallCheck methodId domain exprs tk = do
  let exprsTypes = exprsToTypes exprs
  let exprsTypesL = length exprsTypes
  let domainL = length domain
  if exprsTypesL < domainL then do
    logSemError ("Method " ++ show methodId ++ " called with less arguments than expected") tk
    return Nothing
  else if exprsTypesL > domainL then do
    logSemError ("Method " ++ show methodId ++ " called with more arguments than expected") tk
    return Nothing
  else do
    if T.TypeError `elem` exprsTypes then
      return Nothing
    else case findInvalidArgument $ zip exprsTypes domain of
      -- all looking good, now cast each argument to it's correspondent parameter type
      Nothing -> do
        let castedExprs = addCastToExprs $ zip exprs domain
        return $ Just castedExprs

      -- oh no, an argument could not be implicitly casted to its correspondent argument
      Just x -> do
        let formalType = domain !! x
        paramType <- getType $ exprs !! x
        let errMsgTypes = "Type mismatch: param. #" ++ show x ++ " received " ++ show paramType ++ " and expected " ++ show formalType
        let errMsg =  errMsgTypes ++ " while calling  " ++ show methodId
        logSemError errMsg tk
        return Nothing
  where
    -- left side is the type of the argument, right side is the parameter type
    findInvalidArgument :: [(T.Type, T.Type)] -> Maybe Int
    findInvalidArgument ts = findInvalid ts 0

    findInvalid :: [(T.Type, T.Type)] -> Int -> Maybe Int
    findInvalid [] _ = Nothing
    findInvalid ((argT, paramT) : xs) i = if argT `T.canBeConvertedTo` paramT
      then findInvalid xs (i + 1)
      else Just i

methodsCheck :: G.Id -> [G.Expr] -> T.Token -> ST.ParserMonad (G.Id, T.Type, [G.Expr])
methodsCheck methodId@(G.Id idTk@(T.Token {T.cleanedString=methodName}) _) exprs tk = do
  procType <- getType methodId
  gId <- G.Id idTk . ST.scope . fromJust <$> ST.dictLookup methodName
  case procType of
    T.ProcedureT domain -> do
      mParams <- domainCallCheck methodId domain exprs tk
      case mParams of
        Nothing -> return (gId, T.TypeError, exprs)
        Just params -> return (gId, T.VoidT, params)
    T.FunctionT domain range -> do
      mParams <- domainCallCheck methodId domain exprs tk
      case mParams of
        Nothing -> return (gId, T.TypeError, exprs)
        Just params -> return (gId, range, params)
    T.TypeError -> return (methodId, T.TypeError, exprs)
    _ -> do
      logSemError "You're trying to call a non-callable expression" tk
      return (methodId, T.TypeError, exprs)

functionsCheck :: G.Id -> [G.Expr] -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
functionsCheck i params tk = do
  (gId, t, ps) <- methodsCheck i params tk
  return (t, G.EvalFunc gId ps)

memAccessCheck :: G.Expr -> T.Token -> ST.ParserMonad T.Type
memAccessCheck expr tk = do
  t <- getType expr
  checkType t
  where
    checkType :: T.Type -> ST.ParserMonad T.Type
    checkType t =
      case t of
        T.PointerT t' -> return t'
        T.TypeError -> return T.TypeError
        T.AliasT name -> getTypeFromAliasName name >>= checkType
        _ -> do
          logSemError "Trying to access memory of non-arrow variable" tk
          return T.TypeError

checkAsciiOf :: G.Expr -> T.Token -> ST.ParserMonad T.Type
checkAsciiOf e tk = do
  t <- getType e
  case t of
    T.CharT -> return T.BigIntT
    T.StringT -> return $ T.ArrayT T.BigIntT
    T.TypeError -> return T.TypeError
    _ -> do
      logSemError ("ascii_of requires argument to be a miracle or a sign") tk
      return T.TypeError

checkIsActive :: G.Expr -> T.Token -> ST.ParserMonad T.Type
checkIsActive e tk = do
  t <- getType e
  case t of
    T.TypeError -> return T.TypeError
    _ ->
      if isUnionAttr then return T.TrileanT
      else do
        logSemError ("is_active requires argument to be a union attribute") tk
        return T.TypeError
  where
    isUnionAttr = case e of
      (G.Expr {G.expAst=(G.Access r _)}) -> isUnion r
      _ -> False
    isUnion (G.Expr {G.expType=(T.UnionT _ _)}) = True
    isUnion _ = False

makeStructFromAlias :: T.Type -> ST.ParserMonad (T.Type)
makeStructFromAlias (T.AliasT n) = do
  mEntry <- ST.dictLookup n
  case mEntry of
    -- Case: direct alias for a struct
    Just ST.DictionaryEntry {ST.extra=[ST.Fields t scope]} -> buildStruct t scope
    -- Case: indirect alias
    Just ST.DictionaryEntry {ST.extra=[ST.Simple al]} -> makeStructFromAlias (T.AliasT al)
    -- Case: direct alias for a non-struct
    _ -> return T.TypeError
makeStructFromAlias _ = logRTError "makeStructFromAlias function wrongfully received a type different from T.AliasT"

checkAccess :: G.Expr -> G.Id -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
checkAccess e (G.Id tk'@T.Token{T.cleanedString=i} _) tk = do
  t <- getType e
  case t of
    tp@(T.AliasT _) -> do
      struct <- makeStructFromAlias tp
      case struct of
        -- If makeStructFromAlias returns typeError, then we NEED to report the message
        T.TypeError -> returnWithError
        t -> checkAccessFromType t
    t -> checkAccessFromType t
  where
    -- This function NEVER receives an alias, always a "definite" type
    checkAccessFromType :: T.Type -> ST.ParserMonad (T.Type, G.BaseExpr)
    checkAccessFromType t = case t of
      T.RecordT scope properties -> checkProperty properties scope
      T.UnionT scope properties -> checkProperty properties scope
      T.TypeError -> return defaultReturn
      _ -> returnWithError
    baseExpr :: G.Id -> G.BaseExpr
    baseExpr = G.Access e
    returnWithError :: ST.ParserMonad (T.Type, G.BaseExpr)
    returnWithError = do
      logSemError "Left side of access is not a record nor union" tk
      return defaultReturn
    defaultReturn :: (T.Type, G.BaseExpr)
    defaultReturn = (T.TypeError, baseExpr $ G.Id tk' (-1))
    checkProperty :: [T.PropType] -> Int -> ST.ParserMonad (T.Type, G.BaseExpr)
    checkProperty props scope =
      case filter ((==i) . fst) $ map (\(T.PropType e) -> e) props of
        ((_,a):_) -> do
          return (a, baseExpr $ G.Id tk' scope)
        _ -> do
          logSemError ("Property " ++ i ++ " doesn't exist") tk
          return defaultReturn

unaryOpCheck :: G.Expr -> G.Op1 -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
unaryOpCheck expr op tk = do
  t <- getType expr
  let finalExpr = G.Op1 op expr
  if t == T.TypeError then
    return (T.TypeError, finalExpr)
  else if t `elem` expectedForOperand then
    let finalType = if op == G.Negate then t else T.TrileanT in
    return (finalType, finalExpr)
  else do
    logSemError (T.typeMismatchMessage tk) tk
    return (T.TypeError, finalExpr)
  where
    expectedForOperand
      | op == G.Negate = T.arithmeticTypes
      | otherwise = T.booleanSingleton

binaryOpCheck :: G.Expr -> G.Expr -> G.Op2 -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
binaryOpCheck leftExpr rightExpr op tk = do
  leftType <- getType leftExpr
  rightType <- getType rightExpr
  if leftType == T.TypeError || rightType == T.TypeError then
    return (T.TypeError, G.Op2 op leftExpr rightExpr)
  else if leftType `T.canBeConvertedTo` rightType then
    let [castedLeftExpr] = addCastToExprs [(leftExpr, rightType)] in
    checkIfInExpected castedLeftExpr rightExpr
  else if rightType `T.canBeConvertedTo` leftType then
    let [castedRightExpr] = addCastToExprs [(rightExpr, leftType)] in
    checkIfInExpected leftExpr castedRightExpr
  else do
    logSemError "Left and right side of operand can't be operated together" tk
    return (T.TypeError, G.Op2 op leftExpr rightExpr)

  where
    expectedForOperands :: [T.Type]
    expectedForOperands
      | op `elem` G.arithmeticOp2 = T.arithmeticTypes
      | op `elem` G.comparableOp2 = T.anySingleton -- we can compare any type if they are the same
      | op `elem` G.booleanOp2 = T.booleanSingleton
      | op `elem` G.setOp2 = T.anySetSingleton
      | op `elem` G.arrayOp2 = T.arrayLikeSingleton
      | otherwise = error (show op ++ " doesn't have a value in expectedForOperands function")
    checkIfInExpected :: G.Expr -> G.Expr -> ST.ParserMonad (T.Type, G.BaseExpr)
    checkIfInExpected l r = do
      -- l and r have here the same type so it doesn't matter what we choose
      t <- getType l
      let exp = G.Op2 op l r
      if t `elem` expectedForOperands then

        -- the final type depends on the operator
        let finalType = if op `elem` G.comparableOp2 then T.TrileanT else t in
        return (finalType, exp)
      else do
        logSemError (T.typeMismatchMessage tk) tk
        return (T.TypeError, exp)

checkSize :: G.Expr -> T.Token -> ST.ParserMonad T.Type
checkSize expr tk = do
  t <- getType expr
  if T.isSizeableType t then return T.BigIntT
  else if t == T.TypeError then return T.TypeError
  else do
    logSemError "`size` expects a sizeable collection type" tk
    return T.TypeError

checkStructAssignment :: (TypeCheckable a) => a -> G.Expr -> T.Token -> ST.ParserMonad G.Expr
checkStructAssignment lval e@(G.Expr {G.expAst = (G.StructLit structProps)}) tk = do
  lvalType <- getType lval
  checkStructAssignment' lvalType
  where
    checkStructAssignment' :: T.Type -> ST.ParserMonad G.Expr
    checkStructAssignment' lvalType = case lvalType of
      T.RecordT scope properties -> do
        let propL = length properties
        let structPropsL = length structProps
        if structPropsL /= propL then do
          let mismatch = propL - structPropsL
          let errDetails = if mismatch > 0 then "(missing " ++ show mismatch ++ " properties)" else "(assigning " ++ show (-mismatch) ++ " additional properties)"
          logSemError ("Bezel literal must assign exactly one value to each valid property " ++ errDetails) tk
          return e
        else do
          let propNames = map (\(T.PropType (s, _)) -> s) properties
          let structPropNames = map (\( (G.Id (T.Token {T.cleanedString=spName}) _) , _) -> spName) structProps
          if propNames /= structPropNames then do
            let invalidNames = filter (\x -> not $ x `elem` propNames) structPropNames
            let firstInvalidName = head invalidNames
            logSemError ("Invalid property name " ++ firstInvalidName ++ " for bezel literal assignment") tk
            return e
          else do
            let propTypes = map (\(T.PropType (s, t)) -> t) properties
            let structPropVals = map snd structProps
            checkRecordTypes propNames propTypes structPropVals
            fixIdScopes scope e

      T.UnionT scope properties -> do
        let structPropsL = length structProps
        if structPropsL /= 1 then logSemError ("Link literal must assign exactly one valid property") tk >> return e
        else do
          let propNames = map (\(T.PropType (s, _)) -> s) properties
          let (G.Id (T.Token {T.cleanedString = structPropName}) _, structPropVal) = head structProps
          if not (structPropName `elem` propNames) then do
            logSemError ("Invalid property name " ++ structPropName ++ " for link literal assignment") tk
            return e
          else do
            let T.PropType (_, propType) = head $ filter (\(T.PropType (n, _)) -> n == structPropName) properties
            structPropValT <- getType structPropVal
            if structPropValT `T.canBeConvertedTo` propType then fixIdScopes scope e
            else do
              let errDetails = "(expected " ++ show propType ++ ", received " ++ show structPropValT ++ ")"
              logSemError ("Type mismach on link literal assignment " ++ errDetails) tk >> return e

      tA@(T.AliasT _) -> do
        newT <- makeStructFromAlias tA
        if newT == T.TypeError then logNonStructType (show tA)
        else checkStructAssignment' newT

      T.TypeError -> return e

      _ -> logNonStructType (show lvalType)

    fixIdScopes :: Int -> G.Expr -> ST.ParserMonad G.Expr
    fixIdScopes scope e@(G.Expr {G.expAst = ast@(G.StructLit structProps)}) = do
      let newProps = map (\(G.Id tk _, e) -> (G.Id tk scope, e)) structProps
      return e { G.expAst = G.StructLit newProps}

    checkRecordTypes :: [String] -> [T.Type] -> [G.Expr] -> ST.ParserMonad ()
    checkRecordTypes [] [] [] = return ()
    checkRecordTypes (n:names) (t:types) (v:vals) = do
      vT <- getType v
      if vT `T.canBeConvertedTo` t then checkRecordTypes names types vals
      else do
        logSemError ("Type mismatch on bezel assignment: property " ++ n ++ " required " ++ show t ++ ", received " ++ show vT) tk
        -- Not stopping the propagation 'cause it's useful to know all the type mismatches
        checkRecordTypes names types vals

    logNonStructType :: String -> ST.ParserMonad G.Expr
    logNonStructType bT = logSemError ("Type mismatch on struct assignment (" ++ bT ++ " is not a struct type)") tk >> return e
checkStructAssignment _ _ _ = logRTError "checkStructAssignment function called with a non-struct literal or expression"

checkTypeOfAssignment :: (TypeCheckable a, TypeCheckable b) => a -> b -> T.Token -> ST.ParserMonad ()
checkTypeOfAssignment lval rval tk = do
  lvalType <- getType lval
  rvalType <- getType rval
  if rvalType `T.canBeConvertedTo` lvalType
  then return ()
  else do
    let errorDetails = "(couldn't convert " ++ (show rvalType) ++ " to " ++ (show lvalType) ++ ")"
    -- don't propagate type errors further
    if T.TypeError `elem` [lvalType, rvalType]
    then return ()
    else logSemError ("Type mismatch on assignment " ++ errorDetails) tk

checkLvalue :: G.Expr -> ST.ParserMonad (Bool)
checkLvalue expr =
  if G.isValidLvalue expr then return True
  else do
    logSemError "This expression is not a valid LVAL expression (expected: id, memory access, property access or pointer deref)" (G.expTok expr)
    return False

minBigInt :: Int
minBigInt = - 2147483648

maxBigInt :: Int
maxBigInt = 2147483647

minSmallInt :: Int
minSmallInt = - 32768

maxSmallInt :: Int
maxSmallInt = 32767

buildTypeForNonCasterExprs :: ST.ParserMonad T.Type -> G.BaseExpr -> ST.ParserMonad (T.Type, G.BaseExpr)
buildTypeForNonCasterExprs tt bExpr = tt >>= \t -> return (t, bExpr)

checkBooleanGuard :: G.Expr -> ST.ParserMonad ()
checkBooleanGuard expr = do
  t <- getType expr
  RWS.when (t /= T.TrileanT) $ do
    -- Avoid propagating the error if it has been raised somewhere else already
    if t == T.TypeError
    then return ()
    else logSemError "Guard should be of trilean data type" $ G.expTok expr

buildType :: G.BaseExpr -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
buildType bExpr tk = case bExpr of
  -- Language literals, their types can be (almost everytime) infered
  G.TrueLit -> return (T.TrileanT, bExpr)
  G.FalseLit -> return (T.TrileanT, bExpr)
  G.UndiscoveredLit -> return (T.TrileanT, bExpr)
  G.NullLit -> return (T.PointerT T.Any, bExpr)

  -- A literal of an integer is valid if it is on the correct range
  G.IntLit n ->
    if minSmallInt <= n && n <= maxSmallInt
    then return (T.SmallIntT, bExpr)
    else if minBigInt <= n && n <= maxBigInt
    then return (T.BigIntT, bExpr)
    else logRTError "TODO: check for the int size, modify grammar to carry token position"
  G.FloatLit _ -> return (T.FloatT, bExpr)
  G.CharLit _ -> return (T.CharT, bExpr)
  G.StringLit _ -> return (T.StringT, bExpr)
  G.StructLit props -> return (T.StructLitT, bExpr)

  G.ArrayLit exprs -> containerCheck exprs T.ArrayT G.ArrayLit tk
  G.SetLit exprs -> containerCheck exprs T.SetT G.SetLit tk

  G.Op1 op a -> unaryOpCheck a op tk

  G.Op2 op a b -> binaryOpCheck a b op tk

  G.IdExpr i -> checkIdType i
  G.IndexAccess e i -> buildTypeForNonCasterExprs (checkIndexAccess e i tk) bExpr
  G.EvalFunc i params -> functionsCheck i params tk
  G.MemAccess e -> buildTypeForNonCasterExprs (memAccessCheck e tk) bExpr
  G.IsActive e -> buildTypeForNonCasterExprs (checkIsActive e tk) bExpr
  G.AsciiOf e -> buildTypeForNonCasterExprs (checkAsciiOf e tk) bExpr
  G.Access e i -> checkAccess e i tk
  G.Size e -> buildTypeForNonCasterExprs (checkSize e tk) bExpr

checkIdType :: G.Id -> ST.ParserMonad (T.Type, G.BaseExpr)
checkIdType gId@(G.Id tk _) = do
  idEntry <- checkIdAvailability gId
  case idEntry of
    Nothing -> return (T.TypeError, G.IdExpr gId)
    Just entry -> do
      t <- getType gId
      return (t, G.IdExpr $ G.Id tk $ ST.scope entry)

instance TypeCheckable G.Id where
  getType gId = do
    idEntry <- checkIdAvailability gId
    case idEntry of
      Nothing -> return T.TypeError
      Just entry -> getType entry

instance TypeCheckable G.Expr where
  getType = return . G.expType

instance TypeCheckable ST.DictionaryEntry where
  getType entry@ST.DictionaryEntry{ST.entryType=Just _, ST.category = cat, ST.extra = extras}
    | cat `elem` [ST.Function, ST.Procedure] = do
      range <- (case cat of
        ST.Procedure -> return T.VoidT
        ST.Function -> getType entry{ST.category=ST.Variable}) -- cheating
      domain <- do
          let fields = head $ filter ST.isFieldsExtra extras
          (T.TypeList list) <- getType fields
          return list
      case range of
        -- The range is returned as a one-type typelist, we return the actual type
        T.TypeList (r:_) -> return $ T.FunctionT domain r
        _ -> return $ T.FunctionT domain range

    | cat `elem` [
        ST.Variable, ST.Constant,
        ST.RecordItem, ST.UnionItem,
        ST.RefParam, ST.ValueParam, ST.Type] = getType $ ST.extractTypeFromExtra entry

    | otherwise = logRTError "error on getType for expected dict entries"

  getType _ = logRTError "error on getType for unexpected dict entries"

instance TypeCheckable ST.Extra where
  getType (ST.Recursive s extra) = do
      t <- getType extra
      if s == ST.arrow then return $ T.PointerT t
      else if s == ST.armor then return $ T.SetT t
      else return T.TypeError -- This should not happen, but life is hard

  -- For the moment, ST.Compound only corresponds to string
  getType (ST.Compound _ _) = return $ T.StringT

  -- For the moment, ST.CompoundRect only corresponds to array
  getType (ST.CompoundRec _ _ extra) = getType extra >>= \t -> return $ T.ArrayT t

  getType (ST.Fields ST.Callable scope) = do
    ST.SymTable {ST.stDict=dict} <- RWS.get
    let isArg entry = ST.category entry `elem` [ST.ValueParam, ST.RefParam]
    let args = filter isArg $ ST.findAllInScope scope dict
    -- ?INFO: Relevant anonymous type-aliases might be found on the method's params
    -- ?INFO: scope, so we push it while we get the types
    ST.pushScope scope
    types <- mapM getType $ ST.sortByArgPosition args
    ST.exitScope
    return $ T.TypeList types

  getType (ST.Fields b scope) = buildStruct b scope

  getType (ST.Simple s)
      | s == ST.smallHumanity = return T.SmallIntT
      | s == ST.humanity = return T.BigIntT
      | s == ST.hollow = return T.FloatT
      | s == ST.sign = return T.CharT
      | s == ST.bonfire = return T.TrileanT
      | s == ST.void = return T.VoidT
      | otherwise = do
        maybeEntry <- ST.dictLookup s
        case maybeEntry of
          Nothing -> return T.TypeError
          Just e -> getType e
  getType _ = logRTError "error on getType for SymTable extra"

buildStruct :: ST.TypeFields -> Int -> ST.ParserMonad (T.Type)
buildStruct t scope = do
  ST.SymTable {ST.stDict=dict} <- RWS.get
  let entries = ST.findAllInScope scope dict
  ST.pushScope scope
  let entryNames = map ST.name entries
  types <- mapM getType entries
  ST.exitScope
  case t of
    ST.Record -> return $ T.RecordT scope $ map T.PropType $ zip entryNames types
    ST.Union -> return $ T.UnionT scope $ map T.PropType $ zip entryNames types
    _ -> logRTError "wrong data type for Fields extra"

logSemError :: String -> T.Token -> ST.ParserMonad ()
logSemError msg tk = RWS.tell [Error msg (T.position tk)]

rtMessage :: String -> String
rtMessage msg =
  let header = "Firelink Internal Runtime Error:\t" ++ msg ++ "\n"
      mid = "\tPlease, open a new issue on Github attaching the .souls file you just used\n"
      end = "\tGithub Issue Tracker: https://github.com/aitorres/firelink/issues"
      fullMsg = header ++ mid ++ end
  in fullMsg

logRTErrorNonMonadic :: String -> a
logRTErrorNonMonadic msg = error $ rtMessage msg

logRTError :: String -> ST.ParserMonad (a)
logRTError msg = error $ rtMessage msg

}