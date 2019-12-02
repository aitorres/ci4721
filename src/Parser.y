{
module Parser (
  parse) where

import qualified Lexer as L
import qualified SymTable as ST
import Data.Maybe
import qualified Grammar as G
import qualified Control.Monad.RWS as RWS
import qualified TypeChecking as T
}

%name                                                                     parse
%tokentype                                                              { L.Token }
%error                                                                  { parseErrors }
%monad                                                                  { ST.ParserMonad }


%nonassoc size memAccessor

%left ARRCLOSE

%left eq neq
%left plus minus
%left mult div mod
%left NEG

%left and or

%nonassoc lt lte gt gte

%left colConcat
%left diff
%left union intersect


%left asciiOf
%right not
%left accessor
%left arrOpen arrClose


%token
  programBegin                                                          { L.Token {L.aToken=L.TkProgramBegin} }
  programEnd                                                            { L.Token {L.aToken=L.TkProgramEnd} }

  aliasListBegin                                                        { L.Token {L.aToken=L.TkAliasListBegin} }
  aliasListEnd                                                          { L.Token {L.aToken=L.TkAliasListEnd} }
  alias                                                                 { L.Token {L.aToken=L.TkAlias} }

  id                                                                    { L.Token {L.aToken=L.TkId} }

  ofType                                                                { L.Token {L.aToken=L.TkOfType} }

  paramRequest                                                          { L.Token {L.aToken=L.TkRequesting} }
  parVal                                                                { L.Token {L.aToken=L.TkVal} }
  parRef                                                                { L.Token {L.aToken=L.TkRef} }

  bigInt                                                                { L.Token {L.aToken=L.TkBigInt} }
  smallInt                                                              { L.Token {L.aToken=L.TkSmallInt} }
  float                                                                 { L.Token {L.aToken=L.TkFloat} }
  char                                                                  { L.Token {L.aToken=L.TkChar} }
  bool                                                                  { L.Token {L.aToken=L.TkBool} }
  ltelit                                                                { L.Token {L.aToken=L.TkLteLit} }
  string                                                                { L.Token {L.aToken=L.TkString} }
  array                                                                 { L.Token {L.aToken=L.TkArray} }
  set                                                                   { L.Token {L.aToken=L.TkSet} }
  enum                                                                  { L.Token {L.aToken=L.TkEnum} }
  unionStruct                                                           { L.Token {L.aToken=L.TkUnionStruct} }
  record                                                                { L.Token {L.aToken=L.TkRecord} }
  pointer                                                               { L.Token {L.aToken=L.TkPointer} }

  intLit                                                                { L.Token {L.aToken=L.TkIntLit, L.cleanedString=$$} }
  floatLit                                                              { L.Token {L.aToken=L.TkFloatLit, L.cleanedString=$$} }
  charLit                                                               { L.Token {L.aToken=L.TkCharLit, L.cleanedString=$$} }
  stringLit                                                             { L.Token {L.aToken=L.TkStringLit, L.cleanedString=$$} }
  trueLit                                                               { L.Token {L.aToken=L.TkLit} }
  falseLit                                                              { L.Token {L.aToken=L.TkUnlit} }
  unknownLit                                                            { L.Token {L.aToken=L.TkUndiscovered} }
  nullLit                                                               { L.Token {L.aToken=L.TkNull} }

  functionBegin                                                         { L.Token {L.aToken=L.TkInvocation} }
  functionType                                                          { L.Token {L.aToken=L.TkInvocationType} }
  functionEnd                                                           { L.Token {L.aToken=L.TkInvocationEnd} }

  procedureBegin                                                        { L.Token {L.aToken=L.TkSpell} }
  procedureEnd                                                          { L.Token {L.aToken=L.TkSpellEnd} }

  comma                                                                 { L.Token {L.aToken=L.TkComma} }
  brOpen                                                                { L.Token {L.aToken=L.TkBraceOpen} }
  brClose                                                               { L.Token {L.aToken=L.TkBraceClosed} }

  with                                                                  { L.Token {L.aToken=L.TkWith} }
  declarend                                                             { L.Token {L.aToken=L.TkDeclarationEnd} }

  const                                                                 { L.Token {L.aToken=L.TkConst} }
  var                                                                   { L.Token {L.aToken=L.TkVar} }
  asig                                                                  { L.Token {L.aToken=L.TkAsig} }

  instructionsBegin                                                     { L.Token {L.aToken=L.TkInstructionBegin} }
  instructionsEnd                                                       { L.Token {L.aToken=L.TkInstructionEnd} }
  seq                                                                   { L.Token {L.aToken=L.TkSeq} }

  cast                                                                  { L.Token {L.aToken=L.TkCast} }
  offering                                                              { L.Token {L.aToken=L.TkOffering} }
  toTheKnight                                                           { L.Token {L.aToken=L.TkInvocationParsEnd} }

  summon                                                                { L.Token {L.aToken=L.TkSummon} }
  granting                                                              { L.Token {L.aToken=L.TkGranting} }
  toTheEstusFlask                                                       { L.Token {L.aToken=L.TkSpellParsEnd} }

  return                                                                { L.Token {L.aToken=L.TkReturn} }
  returnWith                                                            { L.Token {L.aToken=L.TkReturnWith} }

  print                                                                 { L.Token {L.aToken=L.TkPrint} }
  read                                                                  { L.Token {L.aToken=L.TkRead} }

  whileBegin                                                            { L.Token {L.aToken=L.TkWhile} }
  whileEnd                                                              { L.Token {L.aToken=L.TkEndWhile} }
  covenantIsActive                                                      { L.Token {L.aToken=L.TkCovenantIsActive} }

  ifBegin                                                               { L.Token {L.aToken=L.TkIf} }
  ifEnd                                                                 { L.Token {L.aToken=L.TkEndIf} }
  colon                                                                 { L.Token {L.aToken=L.TkColon} }
  else                                                                  { L.Token {L.aToken=L.TkElse} }

  switchBegin                                                           { L.Token {L.aToken=L.TkSwitch} }
  switchDefault                                                         { L.Token {L.aToken=L.TkSwitchDefault} }
  switchEnd                                                             { L.Token {L.aToken=L.TkEndSwitch} }

  forBegin                                                              { L.Token {L.aToken=L.TkFor} }
  forEnd                                                                { L.Token {L.aToken=L.TkEndFor} }
  souls                                                                 { L.Token {L.aToken=L.TkSoul} }
  untilLevel                                                            { L.Token {L.aToken=L.TkLevel} }

  forEachBegin                                                          { L.Token {L.aToken=L.TkForEach} }
  forEachEnd                                                            { L.Token {L.aToken=L.TkEndForEach} }
  withTitaniteFrom                                                      { L.Token {L.aToken=L.TkWithTitaniteFrom} }

  parensOpen                                                            { L.Token {L.aToken=L.TkParensOpen} }
  parensClosed                                                          { L.Token {L.aToken=L.TkParensClosed} }

  plus                                                                  { L.Token {L.aToken=L.TkPlus} }
  minus                                                                 { L.Token {L.aToken=L.TkMinus} }
  mult                                                                  { L.Token {L.aToken=L.TkMult} }
  div                                                                   { L.Token {L.aToken=L.TkDiv} }
  mod                                                                   { L.Token {L.aToken=L.TkMod} }
  lt                                                                    { L.Token {L.aToken=L.TkLt} }
  gt                                                                    { L.Token {L.aToken=L.TkGt} }
  lte                                                                   { L.Token {L.aToken=L.TkLte} }
  gte                                                                   { L.Token {L.aToken=L.TkGte} }
  eq                                                                    { L.Token {L.aToken=L.TkEq} }
  neq                                                                   { L.Token {L.aToken=L.TkNeq} }
  not                                                                   { L.Token {L.aToken=L.TkNot} }
  and                                                                   { L.Token {L.aToken=L.TkAnd} }
  or                                                                    { L.Token {L.aToken=L.TkOr} }
  asciiOf                                                               { L.Token {L.aToken=L.TkAsciiOf} }
  colConcat                                                             { L.Token {L.aToken=L.TkConcat} }
  union                                                                 { L.Token {L.aToken=L.TkUnion} }
  intersect                                                             { L.Token {L.aToken=L.TkIntersect} }
  diff                                                                  { L.Token {L.aToken=L.TkDiff} }
  size                                                                  { L.Token {L.aToken=L.TkSize} }

  arrOpen                                                               { L.Token {L.aToken=L.TkArrayOpen} }
  arrClose                                                              { L.Token {L.aToken=L.TkArrayClose} }
  setOpen                                                               { L.Token {L.aToken=L.TkSetOpen} }
  setClose                                                              { L.Token {L.aToken=L.TkSetClose} }

  accessor                                                              { L.Token {L.aToken=L.TkAccessor} }
  memAccessor                                                           { L.Token {L.aToken=L.TkAccessMemory} }

  malloc                                                                { L.Token {L.aToken=L.TkRequestMemory} }
  free                                                                  { L.Token {L.aToken=L.TkFreeMemory} }
%%

PROGRAM :: { G.Program }
PROGRAM
  : programBegin ALIASES METHODS NON_OPENER_CODEBLOCK programEnd        { G.Program $4 }

NON_OPENER_CODEBLOCK :: { G.CodeBlock }
NON_OPENER_CODEBLOCK
  : instructionsBegin DECLARS INSTRL instructionsEnd                     { G.CodeBlock $ reverse $3 }
  | instructionsBegin INSTRL instructionsEnd                             { G.CodeBlock $ reverse $2 }

ALIASES :: { () }
ALIASES
  : aliasListBegin ALIASL aliasListEnd                                  {% do
                                                                            addIdsToSymTable $ reverse $2
                                                                            (dict, _, s) <- RWS.get
                                                                            RWS.put (dict, [1, 0], s) }
  | {- empty -}                                                         { () }


ALIASL :: { [NameDeclaration] }
ALIASL
  : ALIASL comma ALIAS                                                  { $3:$1 }
  | ALIAS                                                               { [$1] }

ALIAS :: { NameDeclaration }
ALIAS
  : alias ID TYPE                                                       { (ST.Type, $2, $3) }

LVALUE :: { G.Expr }
LVALUE
  : ID                                                                  {% do
                                                                            checkIdAvailability $1
                                                                            buildAndCheckExpr $ G.IdExpr $1 }
  | EXPR accessor ID                                                    {% do
                                                                            let expr = G.Access $1 $3
                                                                            ret <- buildAndCheckExpr expr
                                                                            checkPropertyAvailability ret
                                                                            return ret }
  | EXPR arrOpen EXPR arrClose                                          {% buildAndCheckExpr $ G.IndexAccess $1 $3 }
  | memAccessor EXPR                                                    {% buildAndCheckExpr $ G.MemAccess $2 }

EXPR :: { G.Expr }
EXPR
  : intLit                                                              {% buildAndCheckExpr $ G.IntLit (read $1 :: Int) }
  | floatLit                                                            {% buildAndCheckExpr $ G.FloatLit (read $1 :: Float) }
  | charLit                                                             {% buildAndCheckExpr $ G.CharLit $ head $1 }
  | stringLit                                                           {% buildAndCheckExpr $ G.StringLit $1 }
  | trueLit                                                             {% buildAndCheckExpr G.TrueLit }
  | falseLit                                                            {% buildAndCheckExpr G.FalseLit }
  | unknownLit                                                          {% buildAndCheckExpr G.UndiscoveredLit }
  | nullLit                                                             {% buildAndCheckExpr G.NullLit }
  | arrOpen EXPRL arrClose                                              {% buildAndCheckExpr $ G.ArrayLit $ reverse $2 }
  | setOpen EXPRL setClose                                              {% buildAndCheckExpr $ G.SetLit $ reverse $2 }
  | parensOpen EXPR parensClosed                                        { $2 }
  | minus EXPR                                                          {% buildAndCheckExpr $ G.Negative $2 }
  | not EXPR                                                            {% buildAndCheckExpr $ G.Not $2 }
  | asciiOf EXPR                                                        {% buildAndCheckExpr $ G.AsciiOf $2 }
  | size EXPR                                                           {% buildAndCheckExpr $ G.SetSize $2 }
  | EXPR plus EXPR                                                      {% buildAndCheckExpr $ G.Add $1 $3 }
  | EXPR minus EXPR                                                     {% buildAndCheckExpr $ G.Substract $1 $3 }
  | EXPR mult EXPR                                                      {% buildAndCheckExpr $ G.Multiply $1 $3 }
  | EXPR div EXPR                                                       {% buildAndCheckExpr $ G.Divide $1 $3 }
  | EXPR mod EXPR                                                       {% buildAndCheckExpr $ G.Mod $1 $3 }
  | EXPR lt EXPR                                                        {% buildAndCheckExpr $ G.Lt $1 $3 }
  | EXPR gt EXPR                                                        {% buildAndCheckExpr $ G.Gt $1 $3 }
  | EXPR lte EXPR                                                       {% buildAndCheckExpr $ G.Lte $1 $3 }
  | EXPR gte EXPR                                                       {% buildAndCheckExpr $ G.Gte $1 $3 }
  | EXPR eq EXPR                                                        {% buildAndCheckExpr $ G.Eq $1 $3 }
  | EXPR neq EXPR                                                       {% buildAndCheckExpr $ G.Neq $1 $3 }
  | EXPR and EXPR                                                       {% buildAndCheckExpr $ G.And $1 $3 }
  | EXPR or EXPR                                                        {% buildAndCheckExpr $ G.Or $1 $3 }
  | EXPR colConcat EXPR                                                 {% buildAndCheckExpr $ G.ColConcat $1 $3 }
  | EXPR union EXPR                                                     {% buildAndCheckExpr $ G.SetUnion $1 $3 }
  | EXPR intersect EXPR                                                 {% buildAndCheckExpr $ G.SetIntersect $1 $3 }
  | EXPR diff EXPR                                                      {% buildAndCheckExpr $ G.SetDiff $1 $3 }
  | FUNCALL                                                             {% buildAndCheckExpr $ G.EvalFunc (fst $1) (snd $1) }
  | LVALUE                                                              { $1 }

EXPRL :: { [G.Expr] }
EXPRL
  : {- empty -}                                                         { [] }
  | EXPRLNOTEMPTY                                                       { $1 }

EXPRLNOTEMPTY :: { [G.Expr] }
EXPRLNOTEMPTY
  : EXPR                                                                { [$1] }
  | EXPRLNOTEMPTY comma EXPR                                           { $3:$1 }

METHODS :: { () }
METHODS
  : {- empty -}                                                         { () }
  | METHODL                                                             { () }

METHODL :: { () }
METHODL
  : METHODL METHOD                                                      { () }
  | METHOD                                                              { () }

METHOD :: { () }
METHOD
  : FUNC                                                                {% do
                                                                          (dict, _, s) <- RWS.get
                                                                          RWS.put (dict, [1, 0], s) }
  | PROC                                                                {% do
                                                                          (dict, _, s) <- RWS.get
                                                                          RWS.put (dict, [1, 0], s) }

FUNCPREFIX :: { Maybe (ST.Scope, G.Id) }
FUNCPREFIX
  : functionBegin ID METHODPARS functionType TYPE                       {% addFunction (ST.Function,
                                                                              $2, G.Callable (Just $5) $3) }
FUNC :: { () }
FUNC
  : FUNCPREFIX NON_OPENER_CODEBLOCK functionEnd                         {% case $1 of
                                                                            Nothing -> return ()
                                                                            Just (s, i) -> updateCodeBlockOfFun s i $2 }

PROCPREFIX :: { Maybe (ST.Scope, G.Id) }
PROCPREFIX
  : procedureBegin ID PROCPARSDEC                                       {% addFunction (ST.Procedure,
                                                                              $2, G.Callable Nothing $3) }
PROC :: { () }
PROC
  : PROCPREFIX NON_OPENER_CODEBLOCK procedureEnd                        {% case $1 of
                                                                            Nothing -> return ()
                                                                            Just (s, i) -> updateCodeBlockOfFun s i $2 }

PROCPARSDEC :: { [ArgDeclaration] }
PROCPARSDEC
  : METHODPARS toTheEstusFlask                                          { $1 }
  | {- empty -}                                                         { [] }

METHODPARS :: { [ArgDeclaration] }
METHODPARS
  : paramRequest PARS                                                   { reverse $2 }
  | {- empty -}                                                         { [] }

PARS :: { [ArgDeclaration] }
PARS
  : PARS comma PAR                                                      { $3:$1 }
  | PAR                                                                 { [$1] }

PAR :: { ArgDeclaration }
PAR
  : PARTYPE ID ofType TYPE                                              { ($1, $2, $4) }

PARTYPE :: { G.ArgType }
PARTYPE
  : parVal                                                              { G.Val }
  | parRef                                                              { G.Ref }

TYPE :: { G.GrammarType }
TYPE
  : ID                                                                  { let G.Id t = $1 in G.Simple t Nothing }
  | bigInt                                                              { G.Simple $1 Nothing }
  | smallInt                                                            { G.Simple $1 Nothing }
  | float                                                               { G.Simple $1 Nothing }
  | char                                                                { G.Simple $1 Nothing }
  | bool                                                                { G.Simple $1 Nothing }
  | ltelit EXPR array ofType TYPE                                       { G.Compound $3 $5 (Just $2) }
  | ltelit EXPR string                                                  { G.Simple $3 (Just $2) }
  | set ofType TYPE                                                     { G.Compound $1 $3 Nothing }
  | pointer TYPE                                                        { G.Compound $1 $2 Nothing }
  | record brOpen STRUCTITS brClose                                     { G.Record $1 $ reverse $3 }
  | unionStruct brOpen STRUCTITS brClose                                { G.Record $1 $ reverse $3 }

ENUMITS :: { [Int] }
ENUMITS
  : ENUMITS comma ID                                                    { [] }
  | ID                                                                  { [] }

STRUCTITS :: { [RecordItem] }
STRUCTITS
  : STRUCTITS comma STRUCTIT                                            { $3:$1 }
  | STRUCTIT                                                            { [$1] }

STRUCTIT :: { RecordItem }
STRUCTIT
  : ID ofType TYPE                                                      { ($1, $3) }

ID :: { G.Id }
ID
  : id                                                                  { G.Id $1 }

CODEBLOCK :: { G.CodeBlock }
CODEBLOCK
  : INSTBEGIN DECLARS INSTRL INSTEND                                    { G.CodeBlock $ reverse $3 }
  | INSTBEGIN INSTRL INSTEND                                            { G.CodeBlock $ reverse $2 }

INSTBEGIN :: { () }
INSTBEGIN : instructionsBegin                                           {% ST.enterScope }

INSTEND :: { () }
INSTEND : instructionsEnd                                               {% ST.exitScope }

DECLARS :: { () }
DECLARS
  : with DECLARSL declarend                                             {% addIdsToSymTable (reverse $2) }

DECLARSL :: { [NameDeclaration] }
DECLARSL
  : DECLARSL comma DECLAR                                               { $3:$1 }
  | DECLAR                                                              { [$1] }

VARTYPE :: { ST.Category }
VARTYPE
  : const                                                               { ST.Constant }
  | var                                                                 { ST.Variable }

DECLAR :: { NameDeclaration }
DECLAR
  : VARTYPE ID ofType TYPE                                              { ($1, $2, $4) }
  | VARTYPE ID ofType TYPE asig EXPR                                    { ($1, $2, $4) }

INSTRL :: { G.Instructions }
INSTRL
  : INSTRL seq INSTR                                                    { $3 : $1 }
  | INSTR                                                               { [$1] }

INSTR :: { G.Instruction }
INSTR
  : LVALUE asig EXPR                                                    {% do
                                                                          checkConstantReassignment $1
                                                                          return $ G.InstAsig $1 $3 }
  | malloc EXPR                                                         { G.InstMalloc $2 }
  | free EXPR                                                           { G.InstFreeMem $2 }
  | cast ID PROCPARS                                                    {% do
                                                                          checkIdAvailability $2
                                                                          return $ G.InstCallProc $2 $3 }
  | FUNCALL                                                             { G.InstCallFunc (fst $1) (snd $1) }
  | return                                                              { G.InstReturn }
  | returnWith EXPR                                                     { G.InstReturnWith $2 }
  | print EXPR                                                          { G.InstPrint $2 }
  | read LVALUE                                                         { G.InstRead $2 }
  | whileBegin EXPR covenantIsActive colon CODEBLOCK whileEnd           { G.InstWhile $2 $5 }
  | ifBegin IFCASES ELSECASE ifEnd                                      { G.InstIf (reverse ($3 : $2)) }
  | ifBegin IFCASES ifEnd                                               { G.InstIf (reverse $2) }
  | switchBegin EXPR colon SWITCHCASES DEFAULTCASE switchEnd            { G.InstSwitch $2 (reverse ($5 : $4)) }
  | switchBegin EXPR colon SWITCHCASES switchEnd                        { G.InstSwitch $2 (reverse $4) }
  | forBegin ID with EXPR souls untilLevel EXPR CODEBLOCK forEnd        { G.InstFor $2 $4 $7 $8 }
  | forEachBegin ID withTitaniteFrom EXPR CODEBLOCK forEachEnd          { G.InstForEach $2 $4 $5 }

FUNCALL :: { (G.Id, G.Params) }
FUNCALL
  : summon ID FUNCPARS                                                  {% do
                                                                          checkIdAvailability $2
                                                                          return ($2, $3) }

IFCASES :: { G.IfCases }
IFCASES
  : IFCASES IFCASE                                                      { $2 : $1 }
  | IFCASE                                                              { [$1] }

IFCASE :: { G.IfCase }
  : EXPR colon CODEBLOCK                                                { G.GuardedCase $1 $3 }

ELSECASE :: { G.IfCase }
  : else colon CODEBLOCK                                                { G.ElseCase $3 }

SWITCHCASES :: { G.SwitchCases }
  : SWITCHCASES SWITCHCASE                                              { $2 : $1 }
  | SWITCHCASE                                                          { [$1] }

SWITCHCASE :: { G.SwitchCase }
  : EXPR colon CODEBLOCK                                                { G.Case $1 $3 }

DEFAULTCASE :: { G.SwitchCase }
  : switchDefault colon CODEBLOCK                                       { G.DefaultCase $3 }

FUNCPARS :: { G.Params }
  : granting PARSLIST toTheKnight                                       { reverse $2 }
  | {- empty -}                                                         { [] }

PROCPARS :: { G.Params }
  : offering PARSLIST toTheEstusFlask                                   { reverse $2 }
  | {- empty -}                                                         { [] }

PARSLIST :: { G.Params }
  : PARSLIST comma EXPR                                                 { $3 : $1 }
  | EXPR                                                                { [$1] }

{

type NameDeclaration = (ST.Category, G.Id, G.GrammarType)
type ArgDeclaration = (G.ArgType, G.Id, G.GrammarType)
type AliasDeclaration = (G.Id, G.GrammarType)
type RecordItem = AliasDeclaration

buildAndCheckExpr :: G.BaseExpr -> ST.ParserMonad G.Expr
buildAndCheckExpr bExpr = do
  t <- getType bExpr
  if t == T.TypeError
    then RWS.tell [ST.SemanticError "Type error" (L.Token
      { L.aToken = L.TkId
      , L.capturedString = "hola"
      , L.cleanedString = "hola"
      , L.posn = (L.AlexPn 1 1 1)
      })]
    else return ()
  return G.Expr
    { G.expAst = bExpr
    , G.expType = t
    }

extractFieldsForNewScope :: G.GrammarType -> Maybe [RecordItem]
extractFieldsForNewScope (G.Compound _ s _) = extractFieldsForNewScope s
extractFieldsForNewScope (G.Record _ s) = Just s
extractFieldsForNewScope _ = Nothing

extractFunParamsForNewScope :: G.GrammarType -> Maybe [ArgDeclaration]
extractFunParamsForNewScope (G.Callable _ s) = Just s
extractFunParamsForNewScope _ = Nothing

parseErrors :: [L.Token] -> ST.ParserMonad a
parseErrors errors =
  let tk@L.Token {L.aToken=abst, L.posn=pn} = errors !! 0
      name = show tk
      line = show $ L.row pn
      column = show $ L.col pn
      header = "\x1b[1m\x1b[31mYOU DIED!!\x1b[0m Parser error: "
      endmsg = "\n\nFix your syntax errors, ashen one."
      position = "line \x1b[1m\x1b[31m" ++ line ++ "\x1b[0m, column \x1b[1m\x1b[31m" ++ column ++ "\x1b[0m."
      msg = header ++ "Unexpected token \x1b[1m\x1b[31m" ++ name ++ "\x1b[0m at " ++ position ++ endmsg
  in  fail msg

addIdsToSymTable :: [NameDeclaration] -> ST.ParserMonad ()
addIdsToSymTable ids = do
  RWS.mapM_ (addIdToSymTable Nothing) ids

addIdToSymTable :: Maybe Int -> NameDeclaration -> ST.ParserMonad ()
addIdToSymTable mi d@(c, (G.Id tk@(L.Token {L.aToken=at, L.cleanedString=idName})), t) = do
  maybeIdEntry <- ST.dictLookup idName
  maybeTypeEntry <- findTypeOnEntryTable t
  (_, (currScope:_), _) <- RWS.get
  case maybeIdEntry of
    -- The name doesn't exists on the table, we just add it
    Nothing -> insertIdToEntry mi t
      ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = ST.name <$> maybeTypeEntry
        , ST.extra = []
        }

    -- The name does exists on the table, we just add it depending on the scope
    Just entry -> do
      let scope = ST.scope entry
      if currScope /= scope
      then insertIdToEntry mi t ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = ST.name <$> maybeTypeEntry
        , ST.extra = []
        }
      else RWS.tell $ [ST.SemanticError ("Name " ++ idName ++ " was already declared on this scope") tk]


insertIdToEntry :: Maybe Int -> G.GrammarType -> ST.DictionaryEntry -> ST.ParserMonad ()
insertIdToEntry mi t entry = do
  ex <- buildExtraForType t
  let pos = (case mi of
              Nothing -> []
              Just i -> [ST.ArgPosition i])
  ST.addEntry entry{ST.extra = pos ++ ex}
  -- To add the record params to the dictionary
  case extractFieldsForNewScope t of
    Nothing -> return ()
    Just s ->  do
      ST.enterScope
      addIdsToSymTable $ map (\(a, b) -> (ST.RecordItem, a, b)) s
      ST.exitScope
  case extractFunParamsForNewScope t of
    -- If it is nothing then this is not a function
    Nothing -> return ()
    Just s -> do
      ST.enterScope
      RWS.mapM_ (\(i, n) -> addIdToSymTable (Just i) n) $ zip [0..] $
        map (\(argType, i, t) -> (if argType == G.Val
                             then ST.ValueParam
                             else ST.RefParam, i, t)) s

checkConstantReassignment :: G.Expr -> ST.ParserMonad ()
checkConstantReassignment e = case G.expAst e of
  G.IdExpr (G.Id tk@(L.Token {L.cleanedString=idName})) -> do
    maybeEntry <- ST.dictLookup idName
    case maybeEntry of
      Nothing -> do
        return ()
      Just e ->
        case (ST.category e) of
          ST.Constant -> do
            RWS.tell [ST.SemanticError ("Name " ++ idName ++ " is a constant and must not be reassigned") tk]
            return ()
          _ ->
            return ()
  G.IndexAccess gId _ -> checkConstantReassignment gId
  _ -> return ()

checkIdAvailability :: G.Id -> ST.ParserMonad (Maybe ST.DictionaryEntry)
checkIdAvailability (G.Id tk@(L.Token {L.cleanedString=idName})) = do
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      RWS.tell [ST.SemanticError ("Name " ++ idName ++ " is not available on this scope") tk]
      return Nothing
    Just e -> do
      return $ Just e

-- The following function only have sense (for the moment) on lvalues
--  - Ids
--  - Records
--  - Arrays
checkPropertyAvailability :: G.Expr -> ST.ParserMonad ()
checkPropertyAvailability e = case G.expAst e of
  -- If it is a record accessing, we need to find the _scope_ of the
  -- left side of the expression where to search for the variable
  a@(G.Access expr gId@(G.Id tk@(L.Token {L.cleanedString=s}))) -> do
    maybeScope <- findScopeToSearchOf expr
    case maybeScope of
      Nothing -> RWS.tell [ST.SemanticError ("Property " ++ s ++ " does not exists") tk]
      Just s -> do
        (dict, scopes, curr) <- RWS.get
        RWS.put (dict, s:scopes, curr)
        checkIdAvailability gId
        RWS.put (dict, scopes, curr)

  _ -> error "invalid usage of checkPropertyAvailability"

extractFieldsFromExtra :: [ST.Extra] -> ST.Extra
extractFieldsFromExtra [] = error "The `extra` array doesn't have any `Fields` item"
extractFieldsFromExtra (s@ST.Fields{} : _) = s
extractFieldsFromExtra (_:ss) = extractFieldsFromExtra ss

findScopeToSearchOf :: G.Expr -> ST.ParserMonad (Maybe ST.Scope)
findScopeToSearchOf e = case G.expAst e of
  -- The scope of an id is just the scope of its entry
  G.IdExpr gId -> do
    maybeEntry <- checkIdAvailability gId
    case maybeEntry of
      Nothing -> return Nothing
      Just ST.DictionaryEntry {ST.extra=extra} -> do
        let (ST.Fields s) = extractFieldsFromExtra extra
        return $ Just s

  -- The scope of an record accessing is the scope of its accessing property
  G.Access expr gId -> do
    maybeScopeOf <- findScopeToSearchOf expr
    case maybeScopeOf of
      Nothing -> return Nothing
      Just s -> do
        (dict, scopes, curr) <- RWS.get
        RWS.put (dict, s:scopes, curr)
        scope <- findScopeToSearchOf $ G.Expr {G.expAst=G.IdExpr gId, G.expType=T.TypeError}
        RWS.put (dict, scopes, curr)
        return scope

  -- The scope of a index acces is the scope of it's id
  G.IndexAccess expr _ -> findScopeToSearchOf expr

addFunction :: NameDeclaration -> ST.ParserMonad (Maybe (ST.Scope, G.Id))
addFunction d@(_, i@(G.Id tk@(L.Token {L.cleanedString=idName})), _) = do
  (dict, stack, currScope) <- RWS.get
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      addIdToSymTable Nothing d
      return $ Just (currScope, i)
    Just entry -> do
      if ST.scope entry == currScope
      then do
        RWS.tell [ST.SemanticError ("Name " ++ idName ++ " was already declared on this scope") tk]
        return Nothing
      else do
        addIdToSymTable Nothing d
        return $ Just (currScope, i)

updateCodeBlockOfFun :: ST.Scope -> G.Id -> G.CodeBlock -> ST.ParserMonad ()
updateCodeBlockOfFun currScope (G.Id tk@(L.Token {L.cleanedString=idName})) code = do
  let f x = (if and [ST.scope x == currScope, ST.name x == idName, ST.category x `elem` [ST.Function, ST.Procedure]]
            then let e = ST.extra x in x{ST.extra = (ST.CodeBlock code) : e}
            else x)
  ST.updateEntry (\ds -> Just $ map f ds) idName

findTypeOnEntryTable :: G.GrammarType -> ST.ParserMonad (Maybe ST.DictionaryEntry)

-- For simple data types
findTypeOnEntryTable (G.Simple tk mSize) = do
  maybeEntry <- ST.dictLookup $ ST.tokensToEntryName tk
  case maybeEntry of
    Nothing -> do
      RWS.tell [ST.SemanticError ("Type " ++ (show tk) ++ " not found") tk]
      return maybeEntry
    _ -> return maybeEntry

-- For compound data types, this extracts the constructor
findTypeOnEntryTable (G.Compound tk _ _) = do
  ST.dictLookup $ ST.tokensToEntryName tk

-- For record alike data types (unions and structs), this extracts their constructor
findTypeOnEntryTable (G.Record tk _) = ST.dictLookup $ ST.tokensToEntryName tk

-- For functions
findTypeOnEntryTable (G.Callable (Just t) _) = findTypeOnEntryTable t

findTypeOnEntryTable (G.Callable Nothing _) = return Nothing

buildExtraForType :: G.GrammarType -> ST.ParserMonad [ST.Extra]

-- For string alike data types
buildExtraForType t@(G.Simple _ maybeSize) = do
  t' <- (ST.name . fromJust) <$> findTypeOnEntryTable t
  return [case maybeSize of
    Just e -> ST.Compound t' e
    Nothing -> ST.Simple t']

-- For array alike data types
buildExtraForType t@(G.Compound _ tt@(G.Simple _ _) maybeExpr) = do
  maybeTypeEntry <- findTypeOnEntryTable tt
  constructor <- (ST.name . fromJust) <$> findTypeOnEntryTable t -- This call should never fail
  case maybeTypeEntry of
    Just t -> do
      extras <- buildExtraForType tt
      let newExtra = if null extras then (ST.Simple $ ST.name t) else (head extras)
      return (case maybeExpr of
        Just e -> [ST.CompoundRec constructor e newExtra]
        Nothing -> [ST.Recursive constructor newExtra])

buildExtraForType t@(G.Compound _ tt@(G.Compound{}) maybeExpr) = do
  extra' <- head <$> buildExtraForType tt
  constructor <- (ST.name . fromJust) <$> findTypeOnEntryTable t -- This call should never fail
  return $ case maybeExpr of
    Just e -> [ST.CompoundRec constructor e extra']
    Nothing -> [ST.Recursive constructor extra']

buildExtraForType t@(G.Compound _ tt@(G.Record{}) maybeExpr) = do
  extra' <- head <$> buildExtraForType tt
  constructor <- (ST.name . fromJust) <$> findTypeOnEntryTable t -- This call should never fail
  return $ case maybeExpr of
    Just e -> [ST.CompoundRec constructor e extra', extra']
    Nothing -> [ST.Recursive constructor extra', extra']

buildExtraForType t@(G.Record _ _) = do
  (_, _, currScope) <- RWS.get
  return [ST.Fields $ currScope + 1]

buildExtraForType t@(G.Callable _ []) = return [ST.EmptyFunction]

buildExtraForType t@(G.Callable _ _) = do
  (_, _, currScope) <- RWS.get
  return [ST.Fields $ currScope + 1]


-- For anything else
buildExtraForType _ = return []

------------------
-- TYPECHECKING --
------------------

class TypeCheckable a where
  getType :: a -> ST.ParserMonad T.Type
  typeMatches :: a -> a -> ST.ParserMonad Bool
  typeMatches a b = do
    aType <- getType a
    bType <- getType b
    return (aType == bType)

isOneOfTypes :: [T.Type] -> G.Expr -> ST.ParserMonad Bool
isOneOfTypes ts a = do
  t <- getType a
  return $ not . null $ filter (== t) ts

type TypeChecker = G.Expr -> ST.ParserMonad Bool

isLogicalType :: TypeChecker
isLogicalType = isOneOfTypes T.booleanTypes

isNumberType :: T.Type -> Bool
isNumberType t = not . null $ filter (==t) T.numberTypes

isIntegerType :: TypeChecker
isIntegerType = isOneOfTypes T.integerTypes

isComparableType :: TypeChecker
isComparableType = isOneOfTypes T.comparableTypes

isShowableType :: TypeChecker
isShowableType = isOneOfTypes T.showableTypes

exprsToTypes :: [G.Expr] -> [T.Type]
exprsToTypes = map G.expType

containerCheck :: [G.Expr] -> (T.Type -> T.Type) -> ST.ParserMonad T.Type
containerCheck [] c = return $ c T.Any
containerCheck exprs constructor = do
  let types = exprsToTypes exprs
  let t = head types
  let allAreSameType = and $ map (==t) types
  return (
    if allAreSameType && t /= T.TypeError
    then constructor t
    else T.TypeError
    )

typeCheck :: (G.Expr, [T.Type]) -> (G.Expr, [T.Type]) -> Maybe T.Type -> ST.ParserMonad T.Type
typeCheck (a, ea) (b, eb) mt = do
  let aType = G.expType a
  let bType = G.expType b
  let correctType = aType `elem` ea
  let correctType' = bType `elem` eb
  return (
    if correctType && correctType'
      then (case mt of
              Nothing -> aType `max` bType
              Just t -> t)
      else T.TypeError)

arithmeticCheck :: G.Expr -> G.Expr -> ST.ParserMonad T.Type
arithmeticCheck a b = typeCheck (a, T.numberTypes) (b, T.numberTypes) Nothing

logicalCheck :: G.Expr -> G.Expr -> ST.ParserMonad T.Type
logicalCheck a b = typeCheck (a, T.booleanTypes) (b, T.booleanTypes) Nothing

equatableCheck :: G.Expr -> G.Expr -> ST.ParserMonad T.Type
equatableCheck a b = do
  t <- typeCheck (a, T.numberTypes) (b, T.numberTypes) (Just T.TrileanT)
  t' <- typeCheck (a, [T.CharT]) (b, [T.CharT]) (Just T.TrileanT)
  if t /= T.TypeError || t' /= T.TypeError
    then return T.TrileanT
    else return T.TypeError

comparableCheck :: G.Expr -> G.Expr -> ST.ParserMonad T.Type
comparableCheck = equatableCheck


functionsCheck :: G.Id -> [G.Expr] -> ST.ParserMonad T.Type
functionsCheck funId exprs = do
  let exprsTypes = exprsToTypes exprs
  maybeFunEntry <- checkIdAvailability funId
  case maybeFunEntry of
    Nothing -> return T.TypeError
    Just entry -> return T.TypeError

minBigInt :: Int
minBigInt = - 2147483648

maxBigInt :: Int
maxBigInt = 2147483647

minSmallInt :: Int
minSmallInt = - 32768

maxSmallInt :: Int
maxSmallInt = 32767

instance TypeCheckable G.Id where
  getType _ = error "not implemented yet"

instance TypeCheckable G.Expr where
  getType = return . G.expType

instance TypeCheckable G.BaseExpr where
  -- Language literals, their types can be (almost everytime) infered
  getType G.TrueLit = return T.TrileanT
  getType G.FalseLit = return T.TrileanT
  getType G.UndiscoveredLit = return T.TrileanT
  getType G.NullLit = return T.TypeError -- TODO: check if okay

  -- A literal of an integer is valid if it is on the correct range
  getType (G.IntLit n) =
    if minSmallInt <= n && n <= maxSmallInt
    then return T.SmallIntT
    else if minBigInt <= n && n <= maxBigInt
    then return T.BigIntT
    else error "TODO: check for the int size, modify grammar to carry token position"
  getType (G.FloatLit _) = return T.FloatT
  getType (G.CharLit _) = return T.CharT
  getType (G.StringLit _) = return T.StringT

  getType (G.ArrayLit a) = containerCheck a T.ArrayT
  getType (G.SetLit a) = containerCheck a T.SetT

  getType (G.EvalFunc id _) = return T.TypeError -- TODO: Check if okay
  getType (G.Add a b) = arithmeticCheck a b
  getType (G.Substract a b) = arithmeticCheck a b
  getType (G.Multiply a b) = arithmeticCheck a b
  getType (G.Divide a b) = arithmeticCheck a b
  getType (G.Mod a b) = error "TODO: not implemented yet"
  getType (G.Negative a) = arithmeticCheck a a -- cheating
  getType (G.Lt a b) = comparableCheck a b
  getType (G.Lte a b) = comparableCheck a b
  getType (G.Gt a b) = comparableCheck a b
  getType (G.Gte a b) = comparableCheck a b
  getType (G.Eq a b) = equatableCheck a b
  getType (G.Neq a b) = equatableCheck a b
  getType (G.And a b) = logicalCheck a b
  getType (G.Or a b) = logicalCheck a b
  getType (G.Not a) = logicalCheck a a -- cheating
  getType (G.Access e i) = return T.TypeError -- TODO: Accessor type
  getType (G.IndexAccess e1 e2) = return T.TypeError -- TODO: Accessor type
  getType (G.MemAccess e) = return T.TypeError -- TODO: Mem access
  getType _ = return T.TypeError -- TODO: Finish implementation

{-
    | Procedure
    | Function
    | RefParam
    | ValueParam
instance TypeCheckable ST.DictionaryEntry where
  getType entry{ST.entryType=Just entryType, ST.category = cat, ST.extra = extras}
    -- If it is an alias, return just the name
    | cat == T.Type = return $ T.AliasT (ST.name entry)
    | cat `elem` [T.Procedure, T.Function] = do
      let isEmptyFunction = not . null $ filter (\e -> case e of
                                                        ST.EmptyFunction -> True
                                                        _ -> False) extras
      range <- (case cat of
        T.Procedure -> return T.Void
        T.Function -> getType entry{ST.category=ST.variable}) -- cheating

      domain <- (if isEmptyFunction
        then return []
        
      then return $ T.FunctionT [] (if cat == T.Procedure then T.Void else )
    | cat `elem` [ST.Variable, ST.Constant, ST.RecordItem, ST.UnionItem, ST.RefParam, ST.ValueParam]
    case entryType of
 -}

}