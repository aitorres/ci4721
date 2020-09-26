{
module FireLink.FrontEnd.Lexer (
    scanTokens,
    ) where

import Data.List.Extra          (replace)
import FireLink.FrontEnd.Errors
import FireLink.FrontEnd.Tokens
import FireLink.Utils
}
%wrapper "monadUserState"

-- macros for sets and regex
$digits = [0-9]
$characters = [^\\\|]
@ids = [a-z][A-Za-z0-9_]*
@scapedchars = \\[nt\\\|]
@strings = \@([$characters # \@] | "\@" | @scapedchars)*\@
@comments = \-\-$printable*
tokens :-
    $white+                               ;
    -- reserved keywords --
    -- Program structure
    @comments                             { makeToken TkComment }
    hello\ ashen\ one                     { makeToken TkProgramBegin }
    farewell\ ashen\ one                  { makeToken TkProgramEnd }
    traveling\ somewhere                  { makeToken TkInstructionBegin }
    you\ died                             { makeToken TkInstructionEnd }
    with                                  { makeToken TkWith }
    in\ your\ inventory                   { makeToken TkDeclarationEnd }
    transpose\ into                       { makeToken TkRead }
    with\ orange\ soapstone\ say           { makeToken TkPrint }

    -- Functions and procedures
    spell                                 { makeToken TkSpell }
    to\ the\ estus\ flask                 { makeToken TkSpellParsEnd }
    ashen\ estus\ flask\ consumed         { makeToken TkSpellEnd }
    invocation                            { makeToken TkInvocation }
    with\ skill\ of\ type                 { makeToken TkInvocationType }
    requesting                            { makeToken TkRequesting }
    to\ the\ knight                       { makeToken TkInvocationParsEnd }
    val                                   { makeToken TkVal }
    ref                                   { makeToken TkRef }
    summon                                { makeToken TkSummon }
    granting                              { makeToken TkGranting }
    cast                                  { makeToken TkCast }
    offering                              { makeToken TkOffering }
    go\ back                              { makeToken TkReturn }
    go\ back\ with                        { makeToken TkReturnWith }
    after\ this\ return\ to\ your\ world  { makeToken TkInvocationEnd }

    -- classic conditional
    trust\ your\ inventory                { makeToken TkIf }
    liar\!                                { makeToken TkElse }
    inventory\ closed                     { makeToken TkEndIf }

    ascii_of                              { makeToken TkAsciiOf }
    is_active                             { makeToken TkIsActive }

    -- switch statements
    enter\ dungeon\ with                  { makeToken TkSwitch }
    dungeon\ exited                       { makeToken TkEndSwitch }
    empty\ dungeon                        { makeToken TkSwitchDefault }

    -- on-structure looping
    repairing                             { makeToken TkForEach }
    with\ titanite\ from                  { makeToken TkWithTitaniteFrom }
    weaponry\ repaired                    { makeToken TkEndForEach }

    -- iterable looping
    upgrading                             { makeToken TkFor }
    max\ level\ reached                   { makeToken TkEndFor }
    souls?                                { makeToken TkSoul }
    until\ level                          { makeToken TkLevel }

    -- conditionate looping
    while\ the                            { makeToken TkWhile }
    covenant\ is\ active                  { makeToken TkCovenantIsActive }
    covenant\ left                        { makeToken TkEndWhile }
    -- Types
    humanity                              { makeToken TkBigInt }
    small\ humanity                       { makeToken TkSmallInt }
    big\ humanity                         { makeToken TkBigInt }

    bonfire                               { makeToken TkBool }
    lit                                   { makeToken TkLit }
    unlit                                 { makeToken TkUnlit }
    undiscovered                          { makeToken TkUndiscovered }

    hollow                                { makeToken TkFloat }

    sign                                  { makeToken TkChar }

    \>\-miracle                           { makeToken TkString }

    \>\-chest                             { makeToken TkArray }
    \<\$                                  { makeToken TkArrayOpen }
    \$\>                                  { makeToken TkArrayClose }

    armor                                 { makeToken TkSet }
    \{\$                                  { makeToken TkSetOpen }
    \$\}                                  { makeToken TkSetClose }
    union                                 { makeToken TkUnion }
    intersect                             { makeToken TkIntersect }
    diff                                  { makeToken TkDiff }
    size                                  { makeToken TkSize }

    \~\>                                  { makeToken TkAccessor }

    bezel                                 { makeToken TkRecord }

    link                                  { makeToken TkUnionStruct }

    abyss                                 { makeToken TkNull }
    arrow\ to                             { makeToken TkPointer }
    aim\ a                                { makeToken TkRequestMemory }
    throw\ a                              { makeToken TkAccessMemory }
    recover\ a                            { makeToken TkFreeMemory }

    knight                                { makeToken TkAlias }
    requiring\ help\ of                   { makeToken TkAliasListBegin }
    help\ received                        { makeToken TkAliasListEnd }

    -- Literals
    $digits+                              { makeToken TkIntLit }
    $digits+\.$digits+                    { makeToken TkFloatLit }
    @strings                              { makeToken TkStringLit }

    -- Special characters
    \,                                    { makeToken TkComma }
    \\                                    { makeToken TkSeq }
    \:                                    { makeToken TkColon }
    \|$characters\|                       { makeToken TkCharLit }
    \|@scapedchars\|                      { makeToken TkCharLit }

    -- Operators
    \<\<=                                 { makeToken TkAsig }
    \+                                    { makeToken TkPlus }
    \-                                    { makeToken TkMinus }
    \*                                    { makeToken TkMult }
    \/                                    { makeToken TkDiv }
    \%                                    { makeToken TkMod }
    lt                                    { makeToken TkLt }
    gt                                    { makeToken TkGt }
    lte                                   { makeToken TkLte }
    gte                                   { makeToken TkGte }
    eq                                    { makeToken TkEq }
    neq                                   { makeToken TkNeq }
    not                                   { makeToken TkNot }
    and                                   { makeToken TkAnd }
    or                                    { makeToken TkOr }
    \>\-\<                                { makeToken TkConcat }
    \<                                    { makeToken TkLteLit }
    \{                                    { makeToken TkBraceOpen }
    \}                                    { makeToken TkBraceClose }
    \(                                    { makeToken TkParensOpen }
    \)                                    { makeToken TkParensClose }

    const                                 { makeToken TkConst }
    var                                   { makeToken TkVar }
    of\ type                              { makeToken TkOfType }
    @ids                                  { makeToken TkId }

    .                                     { throwLexError }

{

makeToken :: AbstractToken -> AlexAction AlexUserState
makeToken tk' ((AlexPn _ r c), _, _, str) len = do
    let str' = take len str
    addTokenToState Token {
        aToken=tk',
        cleanedString=str',
        capturedString=str',
        position=Position {row=r, column=c}
    }
    alexMonadScan

throwLexError :: AlexAction AlexUserState
throwLexError ((AlexPn _ r c), _, _, str) len = do
    addErrorToState $ Error (take len str) (Position {row=r, column=c})
    alexMonadScan


-- This isn't on the documentation
alexEOF :: Alex AlexUserState
alexEOF = getUserState

data AlexUserState = LexerResult [Error] [Token]

alexInitUserState :: AlexUserState
alexInitUserState = LexerResult [] []

getUserState :: Alex AlexUserState
getUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, ust)

addTokenToState :: Token -> Alex ()
addTokenToState tk' = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = (
            let LexerResult errors tokens = ust in
                LexerResult errors (tk':tokens))
    }, ())

addErrorToState :: Error -> Alex ()
addErrorToState lexError = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = (
            let LexerResult errors tokens = ust in
                LexerResult (lexError:errors) tokens)
    }, ())

scanTokens :: String -> ([Error], [Token])
scanTokens str = case runAlex str alexMonadScan of
    Left e -> do
        error $ "Alex error " ++ show e
    Right userState ->
        let LexerResult errors tokens = userState in (
            reverse errors,
            filterComments $ map postProcess $ reverse tokens)

removeFirstAndLast :: [a] -> [a]
removeFirstAndLast = reverse . tail . reverse . tail

postProcess :: Token -> Token
postProcess (Token TkCharLit s _ p) = Token TkCharLit s (f s) p
    where
        f s = if head a == '\\' then mapEscaped $ last a else a
        a = removeFirstAndLast s
        mapEscaped 'n' = "\n"
        mapEscaped 't' = "\t"
        mapEscaped '\\' = "\\"
        mapEscaped '|' = "\n"
postProcess (Token TkStringLit s _ p) = Token TkStringLit s ss p
    where
        pp = removeFirstAndLast s
        ss = replace "\\@" "@" pp
postProcess a = a

filterComments :: [Token] -> [Token]
filterComments =
    filter (\t -> aToken t /= TkComment)
}
