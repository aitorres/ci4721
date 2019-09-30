{
module Lexer (
    alexMonadScan, runAlex,
    Token (..)
    ) where
}
%wrapper "monad"
$digit = 0-9
-- digits
$alpha = [a-zA-Z]
-- alphabetic characters
tokens :-
    . ;
{
-- Only meant to build the tokens via actions
data TokenIds = TId | TConst | TVar | TOfType | TAsig
    -------------------
    ------ Types ------
    -------------------
    -- Integers
    | TBigHumanity | TSmallHumanity | TInt
    -- Tri-booleans
    | TBonfire | TLit | TUnlit | TUndiscovered
    -- Double precission
    | THollow
    -- Character
    | TSign | TChar | TAsciiOf
    --- Collections
    -- Strings
    | TMiracleKnown | TMiracleUnknown | TAt
    -- Arrays
    | TChestKnown | TChestUnknown | TChestOpen | TChestClose | TSize
    -- Sets
    | TArmor | TArmorOpen | TArmorClose | TUnion | TIntersect | TDiff
    -- Enums
    | TTitanite | TBraceOpen | TBraceClosed | TComma | TAccessor
    -- Records (C-like structs)
    | TBezel
    -- Unions
    | TLink
    -- Null, pointer stuff
    | TAbyss | TArrowTo | TAimA | TThrowA | TRecoverA
    -- Type Aliases
    | TKnight
    ------------------
    -- Instructions --
    ------------------
    -- Program basic structure tokens
    | TComment | TProgramBegin | TProgramEnd
    -- Declarations
    | TBeginDeclarations | TEndDeclarations
    -- Instructions
    | TInstructionBegin
    | TInstructionEnd
    | TSeq -- \
    -- Functions
    | TInvocation | TRequesting | TInvocationType | TInvocationEnd | TVal
    | TRef | TReturn | TSummon | TGranting
    -- Procedures
    | TSpell | TSpellEnd | TCast | TOffering
    -- Basic I/O
    | TPrint | TRead
    -- Basic selection
    | TIf | TColon | TElse | TEndIf
    -- Switch selection
    | TSwitch | TDefault | TEndSwitch
    -- Finite iterations
    | TUpgrading | TWith | TSoul | TLevel | TEndUpgrading | TRepairing
    | TWithTitaniteFrom | TEndRepairing
    -- Conditional iterations
    | TWhile | TCovenantIsActive | TEndWhile

    -------------------
    -- Common tokens --
    -------------------
    -- Binary operators
    | TPlus | TMinus | TMult | TDiv | TMod | TLt | TGt | TLte | TGte
    | TEq | TNeq | TAnd | TOr | TConcat
    -- Unary operators
    | TNot
    -- Eof
    | TEof
    deriving (Eq)

-- Each action has type :: String -> Token
-- The token type:
data Token = TkId String | TkConst | TkVar | TkOfType | TkAsig
    -------------------
    ------ Types ------
    -------------------
    -- Integers
    | TkBigHumanity | TkSmallHumanity | TkInt Int
    -- Tri-booleans
    | TkBonfire | TkLit | TkUnlit | TkUndiscovered
    -- Double precission
    | TkHollow
    -- Character
    | TkSign | TkChar Char | TkAsciiOf
    --- Collections
    -- Strings
    | TkMiracleKnown Int -- size known at compile time
    | TkMiracleUnknown String -- size unknown
    | TkAt -- String delimitators
    -- Arrays
    | TkChestKnown Int | TkChestUnknown String | TkChestOpen | TkChestClose | TkSize
    -- Sets
    | TkArmor | TkArmorOpen | TkArmorClose | TkUnion | TkIntersect | TkDiff
    -- Enums
    | TkTitanite | TkBraceOpen | TkBraceClosed | TkComma | TkAccessor
    -- Records (C-like structs)
    | TkBezel
    -- Unions
    | TkLink
    -- Null, pointer stuff
    | TkAbyss | TkArrowTo | TkAimA | TkThrowA | TkRecoverA
    -- Type Aliases
    | TkKnight

    ------------------
    -- Instructions --
    ------------------
    -- Program basic structure tokens
    | TkComment
    | TkProgramBegin -- hello ashen one
    | TkProgramEnd -- farewell ashen one
    -- Declarations
    | TkBeginDeclarations | TkEndDeclarations
    -- Instructions
    | TkInstructionBegin -- traveling somewhere
    | TkInstructionEnd -- you died
    | TkSeq -- \
    -- Functions
    | TkInvocation | TkRequesting
    | TkInvocationType -- with skill of type
    | TkInvocationEnd -- after this return to your world
    | TkVal | TkRef | TkReturn | TkSummon | TkGranting
    -- Procedures
    | TkSpell
    | TkSpellEnd -- ashen estus flask consumed
    | TkCast
    | TkOffering
    -- Basic I/O
    | TkPrint -- with orange saponite say
    | TkRead -- transpose into
    -- Basic selection
    | TkIf -- trust your inventory
    | TkColon
    | TkElse -- liar!
    | TkEndIf -- inventory closed
    -- Switch selection
    | TkSwitch -- enter dungeon with <id>
    | TkDefault
    | TkEndSwitch -- dungeon exited
    -- Finite iterations
    | TkUpgrading -- for without iterable
    | TkWith -- with
    | TkSoul -- soul[s]
    | TkLevel -- until level
    | TKEndUpgrading -- max level reached
    | TkRepairing -- for with iterable
    | TkWithTitaniteFrom -- with titanite from
    | TkEndRepairing -- weaponry repaired
    -- Conditional iterations
    | TkWhile
    | TkCovenantIsActive -- covenant is active
    | TkEndWhile -- covenant left

    -------------------
    -- Common tokens --
    -------------------
    -- Binary operators
    | TkPlus | TkMinus | TkMult | TkDiv | TkMod | TkLt | TkGt | TkLte
    | TkGte | TkEq | TkNeq | TkAnd | TkOr | TkConcat
    -- Unary operators
    | TkNot
    -- Eof
    | TkEof
    deriving (Eq, Show)

-- This isn't on the documentation
alexEOF :: Alex Token
alexEOF = return TkEof
}
