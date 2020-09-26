{-|
Module      : Tokens
Description : Collection of data types and functions that let us represent
              tokens in FireLink
Maintainer  : 14-10924@usb.ve, 14-11082@usb.ve
Stability   : experimental
-}
module FireLink.FrontEnd.Tokens (
    Token(..), AbstractToken(..)) where
import qualified FireLink.Utils as U

-- |All the posible tokens of the language
data AbstractToken = TkId | TkConst | TkVar | TkOfType | TkAsig
    -------------------
    ------ Types ------
    -------------------
    -- Integers
    | TkBigInt | TkSmallInt | TkIntLit
    -- Tri-booleans
    | TkBool | TkLit | TkUnlit | TkUndiscovered
    -- Double precission
    | TkFloat | TkFloatLit
    -- Character
    | TkChar | TkCharLit | TkAsciiOf
    --- Collections
    -- Strings
    | TkString | TkLteLit
    | TkStringLit -- Strings
    -- Arrays
    | TkArray | TkArrayOpen | TkArrayClose | TkSize
    -- Sets
    | TkSet | TkSetOpen | TkSetClose | TkUnion | TkIntersect | TkDiff
    -- Records (C-like structs)
    | TkRecord | TkBraceOpen | TkBraceClose | TkComma | TkAccessor
    -- Unions
    | TkUnionStruct | TkIsActive
    -- Null, pointer stuff
    | TkNull | TkPointer | TkRequestMemory | TkAccessMemory | TkFreeMemory
    -- Type Aliases
    | TkAlias | TkAliasListBegin | TkAliasListEnd

    ------------------
    -- Instructions --
    ------------------
    -- Program basic structure tokens
    | TkComment
    | TkProgramBegin -- hello ashen one
    | TkProgramEnd -- farewell ashen one
    -- Declarations
    | TkDeclarationEnd
    -- Instructions
    | TkInstructionBegin -- traveling somewhere
    | TkInstructionEnd -- you died
    | TkSeq -- \
    -- Functions
    | TkInvocation | TkRequesting
    | TkInvocationType -- with skill of type
    | TkInvocationParsEnd
    | TkInvocationEnd -- after this return to your world
    | TkVal | TkRef | TkReturn | TkSummon | TkGranting
    -- Procedures
    | TkSpell
    | TkSpellEnd -- ashen estus flask consumed
    | TkCast
    | TkOffering
    | TkSpellParsEnd
    | TkReturnWith
    -- Basic I/O
    | TkPrint -- with orange soapstone say
    | TkRead -- transpose into
    -- Basic selection
    | TkIf -- trust your inventory
    | TkColon
    | TkElse -- liar!
    | TkEndIf -- inventory closed
    -- Switch selection
    | TkSwitch -- enter dungeon with <id>
    | TkSwitchDefault
    | TkEndSwitch -- dungeon exited
    -- Finite iterations
    | TkFor -- for without iterable
    | TkWith -- with
    | TkSoul -- soul[s]
    | TkLevel -- until level
    | TkEndFor -- max level reached
    | TkForEach -- for with iterable
    | TkWithTitaniteFrom -- with titanite from
    | TkEndForEach -- weaponry repaired
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
    -- Parens
    | TkParensOpen | TkParensClose
    -- Unary operators
    | TkNot
    deriving (Eq)

instance Show AbstractToken where
    show TkConst             = U.cyan ++ U.bold
    show TkVar               = U.cyan ++ U.bold
    show TkOfType            = U.dim
    show TkAsig              = U.brightMagenta ++ U.bold
    show TkBigInt            = U.blue ++ U.bold ++ U.underline
    show TkSmallInt          = U.blue ++ U.bold ++ U.underline
    show TkBool              = U.cyan ++ U.bold ++ U.underline
    show TkLit               = U.blue ++ U.bold
    show TkUnlit             = U.blue ++ U.bold
    show TkUndiscovered      = U.blue ++ U.bold
    show TkFloat             = U.magenta ++ U.bold ++ U.underline
    show TkChar              = U.brightRed ++ U.bold ++ U.underline
    show TkString            = U.green ++ U.bold ++ U.underline
    show TkLteLit            = U.green ++ U.bold ++ U.underline
    show TkArray             = U.green ++ U.bold ++ U.underline
    show TkSet               = U.green ++ U.bold ++ U.underline
    show TkRecord            = U.green ++ U.bold ++ U.underline
    show TkUnionStruct       = U.green ++ U.bold ++ U.underline
    show TkAlias             = U.cyan ++ U.bold
    show TkAliasListBegin    = U.magenta ++ U.bold
    show TkAliasListEnd      = U.magenta ++ U.bold
    show TkProgramBegin      = U.italic ++ U.bold
    show TkProgramEnd        = U.italic ++ U.bold
    show TkDeclarationEnd    = U.dim
    show TkInstructionBegin  = U.dim
    show TkInstructionEnd    = U.dim
    show TkSeq               = U.dim
    show TkInvocation        = U.cyan ++ U.italic ++ U.bold
    show TkRequesting        = U.magenta ++ U.bold
    show TkInvocationType    = U.brightCyan ++ U.dim
    show TkInvocationParsEnd = U.brightCyan ++ U.dim
    show TkInvocationEnd     = U.cyan ++ U.bold ++ U.italic
    show TkVal               = U.yellow ++ U.bold
    show TkRef               = U.yellow ++ U.bold
    show TkReturn            = U.magenta ++ U.dim
    show TkSummon            = U.brightCyan ++ U.dim
    show TkGranting          = U.brightCyan ++ U.dim
    show TkSpell             = U.cyan ++ U.italic ++ U.bold
    show TkSpellEnd          = U.cyan ++ U.italic ++ U.bold
    show TkCast              = U.brightCyan ++ U.dim
    show TkOffering          = U.brightCyan ++ U.dim
    show TkSpellParsEnd      = U.brightCyan ++ U.dim
    show TkReturnWith        = U.magenta ++ U.dim
    show TkPrint             = U.brightCyan ++ U.dim
    show TkRead              = U.brightCyan ++ U.dim
    show TkIf                = U.magenta ++ U.bold ++ U.italic
    show TkElse              = U.magenta ++ U.bold
    show TkEndIf             = U.magenta ++ U.bold ++ U.italic
    show TkSwitch            = U.magenta ++ U.bold
    show TkSwitchDefault     = U.magenta ++ U.bold
    show TkEndSwitch         = U.magenta ++ U.bold
    show TkFor               = U.magenta ++ U.bold ++ U.italic
    show TkWith              = U.magenta ++ U.bold
    show TkSoul              = U.red ++ U.italic
    show TkLevel             = U.magenta ++ U.bold
    show TkEndFor            = U.magenta ++ U.bold ++ U.italic
    show TkForEach           = U.magenta ++ U.bold ++ U.italic
    show TkEndForEach        = U.magenta ++ U.bold ++ U.italic
    show TkWhile             = U.magenta ++ U.bold ++ U.italic
    show TkCovenantIsActive  = U.magenta ++ U.bold
    show TkEndWhile          = U.magenta ++ U.bold ++ U.italic
    show TkPlus              = U.brightMagenta ++ U.bold
    show TkMinus             = U.brightMagenta ++ U.bold
    show TkMult              = U.brightMagenta ++ U.bold
    show TkDiv               = U.brightMagenta ++ U.bold
    show TkMod               = U.brightMagenta ++ U.bold
    show TkLt                = U.brightMagenta ++ U.bold
    show TkGt                = U.brightMagenta ++ U.bold
    show TkLte               = U.brightMagenta ++ U.bold
    show TkGte               = U.brightMagenta ++ U.bold
    show TkEq                = U.brightMagenta ++ U.bold
    show TkNeq               = U.brightMagenta ++ U.bold
    show TkAnd               = U.brightMagenta ++ U.bold
    show TkOr                = U.brightMagenta ++ U.bold
    show TkConcat            = U.brightMagenta ++ U.bold
    show TkNot               = U.brightMagenta ++ U.bold
    show TkId                = U.bold
    show TkIntLit            = U.blue ++ U.bold
    show TkFloatLit          = U.blue ++ U.bold
    show TkCharLit           = U.yellow
    show TkStringLit         = U.yellow
    show _                   = ""

-- |Full token with all the description and position info
data Token = Token
    { aToken :: !AbstractToken -- Token perse
    , capturedString :: !String -- The captured string
    , cleanedString :: !String -- The cleaned string, post-processed
    , position :: U.Position -- Position info of the captured token
    }
    deriving (Eq)

instance Show Token where
    show t = (show . aToken $ t) ++ capturedString t ++ U.nocolor
