module ProgramCodeGenerator where

import CodeGenerator
import InstructionCodeGenerator
import Grammar (Program(..), CodeBlock(..))

instance GenerateCode CodeBlock where
    genCode (CodeBlock instrs) = mapM_ genCode instrs >> return Nothing

instance GenerateCode Program where
    genCode (Program codeblock) = genCode codeblock >> return Nothing
