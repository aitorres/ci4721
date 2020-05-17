module FireLink.BackEnd.Utils where

import qualified Data.Set as Set

-- | Polimorfic implementation for fixedPoint algorithms
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint = until =<< ((==) =<<)

-- | Given a list, returns a list of pairs in which
-- | each first component is an unique integer, and each second
-- | component is a (now uniquely identified) element of the first list,
-- | in the original order
numberList :: [a] -> [(Int, a)]
numberList = zip [0..]


-- | List of available registers for non-float operations on MIPS32
availableRegisters :: Set.Set String
availableRegisters = Set.fromList [
    "a0", "a1", "a2", "a3",
    "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9",
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7"
    ]

-- | List of available registers for float operations on MIPS32
availableFloatRegisters :: Set.Set String
availableFloatRegisters = Set.fromList $ map (("f" ++) . show) [0..31]
