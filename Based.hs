module Based (changeBase) where

import           GHC.Natural (Natural)
import           Prelude     hiding (filter, fmap, foldl, foldr, liftA2, map,
                              mapM, mapM_, pure, replicate, return, reverse,
                              sequence, sequenceA, unzip, zip, zip3, zipWith,
                              (*>), (<$), (<$>), (<*), (<*>), (>>), (>>=))

{--

DO NOT use any additional imports. You will be penalised for that.

You are not allowed to use
    - list comprehensions
    - do-notation
    - any imports
    - any zips or unzips
    - any maps and their operator counterparts (including, but not limited to fmap, amap, mapM, mapM_)
    - any functions on lists and their operator counterparts (including but not limited to sorts, folds, filters, reverse, replicate)
    - (<$>), (<*>), (*>). (>>=), (>>).

Gradescope will check your submission for any of the prohibited syntax, functions, operators and imports.

 ** Basically, the rule is: if you want to use something, implement it yourself! **

Execution time and memory consumption are limited.

Memory consumption is limited to 4 GB.


                                                    THE TASK

Given the original base, the target base and a number in original base represented as a list of "digits",
return a list of "digits" representing the same number with the target base.

We guarantee that each digit in the given list is in range [0 .. from - 1]
We guarantee that @from@ and @to@ will always be greater than 1.

Examples:

changeBase 10 2 [4, 2] == [1, 0, 1, 0, 1, 0]

The example above means "convert 42 from decimal to binary". 42 is 101010 in binary.

changeBase 16 2 [15, 10, 1, 0] == [1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
changeBase 16 10 [15, 10, 1, 0] == [6, 4, 0, 1, 6]

Hexadecimal number FA10 (keep in mind that F is 15 and A is 10) is 1111101000010000 in binary and 64016 in decimal.

changeBase 2 8 [1, 0, 0, 1, 0, 1, 0, 1, 0, 1] == [1, 1, 2, 5]
changeBase 2 10 [1, 0, 0, 1, 0, 1, 0, 1, 0, 1] == [5, 9, 7]

Binary number 1001010101 is 1125 in octal and 597 in decimal.

This problem is worth 10 POINTS.

--}


--
-- EASY: 8 POINTS
-- We guarantee that @from@ is always 10. @to@ can be anything above 1.
--
-- HARD: 2 POINTS
-- @from@ and @to@ can be anything above 1.
--

changeBase :: Natural -> Natural -> [Natural] -> [Natural]
changeBase from to digits = fromBase10 (toBase10 from digits) to []
-- changeBase from to digits = fromBase10 (condense 10 (count digits) digits) to []

-- from base 10 n = calculated new step? b = base going to, digits = e.g. [1, 3, 5]
fromBase10 :: Natural -> Natural -> [Natural] -> [Natural]
fromBase10 0 b [] = [0]
fromBase10 0 b xs = xs
fromBase10 n b xs = fromBase10 (div (n - mod n b) b) b ((mod n b):xs)

toBase10 b xs = condense b (count xs) xs

condense b l [] = 0
condense b (-1) xs = 0
condense b l (x:xs) = x * (power b l) + condense b (l - 1) xs

power b 0 = 1
power b p = b * power b (p - 1)
 
count [] = 0
count [a] = 0
count (x:xs) = 1 + count xs
