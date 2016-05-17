-- | Analyze Python code for correctness.
module PyCorrect
  (
    -- * Exported functions
    inc
  ) where

import BasicPrelude

-- | Increment one 'Num' value.
--
--  >>> let answer = 42 :: Int
--  >>> let prev = answer - 1
--  >>> inc prev
--  42
--  >>> succ . Prelude.last . Prelude.take prev . iterate inc $ 1
--  42
--
--  Properties:
--
--  prop> succ x == inc x
--  prop> inc (negate x) == negate (pred x)
--
inc :: Num a => a -- ^ value to increment
             -> a -- ^ result
inc x = x + 1
