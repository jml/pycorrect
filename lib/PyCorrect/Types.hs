module PyCorrect.Types
  (
    Annotation(..)
  ) where

import BasicPrelude


-- | Annotation applied to a token. We are currently interested only in
-- identifiers and how they are used: is an identifier being defined, or is it
-- a reference?
--
-- For definitions, we store something that will help us find the definition
-- again. For references, we store the location of the definition.
data Annotation location = Binding location
                         | Reference location
                         | UnresolvedReference
                         deriving (Eq, Show)
