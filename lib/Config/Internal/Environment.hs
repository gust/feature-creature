module Config.Internal.Environment
( Environment (..)
) where

data Environment = Development
                 | Test
                 | Production
                 deriving (Eq, Show, Read)
