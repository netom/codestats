{-# Language OverloadedStrings #-}
module Lib
    ( rxEmpty
    , cpl
    , (=~)
    ) where

import Text.Regex.PCRE.Light
import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe as M

(=~) :: B.ByteString -> Regex -> Bool
(=~) s r = M.isJust $ match r s []

cpl :: B.ByteString -> Regex
cpl s = compile s []

rxEmpty :: Regex
rxEmpty = cpl "^\\s*$"
