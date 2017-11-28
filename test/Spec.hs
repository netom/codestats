{-# Language OverloadedStrings #-}

import Lib

import qualified Data.ByteString.Char8 as B
import Control.Applicative
import Test.QuickCheck

newtype EmptyByteString = EmptyByteString {getEmptyByteString :: B.ByteString} deriving (Show)

genEmptyBS :: Gen B.ByteString
genEmptyBS = oneof [return B.empty, B.pack <$> (listOf $ elements [' ', '\t', '\n', '\r'])]

instance Arbitrary EmptyByteString where
    arbitrary = EmptyByteString <$> genEmptyBS

prop_canDetectEmpty :: EmptyByteString -> Bool
prop_canDetectEmpty = isEmpty . getEmptyByteString

main :: IO ()
main = do
    quickCheck prop_canDetectEmpty
