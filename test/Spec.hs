{-# Language OverloadedStrings #-}

import Lib
import Regex

import qualified Data.ByteString.Char8 as B
import Test.QuickCheck

newtype EmptyByteString = EmptyByteString {getEmptyByteString :: B.ByteString} deriving (Show)

genEmptyBS :: Gen B.ByteString
genEmptyBS = oneof [return B.empty, B.pack <$> (listOf $ elements [' ', '\t', '\n', '\r'])]

instance Arbitrary EmptyByteString where
    arbitrary = EmptyByteString <$> genEmptyBS

prop_canDetectEmpty :: EmptyByteString -> Bool
prop_canDetectEmpty = isEmpty . getEmptyByteString

prop_newRegexCanDetectEmpty :: EmptyByteString -> Bool
prop_newRegexCanDetectEmpty = matchBytes (star (bytes " " <+> bytes "\t" <+> bytes "\n" <+> bytes "\r")) . getEmptyByteString

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 100000 }  prop_canDetectEmpty
    quickCheckWith stdArgs { maxSuccess = 100000 }  prop_newRegexCanDetectEmpty
