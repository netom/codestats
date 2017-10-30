{-# Language OverloadedStrings #-}

import Lib

--import Text.Regex.PCRE.Light
--import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    putStrLn "Testing empty regex:"
    putStrLn $ show $ "" =~ rxEmpty
    putStrLn $ show $ " " =~ rxEmpty
    putStrLn $ show $ "   " =~ rxEmpty
    putStrLn $ show $ "\t" =~ rxEmpty
    putStrLn $ show $ "\t  \t\t  " =~ rxEmpty
    putStrLn $ show $ "\t  \r\n\n\r   " =~ rxEmpty
