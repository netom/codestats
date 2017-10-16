{-# Language RecordWildCards, DeriveGeneric, OverloadedStrings #-}
module Main where

import Lib
import Data.Trie
import Generics.Deriving.Base (Generic)
import Generics.Deriving.Monoid
import Prelude hiding (lines, readFile)
import Data.ByteString.Char8 (ByteString, readFile, lines)
--import qualified Data.ByteString.Char8 as B
--import Text.Regex.Base.RegexLike
--import Text.Regex.TDFA.ByteString
import Text.Regex.TDFA
import Control.Monad
import System.Directory
import System.FilePath
import System.Posix.Files
import qualified Data.Set as S

data Language
    = Ada
    | C
    | Cabal
    | CSharp
    | Cplusplus
    | CoffeeScript
    | CSS
    | Go
    | Haskell
    | HTML
    | Java
    | JavaScript
    | Lisp
    | Perl
    | PHP
    | Python
    | Ruby
    | Shell
    | SQL
    | XML
    | Yaml
    | Zsh
    | Text
    | Other
    deriving (Eq, Ord, Show)

instance Monoid Int where
    mempty  = 0
    mappend = (+)

data CodeStats = CodeStats
    { csLines       :: Trie (Int, Int) -- Lines, and how many times they occur as fst: effective lines snd: comments
    , csLanguages   :: S.Set Language    -- Set of the names of the recognized programming languages used
    } deriving (Show, Generic)

instance Monoid CodeStats where
    mempty  = memptydefault
    mappend = mappenddefault

rxEmpty = "^\\s*$" :: ByteString

-- Number of every code lines in a file or set of files (project)
csNumLines :: CodeStats -> Int
csNumLines cs = sum $ map (\(e, c) -> e + c) $ elems $ csLines cs

-- Number of lines containint only comments and maybe whitespace
csNumComments :: CodeStats -> Int
csNumComments cs = sum $ map (\(_, c) -> c) $ elems $ csLines cs
 
-- Number of non-comment lines containing only whitespace
csNumEmptyNonComment :: CodeStats -> Int
csNumEmptyNonComment cs = sum $ map (\(e, _) -> e) $ elems $ mapBy (\k v -> if k =~ rxEmpty then Just v else Nothing) $ csLines cs

-- Number of effective line: those that are non-comment and non-whitespace
csEffectiveLines :: CodeStats -> Int
csEffectiveLines cs = csNumLines cs - csNumComments cs - csNumEmptyNonComment cs

-- Lines that are appearing more than once as a non-comment line
-- This function disregards programming language
csRepeated :: CodeStats -> Trie (Int, Int)
csRepeated cs = filterMap (\(e, c) -> if e > 1 then Just (e, c) else Nothing) $ csLines cs

-- Number of distinct, repeated, non-comment lines
csNumRepeated :: CodeStats -> Int
csNumRepeated cs = size $ csRepeated cs

-- Number of repeatitions with multiplicity (non-comment lines)
csNumRepetitions :: CodeStats -> Int
csNumRepetitions cs = sum $ map fst $ elems $ csRepeated cs

-- Number of unique non-comment, non-whitespace lines
csNetLines :: CodeStats -> Int
csNetLines cs = csEffectiveLines cs - csNumRepetitions cs

-- Number of lines without copied lines (e.g. License), tests or generated code lines
csNetLines2 :: CodeStats -> CodeStats -> CodeStats -> CodeStats -> Int
csNetLines2 wholeProject copied tests generated = csNetLines wholeProject - csNetLines copied - csNetLines tests - csNetLines generated

-- Traverse from 'top' directory and return all the relevant files
walk :: FilePath -> IO [FilePath]
walk top = do
    ds <- getDirectoryContents top
    paths <- forM (filter (\p -> length p > 0 && head p /= '.') ds) $ \d -> do
        let path = top </> d
        s <- getFileStatus path
        if isDirectory s
              then walk path
              else return [path]
    return (concat paths)

fileLanguage :: FilePath -> Language
fileLanguage p
    | p =~ ("\\.ada$" :: ByteString)   = Ada
    | p =~ ("\\.c$" :: ByteString)     = C
    | p =~ ("\\.cabal$" :: ByteString) = Cabal
    | p =~ ("\\.cs$" :: ByteString)    = CSharp
    | p =~ ("\\.cpp$" :: ByteString)   = Cplusplus
    | p =~ ("\\.coffee$" :: ByteString)= CoffeeScript
    | p =~ ("\\.css$" :: ByteString)   = CSS
    | p =~ ("\\.go$" :: ByteString)    = Go
    | p =~ ("\\.hs$" :: ByteString)    = Haskell
    | p =~ ("\\.html$" :: ByteString)  = HTML
    | p =~ ("\\.java$" :: ByteString)  = Java
    | p =~ ("\\.js$" :: ByteString)    = JavaScript

    | p =~ ("\\.el$" :: ByteString)    = Lisp
    | p =~ ("\\.lisp$" :: ByteString)  = Lisp
    | p =~ ("\\.cl$" :: ByteString)    = Lisp

    | p =~ ("\\.pl$" :: ByteString)    = Perl
    | p =~ ("\\.pm$" :: ByteString)    = Perl

    | p =~ ("\\.php$" :: ByteString)   = PHP
    | p =~ ("\\.phtml$" :: ByteString) = PHP

    | p =~ ("\\.py$" :: ByteString)    = Python
    | p =~ ("\\.rb$" :: ByteString)    = Ruby
    | p =~ ("\\.sh$" :: ByteString)    = Shell
    | p =~ ("\\.sql$" :: ByteString)   = SQL
    | p =~ ("\\.xml$" :: ByteString)   = XML
    | p =~ ("\\.yaml$" :: ByteString)  = Yaml
    | p =~ ("\\.zsh$" :: ByteString)   = Zsh

    | p =~ ("\\.txt$" :: ByteString)   = Text
    | p =~ ("\\.md$" :: ByteString)    = Text

    | otherwise = Other

fileStats :: FilePath -> IO CodeStats
fileStats p = do
    ls <- lines <$> readFile p

    let csLines = fromList [("# comment", (0, 2)), ("print 'valami';", (2, 1))]
    let csLanguages = S.fromList [fileLanguage p]

    return CodeStats{..}

main :: IO ()
main = do
    let cs1 = CodeStats (fromList [("# comment", (0, 2)), ("print 'valami';", (2, 1))]) (S.fromList [PHP])
    let cs2 = CodeStats (fromList [("# comment", (0, 1)), ("print 'valami';", (1, 0)), ("}", (1, 0))]) (S.fromList [JavaScript, Ada])

    paths <- walk "."
    css <- forM paths $ \p -> fileStats p

    print $ mconcat css
