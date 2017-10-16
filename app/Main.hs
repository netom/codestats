{-# Language RecordWildCards, DeriveGeneric, OverloadedStrings #-}
module Main where

import Prelude hiding (lines, readFile)

import Lib

import qualified Data.Trie as T
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B

import Generics.Deriving.Base (Generic)
import Generics.Deriving.Monoid

import Text.Regex.TDFA
import Text.Printf

import Control.Monad

import System.Directory
import System.FilePath
import System.Posix.Files

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
    | JSON
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
    { csLines       :: T.Trie (Int, Int) -- Lines, and how many times they occur as fst: effective lines snd: comments
    , csLanguages   :: S.Set Language    -- Set of the names of the recognized programming languages used
    } deriving (Show, Generic)

instance Monoid CodeStats where
    mempty  = memptydefault
    mappend = mappenddefault

rxEmpty = "^\\s*$" :: B.ByteString

-- Number of every code lines in a file or set of files (project)
csNumLines :: CodeStats -> Int
csNumLines cs = sum $ map (\(e, c) -> e + c) $ T.elems $ csLines cs

-- Number of lines containint only comments and maybe whitespace
csNumComments :: CodeStats -> Int
csNumComments cs = sum $ map (\(_, c) -> c) $ T.elems $ csLines cs
 
-- Number of non-comment lines containing only whitespace
csNumEmptyNonComment :: CodeStats -> Int
csNumEmptyNonComment cs = sum $ map (\(e, _) -> e) $ T.elems $ T.mapBy (\k v -> if k =~ rxEmpty then Just v else Nothing) $ csLines cs

-- Number of effective line: those that are non-comment and non-whitespace
csNumEffectiveLines :: CodeStats -> Int
csNumEffectiveLines cs = csNumLines cs - csNumComments cs - csNumEmptyNonComment cs

-- Lines that are appearing more than once as a non-comment line
-- This function disregards programming language
csRepeated :: CodeStats -> T.Trie (Int, Int)
csRepeated cs = T.filterMap (\(e, c) -> if e > 1 then Just (e, c) else Nothing) $ csLines cs

-- Number of distinct, repeated, non-comment lines
csNumRepeated :: CodeStats -> Int
csNumRepeated cs = T.size $ csRepeated cs

-- Number of repeatitions with multiplicity (non-comment lines)
csNumRepetitions :: CodeStats -> Int
csNumRepetitions cs = sum $ map fst $ T.elems $ csRepeated cs

-- Number of unique non-comment, non-whitespace lines
csNetLines :: CodeStats -> Int
csNetLines cs = T.size $ T.mapBy (\k (e, c) -> if e > 0 && (not $ k =~ rxEmpty) then Just (e, c) else Nothing) $ csLines cs

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
    | p =~ ("\\.ada$"    :: B.ByteString) = Ada
    | p =~ ("\\.c$"      :: B.ByteString) = C
    | p =~ ("\\.cabal$"  :: B.ByteString) = Cabal
    | p =~ ("\\.cs$"     :: B.ByteString) = CSharp
    | p =~ ("\\.cpp$"    :: B.ByteString) = Cplusplus
    | p =~ ("\\.coffee$" :: B.ByteString) = CoffeeScript
    | p =~ ("\\.css$"    :: B.ByteString) = CSS
    | p =~ ("\\.go$"     :: B.ByteString) = Go
    | p =~ ("\\.hs$"     :: B.ByteString) = Haskell
    | p =~ ("\\.html$"   :: B.ByteString) = HTML
    | p =~ ("\\.java$"   :: B.ByteString) = Java
    | p =~ ("\\.js$"     :: B.ByteString) = JavaScript
    | p =~ ("\\.json$"   :: B.ByteString) = JSON

    | p =~ ("\\.el$"     :: B.ByteString) = Lisp
    | p =~ ("\\.lisp$"   :: B.ByteString) = Lisp
    | p =~ ("\\.cl$"     :: B.ByteString) = Lisp

    | p =~ ("\\.pl$"     :: B.ByteString) = Perl
    | p =~ ("\\.pm$"     :: B.ByteString) = Perl

    | p =~ ("\\.php$"    :: B.ByteString) = PHP
    | p =~ ("\\.phtml$"  :: B.ByteString) = PHP

    | p =~ ("\\.py$"     :: B.ByteString) = Python
    | p =~ ("\\.rb$"     :: B.ByteString) = Ruby
    | p =~ ("\\.sh$"     :: B.ByteString) = Shell
    | p =~ ("\\.sql$"    :: B.ByteString) = SQL
    | p =~ ("\\.xml$"    :: B.ByteString) = XML
    | p =~ ("\\.yaml$"   :: B.ByteString) = Yaml
    | p =~ ("\\.zsh$"    :: B.ByteString) = Zsh

    | p =~ ("\\.txt$"    :: B.ByteString) = Text
    | p =~ ("\\.md$"     :: B.ByteString) = Text

    | otherwise = Other

-- Comment parsing is buggy, yes. And butt ugly too.
langRXs :: Language -> ([B.ByteString], (B.ByteString, B.ByteString))
langRXs Ada = (["a^"], ("a^", "a^"))
langRXs C = (["a^"], ("a^", "a^"))
langRXs Cabal = (["a^"], ("a^", "a^"))
langRXs CSharp = (["a^"], ("a^", "a^"))
langRXs Cplusplus = (["a^"], ("a^", "a^"))
langRXs CoffeeScript = (["a^"], ("a^", "a^"))
langRXs CSS = (["a^"], ("a^", "a^"))
langRXs Go = (["a^"], ("a^", "a^"))
langRXs Haskell = (["^\\s*--"], ("a^", "a^"))
langRXs HTML = (["a^"], ("a^", "a^"))
langRXs Java = (["a^"], ("a^", "a^"))
langRXs JavaScript = (["a^"], ("a^", "a^"))
langRXs JSON = (["a^"], ("a^", "a^"))
langRXs Lisp = (["a^"], ("a^", "a^"))
langRXs Perl = (["a^"], ("a^", "a^"))

langRXs PHP = (["^[ \\t]*//", "^[ \\t]*/\\*.*\\*/^[ \\t]*$"], ("/\\*", "\\*/"))

langRXs Python = (["a^"], ("a^", "a^"))
langRXs Ruby = (["a^"], ("a^", "a^"))
langRXs Shell = (["a^"], ("a^", "a^"))
langRXs SQL = (["a^"], ("a^", "a^"))
langRXs XML = (["a^"], ("a^", "a^"))
langRXs Yaml = (["a^"], ("a^", "a^"))
langRXs Zsh = (["a^"], ("a^", "a^"))
langRXs Text = (["a^"], ("a^", "a^"))
langRXs Other = (["a^"], ("a^", "a^"))

parseLines :: [B.ByteString] -> Bool -> [B.ByteString] -> (B.ByteString, B.ByteString) -> T.Trie (Int, Int)
parseLines [] _ _ _ = mempty
parseLines (l:ls) isPrevLineMlc slcs mlc@(mlcStart, mlcEnd) =
    if isPrevLineMlc
    then
        if l =~ mlcEnd
        then
            T.singleton l (0, 1) `mappend` parseLines ls False slcs mlc
        else
            T.singleton l (0, 1) `mappend` parseLines ls True slcs mlc
    else
        if any (l =~) slcs
        then
            T.singleton l (0, 1) `mappend` parseLines ls False slcs mlc
        else
            if l =~ mlcStart
            then
                T.singleton l (0, 1) `mappend` parseLines ls True slcs mlc -- TODO: this CAN be a code line
            else
                T.singleton l (1, 0) `mappend` parseLines ls False slcs mlc

fileStats :: FilePath -> IO CodeStats
fileStats p = do
    ls <- B.lines <$> B.readFile p

    let lang = fileLanguage p

    let langRX = langRXs lang

    let csLines = parseLines ls False (fst langRX) (snd langRX)

    let csLanguages = S.fromList [fileLanguage p]

    return CodeStats{..}

main :: IO ()
main = do
    paths_ <- walk "."
    let paths = filter (\p -> fileLanguage p /= Other) paths_

    css <- forM paths $ \p -> fileStats p

    let stats = mconcat css

    putStrLn $ "All lines:       " ++ show (csNumLines stats)
    putStrLn $ "Comments:        " ++ show (csNumComments stats)
    putStrLn $ "Empty lines:     " ++ show (csNumEmptyNonComment stats)
    putStrLn $ "Effective lines: " ++ show (csNumEffectiveLines stats)
    putStrLn $ "Repeating lines: " ++ show (csNumRepeated stats)
    putStrLn $ "Repetitions:     " ++ show (csNumRepetitions stats)
    putStrLn $ "Net lines:       " ++ show (csNetLines stats)
    putStrLn $ "Languages: " ++ L.intercalate ", " (map show (S.toList (csLanguages stats)))
