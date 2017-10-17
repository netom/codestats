{-# Language RecordWildCards, DeriveGeneric, OverloadedStrings #-}
module Main where

import Prelude hiding (lines, readFile)

import Lib

import qualified Data.Trie as T
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import qualified Data.ByteString.Char8 as B

import Generics.Deriving.Base (Generic)
import Generics.Deriving.Monoid

import Text.Regex.TDFA hiding (match)
import Text.Regex.TDFA.ByteString
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

rxEmpty = cpl "^\\s*$"

-- Number of every code lines in a file or set of files (project)
csNumLines :: CodeStats -> Int
csNumLines cs = sum $ map (\(e, c) -> e + c) $ T.elems $ csLines cs

-- Number of lines containint only comments and maybe whitespace
csNumComments :: CodeStats -> Int
csNumComments cs = sum $ map (\(_, c) -> c) $ T.elems $ csLines cs
 
-- Number of non-comment lines containing only whitespace
csNumEmptyNonComment :: CodeStats -> Int
csNumEmptyNonComment cs = sum $ map (\(e, _) -> e) $ T.elems $ T.mapBy (\k v -> if k `match` rxEmpty then Just v else Nothing) $ csLines cs

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
csNetLines cs = T.size $ T.mapBy (\k (e, c) -> if e > 0 && (not $ k `match` rxEmpty) then Just (e, c) else Nothing) $ csLines cs

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

-- I know, I know.
fromRight (Right x) = x

-- Compiles a regex
cpl :: B.ByteString -> Regex
cpl s = fromRight $ compile defaultCompOpt defaultExecOpt s

-- Dummy match `operator`
match :: B.ByteString -> Regex -> Bool
match s r = isJust $ matchOnce r s

fileLanguage :: FilePath -> Language
fileLanguage p
    | bsp `match` (cpl "\\.ada$"    ) = Ada
    | bsp `match` (cpl "\\.c$"      ) = C
    | bsp `match` (cpl "\\.cabal$"  ) = Cabal
    | bsp `match` (cpl "\\.cs$"     ) = CSharp
    | bsp `match` (cpl "\\.cpp$"    ) = Cplusplus
    | bsp `match` (cpl "\\.coffee$" ) = CoffeeScript
    | bsp `match` (cpl "\\.css$"    ) = CSS
    | bsp `match` (cpl "\\.go$"     ) = Go
    | bsp `match` (cpl "\\.hs$"     ) = Haskell
    | bsp `match` (cpl "\\.html$"   ) = HTML
    | bsp `match` (cpl "\\.java$"   ) = Java
    | bsp `match` (cpl "\\.js$"     ) = JavaScript
    | bsp `match` (cpl "\\.json$"   ) = JSON

    | bsp `match` (cpl "\\.el$"     ) = Lisp
    | bsp `match` (cpl "\\.lisp$"   ) = Lisp
    | bsp `match` (cpl "\\.cl$"     ) = Lisp

    | bsp `match` (cpl "\\.pl$"     ) = Perl
    | bsp `match` (cpl "\\.pm$"     ) = Perl

    | bsp `match` (cpl "\\.php$"    ) = PHP
    | bsp `match` (cpl "\\.phtml$"  ) = PHP

    | bsp `match` (cpl "\\.py$"     ) = Python
    | bsp `match` (cpl "\\.rb$"     ) = Ruby
    | bsp `match` (cpl "\\.sh$"     ) = Shell
    | bsp `match` (cpl "\\.sql$"    ) = SQL
    | bsp `match` (cpl "\\.xml$"    ) = XML

    | bsp `match` (cpl "\\.yaml$"   ) = Yaml
    | bsp `match` (cpl "\\.yml$"    ) = Yaml

    | bsp `match` (cpl "\\.zsh$"    ) = Zsh

    | bsp `match` (cpl "\\.txt$"    ) = Text
    | bsp `match` (cpl "\\.md$"     ) = Text

    | otherwise = Other
    where
        bsp = B.pack p

-- Comment parsing is buggy, yes. And butt ugly too.
langRXs :: Language -> ([Regex], (Regex, Regex))
langRXs Ada = ([cpl "^[ \\t]*--"], (cpl "a^", cpl "a^"))
langRXs C = ([cpl "^[ \\t]*//", cpl "^[ \\t]*/\\*.*\\*/[ \\t]*$"], (cpl "/\\*", cpl "\\*/"))
langRXs Cabal = ([cpl "^\\s*--"], (cpl "a^", cpl "a^"))
langRXs CSharp = ([cpl "^[ \\t]*//", cpl "^[ \\t]*/\\*.*\\*/[ \\t]*$"], (cpl "/\\*", cpl "\\*/"))
langRXs Cplusplus = ([cpl "^[ \\t]*//", cpl "^[ \\t]*/\\*.*\\*/[ \\t]*$"], (cpl "/\\*", cpl "\\*/"))
langRXs CoffeeScript = ([cpl "^[ \\t]#"], (cpl "^[ \\t]*###", cpl "^[ \\t]*###"))
langRXs CSS = ([cpl "^[ \\t]*/\\*.*\\*/[ \\t]*$"], (cpl "/\\*", cpl "\\*/"))
langRXs Go = ([cpl "^[ \\t]*//", cpl "^[ \\t]*/\\*.*\\*/[ \\t]*$"], (cpl "/\\*", cpl "\\*/"))
langRXs Haskell = ([cpl "^[ \\t]*--"], (cpl "a^", cpl "a^"))
langRXs HTML = ([cpl "^[ \\t]*<!--.*-->[ \\t]*$"], (cpl "<!--", cpl "-->"))
langRXs Java = ([cpl "^[ \\t]*//", cpl "^[ \\t]*/\\*.*\\*/[ \\t]*$"], (cpl "/\\*", cpl "\\*/"))
langRXs JavaScript = ([cpl "^[ \\t]*//", cpl "^[ \\t]*/\\*.*\\*/[ \\t]*$"], (cpl "/\\*", cpl "\\*/"))
langRXs JSON = ([cpl "a^"], (cpl "a^", cpl "a^"))
langRXs Lisp = ([cpl "^[ \\t]*;", cpl "^[ \\t]*;;", cpl "^[ \\t]*;;;", cpl "^[ \\t]*;;;;"], (cpl "a^", cpl "a^"))
langRXs Perl = ([cpl "^[ \\t]*#"], (cpl "a^", cpl "a^"))

langRXs PHP = ([cpl "^[ \\t]*//", cpl "^[ \\t]*/\\*.*\\*/[ \\t]*$"], (cpl "/\\*", cpl "\\*/"))

langRXs Python = ([cpl "^[ \\t]*#"], (cpl "a^", cpl "a^"))
langRXs Ruby = ([cpl "^[ \\t]*#"], (cpl "a^", cpl "a^"))
langRXs Shell = ([cpl "^[ \\t]*#"], (cpl "a^", cpl "a^"))
langRXs SQL = ([cpl "^[ \\t]*--"], (cpl "a^", cpl "a^"))
langRXs XML = ([cpl "^[ \\t]*<!--.*-->[ \\t]*$"], (cpl "<!--", cpl "-->"))
langRXs Yaml = ([cpl "a^"], (cpl "a^", cpl "a^"))
langRXs Zsh = ([cpl "^[ \\t]*#"], (cpl "a^", cpl "a^"))
langRXs Text = ([], (cpl "a^", cpl "a^"))
langRXs Other = ([], (cpl "a^", cpl "a^"))

parseLines :: [B.ByteString] -> Bool -> [Regex] -> (Regex, Regex) -> T.Trie (Int, Int)
parseLines [] _ _ _ = mempty
parseLines (l:ls) isPrevLineMlc slcs mlc@(mlcStart, mlcEnd) =
    if isPrevLineMlc
    then
        if l `match` mlcEnd
        then
            T.singleton l (0, 1) `mappend` parseLines ls False slcs mlc
        else
            T.singleton l (0, 1) `mappend` parseLines ls True slcs mlc
    else
        if any (match l) slcs
        then
            T.singleton l (0, 1) `mappend` parseLines ls False slcs mlc
        else
            if l `match` mlcStart
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
