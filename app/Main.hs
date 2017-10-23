{-# Language RecordWildCards, DeriveGeneric, OverloadedStrings #-}
module Main where

import Prelude hiding (lines, readFile)

--import Lib

import qualified Data.Trie as T
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.ByteString.Char8 as B

import Generics.Deriving.Base (Generic)
import Generics.Deriving.Monoid

import Text.Regex.PCRE.Light

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

-- Number of files
-- Stats per file, per language
-- Report unkown files

data LineStats = LineStats
    { lsEmpty   :: Int
    , lsComment :: Int
    , lsCode    :: Int
    } deriving (Show)

data Line = Line
    { lContent :: B.ByteString
    , lStats   :: LineStats
    } deriving (Show)

data CodeStats = CodeStats
    { csLines       :: T.Trie (Int, Int) -- Lines, and how many times they occur as fst: effective lines snd: comments
    , csLanguages   :: S.Set Language    -- Set of the names of the recognized programming languages used
    } deriving (Show, Generic)

instance Monoid CodeStats where
    mempty  = memptydefault
    mappend = mappenddefault

-- Our match operator
(=~) :: B.ByteString -> Regex -> Bool
(=~) s r = M.isJust $ match r s []

cpl :: B.ByteString -> Regex
cpl s = compile s []

rxEmpty :: Regex
rxEmpty = cpl "^\\s*$"

-- Build a regexp that non-greedily matches a string up until a closing string.
-- Useful for building regexes matching for closing comment tokens such as --> or */
-- It works like this:
-- Given a string, you can have any number of characters before it that is not the same
-- as the first character. If you encounter the first character, something else should follow than the second.
-- If you encounter a sequence that is the same as the first two, you cannot have the third one immediately follow them, etc.
-- If you encounter the full closing string, than there you have it.
--
-- Example: "asd" => "^([^a]|a+[^as]|(a+s)+[^ad])*(a+s)+d" asasdasdasd => asasd
--
-- C++ closing comment: "*/"  => "^([^*]|\\*+[^*/])*\\*+/"
--
-- HTML closing comment: "-->" => "^([^-]|-[^-]|-+[^->])*--+>"

-- (white space)
-- ( a */ closing comment )
-- ( some /* */ comments )
-- ( maybe a // comment at the end of the line  )
-- ( OR a /* comment start, that will continue on the next line)
rxCStyle :: Regex
rxCStyle = cpl "()"

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

langsByExtensions :: [(B.ByteString, Language)]
langsByExtensions =
    [ (".ada", Ada)
    , (".c", C)
    , (".cabal", Cabal)
    , (".cs", CSharp)
    , (".cpp", Cplusplus)
    , (".coffee", CoffeeScript)
    , (".css", CSS)
    , (".go", Go)
    , (".hs", Haskell)
    , (".html", HTML)
    , (".java", Java)
    , (".js", JavaScript)
    , (".json", JSON)

    , (".el", Lisp)
    , (".lisp", Lisp)
    , (".cl", Lisp)

    , (".pl", Perl)
    , (".pm", Perl)

    , (".php", PHP)
    , (".phtml", PHP)

    , (".py", Python)
    , (".rb", Ruby)
    , (".sh", Shell)
    , (".sql", SQL)
    , (".xml", XML)

    , (".yaml", Yaml)
    , (".yml", Yaml)

    , (".zsh", Zsh)

    , (".txt", Text)
    , (".md", Text)
    ]

fileLanguage :: B.ByteString -> Language
fileLanguage p =
    case dropWhile (\(ext, _) -> not $ B.isSuffixOf ext p) langsByExtensions of
    ((_,l):_) -> l
    []        -> Other

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
        if l =~ mlcEnd
        then
            T.singleton l (0, 1) `mappend` parseLines ls False slcs mlc
        else
            T.singleton l (0, 1) `mappend` parseLines ls True slcs mlc
    else
        if any ((=~) l) slcs
        then
            T.singleton l (0, 1) `mappend` parseLines ls False slcs mlc
        else
            if l =~ mlcStart
            then
                T.singleton l (0, 1) `mappend` parseLines ls True slcs mlc -- TODO: this CAN be a code line
            else
                T.singleton l (1, 0) `mappend` parseLines ls False slcs mlc

fileStats :: FilePath -> Language -> IO CodeStats
fileStats p lang = do
    ls <- B.lines <$> B.readFile p

    let langRX = langRXs lang

    let csLines = parseLines ls False (fst langRX) (snd langRX)

    let csLanguages = S.fromList [lang]

    return CodeStats{..}

main :: IO ()
main = do
    paths <- walk "."

    let pathsLangs = filter (\(_,l) -> l /= Other) $ map (\p -> (p, fileLanguage (B.pack p))) paths

    css <- forM pathsLangs $ \(p,l) -> fileStats p l

    let stats = mconcat css

    putStrLn $ "All lines:       " ++ show (csNumLines stats)
    putStrLn $ "Comments:        " ++ show (csNumComments stats)
    putStrLn $ "Empty lines:     " ++ show (csNumEmptyNonComment stats)
    putStrLn $ "Effective lines: " ++ show (csNumEffectiveLines stats)
    putStrLn $ "Repeating lines: " ++ show (csNumRepeated stats)
    putStrLn $ "Repetitions:     " ++ show (csNumRepetitions stats)
    putStrLn $ "Net lines:       " ++ show (csNetLines stats)
    putStrLn $ "Languages: " ++ L.intercalate ", " (map show (S.toList (csLanguages stats)))
