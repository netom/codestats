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
    { lsCode    :: Int
    , lsComment :: Int
    , lsEmpty   :: Int
    } deriving (Show, Generic)

data Line = Line
    { lContent :: B.ByteString
    , lStats   :: LineStats
    } deriving (Show)

data CodeStats = CodeStats
    { csT     :: T.Trie LineStats -- Lines, and how many times they occur as fst: program lines snd: comments
    , csLangs :: S.Set Language    -- Set of the names of the recognized programming languages used
    } deriving (Show, Generic)

instance Monoid LineStats where
    mempty  = memptydefault
    mappend = mappenddefault

instance Monoid CodeStats where
    mempty  = memptydefault
    mappend = mappenddefault

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

-- Number of every code lines in a file or set of files (project)
csLines :: CodeStats -> Int
csLines cs = sum $ map (\LineStats{..} -> lsCode + lsComment) $ T.elems $ csT cs

-- Number of lines with some comments
csCodeLines :: CodeStats -> Int
csCodeLines cs = sum $ map (\LineStats{..} -> lsCode) $ T.elems $ csT cs

-- Number of lines with some comments
csCommentLines :: CodeStats -> Int
csCommentLines cs = sum $ map (\LineStats{..} -> lsComment) $ T.elems $ csT cs
 
-- Number of lines with some comments
csEmptyLines :: CodeStats -> Int
csEmptyLines cs = sum $ map (\LineStats{..} -> lsEmpty) $ T.elems $ csT cs

csNonEmptyCodeLines :: CodeStats -> Int
csNonEmptyCodeLines cs = sum $ map (\LineStats{..} -> if lsEmpty > 0 then 0 else lsCode) $ T.elems $ csT cs

csNonEmptyCommentLines :: CodeStats -> Int
csNonEmptyCommentLines cs = sum $ map (\LineStats{..} -> if lsEmpty > 0 then 0 else lsComment) $ T.elems $ csT cs

csRepeatedCodeLines :: CodeStats -> Int
csRepeatedCodeLines cs = sum $ map (\LineStats{..} -> if lsCode > 1 then 1 else 0) $ T.elems $ csT cs

csCodeLineRepetitions :: CodeStats -> Int
csCodeLineRepetitions cs = sum $ map (\LineStats{..} -> if lsCode > 1 then lsCode else 0) $ T.elems $ csT cs

csRepeatedCommentLines :: CodeStats -> Int
csRepeatedCommentLines cs = sum $ map (\LineStats{..} -> if lsComment > 1 then 1 else 0) $ T.elems $ csT cs

csCommentLineRepetitions :: CodeStats -> Int
csCommentLineRepetitions cs = sum $ map (\LineStats{..} -> if lsComment > 1 then lsComment else 0) $ T.elems $ csT cs

csDistinctNonEmptyCodeLines :: CodeStats -> Int
csDistinctNonEmptyCodeLines cs = sum $ map (\LineStats{..} -> if lsCode > 0 && lsEmpty == 0 then 1 else 0) $ T.elems $ csT cs

csDistinctNonEmptyCommentLines :: CodeStats -> Int
csDistinctNonEmptyCommentLines cs = sum $ map (\LineStats{..} -> if lsComment > 0 && lsEmpty == 0 then 1 else 0) $ T.elems $ csT cs

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

parseLines :: [B.ByteString] -> Bool -> [Regex] -> (Regex, Regex) -> T.Trie LineStats
parseLines [] _ _ _ = mempty
parseLines (l:ls) isMlcContinues slcs mlc@(mlcStart, mlcEnd) =
    mappend
        ( T.singleton l (LineStats (fromEnum isCode) (fromEnum isComment) (fromEnum isEmpty)) )
        ( parseLines ls mlcContinues slcs mlc )
    where
        isMlcStart = l =~ mlcStart
        isMlcEnd = l =~ mlcEnd
        isEmpty = l == "" || l =~ rxEmpty -- The PCRE.Light library won't match empty strings on certain occasions (nullptr strings)
        isComment = isMlcContinues || (any ((=~) l) slcs)
        isCode = not isComment
        mlcContinues = isMlcStart || (isMlcContinues && not isMlcEnd)

fileStats :: FilePath -> Language -> IO CodeStats
fileStats p lang = do
    ls <- B.lines <$> B.readFile p

    let langRX = langRXs lang

    let csT = parseLines ls False (fst langRX) (snd langRX)

    let csLangs = S.fromList [lang]

    return CodeStats{..}

main :: IO ()
main = do
    paths <- walk "."

    let pathsLangs = filter (\(_,l) -> l /= Other) $ map (\p -> (p, fileLanguage (B.pack p))) paths

    css <- forM pathsLangs $ \(p,l) -> fileStats p l

    let stats = mconcat css

    putStrLn $ "All lines:       " ++ show (csLines stats)
    putStrLn $ "Code lines:      " ++ show (csCodeLines stats)
    putStrLn $ "Comment lines:   " ++ show (csCommentLines stats)
    putStrLn $ "Empty lines:     " ++ show (csEmptyLines stats)
    putStrLn $ ""
    putStrLn $ "Non-empty comment lines: " ++ show (csNonEmptyCodeLines stats)
    putStrLn $ "Non-empty code lines:    " ++ show (csNonEmptyCommentLines stats)
    putStrLn $ ""
    putStrLn $ "Repeating code lines: " ++ show (csRepeatedCodeLines stats)
    putStrLn $ "Repetitions:          " ++ show (csCodeLineRepetitions stats)
    putStrLn $ ""
    putStrLn $ "Repeating comment lines: " ++ show (csRepeatedCommentLines stats)
    putStrLn $ "Repetitions:             " ++ show (csCommentLineRepetitions stats)
    putStrLn $ ""
    putStrLn $ "Distinct non-empty code lines:    " ++ show (csDistinctNonEmptyCodeLines stats)
    putStrLn $ "Distinct non-empty comment lines: " ++ show (csDistinctNonEmptyCommentLines stats)
    putStrLn $ ""
    putStrLn $ "Languages: " ++ L.intercalate ", " (map show (S.toList (csLangs stats)))
