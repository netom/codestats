{-# Language RecordWildCards     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (lines, readFile)

import Lib

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Trie as TR
import qualified Data.Set as S
--import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Generics.Deriving.Base (Generic)
import Generics.Deriving.Monoid

import Text.Regex.PCRE.Light

import Control.Monad

import System.Directory
import System.FilePath
import System.Posix.Files

import Control.Lens hiding ((.=))
import Data.Aeson.Lens
import Data.Aeson

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

type SI = Sum Int

-- Number of files
-- Stats per file, per language
-- Report unkown files

data LineStats = LineStats
    { lsCode    :: SI
    , lsComment :: SI
    , lsEmpty   :: SI
    } deriving (Show, Generic)

data Line = Line
    { lContent :: B.ByteString
    , lStats   :: LineStats
    } deriving (Show)

data CodeStats = CodeStats
    { csT     :: TR.Trie LineStats -- Lines, and how many times they occur as fst: program lines snd: comments
    , csLangs :: S.Set Language    -- Set of the names of the recognized programming languages used
    } deriving (Show, Generic)

newtype CategorizedCodeStats = CategorizedCodeStats { getCategorizedCodeStats :: HM.HashMap T.Text CodeStats } deriving (Show, Generic)

instance Semigroup LineStats where
    (<>) = mappend

instance Monoid LineStats where
    mappend = mappenddefault
    mempty  = memptydefault

instance Semigroup CodeStats where
    (<>) = mappend

instance Monoid CodeStats where
    mappend = mappenddefault
    mempty  = memptydefault

instance Semigroup CategorizedCodeStats where
    (<>) = mappend

instance Monoid CategorizedCodeStats where
    mappend = mappenddefault
    mempty  = memptydefault

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
csLines :: CodeStats -> SI
csLines cs = sum $ map (\LineStats{..} -> lsCode + lsComment) $ TR.elems $ csT cs

-- Number of lines with some comments
csCodeLines :: CodeStats -> SI
csCodeLines cs = sum $ map (\LineStats{..} -> lsCode) $ TR.elems $ csT cs

-- Number of lines with some comments
csCommentLines :: CodeStats -> SI
csCommentLines cs = sum $ map (\LineStats{..} -> lsComment) $ TR.elems $ csT cs

-- Number of lines with some comments
csEmptyLines :: CodeStats -> SI
csEmptyLines cs = sum $ map (\LineStats{..} -> lsEmpty) $ TR.elems $ csT cs

csNonEmptyCodeLines :: CodeStats -> SI
csNonEmptyCodeLines cs = sum $ map (\LineStats{..} -> if lsEmpty > 0 then 0 else lsCode) $ TR.elems $ csT cs

csNonEmptyCommentLines :: CodeStats -> SI
csNonEmptyCommentLines cs = sum $ map (\LineStats{..} -> if lsEmpty > 0 then 0 else lsComment) $ TR.elems $ csT cs

csRepeatedCodeLines :: CodeStats -> SI
csRepeatedCodeLines cs = sum $ map (\LineStats{..} -> if lsCode > 1 then 1 else 0) $ TR.elems $ csT cs

csCodeLineRepetitions :: CodeStats -> SI
csCodeLineRepetitions cs = sum $ map (\LineStats{..} -> if lsCode > 1 then lsCode else 0) $ TR.elems $ csT cs

csRepeatedCommentLines :: CodeStats -> SI
csRepeatedCommentLines cs = sum $ map (\LineStats{..} -> if lsComment > 1 then 1 else 0) $ TR.elems $ csT cs

csCommentLineRepetitions :: CodeStats -> SI
csCommentLineRepetitions cs = sum $ map (\LineStats{..} -> if lsComment > 1 then lsComment else 0) $ TR.elems $ csT cs

csDistinctNonEmptyCodeLines :: CodeStats -> SI
csDistinctNonEmptyCodeLines cs = sum $ map (\LineStats{..} -> if lsCode > 0 && lsEmpty == 0 then 1 else 0) $ TR.elems $ csT cs

csDistinctNonEmptyCommentLines :: CodeStats -> SI
csDistinctNonEmptyCommentLines cs = sum $ map (\LineStats{..} -> if lsComment > 0 && lsEmpty == 0 then 1 else 0) $ TR.elems $ csT cs

-- Traverse from 'top' directory and return all the relevant files
walk :: FilePath -> IO [FilePath]
walk top = do
    ds <- getDirectoryContents top
    paths <- forM (filter (\p -> length p > 0 && p /= "." && p /= "..") ds) $ \d -> do
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

parseLines :: [B.ByteString] -> Bool -> [Regex] -> (Regex, Regex) -> TR.Trie LineStats
parseLines [] _ _ _ = mempty
parseLines (l:ls) isMlcContinues slcs mlc@(mlcStart, mlcEnd) =
    mappend
        ( TR.singleton l (LineStats (Sum $ fromEnum isCode) (Sum $ fromEnum isComment) (Sum $ fromEnum (isEmpty l))) )
        ( parseLines ls mlcContinues slcs mlc )
    where
        isMlcStart = l =~ mlcStart
        isMlcEnd = l =~ mlcEnd
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

instance ToJSON CodeStats where
    toJSON cs = Object $ HM.fromList
        [ "all"     .= sum2js (csLines cs)
        , "code"    .= sum2js (csCodeLines cs)
        , "comment" .= sum2js (csCommentLines cs)
        , "empty"   .= sum2js (csEmptyLines cs)
        , "comment-non-empty"  .= sum2js (csNonEmptyCommentLines cs)
        , "code-non-empty"     .= sum2js (csNonEmptyCodeLines cs)
        , "code-repeating"     .= sum2js (csRepeatedCodeLines cs)
        , "code-repetition"    .= sum2js (csCodeLineRepetitions cs)
        , "comment-repeating"  .= sum2js (csRepeatedCommentLines cs)
        , "comment-repetitions"        .= sum2js (csCommentLineRepetitions cs)
        , "code-distinct-non-empty"    .= sum2js (csDistinctNonEmptyCodeLines cs)
        , "comment-distinct-non-empty" .= sum2js (csDistinctNonEmptyCommentLines cs)
        , "langs" .= Array (V.fromList $ map (String . T.pack . show) $ S.toList $ csLangs cs)
        ]
        where
            sum2js = Number . fromIntegral . getSum

statsFromPaths :: [FilePath] -> IO CodeStats
statsFromPaths paths = do
    let pathsLangs = filter (\(_,l) -> l /= Other) $ map (\p -> (p, fileLanguage (B.pack p))) paths
    mconcat <$> forM pathsLangs ( \(p,l) -> fileStats p l )

categorize :: HM.HashMap T.Text Regex -> [FilePath] -> HM.HashMap T.Text [FilePath]
categorize cs paths = fmap (\rx -> filter (\p -> B.pack p =~ rx) paths) cs

main :: IO ()
main = do
    -- Read configuration
    conf <- (^? _JSON) <$> TIO.readFile ".codestats" :: IO (Maybe Value)

    let   excludes :: V.Vector Regex          = (cpl . TE.encodeUtf8 . (^. _String)) <$> conf ^. _Just . ix "excludes"   . _Array
    let categories :: HM.HashMap T.Text Regex = (cpl . TE.encodeUtf8 . (^. _String)) <$> conf ^. _Just . ix "categories" . _Object

    -- Generate a list of paths, weed out anything that matches any of the excludes
    paths :: [FilePath] <- filter (\p -> not $ any (\rx -> B.pack p =~ rx) excludes) <$> walk "."

    -- Group the paths to categories

    stats <- traverse statsFromPaths $ categorize categories paths
    -- stats <- statsFromPaths paths
    -- ForldR with a go function. Start with an empty HashMap Text CodeStats
    -- Go will:
    --  Calculate a CodeStats for the file
    --  Select the list of keys that has matching regex for the path
    -- append the CodeStats for the file onto the CodeStats stored under the category keys
    BL.putStrLn $ encode stats
