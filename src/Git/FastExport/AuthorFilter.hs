{-# LANGUAGE OverloadedStrings #-}
module Git.FastExport.AuthorFilter 
    ( AuthorDB
    , personRename
    , personRenameFilter
    , loadPersonFile
    , loadPersonRename
    )
    where

import Control.Applicative
import Data.Monoid
import Data.Attoparsec.Char8 as A
import Data.Attoparsec.Combinator as A
import Git.FastExport.Types

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Git.FastExport.Parser as P
import qualified Data.Trie as T
import System.IO

newtype AuthorDB = DB (T.Trie Person)
personRename :: AuthorDB -> CommitHeader -> CommitHeader
personRename (DB t) ch@CommitHeader{chAuthor=a, chCommitter=c} =
    ch{chAuthor = fmap fixDated a, chCommitter = fixDated c}
    where
        fixDated d@(Dated{datedValue = p}) = d{datedValue = fixPerson p}
        fixPerson :: Person -> Person
        fixPerson p = maybe p id $ T.lookup (personName p) t <|> T.lookup (personEmail p) t
personRenameFilter :: AuthorDB -> CmdFilter
personRenameFilter t (GCommit commit) =
    [GCommit commit{commitHeader=personRename t . commitHeader $ commit}]

personRenameFilter _ c = [c]


loadPersonRename :: String -> IO CmdFilter
loadPersonRename s = do
    t <- loadPersonFile s
    return $ personRenameFilter t

loadPersonFile :: String -> IO AuthorDB
loadPersonFile s = do
    withFile s ReadMode $ \ f -> do
        let loop ls = do
            eof <- hIsEOF f
            if eof then return ls else do
                l <- B.hGetLine f
                case flip feed "" $ parse parseAuthorLine l of
                    Done _ res -> loop (res:ls)
                    Fail t ctx err -> do
                        --putStrLn $ "Skipped: "++ C.unpack l ++ "(" ++ err ++ ": " ++ C.unpack t ++ ")"
                        loop ls -- - $ err ++ " (line: " ++ C.unpack l ++ ")"
                    Partial p -> loop ls
        ls <- loop []
        return . DB $ T.fromList ls
skipHSpace = skipWhile $ inClass " \t"
parseComment = do
    skipHSpace
    char '#'
    skipWhile (/= '\n')
    skipSpace
parseAuthorLine = do
    s <- takeWhile1 (notInClass "=#")
    char '='
    let (key,_) = B.spanEnd isSpace_w8 s
    skipHSpace
    p <- P.parsePerson
    skipSpace
    return $ (key,p)
