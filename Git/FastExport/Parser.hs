{-# LANGUAGE OverloadedStrings #-}
module Git.FastExport.Parser where

import Data.Monoid
import Data.Word
import Data.Bits
import qualified Data.Attoparsec as AP
import Data.Attoparsec.Char8 as A
import Data.Attoparsec.Combinator as A
import Data.ByteString as B hiding (map,null)
import Control.Applicative hiding (optional)
import Control.Monad (guard)
import Git.FastExport.Types

optional = option Nothing . fmap Just

parseCmd = (((string "commit " *> fmap (\x -> x `seq` GCommit x) parseCommit) <?> "commit")
		<|> ((string "reset " *> fmap GReset parseRef ) <?> "reset")
		<|> ((string "progress " *> fmap GProgress parseLine) <?> "progress")) <* skipMany parseNL

parseCommit :: Parser Commit
parseCommit = do
	branch <- parseRef
	parseNL
	mark <- optional parseMarkLine
	author <- optional $ "author " .*> parsePersonLine
	committer <- "committer " .*> parsePersonLine
	message <- parseData
	from <- option Nothing $ string "from " >> parseRef >>= \r -> parseNL >> (return $ Just $ GitRef r)
	merges <- flip sepBy parseNL $ "merge " .*> parseRef
	changes <- sepBy parseChange parseNL
	return Commit
		{ commitBranch = branch
		, commitChanges = changes
		, commitAuthor = author
		, commitCommitter = committer
		, commitMessage = message
		, commitFrom = from
		, commitMerge = map GitRef merges
		, commitMark = mark
		}

parsePerson = flip (<?>) "parsePerson" $ do
	Just nextCh <- peekChar
	let hasName = nextCh /= '<'
	name <- if hasName then
				fmap (\s -> B.take (B.length s - 1) s) $ takeTill (=='<')
			else
				char '<' *> return B.empty
	email <- option ' ' (char '<') *> takeTill (=='>')
	char '>'
	return Person
		{ personName = name
		, personEmail = email
		}

parsePersonLine = do
	p <- parsePerson
	space
	date <- takeTill (=='\n') <?> "date"
	parseNL
	return $ Dated date p

parseMark :: Parser Mark
parseMark = char ':' *> decimal

parseMarkLine = string "mark " *> parseMark <* parseNL

parseNL = AP.skip (AP.inClass "\n")

parseLine = takeTill (=='\n')

parseRef = takeTill (=='\n')

parseDataRef :: Parser GitRef
parseDataRef = fmap GitMark parseMark <|> fmap GitRef (takeTill (==' '))

parsePath isUnQSpaces = A.takeWhile acceptUnquoted <|> parseQuoted
	where
		parseQuoted = do
			char '"'
			parseQuotedChunks id
		parseQuotedChunks ls = do
			chunk <- A.takeWhile (\c -> c /= '\\' && c /= '"')
			sep <- anyChar
			if sep == '\\' then do
				c <- parseEsc
				parseQuotedChunks $ ls . (\chunks -> chunk:c:chunks)
			 else return . B.concat $ ls [chunk]
		acceptUnquoted
			| isUnQSpaces = \c -> c /= '\n' && c /= ' '
			| otherwise   = (/= '\n')
		parseEsc = char8 '\\' *> choice (escMap ++ [B.singleton <$> AP.satisfy (AP.inClass "\"\\") ])
		parseOctalEsc = do
			d1 <- octalDigit
			guard $ d1 >= 0 && d1 <= 3
			d2 <- octalDigit
			d3 <- octalDigit
			return $ (d1 `shiftL` 6) .|. (d2 `shiftL` 3) .|. d3
		octalDigit = do
			d <- digit
			guard $ d >= '0' && d <= '7'
			return $ fromEnum d - fromEnum '0'
		escMap = map (\(c,s) -> char8 c *> return s)
			[ ('a', "\a")
			, ('b', "\b")
			, ('f', "\f")
			, ('n', "\n")
			, ('r', "\r")
			, ('t', "\t")
			, ('v', "\v")
			]

parseData = do
	string "data "
	content <- choice 
		[ do
			string "<<"
			delim <- takeTill (== '\n')
			c <- manyTill parseLine (string delim >> parseNL)
			option () parseNL
			return $ B.concat c
		, do
			len <- decimal
			parseNL
			A.take len
		] <* option () parseNL
	return $ GitData content
parseChange = do
	choice 
		[ parseDelete
		, parseDeleteAll
		, parseModify
		, parseCopy
		, parseRename
		, parseNote
		]
	where
		parseDeleteAll = string "deleteall" *> return ChgDeleteAll
		parseDelete = do
			char 'D' -- filedelete
			space
			path <- parsePath False
			return ChgDelete
				{ chgPath = path }
		parseCopy = do
			char 'C'
			space
			srcPath <- parsePath False
			space
			path <- parsePath False
			return ChgCopy { chgPath = path, chgFrom = srcPath }
		parseRename = do
			char 'R'
			space
			srcPath <- parsePath False
			space
			path <- parsePath False
			return ChgRename { chgPath = path, chgFrom = srcPath }
		parseNote = do
			char 'N' -- notemodify
			space
			ref <- eitherP (string "inline") parseDataRef
			space
			cmtRef <- parseDataRef
			cData <- case ref of
				Left _ -> do -- inline
					parseNL
					d <- parseData
					return . Right $ d
				Right r ->  return . Left $ r
			return ChgNote
				{ chgRef = cmtRef
				, chgData = cData
				}
		parseModify = do
			char 'M' -- filemodify
			space
			mode <- A.takeWhile isDigit
			space
			ref <- eitherP (string "inline") parseDataRef
			space
			path <- parsePath True
			cData <- case ref of
				Left _ -> do -- inline
					parseNL
					d <- parseData
					return . Right $ d
				Right r ->  return . Left $ r
			return ChgModify
				{ chgPath = path
				, chgData = cData
				, chgMode = mode
				}
