{-# LANGUAGE OverloadedStrings #-}
module Git.FastExport.Parser where

import Data.Monoid
import Data.Word
import qualified Data.Attoparsec as AP (skip, inClass)
import Data.Attoparsec.Char8 as A
import Data.Attoparsec.Combinator as A
import Data.ByteString as B hiding (map,null)
import Control.Applicative

import Git.FastExport.Types

notChar8 :: Char -> Parser Word8
notChar8 = fmap (toEnum . fromEnum) . notChar

parseCmd = (((string "commit " *> fmap (\x -> x `seq` GCommit x) parseCommit) <?> "commit")
		<|> ((string "reset " *> fmap GReset parseRef ) <?> "reset")
		<|> ((string "progress " *> fmap GProgress parseLine) <?> "progress")) <* skipMany parseNL

parseCommit :: Parser Commit
parseCommit = do
	branch <- parseRef
	parseNL
	mark <- option Nothing $ fmap Just parseMarkLine
	author <- option Nothing $ string "author " *> fmap Just parsePersonLine
	committer <- string "committer " *> parsePersonLine
	message <- parseData
	from <- option Nothing $ string "from " >> parseRef >>= \r -> parseNL >> (return $ Just $ GitRef r)
	merges <- flip sepBy parseNL $ string "merge " *> parseRef
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

parsePath isUnQSpaces = A.takeWhile acceptUnquoted
	{-choice
		[ char '\"' *> fmap B.concat (manyTill (fmap B.singleton parseEsc <|> A.take 1) (char8 '\"'))
		, B.cons <$> notChar8 '\"' <*> A.takeWhile acceptUnquoted
		]-}
	where
		acceptUnquoted
			| isUnQSpaces = \c -> c /= '\n' && c /= ' '
			| otherwise   = (/= '\n')
		{-parseEsc = char8 '\\' *> choice
			[ char8 'n' *> return (toEnum . fromEnum $ '\n')
			, char8 '\\'
			, char8 '"'
			]-}

parseData = do
	string "data "
	content <- choice [
		do
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
	choice [
		do
			char 'M' -- filemodify
			space
			mode <- A.takeWhile isDigit
			space
			ref <- eitherP (string "inline") parseDataRef
			space
			path <- parsePath False
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
		,
		do
			char 'D' -- filedelete
			space
			path <- parsePath False
			return ChgDelete
				{ chgPath = path }

		]
