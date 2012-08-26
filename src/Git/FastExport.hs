module Git.FastExport
	( module Git.FastExport.BShow
	, module Git.FastExport.Types
	, processFE
	) where

import Git.FastExport.BShow
import Git.FastExport.Parser
import Git.FastExport.Types
import Data.Attoparsec.Char8 as A
import Data.Attoparsec.Combinator as A
import Data.ByteString as B hiding (null, filter, map, concatMap, any)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char8 as BB
import qualified Blaze.Text as BB
import System.IO
import Data.Monoid (mconcat)

processFE fltr hIn hOut = do readLoop baseparser
	where
		baseparser = parse parseCmd
		readLoop parser = do
			eof <- hIsEOF hIn
			if eof then return () else do
				d <- B.hGet hIn (32 * 1024)
				loop parser d
		loop parser d = do
			case parser d of
				Done t r -> do
					BB.toByteStringIO (B.hPut hOut) . mconcat . map bshow $ fltr r
					if B.null t then return () else loop baseparser t
				res@(Partial _) -> readLoop (feed res)
				Fail t c e -> error $ "<" ++ (C.unpack . B.take 100) t ++ "> at " ++ show c ++ ": " ++ e
