{-# LANGUAGE OverloadedStrings #-}
module Main where

import Git.FastExport
import Git.FastExport.Filter
import Git.FastExport.AuthorFilter
import Git.FastExport.BShow
import Data.Conduit
import qualified Git.FastExport.Conduit as FE
import Data.Conduit.Binary
import System.IO (stdin, stdout)
import System.Exit
import System.Environment (getArgs, getProgName)
import qualified Data.Trie as T
import Data.Maybe (listToMaybe)
import Control.Monad (guard)
import Data.ByteString (ByteString)

data Options m = Options
	{ optSource :: Source m ByteString
	, optSink :: Sink ByteString m ()
	}
parseArgs :: MonadResource m => IO (Options m)
parseArgs = do
	args <- getArgs
	if any (=="--help") args then do
		prog <- getProgName
		putStrLn $ usage prog
		exitSuccess
		else return Options
			{ optSource = maybe (sourceHandle stdin) sourceFile . takeArg          $ args
			, optSink   = maybe (sinkHandle stdout)  sinkFile   . takeArg . Prelude.drop 1 $ args
			}
	where
		takeArg as = do
			a <- listToMaybe as
			guard $ a /= "-"
			return a

usage prog = prog ++ " [<input file> [<output file>]]\n\n" ++
			 "\tFilter git fast-export stream from input to output. '-' means stdin/stdout\n"
main = do
	Options{ optSource = src, optSink = sink} <- parseArgs
	authorFilter <- loadPersonRename "../authors.txt"
	let fltr b = do
		splitBranches
				[ ("trunk/", "refs/heads/trunk")
				, ("branches/branch1/", "refs/heads/branch1")
				, ("branches/branch2/", "refs/heads/branch2")
				] b >>= dropPaths ["web/","server"] >>= authorFilter
	runResourceT $ src $= FE.parser $= FE.filter fltr $= FE.to_bs $$ sink
	--processFE fltr stdin stdout
