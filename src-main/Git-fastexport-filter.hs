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
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC (pack,unpack)
import System.Console.CmdTheLine as CL
import System.FilePath as FP (searchPathSeparator)

newtype PathList = PathList {unPL :: [Path]}
instance ArgVal ByteString where
	converter = let (parser,pp) = converter
				in (fmap BC.pack . parser, pp . BC.unpack)
instance ArgVal PathList where
	converter = (fmap PathList . parserL, ppL . unPL)
		where
			(parserL, ppL) = list searchPathSeparator

doStuff src sink authorsPath droppedPaths = do
	authorFilter <- maybe (return return) loadPersonRename authorsPath
	let fltr b = do
		splitBranches
				[ ("trunk/", "refs/heads/trunk")
				, ("branches/branch1/", "refs/heads/branch1")
				, ("branches/branch2/", "refs/heads/branch2")
				] b >>= dropPaths droppedPaths >>= authorFilter
	runResourceT $ src $= FE.parser $= FE.filter fltr $= FE.to_bs $$ sink

usage prog = prog ++ " [<input file> [<output file>]]\n\n" ++
			 "\tFilter git fast-export stream from input to output. '-' means stdin/stdout\n"
main = run (doStuff <$> src <*> sink <*> authorPath <*> droppedPaths, termInfo)
	where
		termInfo = defTI
			{ termName = "git-fastexport-filter"
			, termDoc  = "Filter git fast-export streams."
			, version = "0.1"
			}
		droppedPaths = fmap unPL . value $
			opt (PathList []) (optInfo ["d","drop-paths"]){
				optDoc = "Paths to drop, separated by " ++ [searchPathSeparator]}
		src = fmap (sourceHandle stdin ||| sourceFile) . fileExists $ value $
			pos 1 "-" posInfo{ posName = "IN", posDoc = "Path to input file. - for stdin." }
		sink = fmap (sinkHandle stdout ||| sinkFile) . value $
			pos 2 "-" posInfo{ posName = "OUT", posDoc = "Path to output file. - for stdout" }
		(def ||| f) s = case s of
			"-" -> def
			_   -> f s
		authorPath = value $ opt Nothing (optInfo ["a","authors"]){ optDoc = "Path to authors file" }
