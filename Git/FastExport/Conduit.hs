{-# LANGUAGE NoMonomorphismRestriction #-}
module Git.FastExport.Conduit where

import Data.Conduit
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Blaze as CB
import qualified Git.FastExport.Parser as P
import Data.Attoparsec as A
import Git.FastExport.BShow

filter fltr = L.concatMap fltr

to_bs = L.map bshow =$= CB.builderToByteString

parser = do
	cmd <- CA.sinkParser P.parseCmd
	yield cmd
	parser
