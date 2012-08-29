{-# LANGUAGE NoMonomorphismRestriction, ImpredicativeTypes, OverloadedStrings #-}
module Git.FastExport.Conduit where

import Data.Conduit
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Blaze as CB
import qualified Blaze.ByteString.Builder as BZ
import qualified Git.FastExport.Parser as P
import Data.ByteString
import Control.Monad.Base
import Control.Applicative
import Data.Attoparsec as A
import Git.FastExport.BShow
import Git.FastExport.Types
import Data.Void
import Data.Monoid((<>))
import Data.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Git.FastExport.AuthorFilter
filter fltr = L.concatMap fltr

to_bs = L.map bshow =$= CB.builderToByteString

parser = do
	cmd <- CA.sinkParser (fmap Just P.parseCmd <|> return Nothing) 
	case cmd of
		Just c -> do
			yield c
			parser
		Nothing -> return ()

parseCommit :: (MonadThrow m) => GLSink ByteString m Commit
parseCommit = do
	h <- CA.sinkParser P.parseCommitHeader
	chs <- untilM isNothing $ CA.sinkParser $ fmap Just (P.parseChange P.parseData) <|> return Nothing
	return Commit { commitHeader = h, commitChanges = catMaybes chs}
	where 
		untilM p m = do
			v <- m
			if p v then do
				vs <- untilM p m 
				return $ v:vs
			 else return []

skipEmptyCommits :: Monad m => GInfConduit GitEvent m GitEvent
skipEmptyCommits = transPipe (flip evalStateT Nothing) $ awaitForever 
	$ \i -> case i of 
		ch@GECommitHeader{} -> lift . put $ Just ch
		c@GEChange{} -> do
			s <- lift get
			case s of
				Just ch -> do
					lift . put $ Nothing
					yield ch
				_ -> return ()
			yield c
		_ -> yield i

personRenameConduit :: Monad m => AuthorDB -> GInfConduit GitEvent m GitEvent
personRenameConduit t = awaitForever $ \i -> case i of
	GECommitHeader ch -> yield . GECommitHeader . personRename t $ ch
	_                 -> yield i

data SplitterState = SS 
	{ ssMainBranch :: Maybe Branch
	, ssExtraBranches :: [(Branch,[GitEvent])]
	, ssCommitHeader  :: Maybe CommitHeader
	}
{-	
splitByBranches :: Monad m => (Path -> (Path,Branch)) -> GInfConduit GitEvent m GitEvent
splitByBranches clsfy = transPipe (flip evalState (SS Nothing [] Nothing)) $ awaitForever
	$ \i -> case i of
		GECommitHeader ch -> lift . modify $ \s -> s{ssCommitHeader = ch}
		GEChange c        -> -}