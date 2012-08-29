{-# LANGUAGE NoMonomorphismRestriction, ImpredicativeTypes, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Git.FastExport.Conduit where

import Data.Conduit
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Blaze as CB
import qualified Blaze.ByteString.Builder as BZ
import qualified Git.FastExport.Parser as P
import qualified Data.Map as M
import Data.ByteString (ByteString)
import Control.Monad.Base
import Control.Applicative
import Data.Attoparsec as A
import Git.FastExport.BShow
import Git.FastExport.Types
import Data.Void
import Data.Monoid((<>))
import Data.Maybe
import Control.Monad (when, unless)
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
data StuffSplitterState k v = SSS
	{ sssCurrentBucket :: Maybe k
	, sssOtherBuckets :: M.Map k v
	}

-- | Map one input into many sets of outputs, grouped by key, and then emit sets
-- by key order. Allows immediately streaming the first of some subset of keys
-- instead of collecting it.
splitStuff :: (Monad m, Ord k) => 
	(a -> [(k,GSource m b)]) -- ^ Map input to sources for certain buckets
	-> (a -> m Bool)         -- ^ Whether to flush all buckets after this
	-> (k -> m Bool)         -- ^ Whether this bucket may be streamed through
	-> GInfConduit a m b
splitStuff classify flushAfterP allowStreamThroughP = transPipe (flip evalStateT (SSS Nothing M.empty)) $ awaitForever
	$ \i -> do
		let kvs = classify i
		unless (null kvs) $ do
			lift . modify
				$ \s -> s{sssCurrentBucket = Just $ fromMaybe (fst . head $ kvs) $ sssCurrentBucket s }
			Just curr <- lift . gets $ sssCurrentBucket
			mapM_ yieldStuff kvs
		flush <- lift . lift $ flushAfterP i
		when flush $ do
			srcs <- lift . gets $ M.elems . sssOtherBuckets
			lift . put $ SSS Nothing M.empty
			transPipe lift $ sequence_ srcs
	where
		yieldStuff (k,src :: GSource m b) = do
			haveCurrent <- lift . gets $ isJust . sssCurrentBucket
			unless haveCurrent $ do
				allowFlush <- lift . lift $ allowStreamThroughP k
				when allowFlush $
					lift . modify $ \s ->s{sssCurrentBucket = Just k}
			curr <- lift . gets $ sssCurrentBucket
			case curr of
				Just k' | k == k' -> 
					transPipe lift src
				Nothing           -> 
					lift . modify $ \s -> s{sssOtherBuckets = M.insertWith (>>) k src (sssOtherBuckets s)}				

{-splitByBranches :: (MonadThrow m) => (GChange () -> [(Branch,[GitEvent])]) -> GInfConduit GitEvent m GitEvent
splitByBranches clsfy = transPipe (flip evalStateT Nothing) $
		splitStuff classifyEvent isCommitEnded
	where
		classifyEvent (GECommitHeader ch) = lift . put $ Just (ch,[])
		classifyEvent (GEChange c)        = do
			let bs = clsfy c
			s <- get
			case s of
				Nothing -> 
			(Just (ch,bs'), emitEvents )
-}
