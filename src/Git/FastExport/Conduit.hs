{-# LANGUAGE NoMonomorphismRestriction, ImpredicativeTypes, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Git.FastExport.Conduit where

import Data.Conduit
import Data.Conduit.Internal
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Blaze as CB
import qualified Blaze.ByteString.Builder as BZ
import qualified Git.FastExport.Parser as P
import qualified Data.Map as M
import Data.ByteString (ByteString)
import Control.Monad.Base
import Control.Applicative
import Data.Attoparsec as A hiding (Done)
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

eitherConduit :: Monad m => Pipe Void i o u m x -> Pipe Void i' o x m y -> Pipe Void (Either i i') o u m y
eitherConduit x y = mapOutput (either id id) $ firstConduit x >+> secondConduit y

firstConduit :: Monad m => Pipe l i o u m x -> Pipe l (Either i c) (Either o c) u m x
firstConduit = go
	where
		go (Leftover p l)       = Leftover (go p) l
		go x@(NeedInput p c)    = NeedInput (either (go . p) (HaveOutput (go x) (return ()) . Right)) (go . c)
		go (HaveOutput p c o)   = HaveOutput (go p) c (Left o)
		go (PipeM m)            = PipeM (m >>= (return . go))
		go (Done r)             = Done r

secondConduit :: Monad m => Pipe l i o u m x -> Pipe l (Either c i) (Either c o) u m x
secondConduit = go
	where
		go (Leftover p l)       = Leftover (go p) l
		go x@(NeedInput p c)    = NeedInput (either (HaveOutput (go x) (return ()) . Left) (go . p)) (go . c)
		go (HaveOutput p c o)   = HaveOutput (go p) c (Right o)
		go (PipeM m)            = PipeM (m >>= (return . go))
		go (Done r)             = Done r

gatherWithPassthrough :: (Monad m, Ord k) => (k -> Bool) -> GConduit (Flush (k,a)) m a
gatherWithPassthrough canPass = loop Nothing >+> eitherConduit gatherStuff unFlushify
	where
		unFlushify = awaitForever $ \i -> case i of
			Chunk (k,a) -> yield a
			Flush       -> return ()
		mark k k'
			| k == k'   = Right
			| otherwise = Left
		go s c@Flush = do
			yield $ Left c
			loop Nothing
		go s c@(Chunk (k, a)) = do
			let (f,s') = maybe (if canPass k then (Right, Just k) else (Left,s)) (\k' -> (mark k k', s)) s
			yield $ f c
			loop s'
		loop s = awaitE >>= either return (go s)

gatherWithPassthrough' :: (Monad m, Ord k) => (k -> Bool) -> GConduit (Flush (k,a)) m a
gatherWithPassthrough' canPass = stateConduit_ Nothing go (\_ -> return ()) >+> eitherConduit gatherStuff unFlushify
	where
		unFlushify = awaitForever $ \i -> case i of
			Chunk (k,a) -> yield a
			Flush       -> return ()
		go c@Flush = do
			yield $ Left c
			lift $ put Nothing
		go c@(Chunk (k, a)) = do
			x <- lift $ gets isNothing
			when (x && canPass k) $ lift $ put (Just k)
			s <- lift get
			case s of
				Just k' 
					| k == k' -> yield $ Right c
				_             -> yield $ Left c

stateConduit' :: (Monad m) => s -> (i -> Pipe l i o u (StateT s m) u) -> Pipe l i o u m u
stateConduit' s p = stateConduit s go
	where go = awaitE >>= either (return . Just) (\i -> p i >> return Nothing)

stateConduit_ :: (Monad m) => s -> 
	(i -> Pipe l i o u (StateT s m) ()) -> 
	(u -> Pipe l i o u (StateT s m) r) -> 
	Pipe l i o u m r
stateConduit_ s p e = stateConduit s go
	where go = awaitE >>= either ((>>= (return . Just)) . e) ((>> return Nothing) . p)
stateConduit :: (Monad m) => s -> Pipe l i o u (StateT s m) (Maybe r) -> Pipe l i o u m r
stateConduit s p = loop s
	where loop s = do
		(x,s') <- transPipe (flip evalStateT s) (p >>= lift . gets . (,))
		case x of
			Just r -> return r
			Nothing -> loop s'

gatherStuff' :: (Monad m, Ord k) => GInfConduit (Flush (k,a)) m a
gatherStuff' = stateConduit_ M.empty handle ((flush >>) . return)
	where
		flush = do
			lift get >>= L.sourceList . concat . M.elems
			lift $ put M.empty
		handle Flush = flush
		handle (Chunk (k,a)) = lift . modify $ M.insertWith (++) k [a]

gatherStuff :: (Monad m, Ord k) => GConduit (Flush (k,a)) m a
gatherStuff = go M.empty
	where
		go bs = awaitE >>= either (const $ flush bs) (handle bs)
		handle bs Flush = flush bs >> go M.empty
		handle bs (Chunk (k,a)) = go $ M.insertWith (++) k [a] bs
		flush = L.sourceList . concat . M.elems

splitByBranches :: (MonadThrow m) => (GChange () -> [(Branch,[GitEvent])]) -> GConduit GitEvent m GitEvent
splitByBranches clsfy = go Nothing M.empty >+> gatherWithPassthrough (const True)
	where
		go curr knownBranches = awaitE >>= either return (classifyEvent curr knownBranches)
		classifyEvent curr knownBranches (GECommitHeader ch) = do
			yield Flush
			go (Just ch) M.empty
		classifyEvent curr@(Just ch) knownBranches (GEChange c) = do
			let bs = clsfy c
			flip mapM_ bs $ \(b,es) -> do
				let header = if b `M.member` knownBranches then [] else  [GECommitHeader ch{chBranch = b}]
				L.sourceList . map (\x -> Chunk (b,x)) $ header ++ es
			go curr (foldr (\(x,_) -> M.insert x True) knownBranches bs)
--splitBranchesConduit :: (Monad m) => [(Path, Branch)] -> GConduit GitEvent m GitEvent
--splitBranchesConduit bs = splitByBranches convert
--	where
--		convert 
dropPathsConduit :: (Monad m) => (Path -> Bool) -> GInfConduit GitEvent m GitEvent
dropPathsConduit p = awaitForever go
	where
		go e@(GEChange c) = case c of
			ChgDeleteAll -> yield e
			-- TODO: If we're filtering the 'from' it might not be there.
			-- There's no easy fix for this, because there's no blob to use instead.
			ChgCopy{} -> unless (p $ chgPath c) $ yield e 
			ChgRename{} -> case (p (chgPath c), p (chgFrom c)) of
				(True,True) -> return ()
				(True,False) -> yield (GEChange (ChgDelete $ chgFrom c))
				-- Convert rename to copy, to kinda fulfill not touching the filtered path
				(False,True) -> yield (GEChange (ChgCopy{chgFrom = chgFrom c, chgPath = chgPath c})) 
				(False,False) -> yield e
			ChgNote{}   -> yield e
			_ -> unless (p $ chgPath c) $ yield e
