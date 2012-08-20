-- |
-- Module : Git.FastExport.Filter
-- Maintainer : zohar@kelrich.com
--
-- A collection of filters for git
module Git.FastExport.Filter where

import Git.FastExport.Types
import qualified Data.ByteString as B
import Data.List
import qualified Data.Trie as T
splitBranches :: [(Path, Branch)] -> CmdFilter
splitBranches paths (GCommit commit@Commit{commitChanges = c}) =
	flip concatMap paths $
		\(p,b) -> let newChanges =
						flip concatMap c $
							\chg -> if p `B.isPrefixOf` chgPath chg then
							 [chg{chgPath = B.drop (B.length p) (chgPath chg)}]
							else []
					in if null newChanges then [] else [GCommit commit{commitHeader=(commitHeader commit){chBranch = b}, commitChanges = newChanges}]
splitBranches _ GReset{} = []
splitBranches _ c@GProgress{} = [c]

dropPaths :: [Path] -> CmdFilter
dropPaths paths (GCommit commit@Commit{commitChanges = c}) =
	let filtered = filter (\p -> not . any (`B.isPrefixOf` chgPath p) $ paths) c
	in if null filtered then [] else [GCommit commit{commitChanges = filtered}]
dropPaths _ c@GProgress{} = [c]
dropPaths _ _ = []
