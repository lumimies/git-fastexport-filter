{-# LANGUAGE OverloadedStrings #-}
module Git.FastExport.BShow (BlazeShow(..)) where

import Data.Monoid
import Data.List as L
import Data.ByteString as B hiding (map)
import qualified Data.ByteString.Char8 as C
import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char8 as BB
import qualified Blaze.Text as BB
import qualified GHC.Exts (IsString(..))
import Git.FastExport.Types

instance GHC.Exts.IsString BB.Builder where
	fromString = BB.fromString
class BlazeShow a where
	bshow :: a -> BB.Builder
	bshowList :: [a] -> BB.Builder
	bshowList = mconcat . L.intersperse ", " . map bshow
instance BlazeShow Integer where
	bshow = BB.integral
instance BlazeShow Int where
	bshow = BB.integral
instance BlazeShow Char where
	bshow = BB.fromChar
	bshowList = BB.fromString
instance BlazeShow a => BlazeShow [a] where
	bshow = bshowList
instance BlazeShow ByteString where
	bshow = BB.fromByteString

instance BlazeShow GitRef where
	bshow (GitRef bs) = bshow bs
	bshow (GitMark m) = BB.fromChar ':' <> bshow m

instance Show GitRef where
	show = showme

instance BlazeShow GitData where
	bshow (GitData bs) = "data " <> bshow (C.length bs) <> "\n" <> bshow bs <> "\n"

instance Show GitData where
	show = showme
instance BlazeShow Change where
	bshow (ChgModify path dat mode) =
		"M " <> bshow mode <> " " <>
			either (\r -> bshow r <> " " <> bshow path)
				(\d -> "inline " <> bshow path <> "\n" <> bshow d)
				dat
	bshow (ChgDelete path) = "D " <> bshow path
instance Show Change where
	show = showme
instance BlazeShow a => BlazeShow (Dated a) where
	bshow (Dated date v) = bshow v  <> " " <> bshow date
instance BlazeShow a => Show (Dated a) where
	show = showme
instance BlazeShow Person where
	bshow (Person name email) = (if B.null name then "" else bshow name <> " ")
		<> "<" <> bshow email <> ">"
instance Show Person where show = showme
instance BlazeShow GitCmd where
	bshow (GCommit cmt) = "commit " <> bshow cmt <> "\n"
	bshow (GReset branch) = "reset " <> bshow branch <> "\n"
	bshow (GProgress bs) = "progress " <> bshow bs <> "\n"
instance Show GitCmd where show = showme
instance BlazeShow Commit where
	bshow c = bshow (commitBranch c) <> "\n"
				<> maybe "" (\m -> "mark :" <> bshow m <> "\n") (commitMark c)
				<> maybe "" (\a -> "author " <> bshow a <> "\n") (commitAuthor c)
				<> "committer " <> bshow (commitCommitter c) <> "\n"
				<> bshow (commitMessage c)
				<> mconcat (map ((<> "\n") . bshow) (commitChanges c))
instance Show Commit where show = showme
showme a = C.unpack . BB.toByteString . bshow $ a