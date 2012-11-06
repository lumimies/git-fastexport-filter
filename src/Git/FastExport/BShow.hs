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

chgHasData ChgModify{} = True
chgHasData ChgNote{} = True
chgHasData _ = False

eitherData = either bshow (const "inline") . chgData
instance BlazeShow (GChange a) where
	bshow c@ChgModify{} =
		"M " <> bshow (chgMode c) <> " " <> eitherData c <> " " <> bshow (chgPath c) <> "\n"
	bshow c@ChgDelete{} = "D " <> bshow (chgPath c) <> "\n"
	bshow c@ChgCopy{}   = "C " <> bshow (chgFrom c) <> " " <> bshow (chgPath c) <> "\n"
	bshow c@ChgRename{} = "R " <> bshow (chgFrom c) <> " " <> bshow (chgPath c) <> "\n"
	bshow ChgDeleteAll  = "deleteall\n"
	bshow c@ChgNote{}   = "N " <> eitherData c <> " " <> (bshow $ chgRef c) <> "\n"
instance Show (GChange a) where
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
instance BlazeShow CommitHeader where
	bshow c = bshow (chBranch c) <> "\n"
				<> maybe "" (\m -> "mark :" <> bshow m <> "\n") (chMark c)
				<> maybe "" (\a -> "author " <> bshow a <> "\n") (chAuthor c)
				<> "committer " <> bshow (chCommitter c) <> "\n"
				<> bshow (chMessage c)
				<> maybe mempty (\b -> "from " <> bshow b <> "\n") (chFrom c)
				<> mconcat (map (\r -> "merge " <> bshow r <> "\n") (chMerge c))
instance BlazeShow Commit where
	bshow c = bshow (commitHeader c) <> mconcat (map bshow (commitChanges c))
instance Show Commit where show = showme
showme a = C.unpack . BB.toByteString . bshow $ a

instance BlazeShow Reset where
	bshow (Reset branch from) 
		= "reset " <> bshow branch <> "\n" 
		<> maybe mempty (\b -> "from " <> bshow b <> "\n") from

instance BlazeShow InfoCmd where
	bshow (InfoLs ref path) = "ls " <> maybe (showQuote path) (\r -> bshow r <> bshow path) ref <> "\n"
	bshow (InfoCatBlob ref) = "cat-blob " <> bshow ref <> "\n"

instance BlazeShow GitEvent where
	bshow (GECommitHeader ch) = "commit " <> bshow ch
	bshow (GEReset r) = bshow r
	bshow (GEInfoCmd i) = bshow i
	bshow (GEComment c) = "#" <> bshow c <> "\n"
	bshow (GEChange c)  = bshow c
	bshow (GEProgress p) = "progress " <> bshow p <> "\n"
	bshow GEDone         = "done\n"
bshowPath :: Path -> BB.Builder
bshowPath p 
	| "\"" `B.isPrefixOf` p || C.any (== '\n') p 
		=  showQuote p
	| otherwise 
		= bshow p

showQuote p = "\"" <> mconcat (map escChunk $ chunks p)
	where
		escChunk c 
			| B.null c  = mempty
			| otherwise = esc (C.head c) <> bshow (B.tail c)
		esc '\n' = "\\n"
		esc c    | c == '\\' || c == '"' = "\\" <> bshow c
				 | otherwise = bshow c
		chunk bs = C.span (\c -> c /= '"' && c /= '\n' && c /= '\\') bs
		chunks bs 
			| B.null bs = []
			| otherwise = let (b,bt) = chunk bs 
					    in
						if B.null b then chunks bt else b:chunks bt
						   
						  