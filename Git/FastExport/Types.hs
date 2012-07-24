module Git.FastExport.Types where

import Data.ByteString

data GitRef
        = GitRef ByteString
        | GitMark Mark
newtype GitData = GitData ByteString
type Path = ByteString
type Date = ByteString
type Branch = ByteString
data Change
        = ChgDelete { chgPath :: !Path }
        | ChgModify { chgPath :: !Path
                    , chgData  :: Either GitRef GitData
                    , chgMode :: !ByteString
                    }
        | ChgCopy { chgPath :: !Path
                  , chgFrom :: !Path
                  }
        | ChgRename { chgPath :: !Path
                    , chgFrom :: !Path
                    }
        | ChgDeleteAll
        | ChgNote { chgData :: Either GitRef GitData
                  , chgRef  :: GitRef
                  }
data Dated a = Dated { datedDate :: !Date, datedValue :: !a }
data Person = Person { personName :: !ByteString, personEmail :: !ByteString}

type Mark = Int
data Commit = Commit {
        commitBranch :: !Branch,
        commitChanges :: [Change],
        commitAuthor :: Maybe (Dated Person),
        commitCommitter :: Dated Person,
        commitMessage :: !GitData,
        commitFrom :: Maybe GitRef,
        commitMerge :: [GitRef],
        commitMark :: Maybe Mark
}

data GitCmd
        = GCommit Commit
        | GReset Branch
        | GProgress ByteString

type CmdFilter = GitCmd -> [GitCmd]