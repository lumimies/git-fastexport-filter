module Git.FastExport.Types where

import Data.ByteString

data GitRef
        = GitRef ByteString
        | GitMark Mark
newtype GitData = GitData ByteString
type Path = ByteString
type Date = ByteString
type Branch = ByteString
type Change = GChange GitData
data GChange inlineData
        = ChgDelete { chgPath :: !Path }
        | ChgModify { chgPath :: !Path
                    , chgData  :: Either GitRef inlineData
                    , chgMode :: !ByteString
                    }
        | ChgCopy { chgPath :: !Path
                  , chgFrom :: !Path
                  }
        | ChgRename { chgPath :: !Path
                    , chgFrom :: !Path
                    }
        | ChgDeleteAll
        | ChgNote { chgData :: Either GitRef inlineData
                  , chgRef  :: GitRef
                  }
data Dated a = Dated { datedDate :: !Date, datedValue :: !a }
data Person = Person { personName :: !ByteString, personEmail :: !ByteString}

data Reset = Reset Branch (Maybe GitRef)

data GitEvent 
    = GECommitHeader CommitHeader
    | GEReset        Reset
    | GEChange       (GChange ())
    | GEInfoCmd      InfoCmd
    | GEComment      ByteString
    | GEProgress     ByteString
    | GEDone

data InfoCmd = InfoLs (Maybe GitRef) Path
             | InfoCatBlob GitRef
data CommitHeader = CommitHeader 
    { chBranch    :: Branch
    , chAuthor    :: Maybe (Dated Person)
    , chCommitter :: Dated Person
    , chMessage   :: !GitData
    , chFrom      :: Maybe GitRef
    , chMerge     :: [GitRef]
    , chMark      :: Maybe Mark
    }
type Mark = Int
data Commit = Commit 
    { commitHeader :: CommitHeader
    , commitChanges :: [Change]
    }

commitAuthor = chAuthor . commitHeader
commitCommitter = chCommitter . commitHeader
commitMessage = chMessage . commitHeader
commitFrom = chFrom . commitHeader
commitMerge = chMerge . commitHeader
commitMark = chMark . commitHeader

data GitCmd
        = GCommit Commit
        | GReset Branch
        | GProgress ByteString

type CmdFilter = GitCmd -> [GitCmd]