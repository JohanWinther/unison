{-# LANGUAGE TemplateHaskell #-}

module Version where

import Data.Text qualified as Text
import System.Process qualified as Process
import Unison.Prelude

-- | A formatted descriptor of when and against which commit this unison executable was built
-- E.g. latest-149-g5cef8f851 (built on 2021-10-04)
--      release/M2i (built on 2021-10-05)
gitDescribeWithDate :: Text
gitDescribeWithDate =
  let formatDate d = " (built on " <> d <> ")"
      (gitRef, date) = gitDescribe
   in gitRef <> formatDate date

type CommitDate = Text

type GitRef = Text

-- | Uses Template Haskell to embed a git descriptor of the commit
--   which was used to build the executable.
-- E.g. latest-149-g5cef8f851 (built on 2021-10-04)
--      release/M2i (built on 2021-10-05)
gitDescribe :: (GitRef, CommitDate)
gitDescribe =
  $( liftIO do
       -- Outputs date of current commit; E.g. 2021-08-06
       let getDate = Text.strip . Text.pack <$> Process.readProcess "git" ["show", "-s", "--format=%cs"] ""
       date <- getDate <|> pure ""
       -- Fetches a unique tag-name to represent the current commit.
       -- Uses human-readable names whenever possible.
       -- Marks version with a `'` suffix if building on a dirty worktree.
       let getTag = Text.strip . Text.pack <$> Process.readProcess "git" ["describe", "--tags", "--always", "--dirty='"] ""
       tag <- getTag <|> pure "unknown"
       [|(tag, date)|]
   )
