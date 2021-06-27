module Sqlite.Syntax.Internal.Type.Statement
  ( Statement (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.AlterTableStatement (AlterTableStatement)
import Sqlite.Syntax.Internal.Type.AnalyzeStatement (AnalyzeStatement)
import Sqlite.Syntax.Internal.Type.AttachStatement (AttachStatement)
import Sqlite.Syntax.Internal.Type.CreateIndexStatement (CreateIndexStatement)
import Sqlite.Syntax.Internal.Type.CreateTableStatement (CreateTableStatement)
import Sqlite.Syntax.Internal.Type.DeleteStatement (DeleteStatement)
import Sqlite.Syntax.Internal.Type.InsertStatement (InsertStatement)
import Sqlite.Syntax.Internal.Type.SelectStatement (SelectStatement)
import Sqlite.Syntax.Internal.Type.TransactionType (TransactionType)
import Prelude

data Statement
  = Statement'AlterTable AlterTableStatement
  | Statement'Analyze AnalyzeStatement
  | Statement'Attach AttachStatement
  | -- |
    -- * https://sqlite.org/lang_transaction.html
    -- * https://sqlite.org/syntax/begin-stmt.html
    Statement'Begin TransactionType
  | -- |
    -- * https://sqlite.org/lang_transaction.html
    -- * https://sqlite.org/syntax/commit-stmt.html
    Statement'Commit
  | Statement'CreateIndex CreateIndexStatement
  | Statement'CreateTable CreateTableStatement
  | Statement'CreateTrigger TODO
  | Statement'CreateView TODO
  | Statement'CreateVirtualTable TODO
  | -- |
    -- * https://sqlite.org/syntax/delete-stmt.html
    -- * https://sqlite.org/lang_delete.html
    Statement'Delete DeleteStatement
  | Statement'Detach TODO
  | Statement'DropIndex TODO
  | Statement'DropTable TODO
  | Statement'DropTrigger TODO
  | Statement'DropView TODO
  | Statement'Insert InsertStatement
  | Statement'Pragma TODO
  | Statement'Reindex TODO
  | Statement'Release TODO
  | -- | https://sqlite.org/syntax/rollback-stmt.html
    Statement'Rollback (Maybe Text)
  | Statement'Savepoint TODO
  | Statement'Select SelectStatement
  | Statement'Update TODO
  | Statement'Vacuum TODO
  deriving stock (Eq, Generic, Show)

data TODO = TODO
  deriving stock (Eq, Generic, Show)
