module Bcc.DbSync.Plugin.Extended
  ( extendedDbSyncNodePlugin
  ) where

import           Bcc.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Bcc.DbSync.Plugin.Epoch (epochPluginInsertBlock, epochPluginOnStartup,
                   epochPluginRollbackBlock)

import           Bcc.Sync (SyncNodePlugin (..))

import           Database.Persist.Sql (SqlBackend)

extendedDbSyncNodePlugin :: SqlBackend -> SyncNodePlugin
extendedDbSyncNodePlugin backend =
  let defPlugin = defDbSyncNodePlugin backend
  in  defPlugin
        { plugOnStartup =
            plugOnStartup defPlugin
              ++ [epochPluginOnStartup backend]
        , plugInsertBlock =
            plugInsertBlock defPlugin
                ++ [epochPluginInsertBlock backend]
        , plugRollbackBlock =
            plugRollbackBlock defPlugin
              ++ [epochPluginRollbackBlock]
        }
