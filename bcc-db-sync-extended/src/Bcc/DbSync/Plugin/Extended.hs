module Godx.DbSync.Plugin.Extended
  ( extendedDbSyncNodePlugin
  ) where

import           Godx.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Godx.DbSync.Plugin.Epoch (epochPluginInsertBlock, epochPluginOnStartup,
                   epochPluginRollbackBlock)

import           Godx.Sync (SyncNodePlugin (..))

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
