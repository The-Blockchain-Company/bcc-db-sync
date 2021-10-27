import           Hedgehog.Main (defaultMain)

import qualified Test.Property.Godx.Db.Migration
import qualified Test.Property.Godx.Db.Types
import qualified Test.Property.Upstream

main :: IO ()
main =
  defaultMain
    [ Test.Property.Upstream.tests
    , Test.Property.Godx.Db.Migration.tests
    , Test.Property.Godx.Db.Types.tests
    ]
