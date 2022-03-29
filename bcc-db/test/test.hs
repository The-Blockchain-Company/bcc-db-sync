import           Hedgehog.Main (defaultMain)

import qualified Test.Property.Bcc.Db.Migration
import qualified Test.Property.Bcc.Db.Types
import qualified Test.Property.Upstream

main :: IO ()
main =
  defaultMain
    [ Test.Property.Upstream.tests
    , Test.Property.Bcc.Db.Migration.tests
    , Test.Property.Bcc.Db.Types.tests
    ]
