{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Print where

import Opaleye
import qualified Database.PostgreSQL.Simple as PGS
import Data.Profunctor.Product.Default (Default)

connectInfo :: PGS.ConnectInfo
connectInfo =  PGS.ConnectInfo { PGS.connectHost     = "212.71.249.246"
                               , PGS.connectPort     = 5432
                               , PGS.connectUser     = "opaleye_guest"
                               , PGS.connectPassword = "opaleye_guest"
                               , PGS.connectDatabase = "opaleye_tutorial" }

printRows :: forall a b proxy.
             (Default QueryRunner a b, Show b)
          => proxy b
          -> Query a
          -> IO ()
printRows _ q = do
  conn <- PGS.connect connectInfo
  rows <- runQuery conn q
  let _ = rows :: [b]
  mapM_ print rows
