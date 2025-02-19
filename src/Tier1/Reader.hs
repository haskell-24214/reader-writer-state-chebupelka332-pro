module Tier1.Reader (cd, su) where

import Control.Monad.Reader
import Tier0.Reader (Environment (..), EnvironmentM)

cd :: String -> EnvironmentM a -> EnvironmentM a
cd dir action = local (\env -> env { currentDir = currentDir env ++ "/" ++ dir }) action


su :: EnvironmentM a -> EnvironmentM a
su = local (\env -> env { isSuperUser = True })
