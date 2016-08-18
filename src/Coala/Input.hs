module Coala.Input
    ( Settings
    , emptySettings
    ) where

import qualified Data.Map as Map
import Data.Aeson ( FromJSON )

type Settings = Map.Map String String
emptySettings = Map.empty :: Settings
