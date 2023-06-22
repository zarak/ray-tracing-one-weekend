{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module NFUtils where

import Color
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Vec3

deriving instance Generic Color

deriving instance Generic Vec3

deriving instance NFData Color

deriving instance NFData Vec3
