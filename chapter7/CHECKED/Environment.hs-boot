module CHECKED.Environment (Env,TEnv) where

import CHECKED.Val  (DenVal)
import CHECKED.Type (Type)

type Env  = Environment DenVal
type TEnv = Environment Type

data Environment v

