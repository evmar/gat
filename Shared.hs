module Shared where

import Control.Monad.Error

type IOE a = ErrorT String IO a
