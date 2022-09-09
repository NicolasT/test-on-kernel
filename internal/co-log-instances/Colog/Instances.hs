{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Colog.Instances () where

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)

import Colog (LogAction)
import Colog.Monad (LoggerT(..))

-- The type around which `LoggerT` is a newtype wrapper
type Internal msg m = ReaderT (LogAction (LoggerT msg m) msg) m

deriving via (Internal msg m) instance MonadFail m => MonadFail (LoggerT msg m)
deriving via (Internal msg m) instance MonadThrow m => MonadThrow (LoggerT msg m)
deriving via (Internal msg m) instance MonadCatch m => MonadCatch (LoggerT msg m)
deriving via (Internal msg m) instance MonadMask m => MonadMask (LoggerT msg m)
