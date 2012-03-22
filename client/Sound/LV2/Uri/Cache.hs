{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.LV2.Uri.Cache where

import           Control.Applicative (Applicative)
import           Control.Monad (MonadPlus)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.HashMap.Strict as H
import           Sound.LV2.Uri as Uri

newtype CacheT m a = CacheT (S.StateT (H.HashMap Uri Urid) m a)
                        deriving (Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance Uri.Map m => Uri.Map (CacheT m) where
    map = lift . Uri.map

instance Uri.Unmap m => Uri.Unmap (CacheT m) where
    unmap = lift . Uri.unmap
