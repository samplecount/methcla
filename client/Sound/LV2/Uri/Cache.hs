-- Copyright 2012-2013 Samplecount S.L.
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

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
