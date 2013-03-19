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

{-# LANGUAGE Rank2Types #-}

module Shakefile.Lens (
    append
  , prepend
) where

import Control.Lens
import Data.Monoid (Monoid, mappend)

append :: forall s t a.
        Monoid a =>
        Setting (->) s t a a -> a -> s -> t
append l n = over l (`mappend` n)

prepend :: forall s t a.
         Monoid a =>
         Setting (->) s t a a -> a -> s -> t
prepend l n = over l (n `mappend`)


