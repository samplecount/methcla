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

module Shakefile.SourceTree (
    SourceTree
  , sourceTree
  , sourceTree_
  , sourceFlags
  , sourceFiles
  , sourceFiles_
  , applySourceTree
) where

import Data.Tree (Tree(Node))

-- | A tree with a transformation and a list of files and their dependencies at each node.
type SourceTree a = Tree (a -> a, [(FilePath, [FilePath])])

sourceTree :: (a -> a) -> [(FilePath, [FilePath])] -> [SourceTree a] -> SourceTree a
sourceTree f fs = Node (f, fs)

sourceTree_ :: (a -> a) -> [(FilePath, [FilePath])] -> SourceTree a
sourceTree_ f fs = sourceTree f fs []

sourceFlags :: (a -> a) -> [SourceTree a] -> SourceTree a
sourceFlags f = sourceTree f []

sourceFiles :: [(FilePath, [FilePath])] -> SourceTree a
sourceFiles fs = sourceTree id fs []

sourceFiles_ :: [FilePath] -> SourceTree a
sourceFiles_ = sourceFiles . map (flip (,) [])

applySourceTree :: a -> SourceTree a -> [(a, (FilePath, [FilePath]))]
applySourceTree = go
    where
        flatten a = map ((,)a)
        go a (Node (f, fs) []) = flatten (f a) fs
        go a (Node (f, fs) ns) = let a' = f a
                                 in flatten a' fs ++ concatMap (go a') ns
