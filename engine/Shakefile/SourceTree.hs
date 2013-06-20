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
  , node
  , empty
  , flags
  , filesWithDeps
  , files
  , list
  , append
  , apply
) where

import Data.Tree (Tree(Node))

-- | A tree with a transformation and a list of files and their dependencies at each node.
type SourceTree a = Tree (a -> a, [(FilePath, [FilePath])])

node :: (a -> a) -> [(FilePath, [FilePath])] -> [SourceTree a] -> SourceTree a
node f fs = Node (f, fs)

empty :: SourceTree a
empty = node id [] []

flags :: (a -> a) -> SourceTree a -> SourceTree a
flags f t = node f [] [t]

filesWithDeps :: [(FilePath, [FilePath])] -> SourceTree a
filesWithDeps fs = node id fs []

files :: [FilePath] -> SourceTree a
files = filesWithDeps . map (flip (,) [])

list :: [SourceTree a] -> SourceTree a
list = node id []

append :: SourceTree a -> SourceTree a -> SourceTree a
append (Node x ts) t = Node x (ts ++ [t])

apply :: a -> SourceTree a -> [(a, (FilePath, [FilePath]))]
apply = go
    where
        flatten a = map ((,)a)
        go a (Node (f, fs) []) = flatten (f a) fs
        go a (Node (f, fs) ns) = let a' = f a
                                 in flatten a' fs ++ concatMap (go a') ns
