module Sound.LV2.Atom.Env where

import Control.Monad.Trans.State as S
import qualified Sound.LV2.Uri as Uri

type Env m a = S.StateT (Uri.Map m) m a
