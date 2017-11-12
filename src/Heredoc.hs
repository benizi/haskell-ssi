module Heredoc where

import Language.Haskell.TH.Lib (stringE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

heredoc = QuasiQuoter stringE undefined undefined undefined :: QuasiQuoter
