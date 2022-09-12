-- Heterogeneous sequence example

module Playground.HeterogeneousSequences


import Data.Vect
import Data.HVect

import Fathom.Base
import Fathom.Data.Sing
import Fathom.Format.Record


namespace Format

  ||| Construct a format based on a type tag
  value : Nat -> Format
  value 1 = u8
  value 2 = u16Be
  value 4 = u32Be
  value _ = fail


  ||| An annoying example from: https://github.com/yeslogic/fathom/issues/394
  ouch : Format
  ouch = do
    len <- u16Be
    types <- repeat len u16Be
    values <- tuple (map value types)
    --        ^^^^^ heterogeneous sequence of formats
    pure ()
