-- Heterogeneous sequence example

module Playground.HeterogeneousSequences


import Data.Vect
import Data.HVect

import Fathom.Base
import Fathom.Data.Sing
import Fathom.Format.Record
-- import Fathom.Format.InductiveRecursiveCustom


namespace Format.Pairs

  ||| Construct a format based on a type tag
  value : Nat -> Format
  value 1 = u8
  value 2 = u16Be
  value 4 = u32Be
  value _ = fail


  ||| A heterogeneous sequence of values where the element formats depend on a
  ||| sequence of type tags
  values : (ts : Vect len Nat) -> Format
  values [] = pure ()
  values (t :: ts) = pair (value t) (values ts)


  ||| An annoying example from: https://github.com/yeslogic/fathom/issues/394
  ouch : Format
  ouch = do
    len <- u16Be
    types <- repeat len u16Be
    values <- values types
    pure ()


  ||| Access an element at index @i of the in-memory representation of @values.
  ||| The type of the returned element is dependent on the sequence of type tags.
  index : {ts : Vect len Nat} -> (i : Fin len) -> (values ts).Rep -> (value (index i ts)).Rep
  index {ts = _ :: _} FZ (x, _) = x
  index {ts = _ :: _} (FS i) (_, xs) = Format.index i xs


namespace Format.HRepeat

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
    values <- hrepeat (map value types)
    --        ^^^^^^^ heterogeneous repetitions
    pure ()
