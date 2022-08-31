module Fathom.Data.Word


import Data.Fin

import Fathom.Data.Bit


||| A binary word with a specified number of bits
public export
data Word : (0 size : Nat) -> Type where
  Z : Word 0
  S : Bit -> {0 n : Nat} -> Word n -> Word (S n)


export
Cast (Word size) Nat where
  cast Z = 0
  cast (S B0 w) = cast w * 2
  cast (S B1 w) = S (cast w * 2)

export
Cast (Word size) Bits8 where
  cast Z = 0
  cast (S B0 w) = cast w * 2
  cast (S B1 w) = (cast w * 2) + 1
