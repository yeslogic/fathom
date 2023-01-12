||| Binary digits

module Fathom.Data.Bit


||| A binary digit
public export
data Bit : Type where
  B0 : Bit
  B1 : Bit


public export
Cast Bit Bool where
  cast B0 = True
  cast B1 = False

public export
Cast Bit Nat where
  cast B0 = 0
  cast B1 = 1

public export
Cast Bit Int where
  cast B0 = 0
  cast B1 = 1

public export
Cast Bit Integer where
  cast B0 = 0
  cast B1 = 1

public export
Cast Bit Bits8 where
  cast B0 = 0
  cast B1 = 1

public export
Cast Bit Bits16 where
  cast B0 = 0
  cast B1 = 1

public export
Cast Bit Bits32 where
  cast B0 = 0
  cast B1 = 1

public export
Cast Bit Bits64 where
  cast B0 = 0
  cast B1 = 1

public export
Cast Bit Int8 where
  cast B0 = 0
  cast B1 = 1

public export
Cast Bit Int16 where
  cast B0 = 0
  cast B1 = 1

public export
Cast Bit Int32 where
  cast B0 = 0
  cast B1 = 1

public export
Cast Bit Int64 where
  cast B0 = 0
  cast B1 = 1
