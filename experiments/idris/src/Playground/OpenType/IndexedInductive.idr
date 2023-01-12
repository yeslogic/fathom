||| Reproduction of difficulties in OpenType format

module Playground.OpenType.IndexedInductive


import Fathom.Data.Sing
import Fathom.Format.IndexedInductiveCustom


namespace FormatOf

  Flag : Type
  Flag =
    (  id : Nat
    ** repeat :
      case id of
        0 => Nat
        S n => Sing {A = Nat} 0
    ** Sing ()
    )


  (.repeat) : Flag -> Nat
  (.repeat) (0 ** repeat ** _) = repeat
  (.repeat) (S _ ** repeat ** _) = val repeat


  -- def flag = {
  --     flag <- u8,
  --     repeat <- match (flag & 8 != (0 : U8)) {
  --       true => u8,
  --       false => succeed U8 0,
  --     },
  -- };
  flag : FormatOf Flag
  flag = do
    flag <- u8
    repeat <- case flag of
      0 => u8
      S _ => Pure {A = Nat} 0
    Pure ()


  SimpleGlyph : Type
  SimpleGlyph =
    (  flag : Flag
    ** Sing (flag.repeat + 1)
    )


  -- def simple_glyph = fun (number_of_contours : U16) => {
  --     ...
  --     let flag_repeat = fun (f : Repr flag) => f.repeat + (1 : U8),
  --     ...
  -- };
  simple_glyph : FormatOf SimpleGlyph
  simple_glyph = do
    flag <- flag
    Pure (flag.repeat + 1)


namespace Format

  -- Reproduction of difficulties in OpenType format

  -- def flag = {
  --     flag <- u8,
  --     repeat <- match (flag & 8 != (0 : U8)) {
  --       true => u8,
  --       false => succeed U8 0,
  --     },
  -- };
  flag : Format
  flag = Format.do
    id <- toFormat u8
    repeat <- case id of
      0 => toFormat u8
      S n => pure {A = Nat} 0
    pure ()


  -- def simple_glyph = fun (number_of_contours : U16) => {
  --     ...
  --     let flag_repeat = fun (f : Repr flag) => f.repeat + (1 : U8),
  --     ...
  -- };
  simple_glyph : Format
  simple_glyph = Format.do
    flag <- flag
    let
      repeat : Nat
      repeat = ?todo_repeat
      -- repeat = case the (Format.Rep flag) flag of
      --   (0 ** repeat ** MkSing ()) => repeat
      --   (S n ** repeat ** MkSing ()) => repeat

      -- Error: While processing right hand side of simple_glyph. While processing right hand side
      -- of simple_glyph,repeat. Can't match on 0 as it must have a polymorphic type.
    pure (repeat + 1)
