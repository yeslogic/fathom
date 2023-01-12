||| Reproduction of difficulties in OpenType format

module Playground.OpenType.InductiveRecursive


import Fathom.Data.Sing
import Fathom.Format.InductiveRecursiveCustom


namespace Format

  -- def flag = {
  --     flag <- u8,
  --     repeat <- match (flag & 8 != (0 : U8)) {
  --       true => u8,
  --       false => succeed U8 0,
  --     },
  -- };
  flag : Format
  flag = do
    id <- u8
    repeat <- case id of
      0 => u8
      S _ => Pure {A = Nat} 0
    Pure ()


  (.repeat) : Format.flag.Rep -> Nat
  (.repeat) (0 ** repeat ** _) = repeat
  (.repeat) (S _ ** repeat ** _) = val repeat


  -- def simple_glyph = fun (number_of_contours : U16) => {
  --     ...
  --     let flag_repeat = fun (f : Repr flag) => f.repeat + (1 : U8),
  --     ...
  -- };
  simple_glyph : Format
  simple_glyph = do
    flag <- flag
    Pure (flag.repeat + 1)


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
  flag = FormatOf.do
    flag <- toFormatOf u8
    repeat <- case flag of
      0 => toFormatOf u8
      S _ => pure {A = Nat} 0
    pure ()


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
  simple_glyph = FormatOf.do
    flag <- flag
    pure (flag.repeat + 1)
