||| Reproduction of difficulties in OpenType format

module Playground.OpenType.InductiveRecursive


import Fathom.Data.Sing
import Fathom.Closed.InductiveRecursiveCustom


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


  (.repeat) : Rep Format.flag -> Nat
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


-- Thinking about compilation

namespace Rust

  data RType : Type where
    Var : String -> RType
    U8 : RType
    U16 : RType
    U32 : RType
    U64 : RType
    I8 : RType
    I16 : RType
    I32 : RType
    I64 : RType
    Never : RType
    Tuple : List RType -> RType
    Vec : RType -> RType

  data Item : Type where
    Struct : List (String, RType) -> Item
    Enum : List (String, RType) -> Item
    DecodeFn : () -> Item
    EncodeFn : () -> Item

  record Module where
    constructor MkModule
    items : List (String, Item)


namespace Compile

  -- TODO: Cache compilations of definitions
  --       eg. of structs, enums, endocers and decoders


  compileFormat : Format -> (Rust.Module -> Maybe Rust.Module)
  compileFormat f =
    -- compile rep
    -- compile decode
    -- compile encode
    ?todo_compileFormat


  compileRep : (f : Format) -> Maybe Rust.RType
  compileRep End = Just (Rust.Tuple [])
  compileRep Fail = Just (Rust.Never)
  compileRep (Ignore _ _) = Just (Rust.Tuple [])
  compileRep (Repeat _ f) =
    Just (Rust.Vec !(compileRep f)) -- TODO: Compile to a contract? Or maybe a
                                    --       fixed size array if the length is known
                                    --       or just throw away the info
  compileRep (Pure x) =
    ?todo_compileSingRep -- TODO: interpret an Idris type as a Rust type??
                         --       perhaps we need to restrict this?
  compileRep (Bind f1 f2) =
    Just (Tuple
      [ !(compileRep f1)
      , !(compileRep (f2 ?todo_compileBind_x))  -- TODO: how to bind the output?
                                                --       enum based on the values of `x : Rep f1`?
                                                --       depends on how `x` is used inside `f2`
      ])
  compileRep (Custom f) =
    -- TODO: f.RustRep
    Nothing


  compileDecode : Format -> (Rust.Module -> Maybe Rust.Module)
  compileDecode End = ?todo_compileDecodeEnd
  compileDecode Fail = ?todo_compileDecodeFail
  compileDecode (Pure x) = ?todo_compileDecodePure
  compileDecode (Ignore f _) = ?todo_compileDecodeIgnore
  compileDecode (Repeat len f) = ?todo_compileDecodeRepeat
  compileDecode (Bind f1 f2) = ?todo_compileDecodeBind
  compileDecode (Custom f) =
    -- TODO: f.rustDecode
    ?todo_compileDecodeCustom


  compileEncode : Format -> (Rust.Module -> Maybe Rust.Module)
  compileEncode End = ?todo_compileEncodeEnd
  compileEncode Fail = ?todo_compileEncodeFail
  compileEncode (Pure x) = ?todo_compileEncodePure
  compileEncode (Ignore f def) = ?todo_compileEncodeIgnore
  compileEncode (Repeat len f) = ?todo_compileEncodeRepeat
  compileEncode (Bind f1 f2) = ?todo_compileEncodeBind
  compileEncode (Custom f) =
    -- TODO: f.rustEncode
    ?todo_compileEncodeCustom
