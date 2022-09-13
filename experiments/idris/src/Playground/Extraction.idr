||| Thinking about extracting format descriptions to Rust

module Playground.Extraction


import Fathom.Data.Sing
import Fathom.Format.InductiveRecursiveCustom


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
  compileRep (Choice f1 f2) =
    ?todo_compileRepChoice
  compileRep (Repeat _ f) =
    Just (Rust.Vec !(compileRep f)) -- TODO: Compile to a contract? Or maybe a
                                    --       fixed size array if the length is known
                                    --       or just throw away the info
  compileRep (Tuple fs) =
    ?todo_compileRepTuple
  compileRep (Pure x) =
    ?todo_compileSingRep -- TODO: interpret an Idris type as a Rust type??
                         --       perhaps we need to restrict this?
  compileRep (Pair f1 f2) =
    Just (Rust.Tuple
      [ !(compileRep f1)
      , !(compileRep f2)
      ])
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
  compileDecode (Choice f1 f2) = ?todo_compileDecodeChoice
  compileDecode (Repeat len f) = ?todo_compileDecodeRepeat
  compileDecode (Tuple fs) = ?todo_compileDecodeTuple
  compileDecode (Pair f1 f2) = ?todo_compileDecodePair
  compileDecode (Bind f1 f2) = ?todo_compileDecodeBind
  compileDecode (Custom f) =
    -- TODO: f.rustDecode
    ?todo_compileDecodeCustom


  compileEncode : Format -> (Rust.Module -> Maybe Rust.Module)
  compileEncode End = ?todo_compileEncodeEnd
  compileEncode Fail = ?todo_compileEncodeFail
  compileEncode (Pure x) = ?todo_compileEncodePure
  compileEncode (Ignore f def) = ?todo_compileEncodeIgnore
  compileEncode (Choice f1 f2) = ?todo_compileEncodeChoice
  compileEncode (Repeat len f) = ?todo_compileEncodeRepeat
  compileEncode (Tuple fs) = ?todo_compileEncodeTuple
  compileEncode (Pair f1 f2) = ?todo_compileEncodePair
  compileEncode (Bind f1 f2) = ?todo_compileEncodeBind
  compileEncode (Custom f) =
    -- TODO: f.rustEncode
    ?todo_compileEncodeCustom
