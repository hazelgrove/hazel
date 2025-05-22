module Aba = Aba;
module BonsaiUtil = BonsaiUtil;
module Bigint = BigInt;
module Direction = Direction;
module Either = Either;
module IntMap = IntMap;
module IntUtil = IntUtil;
module ListFrame = ListFrame;
module ListUtil = ListUtil;
module MapUtil = MapUtil;
module Monads = Monads;
module OptUtil = OptUtil;
module PairUtil = PairUtil;
module Result = Result;
module StateMonad = StateMonad;
module StringUtil = StringUtil;
module TimeUtil = TimeUtil;
module TupleUtil = TupleUtil;
module Web = Web;
module JsUtil = JsUtil;
module Key = Key;
module Os = Os;
module Point = Point;
module Calc = Calc;
module Sets = Sets;
module Maps = Maps;

// Used by [@deriving sexp, yojson)]
include Sexplib.Std;
include Ppx_yojson_conv_lib.Yojson_conv.Primitives;

// /* INTERFACE FILE */
// module type EDITOR = {
//   type t;
//   type projector;
//   let use: t => t;
// };

// module type PROJECTOR = {
//   type t;
//   type editor;
//   let mk: editor => t;
// };

// /* PROJECTOR FILE */
// module F =
//        (E: EDITOR)
//        : (PROJECTOR with type editor = E.t and type t = option(E.t)) => {
//   type t = option(E.t);
//   type editor = E.t;
//   let mk = (x: editor) => Some(x);
// };

// /* EDITOR FILE */
// module type CONV = {
//   type editor;
//   type projector;
//   let conv: list(projector) => editor;
// };

// module G =
//        (
//          P: PROJECTOR,
//          C: CONV with type editor = P.editor and type projector = P.t,
//        )
//        : (EDITOR with type projector = P.t and type t = list(P.t)) => {
//   type t = list(P.t);
//   type projector = P.t;

//   let use = (x: t) => {
//     let _: P.t = P.mk(C.conv(x));
//     x;
//   };
// };

// /* TIED KNOT */

// module rec Projector:
//   PROJECTOR with type editor = Editor.t and type t = option(Editor.t) =
//   F(Editor)
// and Editor: EDITOR = G(Projector, Conv)

// and Conv:
//   CONV with type editor = Projector.editor and type projector = Projector.t = {
//   type editor = Projector.editor;
//   type projector = Projector.t;
//   let conv = (x: Editor.t): Projector.editor => x;
// };
