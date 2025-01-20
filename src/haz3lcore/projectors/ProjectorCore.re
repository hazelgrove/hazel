open Util;

/* Projector dependencies are currently somewhat convoluted.
 * This is the lowermost projectors module; Base depends on
 * this (specifically, it parametrizes the type t below over piece).
 *
 * ProjectorBase then depends on this and on Base.piece,
 * and also on Vdom, necessitating its inclusion in Core.
 * The individual projector implementations depend on ProjectorBase.
 * Projector then depends on the projector implementations.
 *
 * ProjectorInfo depends on ProjectorBase but not on Projectors
 * (to avoid cyclical dependencies due to MakeTern and ExpToSegment) */

/* The different kinds of projector. New projectors
 * types need to be registered here in order to be
 * able to create and update their instances */
[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | Fold
  | Info
  | Probe
  | Checkbox
  | Slider
  | SliderF
  | TextArea;

/* A projector shape determines the space left for
 * that projector, and how text flows around a projector
 * in a text editor. All projectors have a horizontal
 * extend (in characters), and the vertical extent may
 * be either 1 character (Inline), or it may insert
 * an additional number of linebreaks */
[@deriving (show({with_path: false}), sexp, yojson)]
type vertical =
  | Inline
  | Block(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type shape = {
  horizontal: int,
  vertical,
};

/* Projectors in syntax */
[@deriving (show({with_path: false}), sexp, yojson)]
type t('syntax) = {
  id: Id.t,
  kind,
  syntax: 'syntax,
  model: string,
};

let inline = (width: int): shape => {horizontal: width, vertical: Inline};
let default: shape = inline(0);
