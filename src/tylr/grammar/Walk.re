open Util;

module Slot = {
  /**
   * cf `Walk`
   */
  type t = {
    cmp: Cmp.t,
    reg: Regex.t(Sort.t),
  };
};

/**
  * A sequence of intermediate consecutive prototiles that are expected
  * to appear between a pair of bounding prototiles (not specified here).
  * A walk acts as a witness to the precedence relationship between its bounds,
  * and its contents specify how to error-correct if the two bounds appear
  * on either side of some bottom-up-accumulated middle term (possibly empty)
  * while parsing.
  * The slot between a pair of consecutive prototiles indicates how the two
  * are precedence-related and what possible edit state content may appear
  * there; this info is used to determine where to place the middle term
  * relative to the error-correction-inserted prototiles.
  */
type t = Chain.t(Slot.t, Proto.t);
