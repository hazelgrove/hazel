let start: UHExp.t => unit;

/*
 Public read only mutable field.

 This field is mutable, but its mutation is strictly limited.
 It is only mutated by "callback" in Synthesizer.r to make it
 reflect the latest synthesis results immidiatly upon the
 synthesis worker's completion, and before the display is updated.
 Anywhere may view its contents but nowhere else may edit it.
 */
/* let fillings: ref(IntMap.t(UHExp.operand)); */

/*
  Public write once mutable field.
 */
let schedule_action: ref(unit => unit);
