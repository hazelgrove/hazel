/* Public facade for the refactoring subsystem. The transforms live in
 * RefactorBase/Inline/Move/Reduce/Lift; the registry in
 * RefactorRegistry; the three input front-ends in RefactorMenu,
 * RefactorGesture, and RefactorDrag. This module just re-exports the
 * names the rest of the app (web, tests) reaches for, so call sites
 * keep saying `Refactor.x`. */
open Language;

/* term-surgery odds and ends the tests poke at directly */
let roundtrip_settings = RefactorBase.roundtrip_settings;
let reparses_same = RefactorBase.reparses_same;
let dedupe_healed = RefactorBase.dedupe_healed;
let eq_defs = RefactorBase.eq_defs;
type impl =
  RefactorBase.impl = {
    label: string,
    tooltip: string,
    prepare:
      (~info_map: Statics.Map.t, ~target: Id.t, Exp.t) =>
      option((Exp.t, Id.t)),
  };

/* registry / run */
let impl = RefactorRegistry.impl;
let all = RefactorRegistry.all;
let applies = RefactorRegistry.applies;
let go = RefactorRegistry.go;

/* menu front-end */
let menu_items = RefactorMenu.menu_items;

/* keyboard-gesture front-end */
let gesture = RefactorGesture.gesture;
let gesture_insist = RefactorGesture.gesture_insist;
let gesture_blockers = RefactorGesture.gesture_blockers;

/* drag front-end */
module DragCandidate = RefactorDrag.DragCandidate;
let drag_candidates = RefactorDrag.drag_candidates;
let gesture_merge_target = RefactorDrag.gesture_merge_target;
let refactor_merge_target = RefactorDrag.refactor_merge_target;
let gesture_emerge_source = RefactorDrag.gesture_emerge_source;
let refactor_emerge_source = RefactorDrag.refactor_emerge_source;
