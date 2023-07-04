open Sexplib.Std;
open Haz3lcore;

/* MODEL.re

   The model consists of three broad categories. EDITORS is the 'meat',
   containing the code content and cursor/selection/buffer state for all
   active editors. SETTINGS are user-selectable preferences. Together,
   these two comprise the 'persistant' state of the application which is
   saved to localstore.

   META on the other hand consists of everything which is not
   peristant, including transitory UI_STATE such as whether the mouse
   is held down, and cached evaluation RESULTS.

    */

[@deriving (show({with_path: false}), yojson, sexp)]
type timestamp = float;

/* Non-persistant UI state */
[@deriving (show({with_path: false}), yojson, sexp)]
type ui_state = {
  font_metrics: FontMetrics.t,
  show_backpack_targets: bool,
  double_tap: option(timestamp),
  mousedown: bool,
};

let ui_state_init = {
  font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
  mousedown: false,
};

/* Non-persistant application state */
[@deriving (show({with_path: false}), yojson, sexp)]
type meta = {
  ui_state,
  results: ModelResults.t,
  auto: UpdateAction.auto_llm,
  mvu_states: VarMap.t_(DHExp.t),
};

let meta_init = {
  ui_state: ui_state_init,
  results: ModelResults.empty,
  mvu_states: VarMap.empty,
  auto: Auto.init,
};

type t = {
  editors: Editors.t,
  settings: ModelSettings.t,
  langDocMessages: LangDocMessages.t,
  meta,
};

let cutoff = (===);

let mk = editors => {
  editors,
  settings: ModelSettings.init,
  langDocMessages: LangDocMessages.init,
  meta: meta_init,
};

let blank = mk(Editors.Scratch(0, []));
let debug = mk(Editors.DebugLoad);
