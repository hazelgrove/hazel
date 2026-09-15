open Util;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

/* the edited region before and after, as printed program text. As
   Segment.t (ids, molds, shapes per piece) an insert's whole-program diff
   was ~0.8 MB per side in the persisted conversation — 97% of a 17 MB
   autosave by the end of a 28-tool run. */
[@deriving (show({with_path: false}), sexp, yojson)]
type diff = {
  old_text: string,
  new_text: option(string),
};

let skipped_due_to_prior_failure_message = "This tool was not executed because an earlier tool call in the same assistant turn failed.";

[@deriving (show({with_path: false}), sexp, yojson)]
type tool_result = {
  tool_call: OpenRouter.Reply.Model.tool_call,
  success: bool,
  [@yojson.default false]
  skipped: bool,
  expanded: bool,
  diff: option(diff),
  /* whole-program snapshots around the tool, as TEXT (the lossless
     slide format): a Segment.t per snapshot made the conversation's
     autosave re-serialize every program version of the run — 1.8 s on
     the main thread per tick by the end of a 28-tool run */
  before_text: option(string),
  after_text: option(string),
  content: string,
  /* When set, `content` IS the model-facing result payload (e.g. a
     read_docs guide) rather than a UI-side note; the api message carries
     it verbatim instead of the generic success acknowledgment. */
  [@yojson.default false]
  content_is_payload: bool,
};

let mk_skipped = (tool_call: OpenRouter.Reply.Model.tool_call): tool_result => {
  tool_call,
  success: false,
  skipped: true,
  expanded: false,
  diff: None,
  before_text: None,
  after_text: None,
  content: skipped_due_to_prior_failure_message,
  content_is_payload: false,
};

/* a snapshot back into syntax (timeline navigation) */
let segment_of_text = (text: string): Segment.t =>
  Zipper.unselect_and_zip(
    ~erase_buffer=true,
    PersistentZipper.from_backup_text(text, ~root=Sort.Exp),
  );
