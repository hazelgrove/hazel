open Util_web;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

[@deriving (show({with_path: false}), sexp, yojson)]
type diff = {
  old_segment: Segment.t,
  new_segment: option(Segment.t),
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
  before_segment: option(Segment.t),
  after_segment: option(Segment.t),
  content: string,
};

let mk_skipped = (tool_call: OpenRouter.Reply.Model.tool_call): tool_result => {
  tool_call,
  success: false,
  skipped: true,
  expanded: false,
  diff: None,
  before_segment: None,
  after_segment: None,
  content: skipped_due_to_prior_failure_message,
};
