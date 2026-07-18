open Util;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

[@deriving (show({with_path: false}), sexp, yojson)]
type diff = {
  old_segment: Segment.t,
  new_segment: option(Segment.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tool_result = {
  tool_call: OpenRouter.Reply.Model.tool_call,
  success: bool,
  expanded: bool,
  diff: option(diff),
  before_segment: option(Segment.t),
  after_segment: option(Segment.t),
  /* Indicated (cursor) node ids at snapshot time, so replay/timeline can
     restore the caret position along with the program state. */
  [@yojson.default None]
  before_cursor_id: option(Id.t),
  [@yojson.default None]
  after_cursor_id: option(Id.t),
  content: string,
};
