open Util;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

/* Legacy saved tool results only need their segment fields translated.
   Keep this local to the changed record shapes; no versioned store migration. */
let snapshot_text = (segment: Segment.t): string =>
  PersistentZipper.to_string(Zipper.unzip(segment)) ++ "\n";
let migrate_snapshots = (fields, sexp) => {
  open Sexplib.Sexp;
  let text = value => Atom(snapshot_text(Segment.t_of_sexp(value)));
  switch (sexp) {
  | List(xs) =>
    List(
      List.map(
        field =>
          switch (field) {
          | List([Atom(name), value]) =>
            switch (
              List.find_opt(((old_name, _, _)) => old_name == name, fields)
            ) {
            | Some((_, name, optional)) =>
              let value =
                optional
                  ? switch (value) {
                    | List(values) => List(List.map(text, values))
                    | _ => value
                    }
                  : text(value);
              List([Atom(name), value]);
            | None => field
            }
          | _ => field
          },
        xs,
      ),
    )
  | _ => sexp
  };
};

/* the edited region before and after, as printed program text. As
   Segment.t (ids, molds, shapes per piece) an insert's whole-program diff
   was ~0.8 MB per side in the persisted conversation — 97% of a 17 MB
   autosave by the end of a 28-tool run. */
[@deriving (show({with_path: false}), sexp, yojson)]
type diff = {
  old_text: string,
  new_text: option(string),
};

let diff_of_sexp = sexp =>
  diff_of_sexp(
    migrate_snapshots(
      [
        ("old_segment", "old_text", false),
        ("new_segment", "new_text", true),
      ],
      sexp,
    ),
  );

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
  [@yojson.default false] [@sexp.default false]
  content_is_payload: bool,
};

let tool_result_of_sexp = sexp =>
  tool_result_of_sexp(
    migrate_snapshots(
      [
        ("before_segment", "before_text", true),
        ("after_segment", "after_text", true),
      ],
      sexp,
    ),
  );

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

/* Diffs can be binding fragments or module members, not complete programs.
   Avoid adding a terminal hole or an `in` while reconstructing the preview. */
let segment_of_diff_text = (text: string): Segment.t => {
  let parse = root =>
    FastParse.of_text(~materialize=Triggers.invoked_projector, ~root, text);
  switch (parse(Sort.Exp)) {
  | Some(segment) => segment
  | None =>
    switch (parse(Sort.Mod)) {
    | Some(segment) => segment
    | None => segment_of_text(text ++ "\n")
    }
  };
};
