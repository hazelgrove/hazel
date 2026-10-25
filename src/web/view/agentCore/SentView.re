open Util;
open Haz3lcore;

/* What each request showed the agent: the context message exactly as
   sent, the open set, and the program it was made from (probes are in
   its text), so a past turn can be viewed as the agent saw it. */

/* the program a request was made from: its text, or the text another
   record or a tool result already holds */
[@deriving (show({with_path: false}), sexp, yojson)]
type program =
  | Text(string)
  /* the previous record's */
  | Unchanged
  /* a tool result message's after_text */
  | After(Id.t)
  /* dropped for size */
  | Dropped;

/* a context as built, before it is sent */
[@deriving (show({with_path: false}), sexp, yojson)]
type basis = {
  b_root: Sort.t,
  b_text: string,
  b_opened: list(AgentContext.Model.entry),
  b_probes: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  seq: int,
  time: float,
  /* the transcript's last message when it was sent: the request answers
     it */
  after: Id.t,
  view: string,
  opened: list(AgentContext.Model.entry),
  root: Sort.t,
  program,
  probes: int,
};

/* the program's text, as the editor's lossless format */
let text_of_zipper = (z: Zipper.t): string =>
  PersistentZipper.to_string(z) ++ "\n";

/* records kept per chat, and the characters their views and program
   texts may take */
let max_records = 200;
let max_chars = 4_000_000;

let program_chars = (r: t): int =>
  switch (r.program) {
  | Text(s) => String.length(s)
  | Unchanged
  | After(_)
  | Dropped => 0
  };
let chars = (r: t): int => String.length(r.view) + program_chars(r);

/* [r]'s program text: [after_text] reads a tool result's, [earlier] are
   the records before [r], latest first */
let rec text =
        (~after_text: Id.t => option(string), ~earlier: list(t), r: t)
        : option(string) =>
  switch (r.program, earlier) {
  | (Text(s), _) => Some(s)
  | (After(id), _) => after_text(id)
  | (Unchanged, [prev, ...rest]) => text(~after_text, ~earlier=rest, prev)
  | (Unchanged, [])
  | (Dropped, _) => None
  };

/* [records] (oldest first) with [r] added, the program stored by
   reference where it can be, and the oldest dropped past the bounds */
let add =
    (
      ~after_text: Id.t => option(string),
      ~last_tool: option(Id.t),
      records: list(t),
      b: basis,
      r: t,
    )
    : list(t) => {
  let latest_first = List.rev(records);
  let program =
    switch (latest_first) {
    | [prev, ...earlier]
        when text(~after_text, ~earlier, prev) == Some(b.b_text) =>
      Unchanged
    | _ =>
      switch (last_tool) {
      | Some(id) when after_text(id) == Some(b.b_text) => After(id)
      | _ => Text(b.b_text)
      }
    };
  let records =
    records
    @ [
      {
        ...r,
        program,
      },
    ];
  /* drop from the front; the record after a dropped one takes its text
     if it read it */
  let total = rs => List.fold_left((n, r) => n + chars(r), 0, rs);
  let rec trim = (rs: list(t)) =>
    switch (rs) {
    | [old, next, ...rest]
        when List.length(rs) > max_records || total(rs) > max_chars =>
      let next =
        next.program == Unchanged
          ? {
            ...next,
            program:
              switch (text(~after_text, ~earlier=[], old)) {
              | Some(s) => Text(s)
              | None => Dropped
              },
          }
          : next;
      trim([next, ...rest]);
    | _ => rs
    };
  trim(records);
};
