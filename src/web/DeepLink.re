open Util;
open OptUtil.Syntax;

module Action = Haz3lcore.Action;
module Sidebar = SidebarModel.Settings;

/* A link that says where to land, read once at load:
 *
 *   ?slide=livelits-builtins       a documentation slide, by name (a pointer)
 *   ?name=Demo&share=<text>        a new scratchpad with this text (a snapshot)
 *   ?panel=probes                  the sidebar panel to open; `none` closes it
 *   ?caret=12,4                    the caret at line 12, column 4
 *   ?select=12,1-14,20             that span selected, instead of a caret
 *
 * Lines and columns are 1-based, as the gutter numbers them. A caret or a
 * selection needs a slide or a share to be in: without one it is ignored.
 * After landing, everything is ordinary state, saved as usual. */

[@deriving (show({with_path: false}), sexp, yojson)]
type target =
  /* A slide in the documentation deck, by name. It follows later edits to
     the slide, in whatever build the link is opened in. */
  | Documentation(string)
  /* A scratchpad made from this text. It shows exactly what was sent. */
  | Scratchpad({
      name: string,
      text: string,
    });

[@deriving (show({with_path: false}), sexp, yojson)]
type position =
  | Caret(Point.t)
  | Select({
      from: Point.t,
      to_: Point.t,
    });

/* A position only means something inside a target, so it lives here and
   nowhere else. */
[@deriving (show({with_path: false}), sexp, yojson)]
type place = {
  target,
  position: option(position),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type panel_request =
  | Closed
  | Open(Sidebar.panel);

[@deriving (show({with_path: false}), sexp, yojson)]
type request = {
  place: option(place),
  panel: option(panel_request),
};

let none: request = {
  place: None,
  panel: None,
};

/* --- Parsing --- */

/* Lowercase letters and digits only, for comparing names loosely:
   `livelits-builtins`, `Livelits / Builtins` and `LivelitsBuiltins` are the
   same slide. */
let loose_name = (s: string): string =>
  String.to_seq(s)
  |> Seq.filter_map(c =>
       switch (Char.lowercase_ascii(c)) {
       | ('a' .. 'z' | '0' .. '9') as c => Some(c)
       | _ => None
       }
     )
  |> String.of_seq;

/* Past the end of any real line. Moving to a column past the end of a line
   lands at its end, so `select=12-14` selects three whole lines. */
let line_end = 10_000;

/* `<line>` or `<line>,<column>`, 1-based. */
let point = (~default_col: int, s: string): option(Point.t) => {
  let zero_based = n => max(n - 1, 0);
  let int = s => int_of_string_opt(String.trim(s));
  switch (String.split_on_char(',', String.trim(s))) {
  | [row] =>
    let+ row = int(row);
    Point.{
      row: zero_based(row),
      col: zero_based(default_col),
    };
  | [row, col] =>
    let* row = int(row);
    let+ col = int(col);
    Point.{
      row: zero_based(row),
      col: zero_based(col),
    };
  | _ => None
  };
};

/* `<point>-<point>`. Without a column, the start is the head of its line and
   the end is the end of its line. */
let select = (s: string): option(position) =>
  switch (String.split_on_char('-', s)) {
  | [from, to_] =>
    let* from = point(~default_col=1, from);
    let+ to_ = point(~default_col=line_end, to_);
    Select({
      from,
      to_,
    });
  | _ => None
  };

/* The request in a query. `lookup` gives a parameter's value; an empty one
   counts as missing. A share wins over a slide, and a selection over a
   caret. */
let parse = (lookup: string => option(string)): request => {
  let param = name =>
    switch (lookup(name)) {
    | Some("")
    | None => None
    | Some(value) => Some(value)
    };
  let target =
    switch (param("share"), param("slide")) {
    | (Some(share), _) =>
      Some(
        Scratchpad({
          name: Option.value(param("name"), ~default="Unknown Share"),
          text: StringUtil.decompress(share),
        }),
      )
    | (None, Some(slide)) => Some(Documentation(slide))
    | (None, None) => None
    };
  let position =
    switch (param("select"), param("caret")) {
    | (Some(s), _) => select(s)
    | (None, Some(c)) =>
      point(~default_col=1, c) |> Option.map(p => Caret(p))
    | (None, None) => None
    };
  let panel =
    switch (param("panel") |> Option.map(loose_name)) {
    | None => None
    | Some("none" | "off" | "closed") => Some(Closed)
    | Some(name) =>
      Sidebar.panel_of_url_name(name) |> Option.map(p => Open(p))
    };
  {
    place:
      target
      |> Option.map(target =>
           {
             target,
             position,
           }
         ),
    panel,
  };
};

/* The request in this page's URL. */
let current = (): request => parse(JsUtil.QueryParams.get_param);

/* --- Applying --- */

/* Which of `names` is `slide`, if any. */
let slide_index = (~slide: string, names: list(string)): option(int) =>
  List.find_index(name => loose_name(name) == loose_name(slide), names);

/* The settings with the request's panel over them. */
let settings =
    (request: request, settings: Settings.Model.t): Settings.Model.t => {
  let sidebar = settings.sidebar;
  let (show, panel) =
    switch (request.panel) {
    | None => (sidebar.show, sidebar.panel)
    | Some(Closed) => (false, sidebar.panel)
    | Some(Open(p)) => (true, p)
    };
  {
    ...settings,
    sidebar: {
      ...sidebar,
      show,
      panel,
    },
  };
};

/* The editor action for the request's position, once the target is open. */
let action = (request: request): option(Action.t) => {
  let* {position, _} = request.place;
  let+ position = position;
  switch (position) {
  | Caret(p) => Action.Move(Action.Point(p, None))
  | Select({from, to_}) => Action.Select(Action.PointToPoint((from, to_)))
  };
};

/* --- Building a link --- */

/* A name as it goes into a link: `Livelits / Builtins` becomes
   `livelits-builtins`, which `loose_name` reads back as the same slide. */
let slug = (s: string): string =>
  String.to_seq(s)
  |> Seq.map(c =>
       switch (Char.lowercase_ascii(c)) {
       | ('a' .. 'z' | '0' .. '9') as c => c
       | _ => '-'
       }
     )
  |> String.of_seq
  |> String.split_on_char('-')
  |> List.filter(part => part != "")
  |> String.concat("-");

/* 1-based, as `point` reads it back. */
let point_param = (p: Point.t): string =>
  string_of_int(p.row + 1) ++ "," ++ string_of_int(p.col + 1);

/* The query for a request. `parse` reads it back as the same request, except
   that a slide's name comes back as its slug. A closed sidebar is written
   out, because a missing `panel` means "leave it as it is". */
let to_params = (request: request): list((string, string)) => {
  let target =
    switch (request.place) {
    | None => []
    | Some({target: Documentation(slide), _}) => [("slide", slug(slide))]
    | Some({target: Scratchpad({name, text}), _}) => [
        ("name", name),
        ("share", StringUtil.compress(text)),
      ]
    };
  let panel =
    switch (request.panel) {
    | None => []
    | Some(Closed) => [("panel", "none")]
    | Some(Open(p)) => [("panel", Sidebar.url_name_of_panel(p))]
    };
  let position =
    switch (request.place) {
    | Some({position: Some(Caret(p)), _}) => [("caret", point_param(p))]
    | Some({position: Some(Select({from, to_})), _}) => [
        ("select", point_param(from) ++ "-" ++ point_param(to_)),
      ]
    | _ => []
    };
  target @ panel @ position;
};

/* This page's address with the request as its query. */
let url = (request: request): string =>
  JsUtil.QueryParams.url_with(to_params(request));

/* A selection's ends, or a caret where they meet. */
let position_of_span = ((from, to_): (Point.t, Point.t)): position =>
  Point.equals(from, to_)
    ? Caret(from)
    : Select({
        from,
        to_,
      });
