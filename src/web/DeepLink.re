open Util;
open OptUtil.Syntax;

module Action = Haz3lcore.Action;
module Sidebar = SidebarModel.Settings;

/* Where a link lands.
 *
 * Hazel opens wherever this browser was last: the mode and the slide come
 * back out of IndexedDB, and so does the sidebar. That is right for someone
 * returning and wrong for someone arriving, who follows a link about one
 * particular thing and lands in the tutorial, or in a scratchpad, or on
 * whichever slide they happened to leave open months ago. These parameters
 * say where to land instead. They are read once, at load.
 *
 *   ?slide=livelits-builtins       a documentation slide, by name
 *   ?panel=probes                  which sidebar panel is open (none: closed)
 *   ?caret=12,4                    the caret, at line 12 column 4
 *   ?select=12,1-14,20             that span selected, instead of a caret
 *   ?outline=ux                    the slide's outline in UX mode (or dev)
 *   ?ux=arm_length,drawn           the rows UX mode shows, by their labels
 *
 * Lines and columns are 1-based, the way the gutter numbers them, and a
 * column past the end of its line lands at the end of the line -- so
 * `select=12-14` selects three whole lines.
 *
 * Names are matched loosely: everything but letters and digits is dropped
 * from both sides, so `livelits-builtins`, `Livelits / Builtins` and
 * `LivelitsBuiltins` all name the same slide. A link can carry a slug
 * instead of a title with spaces and slashes in it, which is what a link
 * someone hand-writes will look like.
 *
 * `url` builds one of these, for the context menu's "Copy URL": the slide, the
 * panel's view state, and the span the reader was pointing at. What the panel
 * is SHOWING is not in the link, and cannot be: it is whatever running the
 * slide's program produces.
 *
 * Nothing here writes to the URL, and nothing here is a mode of its own: a
 * deep link sets where you are, and from then on it is ordinary state, saved
 * like any other. Reloading without the parameters leaves you where the link
 * put you, not where you were before it.
 */

/* Letters and digits, lowercased. Both sides of every name comparison go
   through this, so spacing, case and punctuation cost nothing. */
let key = (s: string): string =>
  String.to_seq(s)
  |> Seq.filter_map(c =>
       switch (Char.lowercase_ascii(c)) {
       | 'a' .. 'z' as c
       | '0' .. '9' as c => Some(c)
       | _ => None
       }
     )
  |> String.of_seq;

/* An empty parameter is the same as none: `?slide=` in a hand-written link
   should not override anything. */
let param = (name: string): option(string) =>
  switch (JsUtil.QueryParams.get_param(name)) {
  | Some("")
  | None => None
  | Some(value) => Some(value)
  };

/* Pick the variant a parameter names, comparing against the constructor
   names themselves: a panel or a view added later is linkable without
   anything here changing. `aliases` shortens the ones whose constructor name
   is not what a reader would write. */
let variant =
    (
      ~all: list('a),
      ~show: 'a => string,
      ~aliases: list((string, 'a))=[],
      name: string,
    )
    : option('a) => {
  let* wanted = param(name) |> Option.map(key);
  switch (List.find_opt(((alias, _)) => key(alias) == wanted, aliases)) {
  | Some((_, value)) => Some(value)
  | None => List.find_opt(value => key(show(value)) == wanted, all)
  };
};

/* --- The slide --- */

let slide = (): option(string) => param("slide");

/* Which of `names` the `?slide=` names, if any. Matching against the deck
   as loaded -- not against the shipped list -- is what makes this survive a
   slide being added, removed or renamed in a browser that has been here
   before. */
let slide_index = (names: list(string)): option(int) => {
  let* wanted = slide() |> Option.map(key);
  names
  |> List.mapi((i, name) => (i, key(name)))
  |> List.find_opt(((_, name)) => name == wanted)
  |> Option.map(fst);
};

/* --- The sidebar --- */

/* The shorter spellings, and the only place they are written down: `url`
   reads this backwards to pick what to put in a link. */
let panel_aliases =
  Sidebar.[
    ("docs", LanguageDocumentation),
    ("assistant", HelpfulAssistant),
    ("log", LogControl),
    ("task", TaskReference),
    ("debug", DebugInfo),
  ];

/* A closed sidebar is a view state like any other, so a link can ask for it:
   `panel=none`. Without this a copied link could only ever open something. */
type panel_request =
  | Closed
  | Open(Sidebar.panel);

let panel = (): option(panel_request) =>
  switch (param("panel") |> Option.map(key)) {
  | None => None
  | Some("none" | "off" | "closed") => Some(Closed)
  | Some(_) =>
    variant(
      ~all=Sidebar.all_of_panel,
      ~show=Sidebar.show_panel,
      ~aliases=panel_aliases @ [("documentation", LanguageDocumentation)],
      "panel",
    )
    |> Option.map(p => Open(p))
  };

/* The settings a link asks for, over the ones that were stored. */
let settings = (settings: Settings.Model.t): Settings.Model.t => {
  let sidebar = settings.sidebar;
  let (show, panel) =
    switch (panel()) {
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

/* --- The caret --- */

/* Past the end of any line anyone writes. `Move.to_point` walks towards its
   goal and stops when the row runs out, so a column this large lands at the
   end of the line -- which is what a range given as whole lines means. */
let line_end = 10_000;

/* `<line>` or `<line>,<column>`, 1-based, as the gutter numbers them. */
let point = (~default_col: int, s: string): option(Point.t) => {
  let clamp = n => max(n - 1, 0);
  switch (String.split_on_char(',', String.trim(s))) {
  | [row] =>
    let+ row = int_of_string_opt(String.trim(row));
    Point.{
      row: clamp(row),
      col: clamp(default_col),
    };
  | [row, col] =>
    let* row = int_of_string_opt(String.trim(row));
    let+ col = int_of_string_opt(String.trim(col));
    Point.{
      row: clamp(row),
      col: clamp(col),
    };
  | _ => None
  };
};

/* `<point>-<point>`: the first point without a column starts at the head of
   its line, the second without one runs to the end of its line. */
let span = (s: string): option((Point.t, Point.t)) =>
  switch (String.split_on_char('-', s)) {
  | [from_, to_] =>
    let* from_ = point(~default_col=1, from_);
    let+ to_ = point(~default_col=line_end, to_);
    (from_, to_);
  | _ => None
  };

/* What the link asks the editor to do once there is an editor to ask. A
   selection wins over a caret: the two answer the same question, and saying
   both is a mistake worth resolving the more specific way. */
let action = (): option(Action.t) =>
  switch (param("select"), param("caret")) {
  | (Some(select), _) =>
    span(select)
    |> Option.map(points => Action.Select(Action.PointToPoint(points)))
  | (None, Some(caret)) =>
    point(~default_col=1, caret)
    |> Option.map(p => Action.Move(Action.Point(p, None)))
  | (None, None) => None
  };

/* --- The outline --- */

/* `?outline=` picks the linked slide's outline mode, and `?ux=` the rows
   UX mode shows (the livelits' GUIs to start), by their labels: matched
   loosely, like slide names, at any depth, the first row in outline order
   winning. A label naming no row is dropped, as a stale slide name is.
   Either alone is fine: `?ux=` from dev mode sets what UX mode will show.

   What this is for is a script reaching a slide's UX view with chosen rows
   without clicking through the outline (the performance series' "UX mode,
   the slider and the picture"), and a reader following a link to the art
   rather than the code. Like the rest, it applies once, to the linked slide,
   over whatever view that slide had saved, and is then ordinary state. */
type outline_request = {
  /* Some(true): UX mode; Some(false): dev mode */
  ux: option(bool),
  /* the rows' labels, through `key` */
  ux_rows: option(list(string)),
};

let outline_request = (): option(outline_request) => {
  let ux =
    switch (param("outline") |> Option.map(key)) {
    | Some("ux") => Some(true)
    | Some("dev") => Some(false)
    | _ => None
    };
  let ux_rows =
    param("ux")
    |> Option.map(s =>
         String.split_on_char(',', s)
         |> List.map(key)
         |> List.filter(k => k != "")
       );
  ux == None && ux_rows == None
    ? None
    : Some({
        ux,
        ux_rows,
      });
};

/* Applied by the first restore of the linked slide's view, and never
   again: after that a reload keeps what the reader made of it. (The
   view itself is resolved in Workspace: this module sits below the
   slide views, as the editors' "Copy URL" uses it.) */
let outline_applied = ref(false);

/* The linked slide's key, as `key` reads `doc:<name>`, and its request:
   read once, since every calculation asks until it is applied. */
let linked_outline: Lazy.t(option((string, outline_request))) =
  lazy(
    switch (slide()) {
    | Some(name) =>
      outline_request() |> Option.map(r => (key("doc") ++ key(name), r))
    | None => None
    }
  );

/* The request for the slide stored under [slide_key] (`doc:<name>`), if it
   is the linked one and nothing has applied it yet. */
let outline_for = (slide_key: string): option(outline_request) =>
  switch (outline_applied^, Lazy.force(linked_outline)) {
  | (false, Some((linked, r))) when key(slide_key) == linked => Some(r)
  | _ => None
  };

/* --- Building one --- */

/* A name as it goes into a link: `Livelits / Builtins` becomes
   `livelits-builtins`, which `key` reads back as the same thing. The
   percent-encoded title would work too and is unreadable. */
let slug = (s: string): string => {
  let out = Buffer.create(String.length(s));
  String.iter(
    c =>
      switch (Char.lowercase_ascii(c)) {
      | ('a' .. 'z' | '0' .. '9') as c => Buffer.add_char(out, c)
      | _ =>
        if (Buffer.length(out) > 0
            && Buffer.nth(out, Buffer.length(out) - 1) != '-') {
          Buffer.add_char(out, '-');
        }
      },
    s,
  );
  let out = Buffer.contents(out);
  /* A trailing separator comes from trailing punctuation, and says nothing. */
  String.length(out) > 0 && out.[String.length(out) - 1] == '-'
    ? String.sub(out, 0, String.length(out) - 1) : out;
};

/* The shortest spelling that reads back as this panel. */
let panel_key = (p: Sidebar.panel): string =>
  switch (List.find_opt(((_, q)) => q == p, panel_aliases)) {
  | Some((alias, _)) => alias
  | None => slug(Sidebar.show_panel(p))
  };

/* 1-based, as the gutter numbers them and as `point` reads them back. */
let point_key = (p: Point.t): string =>
  string_of_int(p.row + 1) ++ "," ++ string_of_int(p.col + 1);

/* The link that lands someone where this reader is: the slide, the panel's
   view state, and the span they were pointing at. A closed sidebar is stated
   rather than omitted, since a missing parameter means "leave it alone" and
   would hand the follower whatever panel they happened to have open. */
let url =
    (
      ~slide: option(string),
      ~sidebar: Sidebar.t,
      ~span: option((Point.t, Point.t)),
    )
    : string => {
  let slide =
    switch (slide) {
    | None => []
    | Some(name) => [("slide", slug(name))]
    };
  let panel =
    sidebar.show
      ? [("panel", panel_key(sidebar.panel))] : [("panel", "none")];
  let span =
    switch (span) {
    | None => []
    | Some((from_, to_)) when Point.equals(from_, to_) => [
        ("caret", point_key(from_)),
      ]
    | Some((from_, to_)) => [
        ("select", point_key(from_) ++ "-" ++ point_key(to_)),
      ]
    };
  JsUtil.QueryParams.url_with(slide @ panel @ span);
};
