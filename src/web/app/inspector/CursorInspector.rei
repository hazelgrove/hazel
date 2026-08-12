/* Cursor inspector. `view` is the bar Page renders; the four sort-specific
   views are reused by ProblemSidebar. Everything that builds their contents
   is private. */

let exp_view:
  (
    ~globals: Globals.t,
    ~show_type_colon: bool=?,
    Language.Cls.t,
    Language.Message.t,
    Language.Info.exp
  ) =>
  Virtual_dom.Vdom.Node.t;

let pat_view:
  (
    ~globals: Globals.t,
    ~show_type_colon: bool=?,
    Language.Cls.t,
    Language.Message.t,
    Language.Info.pat
  ) =>
  Virtual_dom.Vdom.Node.t;

let typ_view:
  (
    ~globals: Globals.t,
    Language.Cls.t,
    ~marks: list(Language.Mark.t),
    ~message: option(Language.Message.t)
  ) =>
  Virtual_dom.Vdom.Node.t;

let tpat_view:
  (
    ~globals: Globals.t,
    Language.Cls.t,
    ~marks: list(Language.Mark.t),
    ~message: option(Language.Message.t)
  ) =>
  Virtual_dom.Vdom.Node.t;

let view:
  (~globals: Globals.t, Cursor.cursor(Editors.Update.t)) =>
  Virtual_dom.Vdom.Node.t;
