/* The cursor inspector: the bar describing the term at the cursor -- its
   class, type and any error. Each sort-specific view renders that
   description for a given info on its own. */

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

let mod_view:
  (
    ~globals: Globals.t,
    Language.Cls.t,
    ~marks: list(Language.Mark.t),
    ~ty: option(Language.Typ.t)
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

/** The info the bar describes: the implied hole's when quiver is on, else
    the cursor's. Exported for Test_ImpliedHole. */
let info_for_view:
  (~quiver: bool, Cursor.cursor('action)) => option(Language.Info.t);
