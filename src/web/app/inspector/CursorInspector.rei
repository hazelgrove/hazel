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
    ~ctx: Language.Ctx.t,
    Language.Cls.t,
    ~marks: list(Language.Mark.t),
    ~message: option(Language.Message.t)
  ) =>
  Virtual_dom.Vdom.Node.t;

let tpat_view:
  (
    ~globals: Globals.t,
    ~ctx: Language.Ctx.t,
    Language.Cls.t,
    ~marks: list(Language.Mark.t),
    ~message: option(Language.Message.t)
  ) =>
  Virtual_dom.Vdom.Node.t;

let view:
  (
    ~globals: Globals.t,
    ~dynamics: Virtual_dom.Vdom.Node.t=?,
    Cursor.cursor(Editors.Update.t)
  ) =>
  Virtual_dom.Vdom.Node.t;

/** The info the bar describes: the implied hole's when quiver is on, else
    the cursor's. Exported for Test_ImpliedHole. */
let info_for_view:
  (~quiver: bool, Cursor.cursor('action)) => option(Language.Info.t);

/* Added on the livelits branch (#2635), before this interface existed. */
let code_view_settings: Haz3lcore.ExpToSegment.Settings.t;
let bar_toggle: ref(option(Virtual_dom.Vdom.Node.t));
type free_type_hint =
  | ModuleTypes(list(string))
  | MovedTo(string);
/* why a type name is free here: types a module holds, or where it moved */
let free_type_hint: (Language.Ctx.t, string) => option(free_type_hint);
let expanded_view:
  (~globals: Globals.t, Cursor.cursor(Editors.Update.t)) =>
  Virtual_dom.Vdom.Node.t;
