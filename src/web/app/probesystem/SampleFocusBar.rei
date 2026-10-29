/* The bar above the editor showing the call stack of the focused probe
   sample, one breadcrumb per frame, each a jump target. Long stacks are
   shown windowed around the focused frame. */

let view:
  (
    ~globals: Globals.t,
    ~refractors: Haz3lcore.Refractors.t,
    ~info_map: Language.Statics.Map.t,
    ~indicated_id: option(Uuidm.t)
  ) =>
  Virtual_dom.Vdom.Node.t;
