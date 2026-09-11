/* The focused-sample bar above the editor. Item layout and the ellipsis
   windowing are private. */

let view:
  (
    ~globals: Globals.t,
    ~refractors: Haz3lcore.Refractors.t,
    ~info_map: Language.Statics.Map.t,
    ~indicated_id: option(Uuidm.t)
  ) =>
  Virtual_dom.Vdom.Node.t;
