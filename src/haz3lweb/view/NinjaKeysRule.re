open Haz3lcore;
open Js_of_ocaml;
open Util;

let action_refresh = ref(() => ());
let current_hover_rule = ref(Rule.Assumption);

let ( let* ) = Js.Opt.case(_, () => Js._false);

// Wrap a function (to be called by Js setInterval) in a loop call.
// Clear the interval if the function returns true.
let loop = (f: unit => bool, interval: float) => {
  let id_ref = ref(None);
  id_ref :=
    Some(
      Dom_html.window##setInterval(
        Js.wrap_callback(_ =>
          if (f()) {
            switch (id_ref^) {
            | Some(id) => Dom_html.window##clearInterval(id)
            | None => ()
            };
          }
        ),
        Js.float(interval),
      ),
    );
};

let selector = "div.hover-rule-spec";
let selector_origin = "#page > " ++ selector;
let selector_copied = "body > " ++ selector;
let opt_get_origin = () =>
  Dom_html.document##querySelector(Js.string(selector_origin));
let opt_get_copied = () =>
  Dom_html.document##querySelector(Js.string(selector_copied));

let try_remove_copied = _ev => {
  let* copied = opt_get_copied();
  let _ = Dom_html.document##.body##removeChild((copied :> Js.t(Dom.node)));
  Js._true;
};

let elem = JsUtil.get_elem_by_id("ninja-keys-rules");
let shadow_root = Js.Unsafe.get(_, "shadowRoot");

module Open =
       (
         M: {
           let version: RuleImage.version;
           let update_rule: Haz3lcore.RuleImage.t => unit;
         },
       ) => {
  let copy_hover_rule_spec = (target_elem: Js.t(Dom_html.element), ev) => {
    let action = Js.Unsafe.get(target_elem, "action");
    let id = Js.to_string(action##.id);
    let rule_image = RuleImage.t_of_sexp(Sexplib.Sexp.of_string(id));
    let rule = Option.get(RuleImage.to_rule(M.version, rule_image));
    if (current_hover_rule^ != rule) {
      current_hover_rule := rule;
      action_refresh^();
    };
    let* origin = opt_get_origin();
    let _ = try_remove_copied(ev);
    let _ =
      Dom_html.document##.body##appendChild(origin##cloneNode(Js._true));
    let* copied = opt_get_copied();
    let left = ev##.clientX;
    let bottom = Dom_html.window##.innerHeight - ev##.clientY;
    copied##.style##.left := Js.string(Printf.sprintf("%dpx", left));
    copied##.style##.bottom := Js.string(Printf.sprintf("%dpx", bottom));
    Js._true;
  };

  let bind_event_handler = (action: Js.t(Dom_html.element)) => {
    action##.onmousemove := Dom.handler(copy_hover_rule_spec(action));
    action##.onmouseout := Dom.handler(try_remove_copied);
    (); // TODO(zhiyao): I don't know why if it's removed, it doesn't work
  };

  let bind_event_handler_all = () => {
    let elem_root = shadow_root(elem);
    let actions = elem_root##querySelectorAll(Js.string("ninja-action"));
    let _ = actions##forEach(Js.wrap_callback(bind_event_handler));
    actions##.length != 0;
  };

  let bind_event_handler_search = () => {
    let elem_root = shadow_root(elem);
    let ninja_header = elem_root##querySelector(Js.string("ninja-header"));
    let shadow_root = shadow_root(ninja_header);
    let search: Js.t(Dom_html.inputElement) =
      shadow_root##querySelector(Js.string("#search"));
    search##.oninput :=
      Dom.handler(_ev => {Js.bool(bind_event_handler_all())});
  };

  let from_rule =
      (rule: Haz3lcore.RuleImage.t)
      : {
          .
          "handler": Js.readonly_prop(unit => unit),
          "id": Js.readonly_prop(string),
          "title": Js.readonly_prop(string),
          "section": Js.readonly_prop(Js.optdef(string)),
          "keywords": Js.readonly_prop(string),
        } => {
    open Haz3lcore.RuleImage;
    [%js
     {
       val id = sexp_of_t(rule) |> Sexplib.Sexp.to_string;
       val title = show(rule);
       val section = Js.Optdef.option(Some(show_kind(of_kind(rule))));
       val handler = () => M.update_rule(rule);
       val keywords = keywords(rule) |> String.concat(" ")
     }];
  };

  let set_data = () => {
    Js.Unsafe.set(
      elem,
      "data",
      M.version
      |> RuleImage.all_rules_of_version
      |> List.map(from_rule)
      |> Array.of_list
      |> Js.array,
    );
  };
};

let open_command_palette = (~version, ~update_rule): unit => {
  module Open =
    Open({
      let version = version;
      let update_rule = update_rule;
    });
  open Open;
  set_data();
  loop(bind_event_handler_all, 100.);
  bind_event_handler_search();
  Js.Unsafe.meth_call(elem, "open", [||]);
};
