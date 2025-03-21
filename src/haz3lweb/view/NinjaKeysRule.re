open Haz3lcore;
open Js_of_ocaml;
open Util;
let pos = ref(DerivationTree.Trees(0, Value));
let version = ref(RuleImage.PropositionalLogic: RuleImage.version);
let schedule_action =
  ref(
    (
      _: DerivationTree.p(Editor.Model.t) => DerivationTree.p(Editor.Model.t),
    ) =>
    ()
  );
let schedule_action_update_hover_rule_spec = ref(() => ());
let current_hover_rule = ref(Rule.Implies_E);

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
  [%js
   {
     val id = Haz3lcore.RuleImage.sexp_of_t(rule) |> Sexplib.Sexp.to_string;
     val title = Haz3lcore.RuleImage.show(rule);
     val section =
       Js.Optdef.option(
         Some(
           Haz3lcore.RuleImage.show_kind(Haz3lcore.RuleImage.of_kind(rule)),
         ),
       );
     val handler =
       () =>
         schedule_action^(
           DerivationTree.switch_rule(~pos=pos^, ~rule=Some(rule)),
         );
     val keywords = Haz3lcore.RuleImage.keywords(rule) |> String.concat(" ")
   }];
};

let bind_event_handler = (n: int) => {
  let _ =
    Js.Unsafe.eval_string(
      Format.sprintf(
        "
const checkInterval = setInterval(() => {

const actions = document
    .getElementById('ninja-keys-rules')
    .shadowRoot
    .querySelectorAll('ninja-action');
if (actions.length != %d) return;

actions.forEach((action, _) => {
    action.addEventListener(
        'mouseover',
        document.body['ninja-keys-rules-handler']);
});

clearInterval(checkInterval);
}, 100);
",
        n,
      ),
    );
  ();
};

let handler = (_ev: Js.t(#Dom.event('a))) => {
  let document = Dom_html.document;
  let target_elem = Dom_html.eventTarget(_ev);
  let shadow_root = Js.Unsafe.get(target_elem, "action");
  let id = Js.to_string(shadow_root##.id);
  let rule_image = RuleImage.t_of_sexp(Sexplib.Sexp.of_string(id));
  let rule = Option.get(RuleImage.to_rule(version^, rule_image));

  if (current_hover_rule^ != rule) {
    current_hover_rule := rule;
    print_endline("Hovering over rule: " ++ Rule.show(rule));
    schedule_action_update_hover_rule_spec^();
  };

  /* Mouseenter handler */
  target_elem##.onmousemove :=
    Dom.handler(ev => {
      /* Create text element */

      let text_div =
        JsUtil.get_elem_by_selector("#page > div.hover-rule-spec");
      let text_div = text_div##cloneNode(Js.bool(true));
      let _ =
        Js.Opt.case(
          document##querySelector(Js.string("body > div.hover-rule-spec")),
          () => (),
          old_text_div => {
            let _ =
              document##.body##removeChild((old_text_div :> Js.t(Dom.node)));
            ();
          },
        );
      let _ = document##.body##appendChild(text_div);
      let text_div =
        JsUtil.get_elem_by_selector("body > div.hover-rule-spec");
      // document##getElementById(Js.string("hover-rule-spec"));

      let mouseX = ev##.clientX; // + Dom_html.window##scrollHeight;
      let mouseY = Dom_html.window##.innerHeight - ev##.clientY; // + window##.scrollY;
      text_div##.style##.left := Js.string(Printf.sprintf("%dpx", mouseX));
      text_div##.style##.bottom := Js.string(Printf.sprintf("%dpx", mouseY));
      text_div##.style##.display := Js.string("block");
      Js._true;
    });

  target_elem##.onmouseout :=
    Dom.handler(_ => {
      let text_element =
        JsUtil.get_elem_by_selector("body > div.hover-rule-spec");
      text_element##.style##.display := Js.string("none");
      Js._true;
    });

  Js._true;
};

let set_handler = () => {
  Js.Unsafe.set(
    Dom_html.document##.body,
    "ninja-keys-rules-handler",
    Dom.handler(handler),
  );
};

let open_command_palette = (~version as version', ~pos as pos'): unit => {
  let elem = JsUtil.get_elem_by_id("ninja-keys-rules");
  if (version^ != version') {
    version := version';
    let rules = version^ |> Haz3lcore.RuleImage.all_rules_of_version;
    Js.Unsafe.set(
      elem,
      "data",
      rules |> List.map(from_rule) |> Array.of_list |> Js.array,
    );
    bind_event_handler(List.length(rules));
  };
  pos := pos';
  Js.Unsafe.meth_call(elem, "open", [||]);
};
