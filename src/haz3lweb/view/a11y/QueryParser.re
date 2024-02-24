open Accessibility;

let get_input_value = (): string => {
  let input_elem = JsUtil.get_elem_by_id(query_input_id)##.textContent;
  let input_elem = Js_of_ocaml.Js.Opt.get(input_elem, () => {assert(false)});
  Js_of_ocaml.Js.to_string(input_elem);
};

let query_parser = (): QueryCommand.t => {
  let query = get_input_value();
  JsUtil.log(query);
  Select(Term);
};
