open Js_of_ocaml_tyxml.Tyxml_js;

let main_title_bar = (title_text: string) =>
  Html.(
    div(~a=[a_class(["title-bar", "panel-title-bar"])], [txt(title_text)])
  );

/* For title bars that appear mid-panel. These are styled differently. */
let other_title_bar = (title_text: string) =>
  Html.(div(~a=[a_class(["title-bar"])], [txt(title_text)]));
