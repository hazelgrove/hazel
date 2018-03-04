open Tyxml_js;

let titlebar (title_text: string) =>
  Html5.(div a::[a_class ["titlebar"]] [pcdata title_text]);
