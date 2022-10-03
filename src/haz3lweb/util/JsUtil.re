open Js_of_ocaml;

let get_elem_by_id = id => {
  let doc = Dom_html.document;
  Js.Opt.get(doc##getElementById(Js.string(id)), () => {
    assert
      (false)
      //print_endline(id);
  });
};

let date_now = () => {
  %js
  new Js.date_now;
};

let timestamp = () => date_now()##valueOf;

let get_key = evt =>
  Js.to_string(Js.Optdef.get(evt##.key, () => failwith("JsUtil.get_key")));

let ctrl_held = evt => Js.to_bool(evt##.ctrlKey);
let shift_held = evt => Js.to_bool(evt##.shiftKey);
let alt_held = evt => Js.to_bool(evt##.altKey);
let meta_held = evt => Js.to_bool(evt##.metaKey);

let clipboard_id = "clipboard_id";

let copy_to_clipboard = (string: string): unit => {
  //print_endline("copy_to_clipboard");
  let elem = get_elem_by_id(clipboard_id);
  Js.Unsafe.set(elem, "innerHTML", Js.string(string));
  /* TODO: figure out how to do below select call without unsafe
     the way that it is now the element name needs to be hardcoded
     to avoid runtime fallback warning */
  let _ =
    Js.Unsafe.js_expr("document.getElementById('clipboard_id').select()");
  Dom_html.document##execCommand(
    Js.string("copy"),
    Js.bool(true),
    Js.Opt.empty,
  );
  Js.Unsafe.set(elem, "innerHTML", Js.string(""));
};

let get_from_clipboard = (): string => {
  let _ =
    Js.Unsafe.js_expr(
      "window.navigator.clipboard.readText()
      .then(function(text) {
        var guy = document.getElementById('clipboard_id');
        guy.innerHTML = text; console.log('Clipboard content is: ', text);
        return Promise.resolve(text);
        })
      .catch(function(err)
        {console.error('Failed to read clipboard contents: ', err);})",
    );
  let elem = get_elem_by_id(clipboard_id);
  let result =
    Js.Unsafe.get(elem, "innerHTML")
    |> Js.to_string
    |> Str.global_replace(Str.regexp("&gt;"), ">");
  print_endline("get from clipboard: ");
  print_endline(result);
  result;
};

let download_string_file =
    (~filename: string, ~content_type: string, ~contents: string) => {
  let blob = File.blob_from_string(~contentType=content_type, contents);
  let url = Dom_html.window##._URL##createObjectURL(blob);

  let link = Dom_html.createA(Dom_html.document);
  link##.href := url;
  link##setAttribute(Js.string("download"), Js.string(filename));
  link##.onclick := Dom_html.handler(_ => {Js._true});
  link##click;
};

let download_json = (filename, contents): unit =>
  download_string_file(
    ~filename=filename ++ ".json",
    ~content_type="application/json",
    ~contents=contents |> Yojson.Safe.to_string,
  );

let read_file = (file, k) => {
  let reader = [%js new File.fileReader];
  reader##readAsText(file);
  reader##.onload :=
    Dom.handler(_ => {
      let result = reader##.result;
      let option = Js.Opt.to_option(File.CoerceTo.string(result));
      let data = Option.map(Js.to_string, option);
      k(data);
      Js._true;
    });
};

let set_localstore = (k: string, v: string): unit => {
  let local_store =
    Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
  local_store##setItem(Js.string(k), Js.string(v));
};

let get_localstore = (k: string): option(string) =>
  try({
    let local_store =
      Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
    local_store##getItem(Js.string(k))
    |> (
      x => Js.Opt.get(x, () => assert(false)) |> Js.to_string |> Option.some
    );
  }) {
  | _ => None
  };

let confirm = message => {
  Js.to_bool(Dom_html.window##confirm(Js.string(message)));
};
