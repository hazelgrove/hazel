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

let copy_to_clipboard = (string: string): unit => {
  /* Note: To use (deprecated) execommand would need to introduce
     an invisible textarea and insert the string as you cannot
     directly copy from a variable using it */
  /*let _ =
    Dom_html.document##execCommand(
      Js.string("copy"),
      Js.bool(true),
      Js.Opt.return(Js.string("testtest")),
    );*/
  /* So instead we use the mode modern clipboard API. however
     js_of_ocaml doesn't have bindings for it, so in the interest
     of time I'm just using Unsafe.js_expr. Note the use of backticks
     around the string in order to make this robust to the presence
     of linebreaks in the string.

     This currently seems to work fine, but generates a warning to the
     console as described in the below println.
     */

  print_endline(
    "Copying log to keyboard. An exception reading 'fallback to runtime evaluation' is expected.",
  );
  string
  |> Printf.sprintf("window.navigator.clipboard.writeText(`%s`);")
  |> Js.Unsafe.js_expr;
};

let get_from_clipboard = (): string => {
  /* WIP(andrew):
       This sorta works, somewhat hackily and inconsistently (requires a dom element
       called blorg to be present and ideally hidden). However it prompts the user
       for permissions each time.
     */
  let _ =
    Js.Unsafe.js_expr(
      "window.navigator.clipboard.readText().then(
      function(text)
      {var guy = document.getElementById('blorg'); guy.innerHTML = text; console.log('Clipboard content is: ', text)}).catch
      (function(err)
        {console.error('Failed to read clipboard contents: ', err)})",
    );
  let doc = Dom_html.document;
  let elem =
    Js.Opt.get(doc##getElementById(Js.string("blorg")), () => {
      assert(false)
    });
  let result: Js.t('a) = Js.Unsafe.get(elem, "innerHTML");
  let result = Js.to_string(result);
  print_endline(result);
  result;
};

let download_string_file =
    (filename: string, content_type: string, contents: string) => {
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
    filename ++ ".json",
    "application/json",
    contents |> Yojson.Safe.to_string,
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
