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
     of linebreaks in the string. */
  // note: using unsafe as js_of_ocaml doesn't have clipboard bindings
  print_endline(
    "Copying log to keyboard. An exception reading 'fallback to runtime evaluation' is expected.",
  );
  string
  |> Printf.sprintf("window.navigator.clipboard.writeText(`%s`);")
  |> Js.Unsafe.js_expr;
};

let _write_to_clipboard = (_string: string) => {
  //let _ = Dom_html.window##.navigator##.clipboard##writeText(string);
  let _ =
    Dom_html.document##execCommand(
      Js.string("copy"),
      Js.bool(false),
      Js.Opt.return(Js.string("testtest")),
    );
  // note: using unsafe as js_of_ocaml doesn't have clipboard bindings
  //let q =
  // Printf.sprintf("window.navigator.clipboard.writeText(\"%s\")", string);
  //let _ = Js.Unsafe.js_expr(q);
  ();
};

let get_from_clipboard = (): string => {
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
      assert
        (false)
        //print_endline(id);
    });
  let result: Js.t('a) = Js.Unsafe.get(elem, "innerHTML");
  let blah = Js.to_string(result);
  print_endline(blah);
  blah;
};

let get_key = evt =>
  Js.to_string(Js.Optdef.get(evt##.key, () => failwith("JsUtil.get_key")));

let ctrl_held = evt => Js.to_bool(evt##.ctrlKey);
let shift_held = evt => Js.to_bool(evt##.shiftKey);
let alt_held = evt => Js.to_bool(evt##.altKey);
let meta_held = evt => Js.to_bool(evt##.metaKey);
