open Js_of_ocaml;
open Virtual_dom.Vdom;
open Js_of_ocaml.Url;

let get_elem_by_id = id => {
  let doc = Dom_html.document;
  Js.Opt.get(doc##getElementById(Js.string(id)), () => {assert(false)});
};

let get_elem_by_id_opt = id =>
  switch (get_elem_by_id(id)) {
  | exception _ => None
  | e => Some(e)
  };

let get_elem_by_selector = selector => {
  let doc = Dom_html.document;
  Js.Opt.get(
    doc##querySelector(Js.string(selector)),
    () => {
      print_endline("Selector could not be found: " ++ selector);
      assert(false);
    },
  );
};

let get_child_with_class = (element: Js.t(Dom_html.element), className) => {
  let rec loop = (sibling: Js.t(Dom_html.element)) =>
    if (Js.to_bool(sibling##.classList##contains(Js.string(className)))) {
      Some(sibling);
    } else {
      loop(
        Js.Opt.get(sibling##.nextSibling, () => failwith("no sibling"))
        |> Js.Unsafe.coerce,
      );
    };
  loop(
    Js.Opt.get(element##.firstChild, () => failwith("no child"))
    |> Js.Unsafe.coerce,
  );
};

let date_now = () => {
  [%js new Js.date_now];
};

let timestamp = () => date_now()##valueOf;

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

let clear_localstore = () => {
  let local_store =
    Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
  local_store##clear;
};

let confirm = message => {
  Js.to_bool(Dom_html.window##confirm(Js.string(message)));
};

let alert = message => {
  Dom_html.window##alert(Js.string(message));
};

let log = data => {
  Firebug.console##log(data);
};

let clipboard_shim_id = "clipboard-shim";

let focus_clipboard_shim = () => get_elem_by_id(clipboard_shim_id)##focus;

let clipboard_shim = {
  Node.textarea(~attrs=[Attr.id(clipboard_shim_id)], []);
};

let copy = (str: string) => {
  focus_clipboard_shim();
  Dom_html.document##execCommand(
    Js.string("selectAll"),
    Js.bool(false),
    Js.Opt.empty,
  );
  Dom_html.document##execCommand(
    Js.string("insertText"),
    Js.bool(false),
    Js.Opt.option(Some(Js.string(str))),
  );
  Dom_html.document##execCommand(
    Js.string("selectAll"),
    Js.bool(false),
    Js.Opt.empty,
  );
};

let scroll_cursor_into_view_if_needed = () =>
  try({
    let caret_elem = get_elem_by_id("caret");
    caret_elem##scrollIntoView(
      Js.Unsafe.obj([|
        ("block", Js.Unsafe.inject(Js.string("nearest"))),
        ("inline", Js.Unsafe.inject(Js.string("nearest"))),
      |]),
    );
  }) {
  | Assert_failure(_) => ()
  };

module Fragment = {
  let set_current = frag => {
    let frag =
      switch (frag) {
      | "" => ""
      | frag => "#" ++ frag
      };
    let history = Js_of_ocaml.Dom_html.window##.history;
    history##pushState(Js.null, Js.string(""), Js.some(Js.string(frag)));
  };

  let get_current = () => {
    let fragment_of_url = (url: Url.url): string =>
      switch (url) {
      | Http({hu_fragment: str, _})
      | Https({hu_fragment: str, _})
      | File({fu_fragment: str, _}) => str
      };
    Url.Current.get() |> Option.map(fragment_of_url);
  };
};

let setPointerCapture = (e: Js.t(Dom_html.element), pointerId: int): unit =>
  Js.Unsafe.meth_call(
    e,
    "setPointerCapture",
    [|Js.Unsafe.inject(pointerId)|],
  );

let releasePointerCapture = (e: Js.t(Dom_html.element), pointerId: int) =>
  Js.Unsafe.meth_call(
    e,
    "releasePointerCapture",
    [|Js.Unsafe.inject(pointerId)|],
  );

let hasPointerCapture = (e: Js.t(Dom_html.element), pointerId: int) =>
  Js.Unsafe.meth_call(
    e,
    "hasPointerCapture",
    [|Js.Unsafe.inject(pointerId)|],
  );

let delay = (delay: float, callback: unit => unit) => {
  let _ =
    Js_of_ocaml.Dom_html.window##setTimeout(
      Js.wrap_callback(callback),
      delay,
    );
  ();
};

let set_select_value = (select_id, value) => {
  Js_of_ocaml.Js.Unsafe.set(
    get_elem_by_id(select_id),
    "value",
    Js_of_ocaml.Js.string(value),
  );
};

let prompt = (message: string, default: string): option(string) => {
  Js.Opt.to_option(
    Dom_html.window##prompt(Js.string(message), Js.string(default)),
  )
  |> Option.map(Js.to_string);
};

module QueryParams = {
  let get_arguments = (url: Url.url): list((string, string)) =>
    switch (url) {
    | Http({hu_arguments, _}) => hu_arguments
    | Https({hu_arguments, _}) => hu_arguments
    | File({fu_arguments, _}) => fu_arguments
    };

  let set_arguments = (url: Url.url, args: list((string, string))): Url.url =>
    switch (url) {
    | Http(u) =>
      Http({
        ...u,
        hu_arguments: args,
      })
    | Https(u) =>
      Https({
        ...u,
        hu_arguments: args,
      })
    | File(u) =>
      File({
        ...u,
        fu_arguments: args,
      })
    };

  let get_param = (name: string): option(string) => {
    let q_opt =
      Url.Current.get()
      |> Option.map(url =>
           url |> get_arguments |> List.find_opt(((k, _)) => k == name)
         );
    switch (q_opt) {
    | Some(Some((_, v))) => Some(v)
    | _ => None
    };
  };

  let set_param = (name: string, value: string) => {
    Url.Current.get()
    |> Option.iter(url => {
         let args =
           get_arguments(url)
           |> List.filter(((k, _)) => k != name)
           |> List.cons((name, value));

         let new_url = set_arguments(url, args);
         let href = Url.string_of_url(new_url);

         Dom_html.window##.history##pushState(
           Js.null,
           Js.string(""),
           Js.some(Js.string(href)),
         );
       });
  };

  let remove_param = (name: string) =>
    Url.Current.get()
    |> Option.iter(url => {
         let args =
           get_arguments(url) |> List.filter(((k, _)) => k != name);

         let new_url = set_arguments(url, args);
         let href = Url.string_of_url(new_url);

         Dom_html.window##.history##pushState(
           Js.null,
           Js.string(""),
           Js.some(Js.string(href)),
         );
       });
};
