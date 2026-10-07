open Js_of_ocaml;

/* This endpoint exists only in Vite. Static/hosted builds keep using the
   dedicated browser credential store. Never log the response or the request body. */
let request =
    (~method: string, ~body="", handler: option(Yojson.Safe.t) => unit) => {
  let local =
    try(
      List.mem(
        Js.to_string(Dom_html.window##.location##.hostname),
        ["localhost", "127.0.0.1", "[::1]"],
      )
    ) {
    | _ => false
    };
  if (!local) {
    handler(None);
  } else {
    let req = XmlHttpRequest.create();
    req##.onreadystatechange :=
      Js.wrap_callback(_ =>
        if (req##.readyState == XmlHttpRequest.DONE) {
          let result =
            try(
              if (req##.status == 200) {
                Js.Opt.to_option(req##.responseText)
                |> Option.map(x => Yojson.Safe.from_string(Js.to_string(x)));
              } else {
                None;
              }
            ) {
            | _ => None
            };
          handler(result);
        }
      );
    req##_open(
      Js.string(method),
      Js.string("/__hazel_local_key"),
      Js._true,
    );
    Js.Unsafe.set(req, "timeout", 3000);
    req##setRequestHeader(Js.string("X-Hazel-Local-Key"), Js.string("1"));
    req##setRequestHeader(
      Js.string("Content-Type"),
      Js.string("application/json"),
    );
    req##send(Js.some(Js.string(body)));
  };
};

let load = handler =>
  request(~method="GET", result => {
    let key =
      switch (result) {
      | Some(`Assoc(fields)) =>
        switch (List.assoc_opt("key", fields)) {
        | Some(`String(key)) when String.trim(key) != "" => Some(Some(key))
        | Some(`Null) => Some(None)
        | _ => None
        }
      | _ => None
      };
    handler(key);
  });

let save = (key, handler) =>
  request(
    ~method="PUT",
    ~body=Yojson.Safe.to_string(`Assoc([("key", `String(key))])),
    result =>
    handler(Option.is_some(result))
  );

let forget = handler =>
  request(~method="DELETE", result => handler(Option.is_some(result)));
