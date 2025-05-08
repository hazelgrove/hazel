open Virtual_dom.Vdom;
open LivelitCtx;
open Grammar;

let consume_keypress = [
  Attr.on_keydown(_ => Effect.Stop_propagation),
  Attr.on_copy(_ => Effect.Stop_propagation),
  Attr.on_cut(_ => Effect.Stop_propagation),
  Attr.on_paste(_ => Effect.Stop_propagation),
];

module ImageText: BuiltinLivelit = {
  let name = "imagetext";

  /* The model holds the image URL, text, and styling parameters. */
  type model_t = {
    imageUrl: string,
    text: string,
    x: int, /* X position 0‑100  */
    y: int, /* Y position 0‑100  */
    width: int, /* Width      0‑100  */
    textColor: string, /* CSS color value */
    fontSize: int /* Font size in pixels */
  };

  /* The expansion is just a simple success message. */
  type expansion_t = string;

  /* Actions to update the model. */
  type action_t =
    | SetImageUrl(string)
    | SetText(string)
    | SetX(int)
    | SetY(int)
    | SetWidth(int)
    | SetTextColor(string)
    | SetFontSize(int);

  /* Model type in Hazel: 7‑tuple (imageUrl, text, x, y, width, textColor, fontSize). */
  let hazel_model_t: TermBase.Typ.t =
    Prod([
      Typ.temp(Atom(String)),
      Typ.temp(Atom(String)),
      Typ.temp(Atom(Int)),
      Typ.temp(Atom(Int)),
      Typ.temp(Atom(Int)),
      Typ.temp(Atom(String)),
      Typ.temp(Atom(Int)),
    ])
    |> Typ.fresh;

  /* Convert model to a Hazel expression. */
  let model_to_hazel: model_t => model_exp =
    m => {
      let mkInt = i => DHExp.fresh(Atom(Int(Bigint.of_int(i))));
      DHExp.fresh(
        Tuple([
          DHExp.fresh(Atom(String(m.imageUrl))),
          DHExp.fresh(Atom(String(m.text))),
          mkInt(m.x),
          mkInt(m.y),
          mkInt(m.width),
          DHExp.fresh(Atom(String(m.textColor))),
          mkInt(m.fontSize),
        ]),
      );
    };

  /* Convert a Hazel expression back to the model. */
  let model_from_hazel: model_exp => option(model_t) =
    expr =>
      switch (expr.term) {
      | Tuple([
          {term: Atom(String(imageUrl)), _},
          {term: Atom(String(text)), _},
          {term: Atom(Int(x)), _},
          {term: Atom(Int(y)), _},
          {term: Atom(Int(width)), _},
          {term: Atom(String(textColor)), _},
          {term: Atom(Int(fontSize)), _},
        ]) =>
        Some({
          imageUrl,
          text,
          x: int_of_string(Bigint.to_string(x)),
          y: int_of_string(Bigint.to_string(y)),
          width: int_of_string(Bigint.to_string(width)),
          textColor,
          fontSize: int_of_string(Bigint.to_string(fontSize)),
        })
      | _ => None
      };

  /* Default model with placeholder values. */
  let model_default: model_t = {
    imageUrl: "https://placehold.co/600x400",
    text: "Hello, World!",
    x: 50,
    y: 50,
    width: 80,
    textColor: "#ffffff",
    fontSize: 24,
  };

  /* Expansion type in Hazel: a string. */
  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(String));

  /* The expansion is just a simple success message. */
  let expand: model_t => expansion_t = _ => "Success";

  let expand_to_hazel: expansion_t => expansion_exp =
    message => DHExp.fresh(Atom(String(message)));

  /* Update the model based on actions. */
  let update: (action_t, model_t) => model_t =
    (action, m) =>
      switch (action) {
      | SetImageUrl(url) => {
          ...m,
          imageUrl: url,
        }
      | SetText(t) => {
          ...m,
          text: t,
        }
      | SetX(x) => {
          ...m,
          x,
        }
      | SetY(y) => {
          ...m,
          y,
        }
      | SetWidth(w) => {
          ...m,
          width: w,
        }
      | SetTextColor(color) => {
          ...m,
          textColor: color,
        }
      | SetFontSize(size) => {
          ...m,
          fontSize: size,
        }
      };

  /* Hazel action type. */
  let hazel_action_t: TermBase.Typ.t = {
    let string_type = Atom(String) |> Typ.fresh;
    let int_type = Atom(Int) |> Typ.fresh;
    Sum([
      Variant("SetImageUrl", [], Some(string_type)),
      Variant("SetText", [], Some(string_type)),
      Variant("SetX", [], Some(int_type)),
      Variant("SetY", [], Some(int_type)),
      Variant("SetWidth", [], Some(int_type)),
      Variant("SetTextColor", [], Some(string_type)),
      Variant("SetFontSize", [], Some(int_type)),
    ])
    |> Typ.fresh;
  };

  /* Convert action to Hazel expression. */
  let action_to_hazel: action_t => action_exp =
    action =>
      switch (action) {
      | SetImageUrl(url) =>
        Ap(
          Forward,
          DHExp.fresh(
            Constructor(
              "SetImageUrl",
              Some(Some(Atom(String) |> Typ.fresh)),
            ),
          ),
          DHExp.fresh(Atom(String(url))),
        )
        |> DHExp.fresh
      | SetText(t) =>
        Ap(
          Forward,
          DHExp.fresh(
            Constructor("SetText", Some(Some(Atom(String) |> Typ.fresh))),
          ),
          DHExp.fresh(Atom(String(t))),
        )
        |> DHExp.fresh
      | SetX(x) =>
        Ap(
          Forward,
          DHExp.fresh(
            Constructor("SetX", Some(Some(Atom(Int) |> Typ.fresh))),
          ),
          DHExp.fresh(Atom(Int(Bigint.of_int(x)))),
        )
        |> DHExp.fresh
      | SetY(y) =>
        Ap(
          Forward,
          DHExp.fresh(
            Constructor("SetY", Some(Some(Atom(Int) |> Typ.fresh))),
          ),
          DHExp.fresh(Atom(Int(Bigint.of_int(y)))),
        )
        |> DHExp.fresh
      | SetWidth(w) =>
        Ap(
          Forward,
          DHExp.fresh(
            Constructor("SetWidth", Some(Some(Atom(Int) |> Typ.fresh))),
          ),
          DHExp.fresh(Atom(Int(Bigint.of_int(w)))),
        )
        |> DHExp.fresh
      | SetTextColor(color) =>
        Ap(
          Forward,
          DHExp.fresh(
            Constructor(
              "SetTextColor",
              Some(Some(Atom(String) |> Typ.fresh)),
            ),
          ),
          DHExp.fresh(Atom(String(color))),
        )
        |> DHExp.fresh
      | SetFontSize(size) =>
        Ap(
          Forward,
          DHExp.fresh(
            Constructor("SetFontSize", Some(Some(Atom(Int) |> Typ.fresh))),
          ),
          DHExp.fresh(Atom(Int(Bigint.of_int(size)))),
        )
        |> DHExp.fresh
      };

  let action_from_hazel: action_exp => option(action_t) =
    expr => {
      let parseInt = b => int_of_string(Bigint.to_string(b));
      switch (expr.term) {
      | Ap(
          Forward,
          {term: Constructor("SetImageUrl", _), _},
          {term: Atom(String(url)), _},
        ) =>
        Some(SetImageUrl(url))
      | Ap(
          Forward,
          {term: Constructor("SetText", _), _},
          {term: Atom(String(t)), _},
        ) =>
        Some(SetText(t))
      | Ap(
          Forward,
          {term: Constructor("SetX", _), _},
          {term: Atom(Int(x)), _},
        ) =>
        Some(SetX(parseInt(x)))
      | Ap(
          Forward,
          {term: Constructor("SetY", _), _},
          {term: Atom(Int(y)), _},
        ) =>
        Some(SetY(parseInt(y)))
      | Ap(
          Forward,
          {term: Constructor("SetWidth", _), _},
          {term: Atom(Int(w)), _},
        ) =>
        Some(SetWidth(parseInt(w)))
      | Ap(
          Forward,
          {term: Constructor("SetTextColor", _), _},
          {term: Atom(String(color)), _},
        ) =>
        Some(SetTextColor(color))
      | Ap(
          Forward,
          {term: Constructor("SetFontSize", _), _},
          {term: Atom(Int(size)), _},
        ) =>
        Some(SetFontSize(parseInt(size)))
      | _ => None
      };
    };

  /* Render the image with text at specified position and width. */
  let view = (model: model_t, send_action) => {
    let {imageUrl, text, x, y, width, textColor, fontSize} = model;

    /* Helper to build labelled inputs. */
    let makeInput = (id, label, value, handler) =>
      Node.div([
        Node.label(~attrs=[Attr.for_(id)], [Node.text(label)]),
        Node.input(
          ~attrs=
            [
              Attr.id(id),
              Attr.type_("text"),
              Attr.value(value),
              Attr.create("style", "width:100%;"),
              Attr.on_input((_, v) => handler(v)),
            ]
            @ consume_keypress,
          (),
        ),
      ]);

    /* Range sliders. */
    let makeSlider = (id, label, value, min, max, handler) =>
      Node.div([
        Node.label(~attrs=[Attr.for_(id)], [Node.text(label)]),
        Util.Web.range(
          ~attrs=[
            Attr.id(id),
            Attr.value(string_of_int(value)),
            Attr.on_input((_, v) => handler(int_of_string(v))),
          ],
          ~min,
          ~max,
          string_of_int(value),
        ),
      ]);

    Node.div([
      /* Preview of the image with text */
      Node.div(
        ~attrs=[Attr.create("style", "margin-bottom: 10px;")],
        [
          Node.div(
            ~attrs=[
              Attr.create(
                "style",
                "position:relative; display:inline-block;",
              ),
            ],
            [
              Node.img(
                ~attrs=[
                  Attr.src(imageUrl),
                  Attr.create("style", "max-width:100%; display:block;"),
                  Attr.create(
                    "onerror",
                    "this.onerror=null; this.src='https://placehold.co/600x400?text=Image+Error';",
                  ),
                ],
                (),
              ),
              Node.div(
                ~attrs=[
                  Attr.create(
                    "style",
                    "position:absolute; top:"
                    ++ string_of_int(y)
                    ++ "%; left:"
                    ++ string_of_int(x)
                    ++ "%;"
                    ++ "transform:translate(-50%,-50%); color:"
                    ++ textColor
                    ++ "; "
                    ++ "font-size:"
                    ++ string_of_int(fontSize)
                    ++ "px; font-weight:bold; text-align:center; "
                    ++ "text-shadow:2px 2px 4px rgba(0,0,0,0.8); width:"
                    ++ string_of_int(width)
                    ++ "%;",
                  ),
                ],
                [Node.text(text)],
              ),
            ],
          ),
        ],
      ),
      /* Controls. */
      Node.div(
        ~attrs=[
          Attr.create(
            "style",
            "display:flex; flex-direction:column; gap:6px; margin-top:10px;",
          ),
        ],
        [
          /* Image URL input */
          makeInput("imageUrl", "Image URL:", imageUrl, v =>
            send_action(SetImageUrl(v))
          ),
          /* Text input */
          makeInput("overlayText", "Text:", text, v =>
            send_action(SetText(v))
          ),
          /* Text color input */
          makeInput("textColor", "Text Color:", textColor, v =>
            send_action(SetTextColor(v))
          ),
          /* Font size slider */
          makeSlider("fontSize", "Font Size (px):", fontSize, "10", "72", v =>
            send_action(SetFontSize(v))
          ),
          /* X position slider */
          makeSlider("xPosition", "X Position (%):", x, "0", "100", v =>
            send_action(SetX(v))
          ),
          /* Y position slider */
          makeSlider("yPosition", "Y Position (%):", y, "0", "100", v =>
            send_action(SetY(v))
          ),
          /* Width slider */
          makeSlider("textWidth", "Width (%):", width, "10", "100", v =>
            send_action(SetWidth(v))
          ),
        ],
      ),
    ]);
  };

  /* Reasonable default size for the livelit. */
  let size: ProjectorCore.Shape.t =
    ProjectorCore.Shape.{
      vertical: Block(30),
      horizontal: 40,
    };
};
