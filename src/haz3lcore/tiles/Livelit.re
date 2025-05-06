open Virtual_dom.Vdom;
open ProjectorCore;
open LivelitCtx;
open Grammar;

type livelit_name = string;

// referenced in docs/livelits.md
module Slider: BuiltinLivelit = {
  let name = "slider";

  type model_t = Bigint.t;
  type expansion_t = Bigint.t;
  type action_t =
    | SetModel(model_t);

  let hazel_model_t: TermBase.Typ.t = Typ.temp(Atom(Int));
  let model_to_hazel: model_t => model_exp =
    (x: model_t) => DHExp.fresh(Atom(Int(x)));
  let model_from_hazel: model_exp => option(model_t) =
    (x: model_exp) => {
      switch (x.term) {
      | Atom(Int(n)) => Some(n)
      | _ => None
      };
    };
  let model_default: model_t = Bigint.of_int(50);

  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(Int));
  let expand: model_t => expansion_t =
    (x: model_t) =>
      switch (x) {
      | n => n
      };
  let expand_to_hazel: expansion_t => expansion_exp =
    (x: expansion_t) =>
      switch (x) {
      | n => DHExp.fresh(Atom(Int(n)))
      };
  let update: (action_t, model_t) => model_t =
    (action: action_t, _model: model_t) => {
      switch (action) {
      | SetModel(n) => n
      };
    };

  let hazel_action_t: TermBase.Typ.t =
    Sum([Variant("SetModel", [], Some(Atom(Int) |> Typ.fresh))])
    |> Typ.fresh;
  let action_to_hazel: action_t => action_exp =
    (action: action_t) =>
      switch (action) {
      | SetModel(n) =>
        Ap(
          Forward,
          Constructor("SetModel", Some(Some(Atom(Int) |> Typ.fresh)))
          |> DHExp.fresh,
          Atom(Int(n)) |> DHExp.fresh,
        )
        |> DHExp.fresh
      };
  let action_from_hazel: action_exp => option(action_t) =
    (action: action_exp) => {
      switch (action.term) {
      | Ap(
          Forward,
          {term: Constructor("SetModel", _), _},
          {term: Atom(Int(n)), _},
        ) =>
        Some(SetModel(n))
      | _ => None
      };
    };

  let view = (model: model_t, send_action) => {
    let n = model;

    Util.Web.range(
      ~attrs=[
        Attr.on_input((_, v: string) => {
          send_action(SetModel(Bigint.of_string(v)))
        }),
      ],
      ~min="0",
      ~max="100",
      Bigint.to_string(n),
    );
  };

  let size: ProjectorCore.Shape.t =
    ProjectorCore.Shape.{
      vertical: Inline,
      horizontal: 20,
    };
};

module Emotion: BuiltinLivelit = {
  let name = "emotion";

  /* The model is an integer represented as Bigint.t */
  type model_t = Bigint.t;
  /* The expansion is a string representing the emotion */
  type expansion_t = string;
  type action_t =
    | SetModel(model_t);

  /* Hazel model type is an integer */
  let hazel_model_t: TermBase.Typ.t = Typ.temp(Atom(Int));

  let model_to_hazel: model_t => model_exp =
    (x: model_t) => DHExp.fresh(Atom(Int(x)));

  let model_from_hazel: model_exp => option(model_t) =
    (x: model_exp) =>
      switch (x.term) {
      | Atom(Int(n)) => Some(n)
      | _ => None
      };

  /* Default model value is 50 */
  let model_default: model_t = Bigint.of_int(50);

  /* Hazel expansion type is a String */
  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(String));

  /* Compute the emotion based on the slider value:
     - less than 40: "sad"
     - greater than 70: "happy"
     - otherwise: "neutral" */
  let expand: model_t => expansion_t =
    (x: model_t) => {
      let n = int_of_string(Bigint.to_string(x));
      if (n < 40) {
        "sad";
      } else if (n > 70) {
        "happy";
      } else {
        "neutral";
      };
    };

  let expand_to_hazel: expansion_t => expansion_exp =
    (x: expansion_t) => DHExp.fresh(Atom(String(x)));

  let update: (action_t, model_t) => model_t =
    (action: action_t, _model: model_t) => {
      /* Update the model based on the action */
      switch (action) {
      | SetModel(n) => n
      };
    };

  /* Define the action type for Hazel */
  let hazel_action_t: TermBase.Typ.t =
    Sum([Variant("SetModel", [], Some(Atom(Int) |> Typ.fresh))])
    |> Typ.fresh;

  let action_to_hazel: action_t => action_exp =
    (action: action_t) =>
      switch (action) {
      | SetModel(n) =>
        Ap(
          Forward,
          Constructor("SetModel", Some(Some(Atom(Int) |> Typ.fresh)))
          |> DHExp.fresh,
          Atom(Int(n)) |> DHExp.fresh,
        )
        |> DHExp.fresh
      };

  let action_from_hazel: action_exp => option(action_t) =
    (action: action_exp) =>
      switch (action.term) {
      | Ap(
          Forward,
          {term: Constructor("SetModel", _), _},
          {term: Atom(Int(n)), _},
        ) =>
        Some(SetModel(n))
      | _ => None
      };

  let size =
    ProjectorCore.Shape.{
      vertical: Block(10),
      horizontal: 20,
    };

  let view = (model: model_t, send_action) => {
    let n = model;
    let n_int = int_of_string(Bigint.to_string(n));
    /* Calculate mouth curvature from the model value */
    let smile = (100.0 -. float_of_int(n_int)) /. 100.0 *. 50.0 -. 25.0;
    let pathData =
      "M60 130 Q100 " ++ Printf.sprintf("%.1f", 130.0 -. smile) ++ " 140 130";

    Node.div([
      Node.create_svg(
        "svg",
        ~attrs=[Attr.create("width", "200"), Attr.create("height", "200")],
        [
          Node.create_svg(
            "circle",
            ~attrs=[
              Attr.create("cx", "100"),
              Attr.create("cy", "100"),
              Attr.create("r", "90"),
              Attr.create("fill", "yellow"),
              Attr.create("stroke", "black"),
            ],
            [],
          ),
          Node.create_svg(
            "circle",
            ~attrs=[
              Attr.create("cx", "65"),
              Attr.create("cy", "80"),
              Attr.create("r", "10"),
              Attr.create("fill", "black"),
            ],
            [],
          ),
          Node.create_svg(
            "circle",
            ~attrs=[
              Attr.create("cx", "135"),
              Attr.create("cy", "80"),
              Attr.create("r", "10"),
              Attr.create("fill", "black"),
            ],
            [],
          ),
          Node.create_svg(
            "path",
            ~attrs=[
              Attr.create("d", pathData),
              Attr.create("stroke", "black"),
              Attr.create("fill", "transparent"),
              Attr.create("stroke-width", "5"),
            ],
            [],
          ),
        ],
      ),
      Util.Web.range(
        ~attrs=[
          Attr.on_input((_, v) => {
            send_action(SetModel(Bigint.of_string(v)))
          }),
        ],
        ~min="0",
        ~max="100",
        Bigint.to_string(n),
      ),
    ]);
  };
};

module Js: BuiltinLivelit = {
  let name = "js";

  /* The model holds (code, result) both as strings. */
  type model_t = {
    code: string,
    result: string,
  };

  /* The expansion is just the result string. */
  type expansion_t = string;

  /* We update the entire model at once. */
  type action_t =
    | SetModel(model_t);

  /* Model type in Hazel: a 2-tuple of strings. */
  let hazel_model_t: TermBase.Typ.t =
    Prod([Typ.temp(Atom(String)), Typ.temp(Atom(String))]) |> Typ.fresh;

  /* Convert model to a Hazel expression. */
  let model_to_hazel: model_t => model_exp =
    (m: model_t) => {
      let code_expr = DHExp.fresh(Atom(String(m.code)));
      let result_expr = DHExp.fresh(Atom(String(m.result)));
      DHExp.fresh(Tuple([code_expr, result_expr]));
    };

  /* Convert a Hazel expression back to the model. */
  let model_from_hazel: model_exp => option(model_t) =
    (expr: model_exp) => {
      switch (expr.term) {
      | Tuple([
          {term: Atom(String(code)), _},
          {term: Atom(String(result)), _},
        ]) =>
        Some({
          code,
          result,
        })
      | _ => None
      };
    };

  /* Default model: "1 + 1" with empty result. */
  let model_default: model_t = {
    code: "1 + 1",
    result: "",
  };

  /* Expansion type in Hazel: a string. */
  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(String));

  /* The expansion is just the current `result`. */
  let expand: model_t => expansion_t = (m: model_t) => m.result;

  let expand_to_hazel: expansion_t => expansion_exp =
    (res: expansion_t) => DHExp.fresh(Atom(String(res)));

  /* Updating the model means storing the new model. */
  let update: (action_t, model_t) => model_t =
    (action: action_t, _oldModel: model_t) =>
      switch (action) {
      | SetModel(m) => m
      };

  /* Hazel action type: single variant with our product type. */
  let hazel_action_t: TermBase.Typ.t =
    Sum([
      Variant(
        "SetModel",
        [],
        Some(
          Prod([Typ.temp(Atom(String)), Typ.temp(Atom(String))])
          |> Typ.fresh,
        ),
      ),
    ])
    |> Typ.fresh;

  /* Convert action -> Hazel expression. */
  let action_to_hazel: action_t => action_exp =
    (action: action_t) =>
      switch (action) {
      | SetModel(m) =>
        let code_expr = DHExp.fresh(Atom(String(m.code)));
        let result_expr = DHExp.fresh(Atom(String(m.result)));
        let tuple_expr = DHExp.fresh(Tuple([code_expr, result_expr]));

        Ap(
          Forward,
          Constructor(
            "SetModel",
            Some(
              Some(
                Prod([Typ.temp(Atom(String)), Typ.temp(Atom(String))])
                |> Typ.fresh,
              ),
            ),
          )
          |> DHExp.fresh,
          tuple_expr,
        )
        |> DHExp.fresh;
      };

  /* Convert Hazel expression -> action. */
  let action_from_hazel: action_exp => option(action_t) =
    (expr: action_exp) =>
      switch (expr.term) {
      | Ap(
          Forward,
          {term: Constructor("SetModel", _), _},
          {
            term:
              Tuple([
                {term: Atom(String(code)), _},
                {term: Atom(String(result)), _},
              ]),
            _,
          },
        ) =>
        Some(
          SetModel({
            code,
            result,
          }),
        )
      | _ => None
      };

  /* Render: show code input, a compute button, and the result. */
  let view = (model: model_t, send_action) => {
    let {code, result} = model;

    Node.div([
      /* Code input field */
      Node.input(
        ~attrs=[
          Attr.type_("text"),
          Attr.value(code),
          Attr.on_input((_, v: string) => {
            /* Update the code, keep the same result */
            send_action(
              SetModel({
                code: v,
                result: model.result,
              }),
            )
          }),
        ],
        (),
      ),
      /* Compute button */
      Node.button(
        ~attrs=[
          Attr.on_click(_ => {
            /* Evaluate the code and set the result */
            let evaluated =
              Js_of_ocaml.Js.Unsafe.eval_string("String(" ++ code ++ ")");

            send_action(
              SetModel({
                code,
                result: Js_of_ocaml.Js.to_string(evaluated),
              }),
            );
          }),
        ],
        [Node.text("Compute")],
      ),
      /* Display the current result */
      Node.div([Node.text("Result: " ++ result)]),
    ]);
  };

  /* Reasonable default shape. */
  let size: ProjectorCore.Shape.t =
    ProjectorCore.Shape.{
      vertical: Inline,
      horizontal: 40,
    };
};

module BoundedSlider: BuiltinLivelit = {
  let name = "boundedslider";

  type model_t = {
    value: Bigint.t,
    min: Bigint.t,
    max: Bigint.t,
  };

  type expansion_t = Bigint.t;

  type action_t =
    | SetValue(Bigint.t)
    | SetBounds(Bigint.t, Bigint.t);

  // Default model with value=50, min=0, max=100
  let model_default: model_t = {
    value: Bigint.of_int(50),
    min: Bigint.of_int(0),
    max: Bigint.of_int(100),
  };

  // Hazel type for model: tuple of three integers
  let hazel_model_t: TermBase.Typ.t =
    Prod([
      Typ.temp(Atom(Int)), // value
      Typ.temp(Atom(Int)), // min
      Typ.temp(Atom(Int)) // max
    ])
    |> Typ.fresh;

  // Conversion functions between model and Hazel expressions
  let model_to_hazel: model_t => model_exp =
    (m: model_t) => {
      let value_expr = DHExp.fresh(Atom(Int(m.value)));
      let min_expr = DHExp.fresh(Atom(Int(m.min)));
      let max_expr = DHExp.fresh(Atom(Int(m.max)));
      DHExp.fresh(Tuple([value_expr, min_expr, max_expr]));
    };

  let model_from_hazel: model_exp => option(model_t) =
    (expr: model_exp) => {
      switch (expr.term) {
      | Tuple([
          {term: Atom(Int(value)), _},
          {term: Atom(Int(min)), _},
          {term: Atom(Int(max)), _},
        ]) =>
        Some({
          value,
          min,
          max,
        })
      | _ => None
      };
    };

  // Expansion type is just an integer
  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(Int));

  // Expansion function (just returns the current value)
  let expand: model_t => expansion_t = model => model.value;

  // Convert expansion to Hazel expression
  let expand_to_hazel: expansion_t => expansion_exp =
    (value: expansion_t) => DHExp.fresh(Atom(Int(value)));

  // Update function to handle actions
  let update: (action_t, model_t) => model_t =
    (action: action_t, model: model_t) => {
      switch (action) {
      | SetValue(value) => {
          ...model,
          value,
        }
      | SetBounds(min, max) => {
          ...model,
          min,
          max,
        }
      };
    };

  // Hazel action type
  let hazel_action_t: TermBase.Typ.t =
    Sum([
      Variant("SetValue", [], Some(Atom(Int) |> Typ.fresh)),
      Variant(
        "SetBounds",
        [],
        Some(
          Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(Int))]) |> Typ.fresh,
        ),
      ),
    ])
    |> Typ.fresh;

  // Convert action to Hazel expression
  let action_to_hazel: action_t => action_exp =
    (action: action_t) =>
      switch (action) {
      | SetValue(value) =>
        Ap(
          Forward,
          Constructor("SetValue", Some(Some(Atom(Int) |> Typ.fresh)))
          |> DHExp.fresh,
          Atom(Int(value)) |> DHExp.fresh,
        )
        |> DHExp.fresh
      | SetBounds(min, max) =>
        let min_expr = DHExp.fresh(Atom(Int(min)));
        let max_expr = DHExp.fresh(Atom(Int(max)));
        let tuple_expr = DHExp.fresh(Tuple([min_expr, max_expr]));

        Ap(
          Forward,
          Constructor(
            "SetBounds",
            Some(
              Some(
                Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(Int))])
                |> Typ.fresh,
              ),
            ),
          )
          |> DHExp.fresh,
          tuple_expr,
        )
        |> DHExp.fresh;
      };

  // Convert Hazel expression to action
  let action_from_hazel: action_exp => option(action_t) =
    (expr: action_exp) =>
      switch (expr.term) {
      | Ap(
          Forward,
          {term: Constructor("SetValue", _), _},
          {term: Atom(Int(value)), _},
        ) =>
        Some(SetValue(value))
      | Ap(
          Forward,
          {term: Constructor("SetBounds", _), _},
          {
            term:
              Tuple([
                {term: Atom(Int(min)), _},
                {term: Atom(Int(max)), _},
              ]),
            _,
          },
        ) =>
        Some(SetBounds(min, max))
      | _ => None
      };

  // View function to render the slider with bounds
  let view = (model: model_t, send_action) => {
    let {value, min, max} = model;

    Node.div([
      // Inputs to adjust bounds
      Node.div([
        Node.text("Min: "),
        Node.input(
          ~attrs=[
            Attr.type_("number"),
            Attr.value(Bigint.to_string(min)),
            Attr.on_input((_, v: string) => {
              let new_min = Bigint.of_string(v);
              send_action(SetBounds(new_min, max));
            }),
          ],
          (),
        ),
        Node.text(" Max: "),
        Node.input(
          ~attrs=[
            Attr.type_("number"),
            Attr.value(Bigint.to_string(max)),
            Attr.on_input((_, v: string) => {
              let new_max = Bigint.of_string(v);
              send_action(SetBounds(min, new_max));
            }),
          ],
          (),
        ),
      ]),
      // The slider itself
      Util.Web.range(
        ~attrs=[
          Attr.on_input((_, v: string) => {
            send_action(SetValue(Bigint.of_string(v)))
          }),
        ],
        ~min=Bigint.to_string(min),
        ~max=Bigint.to_string(max),
        Bigint.to_string(value),
      ),
    ]);
  };

  // Size specification
  let size: ProjectorCore.Shape.t =
    ProjectorCore.Shape.{
      vertical: Block(5),
      horizontal: 30,
    };
};

module ChessBoard: BuiltinLivelit = {
  let name = "chessboard";

  // Piece representation
  type piece = {
    piece_type: string, // "pawn", "rook", "knight", "bishop", "queen", "king"
    color: string // "white" or "black"
  };

  // Square can be empty (None) or contain a piece (Some(piece))
  type square = option(piece);

  // Board model
  type model_t = {
    board: list(list(square)), // 8x8 board
    turn: string, // "white" or "black"
    selected_square: option((int, int)) // Currently selected square (row, col)
  };

  // Expansion type - we'll use a string representation of the board
  type expansion_t = string;

  // Actions
  type action_t =
    | SelectSquare(int, int) // Select a square at (row, col)
    | MoveSelectedPiece(int, int) // Move selected piece to (row, col)
    | ResetBoard; // Reset the board to initial state

  // Initial board setup
  let initial_board = {
    // Create an 8x8 board with pieces in starting positions
    let empty_row = List.init(8, _ => None);

    // Create pawn rows
    let white_pawns =
      List.init(8, _ =>
        Some({
          piece_type: "pawn",
          color: "white",
        })
      );
    let black_pawns =
      List.init(8, _ =>
        Some({
          piece_type: "pawn",
          color: "black",
        })
      );

    // Create back rows
    let create_back_row = color => {
      [
        Some({
          piece_type: "rook",
          color,
        }),
        Some({
          piece_type: "knight",
          color,
        }),
        Some({
          piece_type: "bishop",
          color,
        }),
        Some({
          piece_type: "queen",
          color,
        }),
        Some({
          piece_type: "king",
          color,
        }),
        Some({
          piece_type: "bishop",
          color,
        }),
        Some({
          piece_type: "knight",
          color,
        }),
        Some({
          piece_type: "rook",
          color,
        }),
      ];
    };

    let white_back_row = create_back_row("white");
    let black_back_row = create_back_row("black");

    // Assemble the board (top to bottom)
    [
      black_back_row, // Row 0: Black back row
      black_pawns, // Row 1: Black pawns
      empty_row, // Row 2: Empty
      empty_row, // Row 3: Empty
      empty_row, // Row 4: Empty
      empty_row, // Row 5: Empty
      white_pawns, // Row 6: White pawns
      white_back_row // Row 7: White back row
    ];
  };

  // Default model
  let model_default: model_t = {
    board: initial_board,
    turn: "white",
    selected_square: None,
  };

  // Hazel model type - we'll use a string to represent the serialized board state
  let hazel_model_t: TermBase.Typ.t = Typ.temp(Atom(String));

  // Serialize the board to a string for Hazel
  let serialize_board = (model: model_t) => {
    // Simple serialization: each square is represented by a 2-character code
    // First character: piece type (p=pawn, r=rook, n=knight, b=bishop, q=queen, k=king, e=empty)
    // Second character: color (w=white, b=black, e=empty)
    let square_to_string = square => {
      switch (square) {
      | None => "ee" // empty square
      | Some(piece) =>
        let type_char =
          switch (piece.piece_type) {
          | "pawn" => "p"
          | "rook" => "r"
          | "knight" => "n"
          | "bishop" => "b"
          | "queen" => "q"
          | "king" => "k"
          | _ => "e"
          };

        let color_char =
          switch (piece.color) {
          | "white" => "w"
          | "black" => "b"
          | _ => "e"
          };

        type_char ++ color_char;
      };
    };

    // Convert each row to a string
    let row_to_string = row => {
      row |> List.map(square_to_string) |> String.concat("");
    };

    // Convert the board to a string
    let board_str =
      model.board |> List.map(row_to_string) |> String.concat("|");

    // Add turn and selected square info
    let turn_str = model.turn;
    let selected_str =
      switch (model.selected_square) {
      | None => "none"
      | Some((row, col)) => string_of_int(row) ++ "," ++ string_of_int(col)
      };

    board_str ++ ";" ++ turn_str ++ ";" ++ selected_str;
  };

  // Deserialize a string back to a board
  let deserialize_board = (str: string) =>
    // Split the string into board, turn, and selected parts
    try({
      let parts = String.split_on_char(';', str);
      let board_str = List.nth(parts, 0);
      let turn_str = List.nth(parts, 1);
      let selected_str = List.nth(parts, 2);

      // Parse the board
      let rows = String.split_on_char('|', board_str);
      let board =
        rows
        |> List.map(row_str => {
             // Each square is 2 characters
             let rec parse_row = (str, acc) =>
               if (String.length(str) == 0) {
                 List.rev(acc);
               } else {
                 let type_char = String.sub(str, 0, 1);
                 let color_char = String.sub(str, 1, 1);

                 let square =
                   if (type_char == "e" && color_char == "e") {
                     None;
                   } else {
                     let piece_type =
                       switch (type_char) {
                       | "p" => "pawn"
                       | "r" => "rook"
                       | "n" => "knight"
                       | "b" => "bishop"
                       | "q" => "queen"
                       | "k" => "king"
                       | _ => "unknown"
                       };

                     let color =
                       switch (color_char) {
                       | "w" => "white"
                       | "b" => "black"
                       | _ => "unknown"
                       };

                     Some({
                       piece_type,
                       color,
                     });
                   };

                 parse_row(
                   String.sub(str, 2, String.length(str) - 2),
                   [square, ...acc],
                 );
               };

             parse_row(row_str, []);
           });

      // Parse the selected square
      let selected_square =
        if (selected_str == "none") {
          None;
        } else {
          let coords = String.split_on_char(',', selected_str);
          let row = int_of_string(List.nth(coords, 0));
          let col = int_of_string(List.nth(coords, 1));
          Some((row, col));
        };

      Some({
        board,
        turn: turn_str,
        selected_square,
      });
    }) {
    | _ => None // Return None if parsing fails
    };

  // Convert model to Hazel expression
  let model_to_hazel: model_t => model_exp =
    (model: model_t) => {
      let serialized = serialize_board(model);
      DHExp.fresh(Atom(String(serialized)));
    };

  // Convert Hazel expression to model
  let model_from_hazel: model_exp => option(model_t) =
    (expr: model_exp) => {
      switch (expr.term) {
      | Atom(String(serialized)) => deserialize_board(serialized)
      | _ => None
      };
    };

  // Expansion type is a string
  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(String));

  // Expansion function - convert the board to FEN notation (standard chess position format)
  let expand: model_t => expansion_t =
    (model: model_t) => {
      // Convert to Forsyth-Edwards Notation (FEN)
      let board_to_fen = board => {
        let row_to_fen = row => {
          let (fen, empty_count) =
            List.fold_left(
              ((acc, empty), square) => {
                switch (square) {
                | None => (acc, empty + 1)
                | Some(piece) =>
                  let piece_char =
                    switch (piece.piece_type) {
                    | "pawn" => "p"
                    | "rook" => "r"
                    | "knight" => "n"
                    | "bishop" => "b"
                    | "queen" => "q"
                    | "king" => "k"
                    | _ => "?"
                    };

                  // Uppercase for white pieces
                  let piece_char =
                    if (piece.color == "white") {
                      String.uppercase_ascii(piece_char);
                    } else {
                      piece_char;
                    };

                  // Add empty count if needed
                  let new_acc =
                    if (empty > 0) {
                      acc ++ string_of_int(empty) ++ piece_char;
                    } else {
                      acc ++ piece_char;
                    };

                  (new_acc, 0);
                }
              },
              ("", 0),
              row,
            );

          // Add any trailing empty squares
          if (empty_count > 0) {
            fen ++ string_of_int(empty_count);
          } else {
            fen;
          };
        };

        board |> List.map(row_to_fen) |> String.concat("/");
      };

      let fen = board_to_fen(model.board);

      // Add turn
      let fen = fen ++ " " ++ String.sub(model.turn, 0, 1);

      // Add castling, en passant, halfmove, and fullmove (simplified)
      fen ++ " KQkq - 0 1";
    };

  // Convert expansion to Hazel expression
  let expand_to_hazel: expansion_t => expansion_exp =
    (fen: expansion_t) => DHExp.fresh(Atom(String(fen)));

  // Helper function to safely get a square from the board
  let get_square = (board, row, col) =>
    try({
      let row_list = List.nth(board, row);
      try(Some(List.nth(row_list, col))) {
      | _ => None
      };
    }) {
    | _ => None
    };

  // Update function to handle actions
  let update: (action_t, model_t) => model_t =
    (action: action_t, model: model_t) => {
      switch (action) {
      | SelectSquare(row, col) =>
        // Check if the square contains a piece of the current player's color
        switch (get_square(model.board, row, col)) {
        | Some(Some(piece)) when piece.color == model.turn => {
            // Select this square
            ...model,
            selected_square: Some((row, col)),
          }
        | _ =>
          // If a square is already selected, try to move there
          switch (model.selected_square) {
          | Some((from_row, from_col)) =>
            // Get the piece from the selected square
            let piece_opt = get_square(model.board, from_row, from_col);

            switch (piece_opt) {
            | Some(Some(piece)) =>
              // Update the board
              let new_board =
                List.mapi(
                  (r, row_list) =>
                    List.mapi(
                      (c, square) =>
                        if (r == from_row && c == from_col) {
                          None; // Remove piece from original position
                        } else if (r == row && c == col) {
                          Some
                            (piece); // Place piece at new position
                        } else {
                          square; // Keep other squares unchanged
                        },
                      row_list,
                    ),
                  model.board,
                );

              // Switch turns
              let new_turn = model.turn == "white" ? "black" : "white";

              {
                board: new_board,
                turn: new_turn,
                selected_square: None,
              };
            | _ => model // No piece at selected square
            };
          | None => model // No square selected, do nothing
          }
        }
      | MoveSelectedPiece(row, col) =>
        // Move the piece from the selected square to the target square
        switch (model.selected_square) {
        | Some((from_row, from_col)) =>
          // Get the piece from the selected square
          let piece_opt = get_square(model.board, from_row, from_col);

          switch (piece_opt) {
          | Some(Some(piece)) =>
            // Update the board
            let new_board =
              List.mapi(
                (r, row_list) =>
                  List.mapi(
                    (c, square) =>
                      if (r == from_row && c == from_col) {
                        None; // Remove piece from original position
                      } else if (r == row && c == col) {
                        Some
                          (piece); // Place piece at new position
                      } else {
                        square; // Keep other squares unchanged
                      },
                    row_list,
                  ),
                model.board,
              );

            // Switch turns
            let new_turn = model.turn == "white" ? "black" : "white";

            {
              board: new_board,
              turn: new_turn,
              selected_square: None,
            };
          | _ => model // No piece at selected square
          };
        | None => model // No square selected, do nothing
        }
      | ResetBoard =>
        // Reset to initial state
        model_default
      };
    };

  // Hazel action type
  let hazel_action_t: TermBase.Typ.t =
    Sum([
      Variant(
        "SelectSquare",
        [],
        Some(
          Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(Int))]) |> Typ.fresh,
        ),
      ),
      Variant(
        "MoveSelectedPiece",
        [],
        Some(
          Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(Int))]) |> Typ.fresh,
        ),
      ),
      Variant("ResetBoard", [], None),
    ])
    |> Typ.fresh;

  // Convert action to Hazel expression
  let action_to_hazel: action_t => action_exp =
    (action: action_t) =>
      switch (action) {
      | SelectSquare(row, col) =>
        let row_expr = DHExp.fresh(Atom(Int(Bigint.of_int(row))));
        let col_expr = DHExp.fresh(Atom(Int(Bigint.of_int(col))));
        let tuple_expr = DHExp.fresh(Tuple([row_expr, col_expr]));

        Ap(
          Forward,
          Constructor(
            "SelectSquare",
            Some(
              Some(
                Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(Int))])
                |> Typ.fresh,
              ),
            ),
          )
          |> DHExp.fresh,
          tuple_expr,
        )
        |> DHExp.fresh;
      | MoveSelectedPiece(row, col) =>
        let row_expr = DHExp.fresh(Atom(Int(Bigint.of_int(row))));
        let col_expr = DHExp.fresh(Atom(Int(Bigint.of_int(col))));
        let tuple_expr = DHExp.fresh(Tuple([row_expr, col_expr]));

        Ap(
          Forward,
          Constructor(
            "MoveSelectedPiece",
            Some(
              Some(
                Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(Int))])
                |> Typ.fresh,
              ),
            ),
          )
          |> DHExp.fresh,
          tuple_expr,
        )
        |> DHExp.fresh;
      | ResetBoard => Constructor("ResetBoard", None) |> DHExp.fresh
      };

  // Convert Hazel expression to action
  let action_from_hazel: action_exp => option(action_t) =
    (expr: action_exp) =>
      switch (expr.term) {
      | Ap(
          Forward,
          {term: Constructor("SelectSquare", _), _},
          {
            term:
              Tuple([
                {term: Atom(Int(row_big)), _},
                {term: Atom(Int(col_big)), _},
              ]),
            _,
          },
        ) =>
        let row = int_of_string(Bigint.to_string(row_big));
        let col = int_of_string(Bigint.to_string(col_big));
        Some(SelectSquare(row, col));
      | Ap(
          Forward,
          {term: Constructor("MoveSelectedPiece", _), _},
          {
            term:
              Tuple([
                {term: Atom(Int(row_big)), _},
                {term: Atom(Int(col_big)), _},
              ]),
            _,
          },
        ) =>
        let row = int_of_string(Bigint.to_string(row_big));
        let col = int_of_string(Bigint.to_string(col_big));
        Some(MoveSelectedPiece(row, col));
      | Constructor("ResetBoard", _) => Some(ResetBoard)
      | _ => None
      };

  // View function to render the chess board
  let view = (model: model_t, send_action) => {
    // Helper to get piece Unicode character
    let piece_to_unicode = piece => {
      let symbol =
        switch (piece.piece_type) {
        | "pawn" => piece.color == "white" ? "♙" : "♟"
        | "rook" => piece.color == "white" ? "♖" : "♜"
        | "knight" => piece.color == "white" ? "♘" : "♞"
        | "bishop" => piece.color == "white" ? "♗" : "♝"
        | "queen" => piece.color == "white" ? "♕" : "♛"
        | "king" => piece.color == "white" ? "♔" : "♚"
        | _ => "?"
        };
      symbol;
    };

    // Render the board
    let board_element =
      Node.div(
        ~attrs=[
          Attr.create(
            "style",
            "display: grid; grid-template-columns: repeat(8, 40px); grid-template-rows: repeat(8, 40px); gap: 0; border: 2px solid black; width: 320px; height: 320px;",
          ),
        ],
        // Create all 64 squares
        List.flatten(
          List.mapi(
            (row, row_squares) =>
              List.mapi(
                (col, square) => {
                  // Determine square color (light or dark)
                  let is_light = (row + col) mod 2 == 0;
                  let bg_color = is_light ? "#f0d9b5" : "#b58863";

                  // Check if this square is selected
                  let is_selected =
                    switch (model.selected_square) {
                    | Some((sel_row, sel_col)) =>
                      sel_row == row && sel_col == col
                    | None => false
                    };

                  // Add highlight for selected square
                  let bg_color = is_selected ? "#aaffaa" : bg_color;

                  // Render the square with piece if present
                  Node.div(
                    ~attrs=[
                      Attr.create(
                        "style",
                        "background-color: "
                        ++ bg_color
                        ++ "; display: flex; justify-content: center; align-items: center; font-size: 30px; cursor: pointer;",
                      ),
                      Attr.on_click(_ => {
                        send_action(SelectSquare(row, col))
                      }),
                    ],
                    [
                      // Render piece if present
                      switch (square) {
                      | Some(piece) => Node.text(piece_to_unicode(piece))
                      | None => Node.text("")
                      },
                    ],
                  );
                },
                row_squares,
              ),
            model.board,
          ),
        ),
      );

    // Render turn indicator and reset button
    let controls =
      Node.div(
        ~attrs=[
          Attr.create(
            "style",
            "margin-top: 10px; display: flex; justify-content: space-between; align-items: center;",
          ),
        ],
        [
          Node.div(
            ~attrs=[Attr.create("style", "font-weight: bold;")],
            [Node.text(model.turn ++ "'s turn")],
          ),
          Node.button(
            ~attrs=[Attr.on_click(_ => {send_action(ResetBoard)})],
            [Node.text("Reset Board")],
          ),
        ],
      );

    // Combine board and controls
    Node.div([board_element, controls]);
  };

  // Size specification
  let size: ProjectorCore.Shape.t =
    ProjectorCore.Shape.{
      vertical: Block(20),
      horizontal: 40,
    };
};

let livelits: list(raw_livelit) =
  [
    (module Slider),
    (module Emotion),
    (module Js),
    (module BoundedSlider),
    (module ChessBoard),
  ]
  |> List.map(raw_of_builtin);
