open Util;
open Js_of_ocaml;
open Language;

/**
 * Types for communication protocol between external apps and Hazel
 * Message types matching hazel-protocol.ts
 */

/* Messages sent from child (React app) to parent (Hazel) */
[@deriving (show({with_path: false}), sexp, yojson)]
type to_hazel_message =
  | Ready({id: Id.t})
  | SetSyntax({
      id: Id.t,
      codec: string,
      value: string,
    })
  | Resize({
      id: Id.t,
      width: int,
      height: int,
    });

/* Messages sent from parent (Hazel) to child (React app) */
[@deriving (show({with_path: false}), sexp, yojson)]
type from_hazel_message =
  | Init({
      id: Id.t,
      value: string,
    })
  | Constraints({
      id: Id.t,
      max_width: int,
      max_height: int,
      min_width: option(int),
      min_height: option(int),
    });

let id_of = (msg: to_hazel_message): Id.t =>
  switch (msg) {
  | Ready(m) => m.id
  | SetSyntax(m) => m.id
  | Resize(m) => m.id
  };

let get_string = (obj: Js.t(_), key: string): option(string) =>
  try(Some(Js.to_string(Js.Unsafe.get(obj, key)))) {
  | _ => None
  };

let get_int = (obj: Js.t(_), key: string): option(int) =>
  try(Some(int_of_float(Js.float_of_number(Js.Unsafe.get(obj, key))))) {
  | _ => None
  };

/* Parse JavaScript message data directly into typed to_hazel_message */
let parse_to_hazel_message = (data: Js.t(_)): option(to_hazel_message) =>
  switch (get_string(data, "id") |> OptUtil.and_then(Id.of_string)) {
  | Some(id) =>
    switch (get_string(data, "type")) {
    | Some("ready") => Some(Ready({id: id}))
    | Some("setSyntax") =>
      switch (get_string(data, "codec"), get_string(data, "value")) {
      | (Some(codec), Some(value)) =>
        Some(
          SetSyntax({
            id,
            codec,
            value,
          }),
        )
      | _ => None
      }
    | Some("resize") =>
      switch (get_int(data, "width"), get_int(data, "height")) {
      | (Some(width), Some(height)) =>
        Some(
          Resize({
            id,
            width,
            height,
          }),
        )
      | _ =>
        prerr_endline(
          "parse_to_hazel_message: resize message missing width/height",
        );
        None;
      }
    | Some(action) =>
      prerr_endline(
        "parse_to_hazel_message: unknown message type: " ++ action,
      );
      None;
    | None =>
      prerr_endline("parse_to_hazel_message: message missing type");
      None;
    }
  | None =>
    prerr_endline("parse_to_hazel_message: missing id or wrong id format");
    None;
  };

/* Convert from_hazel_message to JavaScript object */
let from_hazel_to_js = (msg: from_hazel_message): Js.t(_) => {
  let js_obj = Js.Unsafe.obj([||]);
  let set_attr = (key, value) =>
    Js.Unsafe.set(js_obj, key, Js.string(value));
  let set_num = (key, value) =>
    Js.Unsafe.set(js_obj, key, Js.number_of_float(float_of_int(value)));
  switch (msg) {
  | Init({id, value}) =>
    set_attr("type", "init");
    set_attr("id", Id.to_string(id));
    set_attr("value", value);
  | Constraints({id, max_width, max_height, min_width, min_height}) =>
    set_attr("type", "constraints");
    set_attr("id", Id.to_string(id));
    set_num("maxWidth", max_width);
    set_num("maxHeight", max_height);
    switch (min_width) {
    | Some(w) => set_num("minWidth", w)
    | None => ()
    };
    switch (min_height) {
    | Some(h) => set_num("minHeight", h)
    | None => ()
    };
  };
  js_obj;
};

/* JSON Codec for converting between Hazel expressions and Yojson */
module JsonCodec = {
  /* Stage 5: Support base types (int, float, string, bool), lists, tuples (plain and labeled), and ADTs; stub out other types with clear errors */

  let rec exp_to_yojson =
          (exp: Language.Exp.t): result(Yojson.Safe.t, string) => {
    /* Helper functions for tuple processing */
    let rec check_tuple_type = (remaining, first_is_labeled) =>
      switch (remaining) {
      | [] => Ok(first_is_labeled)
      | [hd, ...tl] =>
        let is_labeled =
          switch (Exp.term_of(hd)) {
          | TupLabel(_, _) => true
          | _ => false
          };
        switch (first_is_labeled) {
        | None => check_tuple_type(tl, Some(is_labeled))
        | Some(expected) =>
          if (is_labeled == expected) {
            check_tuple_type(tl, Some(expected));
          } else {
            Error("Mixed labeled/unlabeled tuples not supported");
          }
        };
      };

    let rec convert_labeled_elements = (acc, remaining) =>
      switch (remaining) {
      | [] => Ok(List.rev(acc))
      | [hd, ...tl] =>
        switch (Exp.term_of(hd)) {
        | TupLabel(label_exp, value) =>
          switch (Exp.term_of(label_exp)) {
          | Label(label) =>
            switch (exp_to_yojson(value)) {
            | Ok(json_elem) =>
              convert_labeled_elements([(label, json_elem), ...acc], tl)
            | Error(msg) => Error(msg)
            }
          | _ => Error("Invalid label in labeled tuple")
          }
        | _ => Error("Expected labeled element in labeled tuple")
        }
      };

    let rec convert_unlabeled_elements = (acc, remaining, index) =>
      switch (remaining) {
      | [] => Ok(List.rev(acc))
      | [hd, ...tl] =>
        switch (exp_to_yojson(hd)) {
        | Ok(json_elem) =>
          convert_unlabeled_elements(
            [(string_of_int(index), json_elem), ...acc],
            tl,
            index + 1,
          )
        | Error(msg) => Error(msg)
        }
      };

    switch (exp.term) {
    | Atom(Int(i)) =>
      switch (Bigint.to_int(i)) {
      | Some(int_val) => Ok(`Int(int_val))
      | None => Error("Integer too large to convert to JSON")
      }
    | Atom(Float(f)) => Ok(`Float(f))
    | Atom(String(s)) => Ok(`String(s))
    | Atom(Bool(b)) => Ok(`Bool(b))
    | ListLit(elements) =>
      /* Convert each element to JSON and collect results */
      let rec convert_elements = (acc, remaining) =>
        switch (remaining) {
        | [] => Ok(List.rev(acc))
        | [hd, ...tl] =>
          switch (exp_to_yojson(hd)) {
          | Ok(json_elem) => convert_elements([json_elem, ...acc], tl)
          | Error(msg) => Error(msg)
          }
        };
      switch (convert_elements([], elements)) {
      | Ok(json_elements) => Ok(`List(json_elements))
      | Error(msg) => Error(msg)
      };

    | Tuple(elements) =>
      /* Check if tuple is all-labeled or all-unlabeled */

      switch (check_tuple_type(elements, None)) {
      | Error(msg) => Error(msg)
      | Ok(Some(true)) =>
        /* All labeled */
        switch (convert_labeled_elements([], elements)) {
        | Ok(pairs) => Ok(`Assoc(pairs))
        | Error(msg) => Error(msg)
        }
      | Ok(Some(false)) =>
        /* All unlabeled */
        switch (convert_unlabeled_elements([], elements, 0)) {
        | Ok(pairs) => Ok(`Assoc(pairs))
        | Error(msg) => Error(msg)
        }
      | Ok(None) =>
        /* Empty tuple case should not reach here */
        Ok(`Assoc([]))
      }
    | Parens(inner) =>
      /* Check what's inside the parens */
      switch (inner.term) {
      | Tuple(elements) =>
        /* Tuple wrapped in parens: could be plain or labeled */

        /* Check if tuple is all-labeled or all-unlabeled */
        switch (check_tuple_type(elements, None)) {
        | Error(msg) => Error(msg)
        | Ok(Some(true)) =>
          /* All labeled */
          switch (convert_labeled_elements([], elements)) {
          | Ok(pairs) => Ok(`Assoc(pairs))
          | Error(msg) => Error(msg)
          }
        | Ok(Some(false)) =>
          /* All unlabeled */
          switch (convert_unlabeled_elements([], elements, 0)) {
          | Ok(pairs) => Ok(`Assoc(pairs))
          | Error(msg) => Error(msg)
          }
        | Ok(None) => Error("Empty tuple handling error")
        }
      | _ =>
        /* Non-tuple parens: just process the inner expression */
        exp_to_yojson(inner)
      }
    | Constructor(name, _) =>
      /* ADT Constructor: nullary constructor -> {"t": "Name"} */
      Ok(`Assoc([("t", `String(name))]))
    | UnOp(Float(Minus), {term: Atom(Float(f)), _}) => Ok(`Float(-. f))
    | UnOp(Int(Minus), {term: Atom(Int(i)), _}) =>
      switch (Bigint.to_int(i)) {
      | Some(int_val) => Ok(`Int(- int_val))
      | None => Error("Integer too large to convert to JSON")
      }
    | UnOp(_, _) => Error("Unary operations not yet supported in JsonCodec")
    | Ap(_, {term: Constructor(name, _), _}, value) =>
      /* ADT Constructor application: Constructor(value) -> {"t": "Constructor", "v": value} */
      switch (exp_to_yojson(value)) {
      | Ok(json_value) =>
        Ok(`Assoc([("t", `String(name)), ("v", json_value)]))
      | Error(msg) => Error(msg)
      }
    | _ => Error("Unsupported expression type for JsonCodec")
    };
  };

  let any_to_yojson = (any: Language.Any.t): result(Yojson.Safe.t, string) => {
    switch (any) {
    | Exp(exp) => exp_to_yojson(exp)
    | _ => Error("Only Exp terms are supported in JsonCodec")
    };
  };

  let rec yojson_to_exp =
          (json: Yojson.Safe.t): result(Language.Exp.t, string) => {
    switch (json) {
    | `Int(i) when i < 0 =>
      try(
        Ok(
          Language.IdTagged.FreshGrammar.Exp.un_op(
            Int(Minus),
            Language.IdTagged.FreshGrammar.Exp.big_int(Bigint.of_int(- i)),
          ),
        )
      ) {
      | _ => Error("Failed to convert int to Bigint")
      }
    | `Int(i) =>
      try({
        let _big_int = Bigint.of_int(i);
        Ok(Language.IdTagged.FreshGrammar.Exp.big_int(Bigint.of_int(i)));
      }) {
      | _ => Error("Failed to convert int to Bigint")
      }
    | `Float(f) when f < 0.0 =>
      Ok(
        Language.IdTagged.FreshGrammar.Exp.un_op(
          Float(Minus),
          Language.IdTagged.FreshGrammar.Exp.float(-. f),
        ),
      )
    | `Float(f) => Ok(Language.IdTagged.FreshGrammar.Exp.float(f))
    | `String(s) => Ok(Language.IdTagged.FreshGrammar.Exp.string(s))
    | `Bool(b) => Ok(Language.IdTagged.FreshGrammar.Exp.bool(b))
    | `List(json_elements) =>
      /* Convert each JSON element to a Hazel expression */
      let rec convert_elements = (acc, remaining) =>
        switch (remaining) {
        | [] => Ok(List.rev(acc))
        | [hd, ...tl] =>
          switch (yojson_to_exp(hd)) {
          | Ok(exp_elem) => convert_elements([exp_elem, ...acc], tl)
          | Error(msg) => Error(msg)
          }
        };
      switch (convert_elements([], json_elements)) {
      | Ok(exp_elements) =>
        Ok(Language.IdTagged.FreshGrammar.Exp.list_lit(exp_elements))
      | Error(msg) => Error(msg)
      };
    | `Assoc(pairs) =>
      /* JSON object: could be ADT constructor or tuple */
      /* First check if it's an ADT constructor */
      switch (List.assoc_opt("t", pairs)) {
      | Some(`String(constructor_name)) =>
        /* ADT Constructor */
        switch (List.assoc_opt("v", pairs)) {
        | Some(json_value) =>
          /* Constructor with argument: {"t": "Some", "v": 42} */
          switch (yojson_to_exp(json_value)) {
          | Ok(value_exp) =>
            /* Unwrap unnecessary parens around tuples when used as constructor arguments */
            let unwrapped_exp =
              switch (Exp.term_of(value_exp)) {
              | Parens({term: Tuple(_), _} as inner_tuple) => inner_tuple
              | _ => value_exp
              };
            Ok(
              Language.IdTagged.FreshGrammar.Exp.ap(
                Forward,
                Language.IdTagged.FreshGrammar.Exp.constructor(
                  constructor_name,
                  None,
                ),
                unwrapped_exp,
              ),
            );
          | Error(msg) => Error(msg)
          }
        | None =>
          /* Nullary constructor: {"t": "None"} */
          if (List.length(pairs) == 1) {
            Ok(
              Language.IdTagged.FreshGrammar.Exp.constructor(
                constructor_name,
                None,
              ),
            );
          } else {
            Error(
              "Invalid ADT constructor: extra fields besides 't' for nullary constructor",
            );
          }
        }
      | Some(_) =>
        Error("Invalid ADT constructor: 't' field must be a string")
      | None =>
        /* Not an ADT constructor, treat as tuple */
        if (List.length(pairs) == 0) {
          /* Empty tuple */
          Ok(Language.IdTagged.FreshGrammar.Exp.tuple([]));
        } else {
          let all_numeric =
            List.for_all(
              ((key, _value)) =>
                /* Check if all keys are numeric strings (0, 1, 2, ...) */
                try({
                  let index = int_of_string(key);
                  index >= 0;
                }) {
                | _ => false
                },
              pairs,
            );

          let any_numeric =
            List.exists(
              ((key, _value)) =>
                try({
                  let _ = int_of_string(key);
                  true;
                }) {
                | _ => false
                },
              pairs,
            );

          /* Reject mixed numeric/non-numeric keys */
          if (any_numeric && !all_numeric) {
            Error("Mixed labeled/unlabeled tuples not supported");
          } else if (all_numeric) {
            /* Plain tuple: sort by numeric key and create Parens(Tuple(...)) */
            let sorted_pairs =
              List.sort(
                ((k1, _), (k2, _)) => {
                  let i1 = int_of_string(k1);
                  let i2 = int_of_string(k2);
                  compare(i1, i2);
                },
                pairs,
              );
            let rec convert_plain_elements = (acc, remaining) =>
              switch (remaining) {
              | [] => Ok(List.rev(acc))
              | [(_key, value), ...tl] =>
                switch (yojson_to_exp(value)) {
                | Ok(exp_elem) =>
                  convert_plain_elements([exp_elem, ...acc], tl)
                | Error(msg) => Error(msg)
                }
              };
            switch (convert_plain_elements([], sorted_pairs)) {
            | Ok(elements) =>
              Ok(
                Language.IdTagged.FreshGrammar.Exp.parens(
                  Language.IdTagged.FreshGrammar.Exp.tuple(elements),
                ),
              )
            | Error(msg) => Error(msg)
            };
          } else {
            /* Labeled tuple: create tuple with TupLabel elements */
            let rec convert_labeled_elements = (acc, remaining) =>
              switch (remaining) {
              | [] => Ok(List.rev(acc))
              | [(key, value), ...tl] =>
                switch (yojson_to_exp(value)) {
                | Ok(exp_elem) =>
                  let labeled_elem =
                    Language.IdTagged.FreshGrammar.Exp.tup_label(
                      Language.IdTagged.FreshGrammar.Exp.label(key),
                      exp_elem,
                    );
                  convert_labeled_elements([labeled_elem, ...acc], tl);
                | Error(msg) => Error(msg)
                }
              };
            switch (convert_labeled_elements([], pairs)) {
            | Ok(elements) =>
              Ok(
                Language.IdTagged.FreshGrammar.Exp.parens(
                  Language.IdTagged.FreshGrammar.Exp.tuple(elements),
                ),
              )
            | Error(msg) => Error(msg)
            };
          };
        }
      }
    | `Null => Error("Null values not supported in JsonCodec")
    | `Tuple(_) => Error("Tuple values not yet supported in JsonCodec")
    | `Intlit(_) => Error("Intlit values not yet supported in JsonCodec")
    | `Variant(_) => Error("Variant values not yet supported in JsonCodec")
    };
  };

  let yojson_to_any = (json: Yojson.Safe.t): result(Language.Any.t, string) =>
    switch (yojson_to_exp(json)) {
    | Ok(exp) => Ok(Exp(exp))
    | Error(msg) => Error(msg)
    };
};
