open OptUtil.Syntax;

/* Pure JSON helpers shared by web and native code. The HTTP transport
 * (browser XHR / node https) lives in the web library (ApiHttp.re). */

module Json = {
  type t = Yojson.Safe.t;

  let t_of_yojson = json => json;
  let yojson_of_t = json => json;

  let t_of_sexp = sexp => {
    switch (sexp) {
    | Sexplib.Sexp.Atom(str) => Yojson.Safe.from_string(str)
    | _ => failwith("Invalid JSON sexp")
    };
  };

  let sexp_of_t = json => {
    Sexplib.Sexp.Atom(Yojson.Safe.to_string(json));
  };

  let pp = (fmt, json) => {
    Format.fprintf(fmt, "%s", Yojson.Safe.to_string(json));
  };

  let to_string = Yojson.Safe.to_string;
  let from_string = Yojson.Safe.from_string;

  let bool = (json: t): option(bool) =>
    switch (json) {
    | `Bool(b) => Some(b)
    | _ => None
    };
  let int = (json: t): option(int) =>
    switch (json) {
    | `Int(n) => Some(n)
    | _ => None
    };
  let float = (json: t): option(float) =>
    switch (json) {
    | `Float(f) => Some(f)
    | _ => None
    };
  let str = (json: t): option(string) =>
    switch (json) {
    | `String(str) => Some(str)
    | _ => None
    };
  let list = (json: t): option(list(t)) =>
    switch (json) {
    | `List(xs) => Some(xs)
    | _ => None
    };
  let get_kvs = (json: t): option(list((string, t))) =>
    switch (json) {
    | `Assoc(pairs) => Some(pairs)
    | _ => None
    };
  let dot = (key: string, json: t): option(t) => {
    let* pairs = get_kvs(json);
    List.assoc_opt(key, pairs);
  };

  module Parsers = {
    let int_field = (json: t, field: string): option(int) => {
      let* num = dot(field, json);
      int(num);
    };

    let get_json = (item: t, entity: string) => {
      switch (dot(entity, item)) {
      | Some(value) => value
      | None =>
        raise(
          Failure(
            "The entity " ++ entity ++ " must be provided for this action",
          ),
        )
      };
    };

    let get_string = (item: t, entity: string) => {
      switch (dot(entity, item)) {
      | Some(`String(entity)) => entity
      | _ =>
        raise(
          Failure(
            "A string for " ++ entity ++ " must be provided for this action",
          ),
        )
      };
    };

    let get_json_list = (item: t, entities: string) => {
      switch (dot(entities, item)) {
      | Some(`List(entities_list)) => entities_list
      | _ =>
        raise(
          Failure(
            "A list of " ++ entities ++ " must be provided for the action",
          ),
        )
      };
    };

    let get_string_list = (item: t, entities: string) => {
      let entities_list = get_json_list(item, entities);
      List.map(
        (entity: t) =>
          switch (entity) {
          | `String(entity) => entity
          | _ =>
            raise(
              Failure("Each " ++ entities ++ " in the list must be a string"),
            )
          },
        entities_list,
      );
    };
  };
};
