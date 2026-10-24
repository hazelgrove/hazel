open Util;

/* The agent's open entries: the ones its view shows in full below the map
   (CompositionView). Kept by id, so a rename keeps them open, with the
   path each had when last seen: a reload re-mints ids, and the path finds
   the entry again. */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry = {
    id: Id.t,
    path: string,
  };

  let of_outline = (x: Outline.entry): entry => {
    id: x.id,
    path: Outline.path_string(x),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type current = {opened: list(entry)};

  /* before entries: open bindings as path strings */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type legacy = {expanded_paths: list(string)};

  [@deriving show({with_path: false})]
  type t = current;

  let of_legacy = (l: legacy): t => {
    opened:
      List.map(
        path =>
          {
            id: Id.invalid,
            path,
          },
        l.expanded_paths,
      ),
  };

  let t_of_sexp = (s: Sexplib.Sexp.t): t =>
    try(current_of_sexp(s)) {
    | _ => of_legacy(legacy_of_sexp(s))
    };
  let sexp_of_t = sexp_of_current;
  let t_of_yojson = (j: Yojson.Safe.t): t =>
    try(current_of_yojson(j)) {
    | _ => of_legacy(legacy_of_yojson(j))
    };
  let yojson_of_t = yojson_of_current;
};

module Utils = {
  let init = (): Model.t => {opened: []};

  let ids = (m: Model.t): list(Id.t) =>
    List.map((e: Model.entry) => e.id, m.opened);

  /* the open set against [o]: each entry by its id, else by its last
     path (and its id taken anew); gone when neither finds it. Paths are
     the entries' current ones */
  let refresh = (o: Outline.t, m: Model.t): Model.t => {
    let found = (e: Model.entry) =>
      switch (Outline.find(o, e.id)) {
      | Some((_, x)) => Some(x)
      | None =>
        switch (Outline.resolve(o, e.path)) {
        | Ok(x) => Some(x)
        | Error(_) => None
        }
      };
    let opened =
      List.fold_left(
        (acc, e) =>
          switch (found(e)) {
          | Some(x) when !List.exists((y: Model.entry) => y.id == x.id, acc) => [
              Model.of_outline(x),
              ...acc,
            ]
          | _ => acc
          },
        [],
        m.opened,
      );
    {opened: List.rev(opened)};
  };

  let remove = (ids: list(Id.t), m: Model.t): Model.t => {
    opened: List.filter((e: Model.entry) => !List.mem(e.id, ids), m.opened),
  };
};

module Update = {
  /* as the tools ask: by path */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Expand(list(string))
    | Collapse(list(string))
    | CollapseAll;

  /* resolved against the program */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type resolved =
    | Open(list(Model.entry))
    | Close(list(Id.t))
    | CloseAll;

  /* [action]'s paths on [o]: every path must name an entry */
  let resolve = (o: Outline.t, action: action): result(resolved, string) => {
    let entries = paths => {
      let results = List.map(p => (p, Outline.resolve(o, p)), paths);
      switch (
        List.filter_map(
          ((_, r)) =>
            switch (r) {
            | Error(e) => Some(e)
            | Ok(_) => None
            },
          results,
        )
      ) {
      | [] =>
        Ok(
          List.filter_map(((_, r)) => Stdlib.Result.to_option(r), results),
        )
      | errors => Error(String.concat("\n", errors))
      };
    };
    switch (action) {
    | Expand([])
    | Collapse([]) => Error("Give at least one path.")
    | Expand(paths) =>
      Stdlib.Result.map(
        xs => Open(List.map(Model.of_outline, xs)),
        entries(paths),
      )
    | Collapse(paths) =>
      Stdlib.Result.map(
        xs => Close(List.map((x: Outline.entry) => x.id, xs)),
        entries(paths),
      )
    | CollapseAll => Ok(CloseAll)
    };
  };

  let update = (action: resolved, model: Model.t): Model.t =>
    switch (action) {
    | Open(es) => {
        opened:
          model.opened
          @ List.filter(
              (e: Model.entry) =>
                !List.exists((o: Model.entry) => o.id == e.id, model.opened),
              es,
            ),
      }
    | Close(ids) => Utils.remove(ids, model)
    | CloseAll => Utils.init()
    };
};
