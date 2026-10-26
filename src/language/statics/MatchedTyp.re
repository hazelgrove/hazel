type failure = {
  ana: Typ.t,
  syn: Typ.t,
};

type product = {
  types: list(Typ.t),
  alignment: list((int, option(string))),
};

type request('a) =
  | Arrow: request((Typ.t, Typ.t))
  | Poly: request((option(TPat.t), Typ.t))
  | List: request(Typ.t)
  | Product(list(option(string))): request(product)
  | Label: request((Typ.t, Typ.t))
  | Args(int): request(list(Typ.t));

type handling('a, 'result) =
  | Optional: handling('a, option('a))
  | Report(failure => 'mark): handling('a, ('a, list('mark)));

/* A failed judgment only establishes the outer structure of the expression,
   not its component types. Report that structure using ordinary unknowns so
   callers can use their existing expectation-mismatch diagnostic. */
let required_type: type a. request(a) => Typ.t =
  request => {
    let unknown = Unknown(Internal) |> Typ.temp;
    let term: Typ.term =
      switch (request) {
      | Arrow => Arrow(unknown, unknown)
      | Poly => Poly(TPat.temp(EmptyHole), unknown)
      | List => List(unknown)
      | Product(labels) =>
        Prod(
          List.map(
            label =>
              switch (label) {
              | None => unknown
              | Some(name) =>
                Typ.temp(TupLabel(Typ.temp(Label(name)), unknown))
              },
            labels,
          ),
        )
      | Label => TupLabel(unknown, unknown)
      | Args(n) => Prod(List.init(n, _ => unknown))
      };
    Typ.temp(term);
  };

/* Recovery is determined by the request, never by the diagnostic policy. */
let unknown_parts: type a. (request(a), TermBase.type_provenance) => a =
  (request, provenance) => {
    let unknown = Unknown(provenance) |> Typ.temp;
    switch (request) {
    | Arrow => (unknown, unknown)
    | Label => (unknown, unknown)
    | Poly => (None, unknown)
    | List => unknown
    | Product(labels) => {
        types: List.map(_ => unknown, labels),
        alignment: List.mapi((i, _) => (i, None), labels),
      }
    | Args(n) => List.init(n, _ => unknown)
    };
  };

let align_product = (labels, types) => {
  let indexed = List.mapi((i, label) => (label, i), labels);
  let aligned =
    LabeledTuple.rearrange(
      Typ.match_tup_label,
      ((label, i)) => Option.map(name => (name, (label, i)), label),
      types,
      indexed,
      (name, (_, i)) => (Some(name), i),
    );
  let original = Array.of_list(labels);
  {
    types,
    alignment:
      List.map(
        ((label, i)) => (i, original[i] == None ? label : None),
        aligned,
      ),
  };
};

/* Choose the components to extract (Arrow: input/output pair; List: element
   type) and how to handle failure. These choices determine the return type:
     match_type(ctx, Arrow, Optional, ty) -> Some((input, output)) or None
     match_type(ctx, Arrow, Report(make_mark), ty) -> ((input, output), marks)
   Report recovers unknown components on failure; make_mark({ana, syn}) chooses
   the diagnostic for a concrete mismatch. Attach the returned marks in the
   statics rule. Unknown inputs never produce a diagnostic. The GADT type
   parameters enforce these return types; callers need no special syntax. */
let rec match_type:
  type a result. (Ctx.t, request(a), handling(a, result), Typ.t) => result =
  (ctx, request, handling, typ) => {
    let head = Typ.weak_head_normalize(ctx, typ);
    switch (head.term) {
    | Parens(inner) => match_type(ctx, request, handling, inner)
    | _ =>
      let parts: option(a) =
        switch (request, head.term) {
        | (Arrow, Arrow(input, output)) => Some((input, output))
        | (Poly, Poly(binder, body)) => Some((Some(binder), body))
        | (List, List(element)) => Some(element)
        | (Label, TupLabel(label, value)) => Some((label, value))
        | (Product(labels), Prod(types))
            when List.length(labels) == List.length(types) =>
          Some(align_product(labels, types))
        | (Args(n), Prod(types)) when List.length(types) == n =>
          Some(types)
        | (Args(_), Prod(_)) => None
        | (Args(1), _) => Some([typ])
        | (Args(n), Unknown(_)) =>
          Some(List.init(n, _ => Unknown(Internal) |> Typ.temp))
        | (_, Unknown(SynSwitch)) =>
          Some(unknown_parts(request, SynSwitch))
        | _ => None
        };
      switch (handling) {
      | Optional => parts
      | Report(mark) =>
        switch (parts, head.term) {
        | (Some(parts), _) => (parts, [])
        /* Unknowns are not concrete failures. Optional callers can still
           distinguish them from a successful decomposition. */
        | (None, Unknown(_)) => (unknown_parts(request, Internal), [])
        | (None, _) => (
            unknown_parts(request, Internal),
            [
              mark({
                ana: typ,
                syn: required_type(request),
              }),
            ],
          )
        }
      };
    };
  };
