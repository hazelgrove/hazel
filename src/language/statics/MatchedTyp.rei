/* Matched-type judgments (▷). The request determines the component type;
   the mandatory handling policy determines how a failed judgment is used. */

/* The expected type and the expression's required outer structure, with
   unknown components. This does not replace the rule's actual synthesis. */
type failure = {
  ana: Typ.t,
  syn: Typ.t,
};

/* Components in expected order. Alignment identifies the original element
   and any label to introduce. Syntax construction stays with the caller. */
type product =
  pri {
    types: list(Typ.t),
    alignment: list((int, option(string))),
  };

type request('a) =
  | Arrow: request((Typ.t, Typ.t))
  | Poly: request((option(TPat.t), Typ.t))
  | List: request(Typ.t)
  | Product(list(option(string))): request(product)
  | Label: request((Typ.t, Typ.t))
  /* A scalar is one argument; an unknown can stand for any requested arity. */
  | Args(int): request(list(Typ.t));

type handling('a, 'result) =
  | Optional: handling('a, option('a))
  | Report(failure => 'mark): handling('a, ('a, list('mark)));

/* Optional rejects a shape it cannot decompose. Report always supplies
   recovery components and invokes the diagnostic constructor only on a
   concrete mismatch. Neither mode reports an unknown as an error.
   SynSwitch propagates to the components. Other unknowns produce Internal
   recovery components in Report mode and fail Optional decomposition (apart
   from Args, whose request supplies the arity).
   Diagnostics are generic: matching does not depend on Mark. */
let match_type:
  (Ctx.t, request('a), handling('a, 'result), Typ.t) => 'result;
