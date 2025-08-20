module Fresh = IdTagged.FreshGrammar;
open Fresh.Typ;
open BuiltinsUtil;
open Util;
let builtins: list(BuiltinsUtil.fn) = [
  {
    name: "group_by_label",
    arg: Prod([list(unknown(Internal)), unknown(Internal)]),
    ret: Unknown(Internal),
    imp:
      binary((d: DHExp.t, lab: DHExp.t) => {
        switch (lab.term) {
        | Label(name) =>
          let-unbox l = (ListLit, d);
          let unboxed:
            option(list((LabeledTuple.label, list(TermBase.exp_t)))) =
            List.map(
              e => {
                let-unbox (name, es) = (TupleElementPivot(name), e);
                Some((name, Tuple(es) |> Exp.fresh));
              },
              l,
            )
            |> Util.OptUtil.sequence
            |> Option.map(List.rev)  // We have to reverse because ListUtil.group_by
            |> Option.map(ListUtil.group_by(fst))
            |> Option.map(List.map(PairUtil.map_snd(List.map(snd))));
          Option.map(
            List.map(((name: string, es)) =>
              Fresh.Exp.(tup_label(label(name), list_lit(es)))
            ),
            unboxed,
          )
          |> Option.map(Exp.to_tuple);

        | _ => None
        }
      }),
    custom_statics: Some(GroupByLabel),
  },
  {
    name: "melt",
    arg: Unknown(Internal),
    ret:
      List(
        prod([
          tup_label(label("label"), string()),
          tup_label(label("value"), unknown(Internal)),
        ]),
      ),
    imp: (e: DHExp.t) => {
      open OptUtil.Syntax;
      let-unbox entries:
        list((option(string), Grammar.exp_t(IdTagged.IdTag.t))) = (
        LabeledTupleEntries,
        e,
      );
      let* entries: list((string, Grammar.exp_t(IdTagged.IdTag.t))) =
        OptUtil.traverse(
          fun
          | (Some(name), e) => Some((name, e))
          | _ => None,
          entries,
        );
      let unpivoted_entries =
        List.map(
          ((name, e)) =>
            IdTagged.FreshGrammar.(
              Exp.(
                tuple([
                  tup_label(label("label"), string(name)),
                  tup_label(label("value"), e),
                ])
              )
            ),
          entries,
        );
      Some(IdTagged.FreshGrammar.Exp.list_lit(unpivoted_entries));
    },
    custom_statics: Some(Ctx.Melt),
  },
  {
    name: "from_entries",
    arg:
      List(
        prod([
          tup_label(label("label"), string()),
          tup_label(label("value"), unknown(Internal)),
        ]),
      ),
    ret: Unknown(Internal),
    imp: (e: DHExp.t) => {
      open OptUtil.Syntax;
      let-unbox elems: list(Exp.t) = (ListLit, e);
      let* tuple_entries =
        OptUtil.traverse(
          elem => {
            let-unbox tuple_parts = (LabeledTupleEntries, elem);

            switch (tuple_parts) {
            | [
                (Some("label"), {term: Atom(String(s)), _}),
                (Some("value"), v),
              ] =>
              Some((Label(s) |> Exp.temp, v))
            | [(Some("label"), {term: EmptyHole, _}), (Some("value"), v)] =>
              Some((EmptyHole |> Exp.temp, v))
            | [(Some("label"), bad_label), (Some("value"), v)] =>
              Some((MultiHole([Exp(bad_label)]) |> Exp.temp, v))
            | _ => None
            };
          },
          elems,
        );

      let tuple_entries =
        List.map(
          ((e1, e2)) => {TupLabel(e1, e2) |> Exp.temp},
          tuple_entries,
        );

      Some(Exp.to_tuple(tuple_entries));
    },
    custom_statics: None,
  },
  {
    name: "project_labels",
    arg: Unknown(Internal),
    ret: Unknown(Internal),
    imp: d => {
      let-unbox args = (LabeledTupleEntries, d);
      switch (args) {
      | [] // No argument indet
      | [_] => None // Singleton labeled tuple indet
      | [(Some(_), _), ..._] => None // First element is labeled
      | [(None, tup), ...labels] =>
        let labs =
          List.map(
            ((arg_l, exp)) => {
              switch (arg_l) {
              | Some(_) => None
              | None =>
                let-unbox label = (Label, exp);
                Some(label); // We should never have a None here
              }
            },
            labels,
          );
        let labels = OptUtil.sequence(labs);
        switch (labels) {
        | Some(labels: list(string)) =>
          let entries =
            List.map(
              (l: string) => IdTagged.FreshGrammar.Exp.(dot(tup, label(l))),
              labels,
            );
          Some(Exp.to_tuple(entries));
        | None => None
        };
      };
    },
    custom_statics: Some(Ctx.ProjectLabels),
  },
  {
    name: "select_labels",
    arg: Unknown(Internal),
    ret: Unknown(Internal),
    imp: d => {
      let-unbox args = (LabeledTupleEntries, d);
      switch (args) {
      | [] // No argument indet
      | [_] => None // Singleton labeled tuple indet
      | [(Some(_), _), ..._] => None // First element is labeled
      | [(None, tup), ...labels] =>
        let labs =
          List.map(
            ((arg_l, exp)) => {
              switch (arg_l) {
              | Some(_) => None
              | None =>
                let-unbox label = (Label, exp);
                Some(label); // We should never have a None here
              }
            },
            labels,
          );
        let labels = OptUtil.sequence(labs);
        switch (labels) {
        | Some(labels: list(string)) =>
          let entries =
            List.map(
              l =>
                IdTagged.FreshGrammar.Exp.(
                  tup_label(label(l), dot(tup, label(l)))
                ),
              labels,
            );
          Some(Exp.to_tuple(entries));
        | None => None
        };
      };
    },
    custom_statics: Some(SelectLabels),
  },
  {
    name: "omit_labels",
    arg: Unknown(Internal),
    ret: Unknown(Internal),
    imp: d => {
      let-unbox args = (LabeledTupleEntries, d);
      switch (args) {
      | [] // No argument indet
      | [_] => None // Singleton labeled tuple indet
      | [(Some(_), _), ..._] => None // First element is labeled
      | [(None, tup), ...labels] =>
        open IdTagged.FreshGrammar.Exp;
        let labs =
          List.map(
            ((arg_l, exp)) => {
              switch (arg_l) {
              | Some(_) => None
              | None =>
                let-unbox label = (Label, exp);
                Some(label); // We should never have a None here
              }
            },
            labels,
          );
        let labels = OptUtil.sequence(labs);
        module StringSet = Set.Make(String);
        switch (labels) {
        | Some(labels: list(string)) =>
          let labels_set = StringSet.of_list(labels);
          let-unbox entries = (LabeledTupleEntries, tup);
          let entries =
            List.filter_map(
              ((l, e)) => {
                switch (l) {
                | Some(l) =>
                  if (StringSet.mem(l, labels_set)) {
                    None;
                  } else {
                    Some(tup_label(label(l), e));
                  }
                | None => Some(e)
                }
              },
              entries,
            );
          Some(Exp.to_tuple(entries));
        | None => None
        };
      };
    },
    custom_statics: Some(OmitLabels),
  },
  {
    name: "drop_labels",
    arg: Unknown(Internal),
    ret: Unknown(Internal),
    imp: d => {
      let-unbox entries = (LabeledTupleEntries, d);
      let entries = List.filter_map(((_, e)) => Some(e), entries);
      Some(Exp.to_tuple(entries));
    },
    custom_statics: Some(DropLabels),
  },
];
