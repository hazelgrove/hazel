module Fresh = IdTagged.FreshGrammar;
open Fresh.Typ;
open BuiltinsUtil;
open Util;
let builtins: list(BuiltinsUtil.fn) = [
  {
    name: "group_by_label",
    arg:
      Prod([
        Unlabeled(list(unknown(Internal))),
        Unlabeled(unknown(Internal)),
      ]),
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
            List.map(
              ((name: string, es)): Grammar.exp_tuple_entry(
                                        IdTagged.IdTag.t,
                                      ) => {
              Labeled(
                IdTagged.IdTag.fresh(),
                Label(name),
                Fresh.Exp.list_lit(es),
              )
            }),
            unboxed,
          )
          |> Option.map(Fresh.Exp.tuple);

        | _ => None
        }
      }),
    custom_statics: Some(GroupByLabel),
  },
  {
    name: "to_lvs",
    arg: Unknown(Internal),
    ret:
      List(
        prod([
          labeled("label", string()),
          labeled("value", unknown(Internal)),
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
                  labeled("label", string(name)),
                  labeled("value", e),
                ])
              )
            ),
          entries,
        );
      Some(IdTagged.FreshGrammar.Exp.list_lit(unpivoted_entries));
    },
    custom_statics: Some(Ctx.ToLvs),
  },
  {
    name: "from_lvs",
    arg:
      List(
        prod([
          labeled("label", string()),
          labeled("value", unknown(Internal)),
        ]),
      ),
    ret: Unknown(Internal),
    imp: (e: DHExp.t) => {
      open OptUtil.Syntax;
      let-unbox elems: list(Exp.t) = (ListLit, e);
      let* tuple_entries: list(Exp.tuple_entry) =
        OptUtil.traverse(
          elem => {
            open IdTagged.TempGrammar;
            let-unbox tuple_parts = (LabeledTupleEntries, elem);

            switch (tuple_parts) {
            | [
                (Some("label"), {term: Atom(String(s)), _}),
                (Some("value"), v),
              ] =>
              Some(Exp.labeled(s, v)) // TODO This was temp now it's fresh
            | [(Some("label"), {term: EmptyHole, _}), (Some("value"), v)] =>
              Some(Exp.labeled'(EmptyLabel, v))
            | [(Some("label"), bad_label), (Some("value"), v)] =>
              Some(Exp.labeled'(MultiHole([Exp(bad_label)]), v))
            | _ => None
            };
          },
          elems,
        );

      Some(IdTagged.TempGrammar.Exp.tuple(tuple_entries));
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
                IdTagged.FreshGrammar.Exp.(labeled(l, dot(tup, label(l)))),
              labels,
            );
          Some(IdTagged.TempGrammar.Exp.tuple(entries));
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
                    Some(labeled(l, e));
                  }
                | None => Some(Unlabeled(e))
                }
              },
              entries,
            );
          Some(IdTagged.TempGrammar.Exp.tuple(entries));
        | None => None
        };
      };
    },
    custom_statics: Some(OmitLabels),
  },
  {
    name: "omit_all_labels",
    arg: Unknown(Internal),
    ret: Unknown(Internal),
    imp: d => {
      let-unbox entries = (LabeledTupleEntries, d);
      let entries = List.filter_map(((_, e)) => Some(e), entries);
      Some(Exp.to_tuple(entries));
    },
    custom_statics: Some(OmitAllLabels),
  },
];
