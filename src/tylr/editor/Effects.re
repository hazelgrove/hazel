open Effect;
open Effect.Deep;

type t(_) +=
  | Insert(EToken.t): t(unit)
  | Remove(EToken.t): t(unit);

// necessary to wrap recorded effects in existential wrapper for reasons
// I don't fully understand:
// https://stackoverflow.com/questions/49538251/heterogeneous-list-of-gadt-in-ocaml
type recorded =
  | R(t('a)): recorded;

// execute and record effects emitted by applying f to x.
// useful for choosing between numerous effectful paths
// and choosing one based on their results.
let record = (f, x): (_, list(recorded)) => {
  let recorded = ref([]);
  let effc: 'a. t('a) => option(continuation('a, _) => _) =
    (type a, eff: t(a)) =>
      switch (eff) {
      | Insert(t) =>
        recorded := [R(eff), ...recorded^];
        Some((k: continuation(a, _)) => continue(k, ()));
      | Remove(t) =>
        recorded := [R(eff), ...recorded^];
        Some((k: continuation(a, _)) => continue(k, ()));
      | _ => None
      };
  let result = try_with(f, x, {effc: effc});
  (result, recorded^);
};

let commit = (effs: list(recorded)) =>
  List.iter((R(eff)) => ignore(perform(eff)), effs);

// let perform_all = set =>
//   to_list(set)
//   |> List.iter(((_, eff)) => Effect.perform(eff));

let perform_if = (o, eff: t(unit)) =>
  switch (o) {
  | None => None
  | Some(x) =>
    perform(eff);
    Some(x);
  };
