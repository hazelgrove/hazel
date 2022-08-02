include Ptmap;

let singleton = (k, v) => Ptmap.add(k, v, Ptmap.empty);

let disj_union = (m: t('a), m': t('a)): t('a) =>
  union(
    (_, _, _) =>
      raise(
        Invalid_argument(
          "IntMap.disj_union expects input maps to have disjoint key sets",
        ),
      ),
    m,
    m',
  );
