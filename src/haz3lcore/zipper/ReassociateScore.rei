let accept_candidate:
  (
    ~base_scope: Relatives.t,
    ~candidate_siblings: Siblings.t,
    ~outer_ancestors: Ancestors.t,
    ZipperBase.t,
  ) =>
  ZipperBase.t;
