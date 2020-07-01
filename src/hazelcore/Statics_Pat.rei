let tuple_zip:
  (Skel.t(UHPat.operator), HTyp.t) =>
  option(list((Skel.t(UHPat.operator), HTyp.t)));

let syn: (Contexts.t, UHPat.t) => option((HTyp.t, Contexts.t));

let syn_opseq: (Contexts.t, UHPat.t) => option((HTyp.t, Contexts.t));

let syn_skel:
  (
    Contexts.t,
    OpSeq.skel(UHPat.operator),
    OpSeq.seq(UHPat.operand, UHPat.operator)
  ) =>
  option((HTyp.t, Contexts.t));

let syn_operand: (Contexts.t, UHPat.operand) => option((HTyp.t, Contexts.t));

let ana: (Contexts.t, UHPat.t, HTyp.t) => option(Contexts.t);

let ana_opseq: (Contexts.t, UHPat.t, HTyp.t) => option(Contexts.t);

let ana_skel:
  (Contexts.t, UHPat.skel, UHPat.seq, HTyp.t) => option(Contexts.t);

let ana_operand: (Contexts.t, UHPat.operand, HTyp.t) => option(Contexts.t);

/**
     * Get type mode of nth operand of an opseq in synthetic position
     */
let syn_nth_type_mode:
  (Contexts.t, int, UHPat.opseq) => option(Statics_common.type_mode);

let syn_nth_type_mode':
  (
    Contexts.t,
    int,
    OpSeq.skel(UHPat.operator),
    OpSeq.seq(UHPat.operand, UHPat.operator)
  ) =>
  option(Statics_common.type_mode);

/**
     * Get type mode of nth operand of an opseq in analytic position
     */
let ana_nth_type_mode:
  (Contexts.t, int, UHPat.opseq, HTyp.t) => option(Statics_common.type_mode);

let ana_nth_type_mode':
  (Contexts.t, int, UHPat.skel, UHPat.seq, HTyp.t) =>
  option(Statics_common.type_mode);

let syn_fix_holes:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHPat.t) =>
  (UHPat.t, HTyp.t, Contexts.t, MetaVarGen.t);

let syn_fix_holes_opseq:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHPat.t) =>
  (UHPat.t, HTyp.t, Contexts.t, MetaVarGen.t);

let syn_fix_holes_skel:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    OpSeq.skel(UHPat.operator),
    OpSeq.seq(UHPat.operand, UHPat.operator)
  ) =>
  (UHPat.skel, UHPat.seq, HTyp.t, Contexts.t, MetaVarGen.t);

let syn_fix_holes_operand:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHPat.operand) =>
  (UHPat.operand, HTyp.t, Contexts.t, MetaVarGen.t);

let ana_fix_holes:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHPat.t, HTyp.t) =>
  (UHPat.t, Contexts.t, MetaVarGen.t);

let ana_fix_holes_opseq:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHPat.t, HTyp.t) =>
  (UHPat.t, Contexts.t, MetaVarGen.t);

let ana_fix_holes_skel:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHPat.skel,
    UHPat.seq,
    HTyp.t
  ) =>
  (UHPat.skel, UHPat.seq, Contexts.t, MetaVarGen.t);

let ana_fix_holes_operand:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHPat.operand,
    HTyp.t
  ) =>
  (UHPat.operand, Contexts.t, MetaVarGen.t);

let syn_fix_holes_z:
  (Contexts.t, MetaVarGen.t, ZPat.t) =>
  (ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t);

let ana_fix_holes_z:
  (Contexts.t, MetaVarGen.t, ZPat.t, HTyp.t) =>
  (ZPat.t, Contexts.t, MetaVarGen.t);
