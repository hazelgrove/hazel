let tuple_zip:
  (Skel.t(UHExp.operator), HTyp.t) =>
  option(list((Skel.t(UHExp.operator), HTyp.t)));

/* returns recursive ctx + name of recursively defined var */
let ctx_for_let':
  (Contexts.t, UHPat.t, HTyp.t, UHExp.t) => (Contexts.t, option(Var.t));

let ctx_for_let: (Contexts.t, UHPat.t, HTyp.t, UHExp.t) => Contexts.t;

/**
     * Synthesize a type, if possible, for e
     */
let syn: (Contexts.t, UHExp.t) => option(HTyp.t);

let syn_block: (Contexts.t, UHExp.t) => option(HTyp.t);

let syn_lines: (Contexts.t, list(UHExp.line)) => option(Contexts.t);

let syn_line: (Contexts.t, UHExp.line) => option(Contexts.t);

let syn_opseq: (Contexts.t, UHExp.opseq) => option(HTyp.t);

let syn_skel:
  (
    Contexts.t,
    OpSeq.skel(UHExp.operator),
    OpSeq.seq(UHExp.operand, UHExp.operator)
  ) =>
  option(HTyp.t);

let syn_operand: (Contexts.t, UHExp.operand) => option(HTyp.t);

let syn_rules: (Contexts.t, UHExp.rules, HTyp.t) => option(HTyp.t);

let syn_rule: (Contexts.t, UHExp.rule, HTyp.t) => option(HTyp.t);

let ana_splice_map:
  (Contexts.t, SpliceInfo.splice_map(UHExp.t)) => option(Contexts.t);

/**
     * Analyze e against expected type ty
     */
let ana: (Contexts.t, UHExp.t, HTyp.t) => option(unit);

let ana_block: (Contexts.t, UHExp.t, HTyp.t) => option(unit);

let ana_opseq: (Contexts.t, UHExp.opseq, HTyp.t) => option(unit);

let ana_skel: (Contexts.t, UHExp.skel, UHExp.seq, HTyp.t) => option(unit);

let ana_operand: (Contexts.t, UHExp.operand, HTyp.t) => option(unit);

let ana_rules: (Contexts.t, UHExp.rules, HTyp.t, HTyp.t) => option(unit);

let ana_rule: (Contexts.t, UHExp.rule, HTyp.t, HTyp.t) => option(unit);

/**
     * Get type mode of nth operand of an opseq in synthetic position
     */
let syn_nth_type_mode:
  (Contexts.t, int, UHExp.opseq) => option(Statics_common.type_mode);

let syn_nth_type_mode':
  (
    Contexts.t,
    int,
    OpSeq.skel(UHExp.operator),
    OpSeq.seq(UHExp.operand, UHExp.operator)
  ) =>
  option(Statics_common.type_mode);

/**
     * Get type mode of nth operand of an opseq in analytic position
     */
let ana_nth_type_mode:
  (Contexts.t, int, UHExp.opseq, HTyp.t) => option(Statics_common.type_mode);

let ana_nth_type_mode':
  (Contexts.t, int, UHExp.skel, UHExp.seq, HTyp.t) =>
  option(Statics_common.type_mode);

/* If renumber_empty_holes is true, then the metavars in empty holes will be assigned
 * new values in the same namespace as non-empty holes. Non-empty holes are renumbered
 * regardless.
 */
let syn_fix_holes:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.t) =>
  (UHExp.t, HTyp.t, MetaVarGen.t);

let syn_fix_holes_block:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.t) =>
  (UHExp.t, HTyp.t, MetaVarGen.t);

let syn_fix_holes_lines:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.block) =>
  (list(UHExp.line), Contexts.t, MetaVarGen.t);

let syn_fix_holes_line:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.line) =>
  (UHExp.line, Contexts.t, MetaVarGen.t);

let syn_fix_holes_opseq:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.opseq) =>
  (UHExp.opseq, HTyp.t, MetaVarGen.t);

let syn_fix_holes_skel:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    OpSeq.skel(UHExp.operator),
    OpSeq.seq(UHExp.operand, UHExp.operator)
  ) =>
  (UHExp.skel, UHExp.seq, HTyp.t, MetaVarGen.t);

let syn_fix_holes_operand:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.operand) =>
  (UHExp.operand, HTyp.t, MetaVarGen.t);

let syn_fix_holes_rules:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.rules,
    HTyp.t
  ) =>
  (UHExp.rules, MetaVarGen.t, list(HTyp.t), option(HTyp.t));

let syn_fix_holes_rule:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.rule,
    HTyp.t
  ) =>
  (UHExp.rule, MetaVarGen.t, HTyp.t);

let ana_fix_holes_rules:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.rules,
    HTyp.t,
    HTyp.t
  ) =>
  (UHExp.rules, MetaVarGen.t);

let ana_fix_holes_rule:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.rule,
    HTyp.t,
    HTyp.t
  ) =>
  (UHExp.rule, MetaVarGen.t);

let ana_fix_holes_splice_map:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    SpliceInfo.splice_map(UHExp.t)
  ) =>
  (UHExp.splice_map, MetaVarGen.t);

let ana_fix_holes:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.t, HTyp.t) =>
  (UHExp.t, MetaVarGen.t);

let ana_fix_holes_block:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.t, HTyp.t) =>
  (UHExp.t, MetaVarGen.t);

let ana_fix_holes_opseq:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.opseq,
    HTyp.t
  ) =>
  (UHExp.opseq, MetaVarGen.t);

let ana_fix_holes_skel:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.skel,
    UHExp.seq,
    HTyp.t
  ) =>
  (UHExp.skel, UHExp.seq, MetaVarGen.t);

let ana_fix_holes_operand:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.operand,
    HTyp.t
  ) =>
  (UHExp.operand, MetaVarGen.t);

let syn_fix_holes_z:
  (Contexts.t, MetaVarGen.t, ZExp.t) => (ZExp.t, HTyp.t, MetaVarGen.t);

let syn_fix_holes_zlines:
  (Contexts.t, MetaVarGen.t, ZExp.zblock) =>
  (ZExp.zblock, Contexts.t, MetaVarGen.t);

let syn_fix_holes_zrules:
  (Contexts.t, MetaVarGen.t, ZExp.zrules, HTyp.t) =>
  (ZExp.zrules, list(HTyp.t), option(HTyp.t), MetaVarGen.t);

let ana_fix_holes_z:
  (Contexts.t, MetaVarGen.t, ZExp.t, HTyp.t) => (ZExp.t, MetaVarGen.t);

/* Only to be used on top-level expressions, as it starts hole renumbering at 0 */
let fix_and_renumber_holes:
  (Contexts.t, UHExp.t) => (UHExp.t, HTyp.t, MetaVarGen.t);

let fix_and_renumber_holes_z:
  (Contexts.t, ZExp.t) => Statics_common.edit_state;
