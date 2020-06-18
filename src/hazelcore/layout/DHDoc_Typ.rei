let promote_annot: HTypAnnot.t => DHAnnot.t;

let promote: HTypDoc.t => DHDoc_common.t;

let mk: (~enforce_inline: bool, HTyp.t) => DHDoc_common.t;
