let init = [
  ("Basic Reference", SerializedExamples.lang_ref),
  ("Types & errors", SerializedExamples.basic_type_egs),
  ("ADT Statics", SerializedExamples.adt_egs),
  ("ADT Dynamics", SerializedExamples.adt_dynamics_tests),
];

let init_name = name => ScratchSlide.unpersist(List.assoc(name, init));
