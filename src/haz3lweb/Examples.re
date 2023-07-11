let init_data = [
  ("Basic Reference", LanguageRefSlide.lang_ref),
  ("Basic types", LanguageRefSlide.basic_type_egs),
  ("ADT Statics", LanguageRefSlide.adt_egs),
  ("ADT Dynamics", LanguageRefSlide.adt_dynamics_tests),
];

let init_name = name => {
  let data = List.assoc(name, init_data);
  ScratchSlide.unpersist(data);
};
