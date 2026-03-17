let out : string * Haz3lcore.PersistentSegment.t =
  ( "TLDraw",
    {
      segment = "";
      backup_text =
        "let starts_with = fun (prefix, json) -> case \
         jq1([jq_startswith(prefix)])(json) | Bool(b) => b | _ => false end in\n\
         let offset_float = fun (field, n) -> jq_update(field, fun v -> case v \
         | Float(x) => [Float(x +. n)] | _ => [v] end) in\n\n\
         let draw_shadow = fun doc ->\n\
         jq1([jq_update(\"store\", jq_with_entries(fun entry ->\n\
         let key = jq1([jq_field(\"key\")])(entry) in\n\
         if starts_with(\"shape:ht_\", key) then []\n\
         else if starts_with(\"shape:\", key) then\n\
         let new_key = jq1([jq_field(\"key\"), jq_string_sub(\"shape:\", \
         \"shape:ht_\")])(entry) in\n\
         [entry, Assoc([(\"key\", new_key), (\"value\", \
         jq1([jq_field(\"value\"),\n\
         jq_set(\"id\", new_key), jq_set(\"opacity\", Float(0.5)),\n\
         offset_float(\"x\", 10.0), offset_float(\"y\", 10.0)])(entry))])]\n\
         else [entry]\n\
         ))])(doc)\n\
         in\n\n\
         let doc = ^^PatchworkTool(Null) in\n\
         ^^AutomergeWriteBack(draw_shadow(doc))";
      refractors = "()";
    } )
