let out : string * Haz3lcore.PersistentSegment.t =
  ( "DrawPetri",
    {
      segment = "";
      backup_text =
        "let jq1 = fun filters -> fun json ->\n\
         case jq(filters)(json)\n\
         | hd :: _ => hd\n\
         | [] => Null\n\
         end\n\
         in\n\n\
         let str = fun json ->\n\
         case json\n\
         | String(s) => s\n\
         | _ => \"\"\n\
         end\n\
         in\n\n\
         let tldraw_to_petrinet = fun doc ->\n\
         let shapes = jq([jq_field(\"store\"), jq_to_entries, \
         jq_iterate])(doc) in\n\n\
         let circles = flat_map(shapes, fun entry ->\n\
         if str(jq1([jq_field(\"value\"), jq_field(\"type\")])(entry)) $== \
         \"geo\"\n\
         && str(jq1([jq_field(\"value\"), jq_field(\"props\"), \
         jq_field(\"geo\")])(entry)) $== \"ellipse\"\n\
         then [entry] else []\n\
         ) in\n\n\
         let rects = flat_map(shapes, fun entry ->\n\
         if str(jq1([jq_field(\"value\"), jq_field(\"type\")])(entry)) $== \
         \"geo\"\n\
         && str(jq1([jq_field(\"value\"), jq_field(\"props\"), \
         jq_field(\"geo\")])(entry)) $== \"rectangle\"\n\
         then [entry] else []\n\
         ) in\n\n\
         let arrows = flat_map(shapes, fun entry ->\n\
         if str(jq1([jq_field(\"value\"), jq_field(\"type\")])(entry)) $== \
         \"arrow\"\n\
         then [entry] else []\n\
         ) in\n\n\
         let circle_ids = map(circles, fun entry ->\n\
         str(jq1([jq_field(\"value\"), jq_field(\"id\")])(entry))\n\
         ) in\n\n\
         let is_circle = fun id ->\n\
         case filter(circle_ids, fun cid -> cid $== id)\n\
         | _ :: _ => true\n\
         | [] => false\n\
         end\n\
         in\n\n\
         let node_id = fun shape_id ->\n\
         if is_circle(shape_id)\n\
         then \"place__\" ++ shape_id\n\
         else \"transition__\" ++ shape_id\n\
         in\n\n\
         let label_of = fun default_label -> fun entry ->\n\
         let rt = str(jq1([jq_field(\"value\"), jq_field(\"props\"), \
         jq_field(\"richText\"), jq_field(\"content\"), jq_index(0), \
         jq_field(\"content\"), jq_index(0), jq_field(\"text\")])(entry)) in\n\
         let plain = str(jq1([jq_field(\"value\"), jq_field(\"props\"), \
         jq_field(\"text\")])(entry)) in\n\
         if !(rt $== \"\") then rt\n\
         else if !(plain $== \"\") then plain\n\
         else default_label\n\
         in\n\n\
         let make_place = fun entry ->\n\
         let v = jq1([jq_field(\"value\")])(entry) in\n\
         let sid = str(jq1([jq_field(\"id\")])(v)) in\n\
         let name = label_of(\"place\")(entry) in\n\
         Assoc([(\"id\", String(\"place__\" ++ sid)),\n\
         (\"type\", String(\"place\")),\n\
         (\"position\", Assoc([(\"x\", jq1([jq_field(\"x\")])(v)),\n\
         (\"y\", jq1([jq_field(\"y\")])(v))])),\n\
         (\"width\", Int(130)),\n\
         (\"height\", Int(130)),\n\
         (\"data\", Assoc([(\"label\", String(name)),\n\
         (\"type\", String(\"place\")),\n\
         (\"tokenCounts\", Assoc([])),\n\
         (\"initialTokenCounts\", Assoc([(\"default\", Int(1))]))]))])\n\
         in\n\n\
         let make_transition = fun entry ->\n\
         let v = jq1([jq_field(\"value\")])(entry) in\n\
         let sid = str(jq1([jq_field(\"id\")])(v)) in\n\
         let name = label_of(\"transition\")(entry) in\n\
         Assoc([(\"id\", String(\"transition__\" ++ sid)),\n\
         (\"type\", String(\"transition\")),\n\
         (\"position\", Assoc([(\"x\", jq1([jq_field(\"x\")])(v)),\n\
         (\"y\", jq1([jq_field(\"y\")])(v))])),\n\
         (\"width\", Int(160)),\n\
         (\"height\", Int(80)),\n\
         (\"data\", Assoc([(\"label\", String(name)),\n\
         (\"type\", String(\"transition\"))]))])\n\
         in\n\n\
         let make_arc = fun entry ->\n\
         let v = jq1([jq_field(\"value\")])(entry) in\n\
         let start_b = jq1([jq_field(\"props\"), jq_field(\"start\")])(v) in\n\
         let end_b = jq1([jq_field(\"props\"), jq_field(\"end\")])(v) in\n\
         if str(jq1([jq_field(\"type\")])(start_b)) $== \"binding\"\n\
         && str(jq1([jq_field(\"type\")])(end_b)) $== \"binding\"\n\
         then\n\
         let src = node_id(str(jq1([jq_field(\"boundShapeId\")])(start_b))) in\n\
         let tgt = node_id(str(jq1([jq_field(\"boundShapeId\")])(end_b))) in\n\
         [Assoc([(\"source\", String(src)),\n\
         (\"sourceHandle\", Null),\n\
         (\"target\", String(tgt)),\n\
         (\"targetHandle\", Null),\n\
         (\"id\", String(\"arc__\" ++ src ++ \"-\" ++ tgt)),\n\
         (\"type\", String(\"default\")),\n\
         (\"data\", Assoc([(\"tokenWeights\", Assoc([(\"default\", \
         Int(1))]))])),\n\
         (\"interactionWidth\", Int(8))])]\n\
         else []\n\
         in\n\n\
         let places = map(circles, make_place) in\n\
         let transitions = map(rects, make_transition) in\n\
         let arcs = flat_map(arrows, make_arc) in\n\
         let nodes = places @ transitions in\n\n\
         Assoc([\n\
         (\"@patchwork\", Assoc([\n\
         (\"suggestedImportUrl\", \
         String(\"automerge:3phkB7HzGoQ67w2ahmj9gepELErw\")),\n\
         (\"type\", String(\"petrinaut\"))])),\n\
         (\"petriNetDefinition\", Assoc([\n\
         (\"arcs\", List(arcs)),\n\
         (\"nodes\", List(nodes)),\n\
         (\"tokenTypes\", List([Assoc([\n\
         (\"color\", String(\"#3498db\")),\n\
         (\"id\", String(\"default\")),\n\
         (\"name\", String(\"Default\"))])]))])),\n\
         (\"title\", String(\"TLDraw to Petri Net\"))])\n\
         in\n\n\
         let tldraw_doc = ^^PatchworkTool(Null) in\n\n\
         let petrinet = tldraw_to_petrinet(tldraw_doc) in\n\n\
         let _ = ^^AutomergeWriteBack(petrinet) in\n\n\
         ^^PatchworkTool(petrinet)";
      refractors = "()";
    } )
