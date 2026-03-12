let out : string * Haz3lcore.PersistentSegment.t =
  ( "BrushPath",
    {
      segment = "invalid";
      backup_text =
        "let lookup : (String, [(String, ?)]) -> ? =\n\
         fun (key, pairs) ->\n\
         case pairs\n\
         | [] => Null\n\
         | (k, v)::rest => if k == key then v else lookup(key, rest)\n\
         end\n\
         in\n\n\
         let starts_with : (String, String) -> Bool =\n\
         fun (s, prefix) ->\n\
         let plen = string_length(prefix) in\n\
         if string_length(s) < plen then false\n\
         else string_sub(s, 0, plen) == prefix\n\
         in\n\n\
         let update_key : (String, ?, [(String, ?)]) -> [(String, ?)] =\n\
         fun (key, new_val, pairs) ->\n\
         map(pairs, fun (k, v) -> if k == key then (k, new_val) else (k, v))\n\
         in\n\n\
         let to_float : ? -> Float =\n\
         fun v ->\n\
         case v\n\
         | Float(f) => f\n\
         | Int(i) => float_of_int(i)\n\
         | _ => 0.0\n\
         end\n\
         in\n\n\
         let stamp_prefix = \"shape:stamp_\" in\n\n\
         let get_store : ? -> [(String, ?)] =\n\
         fun doc ->\n\
         case doc\n\
         | Assoc(top) =>\n\
         case lookup(\"store\", top)\n\
         | Assoc(pairs) => pairs\n\
         | _ => []\n\
         end\n\
         | _ => []\n\
         end\n\
         in\n\n\
         let get_user_shapes : ? -> [(String, ?)] =\n\
         fun doc ->\n\
         filter(get_store(doc), fun (k, _) ->\n\
         starts_with(k, \"shape:\") &&\n\
         !(starts_with(k, stamp_prefix))\n\
         )\n\
         in\n\n\
         let get_draw_shapes : ? -> [(String, ?)] =\n\
         fun doc ->\n\
         filter(get_user_shapes(doc), fun (_, v) ->\n\
         case v\n\
         | Assoc(pairs) => lookup(\"type\", pairs) == String(\"draw\")\n\
         | _ => false\n\
         end\n\
         )\n\
         in\n\n\
         let get_path_str : ? -> String =\n\
         fun shape ->\n\
         case shape\n\
         | Assoc(pairs) =>\n\
         case lookup(\"props\", pairs)\n\
         | Assoc(pp) =>\n\
         case lookup(\"segments\", pp)\n\
         | List(Assoc(sp)::_) =>\n\
         case lookup(\"path\", sp)\n\
         | String(p) => p\n\
         | _ => \"\"\n\
         end\n\
         | _ => \"\"\n\
         end\n\
         | _ => \"\"\n\
         end\n\
         | _ => \"\"\n\
         end\n\
         in\n\n\
         let get_origin : ? -> (Float, Float) =\n\
         fun shape ->\n\
         case shape\n\
         | Assoc(pairs) =>\n\
         (to_float(lookup(\"x\", pairs)), to_float(lookup(\"y\", pairs)))\n\
         | _ => (0.0, 0.0)\n\
         end\n\
         in\n\n\
         let sample_every = 3 in\n\n\
         let compute_stamps : (?, ?) -> ? =\n\
         fun (brush_doc, path_doc) ->\n\
         let brush_shapes = get_user_shapes(brush_doc) in\n\
         let path_shapes = get_draw_shapes(path_doc) in\n\n\
         let brush_cx = case brush_shapes\n\
         | [] => 0.0\n\
         | _ =>\n\
         let xs = map(brush_shapes, fun (_, v) -> to_float(lookup(\"x\", case \
         v | Assoc(p) => p | _ => [] end))) in\n\
         fold_left(xs, fun (acc, x) -> acc +. x, 0.0) /. \
         float_of_int(length(xs))\n\
         end in\n\n\
         let brush_cy = case brush_shapes\n\
         | [] => 0.0\n\
         | _ =>\n\
         let ys = map(brush_shapes, fun (_, v) -> to_float(lookup(\"y\", case \
         v | Assoc(p) => p | _ => [] end))) in\n\
         fold_left(ys, fun (acc, y) -> acc +. y, 0.0) /. \
         float_of_int(length(ys))\n\
         end in\n\n\
         let stamps = flat_map(enumerate(path_shapes), fun (path_idx, (_, \
         path_shape)) ->\n\
         let path_str = get_path_str(path_shape) in\n\
         let (po_x, po_y) = get_origin(path_shape) in\n\
         let all_points = tldraw_decode_path(path_str) in\n\
         let sampled = map(\n\
         filter(enumerate(all_points), fun (i, _) ->\n\
         int_mod(i, sample_every) == 0),\n\
         fun (_, pt) -> pt) in\n\
         flat_map(enumerate(sampled), fun (idx, (px, py)) ->\n\
         map(brush_shapes, fun (k, shape) ->\n\
         let suffix = string_sub(k, 6, string_length(k) - 6) in\n\
         let new_key = stamp_prefix ++ string_of_int(path_idx) ++ \"_\" ++ \
         string_of_int(idx) ++ \"_\" ++ suffix in\n\
         case shape\n\
         | Assoc(pairs) =>\n\
         let pairs = update_key(\"id\", String(new_key), pairs) in\n\
         let pairs = update_key(\"opacity\", Float(0.3), pairs) in\n\
         let bx = to_float(lookup(\"x\", pairs)) in\n\
         let by = to_float(lookup(\"y\", pairs)) in\n\
         let new_x = bx -. brush_cx +. po_x +. px in\n\
         let new_y = by -. brush_cy +. po_y +. py in\n\
         let pairs = update_key(\"x\", Float(new_x), pairs) in\n\
         let pairs = update_key(\"y\", Float(new_y), pairs) in\n\
         (new_key, Assoc(pairs))\n\
         | _ => (new_key, shape)\n\
         end\n\
         )\n\
         )\n\
         ) in\n\n\
         let result = case path_doc\n\
         | Assoc(top) =>\n\
         case lookup(\"store\", top)\n\
         | Assoc(store) =>\n\
         let clean = filter(store, fun (k, _) -> !(starts_with(k, \
         stamp_prefix))) in\n\
         Assoc(update_key(\"store\", Assoc(clean @ stamps), top))\n\
         | _ => path_doc\n\
         end\n\
         | _ => path_doc\n\
         end in\n\
         result\n\
         in\n\n\
         let brush_doc = ^^PatchworkTool(Assoc([(\"@patchwork\", \
         Assoc([(\"suggestedImportUrl\", \
         String(\"automerge:Qq3G9LB5bNHwSVJ6m29Tz8zgb4E\")), (\"type\", \
         String(\"tldraw4\"))])), (\"schema\", Assoc([(\"schemaVersion\", \
         Int(2)), (\"sequences\", Assoc([(\"com.tldraw.asset\", Int(1)), \
         (\"com.tldraw.asset.bookmark\", Int(2)), (\"com.tldraw.asset.image\", \
         Int(5)), (\"com.tldraw.asset.video\", Int(5)), \
         (\"com.tldraw.binding.arrow\", Int(1)), (\"com.tldraw.camera\", \
         Int(1)), (\"com.tldraw.document\", Int(2)), (\"com.tldraw.instance\", \
         Int(25)), (\"com.tldraw.instance_page_state\", Int(5)), \
         (\"com.tldraw.instance_presence\", Int(6)), (\"com.tldraw.page\", \
         Int(1)), (\"com.tldraw.pointer\", Int(1)), (\"com.tldraw.shape\", \
         Int(4)), (\"com.tldraw.shape.arrow\", Int(8)), \
         (\"com.tldraw.shape.bookmark\", Int(2)), (\"com.tldraw.shape.draw\", \
         Int(4)), (\"com.tldraw.shape.embed\", Int(4)), \
         (\"com.tldraw.shape.frame\", Int(1)), (\"com.tldraw.shape.geo\", \
         Int(11)), (\"com.tldraw.shape.group\", Int(0)), \
         (\"com.tldraw.shape.highlight\", Int(3)), \
         (\"com.tldraw.shape.image\", Int(5)), (\"com.tldraw.shape.line\", \
         Int(5)), (\"com.tldraw.shape.note\", Int(10)), \
         (\"com.tldraw.shape.text\", Int(4)), (\"com.tldraw.shape.video\", \
         Int(4)), (\"com.tldraw.store\", Int(5))]))])), (\"store\", \
         Assoc([(\"page:page\", Assoc([(\"id\", String(\"page:page\")), \
         (\"index\", String(\"a1\")), (\"meta\", Assoc([])), (\"name\", \
         String(\"brush tldraw\")), (\"typeName\", String(\"page\"))])), \
         (\"shape:0qy8dInoTLlwex2JX2sDd\", Assoc([(\"id\", \
         String(\"shape:0qy8dInoTLlwex2JX2sDd\")), (\"index\", \
         String(\"a1HIcx1V\")), (\"isLocked\", Bool(false)), (\"meta\", \
         Assoc([])), (\"opacity\", Int(1)), (\"parentId\", \
         String(\"page:page\")), (\"props\", Assoc([(\"align\", \
         String(\"middle\")), (\"color\", String(\"black\")), (\"dash\", \
         String(\"draw\")), (\"fill\", String(\"none\")), (\"font\", \
         String(\"draw\")), (\"geo\", String(\"rectangle\")), (\"growY\", \
         Int(0)), (\"h\", Float(63.023453)), (\"labelColor\", \
         String(\"black\")), (\"richText\", Assoc([(\"content\", \
         List([Assoc([(\"type\", String(\"paragraph\"))])])), (\"type\", \
         String(\"doc\"))])), (\"scale\", Int(1)), (\"size\", String(\"m\")), \
         (\"url\", String(\"\")), (\"verticalAlign\", String(\"middle\")), \
         (\"w\", Float(69.065125))])), (\"rotation\", Int(0)), (\"type\", \
         String(\"geo\")), (\"typeName\", String(\"shape\")), (\"x\", \
         Float(228.382813)), (\"y\", Float(178.070316))]))]))])) in\n\
         let path_doc = ^^PatchworkTool(Assoc([(\"@patchwork\", \
         Assoc([(\"suggestedImportUrl\", \
         String(\"automerge:Qq3G9LB5bNHwSVJ6m29Tz8zgb4E\")), (\"type\", \
         String(\"tldraw4\"))])), (\"schema\", Assoc([(\"schemaVersion\", \
         Int(2)), (\"sequences\", Assoc([(\"com.tldraw.asset\", Int(1)), \
         (\"com.tldraw.asset.bookmark\", Int(2)), (\"com.tldraw.asset.image\", \
         Int(5)), (\"com.tldraw.asset.video\", Int(5)), \
         (\"com.tldraw.binding.arrow\", Int(1)), (\"com.tldraw.camera\", \
         Int(1)), (\"com.tldraw.document\", Int(2)), (\"com.tldraw.instance\", \
         Int(25)), (\"com.tldraw.instance_page_state\", Int(5)), \
         (\"com.tldraw.instance_presence\", Int(6)), (\"com.tldraw.page\", \
         Int(1)), (\"com.tldraw.pointer\", Int(1)), (\"com.tldraw.shape\", \
         Int(4)), (\"com.tldraw.shape.arrow\", Int(8)), \
         (\"com.tldraw.shape.bookmark\", Int(2)), (\"com.tldraw.shape.draw\", \
         Int(4)), (\"com.tldraw.shape.embed\", Int(4)), \
         (\"com.tldraw.shape.frame\", Int(1)), (\"com.tldraw.shape.geo\", \
         Int(11)), (\"com.tldraw.shape.group\", Int(0)), \
         (\"com.tldraw.shape.highlight\", Int(3)), \
         (\"com.tldraw.shape.image\", Int(5)), (\"com.tldraw.shape.line\", \
         Int(5)), (\"com.tldraw.shape.note\", Int(10)), \
         (\"com.tldraw.shape.text\", Int(4)), (\"com.tldraw.shape.video\", \
         Int(4)), (\"com.tldraw.store\", Int(5))]))])), (\"store\", \
         Assoc([(\"page:page\", Assoc([(\"id\", String(\"page:page\")), \
         (\"index\", String(\"a1\")), (\"meta\", Assoc([])), (\"name\", \
         String(\"path tldraw\")), (\"typeName\", String(\"page\"))])), \
         (\"shape:6bmj22jIwbia1cS9Yd0qX\", Assoc([(\"id\", \
         String(\"shape:6bmj22jIwbia1cS9Yd0qX\")), (\"index\", \
         String(\"a1cR7dYV\")), (\"isLocked\", Bool(false)), (\"meta\", \
         Assoc([])), (\"opacity\", Int(1)), (\"parentId\", \
         String(\"page:page\")), (\"props\", Assoc([(\"color\", \
         String(\"black\")), (\"dash\", String(\"draw\")), (\"fill\", \
         String(\"none\")), (\"isClosed\", Bool(false)), (\"isComplete\", \
         Bool(true)), (\"isPen\", Bool(false)), (\"scale\", Int(1)), \
         (\"scaleX\", Int(1)), (\"scaleY\", Int(1)), (\"segments\", \
         List([Assoc([(\"path\", \
         String(\"AAAAAAAAAAAAAAA/UjRStAAAUjhmuAAAbkfKxQAA7U6xzAAABFAEzAAAhFLDzQAAVFSZzwAAnFT7zgAAB1MdzgAAslBozAAAJ0y4yQAAUjrhvQAA\")), \
         (\"type\", String(\"free\"))])])), (\"size\", String(\"m\"))])), \
         (\"rotation\", Int(0)), (\"type\", String(\"draw\")), (\"typeName\", \
         String(\"shape\")), (\"x\", Float(113.179688)), (\"y\", \
         Float(291.822927))])), (\"shape:stamp_0_0qy8dInoTLlwex2JX2sDd\", \
         Assoc([(\"id\", String(\"shape:stamp_0_0qy8dInoTLlwex2JX2sDd\")), \
         (\"index\", String(\"a1HIcx1V\")), (\"isLocked\", Bool(false)), \
         (\"meta\", Assoc([])), (\"opacity\", Float(0.300000)), (\"parentId\", \
         String(\"page:page\")), (\"props\", Assoc([(\"align\", \
         String(\"middle\")), (\"color\", String(\"black\")), (\"dash\", \
         String(\"draw\")), (\"fill\", String(\"none\")), (\"font\", \
         String(\"draw\")), (\"geo\", String(\"rectangle\")), (\"growY\", \
         Int(0)), (\"h\", Float(63.023453)), (\"labelColor\", \
         String(\"black\")), (\"richText\", Assoc([(\"content\", \
         List([Assoc([(\"type\", String(\"paragraph\"))])])), (\"type\", \
         String(\"doc\"))])), (\"scale\", Int(1)), (\"size\", String(\"m\")), \
         (\"url\", String(\"\")), (\"verticalAlign\", String(\"middle\")), \
         (\"w\", Float(69.065125))])), (\"rotation\", Int(0)), (\"type\", \
         String(\"geo\")), (\"typeName\", String(\"shape\")), (\"x\", \
         Float(113.179688)), (\"y\", Float(291.822927))])), \
         (\"shape:stamp_1_0qy8dInoTLlwex2JX2sDd\", Assoc([(\"id\", \
         String(\"shape:stamp_1_0qy8dInoTLlwex2JX2sDd\")), (\"index\", \
         String(\"a1HIcx1V\")), (\"isLocked\", Bool(false)), (\"meta\", \
         Assoc([])), (\"opacity\", Float(0.300000)), (\"parentId\", \
         String(\"page:page\")), (\"props\", Assoc([(\"align\", \
         String(\"middle\")), (\"color\", String(\"black\")), (\"dash\", \
         String(\"draw\")), (\"fill\", String(\"none\")), (\"font\", \
         String(\"draw\")), (\"geo\", String(\"rectangle\")), (\"growY\", \
         Int(0)), (\"h\", Float(63.023453)), (\"labelColor\", \
         String(\"black\")), (\"richText\", Assoc([(\"content\", \
         List([Assoc([(\"type\", String(\"paragraph\"))])])), (\"type\", \
         String(\"doc\"))])), (\"scale\", Int(1)), (\"size\", String(\"m\")), \
         (\"url\", String(\"\")), (\"verticalAlign\", String(\"middle\")), \
         (\"w\", Float(69.065125))])), (\"rotation\", Int(0)), (\"type\", \
         String(\"geo\")), (\"typeName\", String(\"shape\")), (\"x\", \
         Float(181.247559)), (\"y\", Float(250.385915))])), \
         (\"shape:stamp_2_0qy8dInoTLlwex2JX2sDd\", Assoc([(\"id\", \
         String(\"shape:stamp_2_0qy8dInoTLlwex2JX2sDd\")), (\"index\", \
         String(\"a1HIcx1V\")), (\"isLocked\", Bool(false)), (\"meta\", \
         Assoc([])), (\"opacity\", Float(0.300000)), (\"parentId\", \
         String(\"page:page\")), (\"props\", Assoc([(\"align\", \
         String(\"middle\")), (\"color\", String(\"black\")), (\"dash\", \
         String(\"draw\")), (\"fill\", String(\"none\")), (\"font\", \
         String(\"draw\")), (\"geo\", String(\"rectangle\")), (\"growY\", \
         Int(0)), (\"h\", Float(63.023453)), (\"labelColor\", \
         String(\"black\")), (\"richText\", Assoc([(\"content\", \
         List([Assoc([(\"type\", String(\"paragraph\"))])])), (\"type\", \
         String(\"doc\"))])), (\"scale\", Int(1)), (\"size\", String(\"m\")), \
         (\"url\", String(\"\")), (\"verticalAlign\", String(\"middle\")), \
         (\"w\", Float(69.065125))])), (\"rotation\", Int(0)), (\"type\", \
         String(\"geo\")), (\"typeName\", String(\"shape\")), (\"x\", \
         Float(470.153809)), (\"y\", Float(126.948415))]))]))])) in\n\n\
         let result = compute_stamps(brush_doc, path_doc) in\n\n\
         ^^AutomergeWriteBack(result)";
      refractors = "()";
    } )
