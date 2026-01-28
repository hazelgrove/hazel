/*
 * GenSlides: Generate ML slide files from .hz programs
 * =====================================================
 *
 * USAGE:
 *   ./hazel gen-slides        # Generate all slides from .hz files
 *   ./hazel gen-slides-clean  # Remove generated slides, restore empty stub
 *   dune build                # Rebuild after generating
 *
 * WHAT IT DOES:
 *   1. Reads all .hz files from input_dir recursively
 *   2. Parses each file and serializes to PersistentSegment format
 *   3. Generates individual ML files (one per .hz file)
 *   4. Generates AllExamples.re aggregation module
 *   5. Generates dune file for the examples library
 *
 * OUTPUT FILES (in output_dir):
 *   - ExamplesStudyCalculator.ml, etc. (one per .hz file)
 *   - AllExamples.re (lists all slides, or empty stub after clean)
 *   - dune (library definition)
 *
 * INTEGRATION (already done, listed here for reference):
 *   - src/web/dune: added "examples" to libraries
 *   - src/web/init/Init.re: added "@ Examples.AllExamples.all"
 *
 * TO COMPLETELY REMOVE THIS FEATURE:
 *   1. Run: ./hazel gen-slides-clean
 *   2. Remove "examples" from src/web/dune libraries list
 *   3. Remove "@ Examples.AllExamples.all" from src/web/init/Init.re
 *   4. Delete src/web/init/examples/ directory
 *   5. Delete this file (src/CLI/GenSlides.re)
 *
 * CONFIGURATION (change these constants below to customize):
 */

/* Source directory containing .hz files */
let input_dir = "hazel-programs/examples";

/* Output directory for generated ML files */
let output_dir = "src/web/init/examples";

/* Prefix for slide titles (e.g., "Examples / study / ..." ) */
let root_title = "Examples";

/* Name of the aggregation module */
let aggregation_module = "AllExamples.re";

/* Strip leading indentation from .hz files before parsing?
 * Uses Util.StringUtil.trim_leading (src/util/StringUtil.re:75-79) */
let strip_indentation = true;

/* ============================================================================
 * Implementation
 * ============================================================================ */

/* Recursively find all .hz files in a directory */
let rec find_hz_files = (base_dir: string, rel_path: string): list(string) => {
  let full_path =
    if (rel_path == "") {
      base_dir;
    } else {
      base_dir ++ "/" ++ rel_path;
    };

  try({
    let entries = Sys.readdir(full_path) |> Array.to_list;
    List.concat_map(
      entry => {
        let entry_rel =
          if (rel_path == "") {
            entry;
          } else {
            rel_path ++ "/" ++ entry;
          };
        let entry_full = base_dir ++ "/" ++ entry_rel;

        if (Sys.is_directory(entry_full)) {
          find_hz_files(base_dir, entry_rel);
        } else if (Filename.check_suffix(entry, ".hz")) {
          [entry_rel];
        } else {
          [];
        };
      },
      entries,
    );
  }) {
  | Sys_error(msg) =>
    prerr_endline("Warning: " ++ msg);
    [];
  };
};

/* Convert a relative path to a module name
   e.g., "study/calculator/calculator.hz" -> "ExamplesStudyCalculatorCalculator" */
let path_to_module_name = (root_prefix: string, rel_path: string): string => {
  let without_ext = Filename.chop_suffix(rel_path, ".hz");
  let parts = String.split_on_char('/', without_ext);
  let capitalize_part = (s: string): string => {
    let subparts = String.split_on_char('-', s);
    String.concat("", List.map(String.capitalize_ascii, subparts));
  };
  root_prefix ++ String.concat("", List.map(capitalize_part, parts));
};

/* Convert a relative path to a slide title
   e.g., "study/calculator/calculator.hz" -> "Examples / study / calculator / calculator" */
let path_to_title = (root_title: string, rel_path: string): string => {
  let without_ext = Filename.chop_suffix(rel_path, ".hz");
  let parts = String.split_on_char('/', without_ext);
  root_title ++ " / " ++ String.concat(" / ", parts);
};

/* Ensure a directory exists, creating it and parents if needed */
let rec ensure_dir = (path: string): unit =>
  if (!Sys.file_exists(path)) {
    let parent = Filename.dirname(path);
    if (parent != path && parent != ".") {
      ensure_dir(parent);
    };
    try(Unix.mkdir(path, 0o755)) {
    | Unix.Unix_error(Unix.EEXIST, _, _) => ()
    | _ => ()
    };
  };

/* Write a string to a file */
let write_file = (path: string, content: string): unit => {
  Core.Out_channel.write_all(path, ~data=content);
};

/* Generate a single ML file for a .hz program */
let generate_ml_file = (rel_path: string): option((string, string)) => {
  let input_path = input_dir ++ "/" ++ rel_path;
  let module_name = path_to_module_name(root_title, rel_path);
  let title = path_to_title(root_title, rel_path);
  let output_path = output_dir ++ "/" ++ module_name ++ ".ml";

  try({
    let content = Core.In_channel.read_all(input_path);

    let content =
      if (strip_indentation) {
        Util.StringUtil.trim_leading(content);
      } else {
        content;
      };

    switch (Haz3lcore.Parser.to_zipper(content)) {
    | None =>
      prerr_endline("Failed to parse: " ++ rel_path);
      None;
    | Some(zipper) =>
      let persistent = Haz3lcore.PersistentSegment.persist(zipper);
      let tuple_content =
        [%derive.show: (string, Haz3lcore.PersistentSegment.t)]((
          title,
          persistent,
        ));
      let ml_content =
        "let out : string * Haz3lcore.PersistentSegment.t = " ++ tuple_content;

      write_file(output_path, ml_content);
      print_endline("Generated: " ++ module_name ++ ".ml");
      Some((module_name, title));
    };
  }) {
  | exn =>
    prerr_endline(
      "Error processing " ++ rel_path ++ ": " ++ Printexc.to_string(exn),
    );
    None;
  };
};

/* Generate the aggregation module that lists all slides */
let generate_aggregation_module = (modules: list((string, string))): unit => {
  let output_path = output_dir ++ "/" ++ aggregation_module;

  let module_refs =
    List.map(((name, _title)) => "  " ++ name ++ ".out,", modules)
    |> String.concat("\n");

  let content =
    "/* Auto-generated by: ./hazel gen-slides */\n"
    ++ "/* To remove: ./hazel gen-slides-clean */\n\n"
    ++ "let all: list((string, Haz3lcore.PersistentSegment.t)) = [\n"
    ++ module_refs
    ++ "\n];\n";

  write_file(output_path, content);
  print_endline("Generated: " ++ aggregation_module);
};

/* Generate stub aggregation module (empty list) */
let generate_stub_aggregation_module = (): unit => {
  let output_path = output_dir ++ "/" ++ aggregation_module;

  let content =
    "/* Stub module - run ./hazel gen-slides to populate */\n\n"
    ++ "let all: list((string, Haz3lcore.PersistentSegment.t)) = [];\n";

  write_file(output_path, content);
  print_endline("Generated stub: " ++ aggregation_module);
};

/* Generate the dune file for the examples library */
let generate_dune_file = (): unit => {
  let content =
    "(include_subdirs unqualified)\n\n"
    ++ "(library\n"
    ++ " (name examples)\n"
    ++ " (libraries haz3lcore))\n";

  write_file(output_dir ++ "/dune", content);
  print_endline("Generated: dune");
};

/* Remove all generated ML files (but keep dune and stub) */
let clean_generated_files = (): unit =>
  try({
    let entries = Sys.readdir(output_dir) |> Array.to_list;
    List.iter(
      entry =>
        /* Remove .ml files (generated slides) but keep dune and AllExamples.re */
        if (Filename.check_suffix(entry, ".ml")) {
          let path = output_dir ++ "/" ++ entry;
          Sys.remove(path);
          print_endline("Removed: " ++ entry);
        },
      entries,
    );
  }) {
  | Sys_error(msg) => prerr_endline("Warning: " ++ msg)
  };

/* Main generate command */
let generate = (): unit => {
  print_endline("Generating slides from: " ++ input_dir);
  print_endline("Output directory: " ++ output_dir);
  print_endline("");

  ensure_dir(output_dir);

  let hz_files = find_hz_files(input_dir, "");
  let hz_files = List.sort(String.compare, hz_files);

  print_endline(
    "Found " ++ string_of_int(List.length(hz_files)) ++ " .hz files",
  );
  print_endline("");

  let generated_modules = List.filter_map(generate_ml_file, hz_files);

  print_endline("");

  generate_aggregation_module(generated_modules);
  generate_dune_file();

  print_endline("");
  print_endline(
    "Done! Generated "
    ++ string_of_int(List.length(generated_modules))
    ++ " slide files.",
  );
};

/* Main clean command */
let clean = (): unit => {
  print_endline("Cleaning generated slides from: " ++ output_dir);
  print_endline("");

  clean_generated_files();
  generate_stub_aggregation_module();
  generate_dune_file();

  print_endline("");
  print_endline("Done! Restored to stub state.");
};
