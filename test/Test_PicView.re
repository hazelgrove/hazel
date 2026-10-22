/* MEASURING ONLY (^ticket(535), trial branch): the woods picture's ^picture
   view, timed with has_type off, limited to depth 1, and whole. Program A
   is the Scenes slide up to its picture, ending with the picture's length;
   program B is the same, then ^picture's view code (copied from the
   slide's own definition) applied to `pic`. B - A is the view. Each run
   times every program at every setting, in turn, after a full GC.
   PICVIEW_RUNS sets the runs (default 7). From ~/.cache/hazel-diag's
   Test_PicView.timing.re of 2026-10-13, which measured #2681 at first. */
open Alcotest;
open Language;

let picture_line = "let ^^probe(drawn) = ^^livelit(Drawing.^picture((picture = (Drawing.frame((x0, y0, x1, y1)) @ backdrop @ figure)))) in";

let read = path => {
  let ic = open_in_bin(path);
  let s = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  s;
};

let index_of = (s, sub) => {
  let n = String.length(sub);
  let rec go = i =>
    if (i + n > String.length(s)) {
      failwith("not found: " ++ String.sub(sub, 0, min(n, 40)));
    } else if (String.sub(s, i, n) == sub) {
      i;
    } else {
      go(i + 1);
    };
  go(0);
};

let helpers = (src: string): string => {
  let start = index_of(src, "  let ^picture = {");
  let body = String.sub(src, start, String.length(src) - start);
  let a = index_of(body, "    let num = fun x : Float -> to_fixed(x, 1);");
  let b = index_of(body, "    let view = fun m : Model ->");
  String.sub(body, a, b - a)
  |> String.split_on_char('\n')
  |> List.map(l => {
       let t = String.trim(l);
       String.length(t) > 0 && t.[String.length(t) - 1] == ';'
         ? String.sub(l, 0, String.length(l) - 1) ++ " in" : l;
     })
  |> String.concat("\n");
};

let svg = {|Html.node("svg",
  [Attr.create("viewBox", view_box(pic)), Attr.width("320"),
   Attr.height("440"),
   Attr.style([("border", "1px solid #ccc"), ("background", "#fafafa")])],
  shape_nodes(pic))|};

let settings = [("off", (-1)), ("depth1", 1), ("full", max_int)];

let measure = () => {
  let src = read(Test_IncrSlides.playground ++ "scenes.hz");
  let cut = index_of(src, picture_line);
  let prefix = String.sub(src, 0, cut);
  let pic = "let pic = Drawing.frame((x0, y0, x1, y1)) @ backdrop @ figure in\n";
  let len = "let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in\n";
  let a = prefix ++ pic ++ len ++ "len(pic)";
  let b = prefix ++ pic ++ helpers(src) ++ "\n" ++ svg;
  let runs =
    Option.value(
      ~default=7,
      Option.map(int_of_string, Sys.getenv_opt("PICVIEW_RUNS")),
    );
  let progs =
    [("A", a), ("B", b)]
    |> List.map(((n, s)) => {
         let (_, _, elab) = Test_IncrSlides.elab_of(s);
         (n, elab);
       });
  let times = Hashtbl.create(8);
  let results = Hashtbl.create(8);
  for (_ in 1 to runs) {
    List.iter(
      ((p, elab)) =>
        List.iter(
          ((name, depth)) => {
            Ascriptions.memo_depth := depth;
            let n = p ++ " " ++ name;
            Gc.full_major();
            let t0 = Sys.time();
            let r = Evaluator.evaluate_open(~env=Builtins.env_init, elab);
            let ms = 1000. *. (Sys.time() -. t0);
            Hashtbl.replace(results, n, r);
            Hashtbl.replace(
              times,
              n,
              [
                ms,
                ...Option.value(~default=[], Hashtbl.find_opt(times, n)),
              ],
            );
          },
          settings,
        ),
      progs,
    );
  };
  Ascriptions.memo_depth := 1;
  let median = l => List.nth(List.sort(compare, l), List.length(l) / 2);
  List.iter(
    p =>
      List.iter(
        ((name, _)) => {
          let l = Hashtbl.find(times, p ++ " " ++ name);
          Printf.printf(
            "PICVIEW %s %s: %s ms, median %.0f\n%!",
            p,
            name,
            String.concat(" ", List.rev_map(Printf.sprintf("%.0f"), l)),
            median(l),
          );
        },
        settings,
      ),
    ["A", "B"],
  );
  List.iter(
    ((name, _)) =>
      Printf.printf(
        "PICVIEW view %s (B - A medians): %.0f ms; results equal to off: %b\n%!",
        name,
        median(Hashtbl.find(times, "B " ++ name))
        -. median(Hashtbl.find(times, "A " ++ name)),
        Exp.fast_equal(
          Hashtbl.find(results, "B " ++ name),
          Hashtbl.find(results, "B off"),
        ),
      ),
    settings,
  );
  check(bool, "ran", true, true);
};

let tests = (
  "PicView",
  [test_case("woods picture view, timed", `Slow, measure)],
);
