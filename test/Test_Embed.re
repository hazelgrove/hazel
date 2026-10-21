/* Embed mode (src/web/embed) without a page: what the frame shows and
   reports for a program a page sends, and its messages as JS values. The
   programs are Dress Up's Scripting panel's (Test_ModuleProgram has the
   texts). */
open Alcotest;
open Haz3lcore;
open Js_of_ocaml;
open Web;

module Panel = Test_ModuleProgram;

let parts = (~common=Panel.common, ~trap=Panel.trap, ~tail=Panel.tail, ()) =>
  EmbedModel.Program.Parts({
    preamble: Panel.preamble,
    modules: [("Common", common), ("TrapRoom", trap)],
    tail,
  });

let set = (program: EmbedModel.Program.t): EmbedModel.Model.t =>
  EmbedModel.Update.update(Set(program), EmbedModel.Model.init);

let status = (m: EmbedModel.Model.t): EmbedModel.Status.t =>
  switch (m.status) {
  | Some(s) => s
  | None => fail("no status")
  };

let shown = (m: EmbedModel.Model.t): string =>
  CodeWithStatics.Model.to_string(m.cell);

let json = (v: Js.Unsafe.any): string =>
  Js.to_string(Js.Unsafe.meth_call(Js._JSON, "stringify", [|v|]));

let as_js = (j: string): Js.Unsafe.any =>
  Js.Unsafe.meth_call(
    Js._JSON,
    "parse",
    [|Js.Unsafe.inject(Js.string(j))|],
  );

let decode = (j: string) => EmbedProtocol.decode(as_js(j));

let messages = (ms: list(EmbedModel.Outbox.message)): list(string) =>
  List.map(
    (m: EmbedModel.Outbox.message) =>
      switch (m) {
      | Ready(_) => "ready"
      | Size(h) => "size " ++ string_of_int(h)
      | Status(_) => "status"
      | Crash(_) => "crash"
      },
    ms,
  );

let tests = (
  "Embed",
  [
    test_case(
      "a text is one read-only cell, with statics",
      `Quick,
      () => {
        let text = "let x : Int = true in x";
        let m = set(Text(text));
        check(string, "the cell holds the text", text, shown(m));
        check(
          bool,
          "its tiles carry an error",
          true,
          m.cell.statics.error_ids != [],
        );
        check(
          list(string),
          "the status has it, the program's",
          ["(program)"],
          Panel.modules_of(status(m).errors),
        );
      },
    ),
    test_case(
      "a program's parts are laid out as one text, stitched as analyzeProgram stitches them",
      `Quick,
      () => {
        let m = set(parts());
        check(
          string,
          "the cell's text",
          Util.StringUtil.trim_leading(
            ModuleProgram.text_of_parts(
              ~preamble=Panel.preamble,
              ~modules=[("Common", Panel.common), ("TrapRoom", Panel.trap)],
              ~tail=Panel.tail,
            ),
          ),
          shown(m),
        );
        check(
          list(string),
          "no errors",
          [],
          Panel.modules_of(status(m).errors),
        );
        check(
          list(pair(string, int)),
          "no holes",
          [("Common", 0), ("TrapRoom", 0)],
          status(m).holes,
        );
        check(int, "none outside the modules", 0, status(m).program_holes);
      },
    ),
    test_case(
      "the frame and analyzeProgram agree on the panel's rules", `Quick, () =>
      List.iter(
        ((what, common, trap)) => {
          let s = status(set(parts(~common, ~trap, ())));
          let p = Panel.program(~common, ~trap, ());
          check(
            list(string),
            what ++ ": the errors' modules",
            Panel.modules_of(Panel.errors(p)),
            Panel.modules_of(s.errors),
          );
          check(
            list(pair(string, int)),
            what ++ ": the holes",
            fst(ModuleProgram.holes(p)),
            s.holes,
          );
          check(
            pair(list(pair(string, int)), int),
            what ++ ": the incomplete tiles",
            ModuleProgram.incomplete(p),
            (s.incomplete, s.program_incomplete),
          );
        },
        [
          ("shipped", Panel.common, Panel.trap),
          ("Common.flip2", Panel.common, Panel.flip2_rule),
          ("Say", Panel.common, Panel.say_rule),
          ("Flip(1, Done)", Panel.common, Panel.error_rule),
          ("Common.flip2 with no flip2", "let other = 1", Panel.flip2_rule),
          ("a blank Common", "", Panel.flip2_rule),
          ("a Common of comments", "  # nothing yet #\n", Panel.say_rule),
          ("a `;` after Common's last member", "let a = 1;", Panel.trap),
          (
            "a hole in TrapRoom",
            Panel.common,
            "let on_event = fun (g, e) : (Globals, Event) -> ?",
          ),
          ("an unclosed comment, then code", "# todo\nflip #", Panel.trap),
          (
            "Common missing its last )",
            Panel.common_missing_paren,
            Panel.flip2_rule,
          ),
          ("a stray } in Common", Panel.common_stray_brace, Panel.flip2_rule),
          ("TrapRoom missing its end", Panel.common, Panel.trap_missing_end),
          ("a blank TrapRoom", Panel.common, ""),
        ],
      )
    ),
    test_case(
      "an error is TrapRoom's, on an id the cell's tiles are marked by",
      `Quick,
      () => {
        let m = set(parts(~trap=Panel.error_rule, ()));
        let errors = status(m).errors;
        check(bool, "there is one", true, errors != []);
        check(
          list(string),
          "each TrapRoom's",
          List.map(_ => "TrapRoom", errors),
          Panel.modules_of(errors),
        );
        check(
          bool,
          "each marked in the cell",
          true,
          List.for_all(
            (e: ModuleProgram.error) =>
              List.mem(e.id, m.cell.statics.error_ids),
            errors,
          ),
        );
      },
    ),
    test_case(
      "in a text, the modules are those it defines",
      `Quick,
      () => {
        let text =
          ModuleProgram.text_of_parts(
            ~preamble=Panel.preamble,
            ~modules=[
              ("Common", Panel.common),
              ("TrapRoom", "let on_event = ?"),
            ],
            ~tail=Panel.tail,
          );
        check(
          list(pair(string, int)),
          "holes",
          [("Common", 0), ("TrapRoom", 1)],
          status(set(Text(text))).holes,
        );
      },
    ),
    test_case(
      "a bracket a module leaves open stays inside its braces",
      `Quick,
      () => {
        let m = set(parts(~common=Panel.common_missing_paren, ()));
        check(
          bool,
          "TrapRoom is still laid out as its own module",
          true,
          switch (
            Str.search_forward(
              Str.regexp_string("module TrapRoom = {"),
              shown(m),
              0,
            )
          ) {
          | _ => true
          | exception Not_found => false
          },
        );
        check(
          list(string),
          "TrapRoom sees Common.flip2",
          [],
          Panel.modules_of(status(m).errors),
        );
        check(
          list(pair(string, int)),
          "the bracket is Common's",
          [("Common", 1), ("TrapRoom", 0)],
          status(m).incomplete,
        );
      },
    ),
    test_case(
      "a set reads again only the bodies whose text changed",
      `Quick,
      () => {
        let m = set(parts());
        let m' =
          EmbedModel.Update.update(Set(parts(~trap=Panel.say_rule, ())), m);
        let read = (m: EmbedModel.Model.t, name) =>
          List.find((b: EmbedModel.body) => b.name == name, m.bodies).read;
        check(
          bool,
          "Common, unchanged, as it was read",
          true,
          read(m, "Common") === read(m', "Common"),
        );
        check(
          bool,
          "TrapRoom, changed, read again",
          false,
          read(m, "TrapRoom") === read(m', "TrapRoom"),
        );
        check(
          string,
          "and shown",
          Util.StringUtil.trim_leading(
            ModuleProgram.text_of_parts(
              ~preamble=Panel.preamble,
              ~modules=[
                ("Common", Panel.common),
                ("TrapRoom", Panel.say_rule),
              ],
              ~tail=Panel.tail,
            ),
          ),
          shown(m'),
        );
      },
    ),
    test_case(
      "the same program again changes nothing",
      `Quick,
      () => {
        let m = set(parts());
        check(
          bool,
          "the same model",
          true,
          EmbedModel.Update.update(Set(parts()), m) === m,
        );
        let m' =
          EmbedModel.Update.update(
            SetFontMetrics({
              row_height: 20.,
              col_width: 9.,
            }),
            m,
          );
        check(
          bool,
          "font metrics leave the cell be",
          true,
          m'.cell === m.cell && m'.font_metrics.row_height == 20.,
        );
      },
    ),
    test_case(
      "size and status are posted only when they change",
      `Quick,
      () => {
        open EmbedModel.Outbox;
        let (ms, sent) =
          after_display(~height=40, EmbedModel.Model.init, nothing_sent);
        check(
          list(string),
          "no program: its size",
          ["size 40"],
          messages(ms),
        );
        let m = set(parts());
        let (ms, sent) = after_display(~height=40, m, sent);
        check(list(string), "a program", ["status"], messages(ms));
        let (ms, sent) = after_display(~height=40, m, sent);
        check(list(string), "nothing new", [], messages(ms));
        let (ms, sent) = after_display(~height=55, m, sent);
        check(list(string), "taller", ["size 55"], messages(ms));
        let m =
          EmbedModel.Update.update(
            Set(parts(~trap=Panel.error_rule, ())),
            m,
          );
        let (ms, sent) = after_display(~height=55, m, sent);
        check(list(string), "an error", ["status"], messages(ms));
        let m = EmbedModel.Update.update(Set(parts()), m);
        let (ms, sent) = after_display(~height=55, m, sent);
        check(list(string), "fixed", ["status"], messages(ms));
        let (ms, _) =
          after_display(
            ~height=55,
            EmbedModel.Update.update(Set(parts()), m),
            sent,
          );
        check(list(string), "sent again", [], messages(ms));
      },
    ),
    test_case(
      "the messages a page sends",
      `Quick,
      () => {
        let is = (what, expected, got) =>
          check(bool, what, true, got == expected);
        is(
          "a text",
          Some(EmbedProtocol.Set(Text("1 + 2"))),
          decode({|{"type": "set", "text": "1 + 2"}|}),
        );
        is(
          "parts, with result for the tail",
          Some(
            EmbedProtocol.Set(
              Parts({
                preamble: "type T = Int in",
                modules: [("Common", "let a = 1")],
                tail: "Common.a",
              }),
            ),
          ),
          decode(
            {|{"type": "set", "preamble": "type T = Int in",
               "modules": [{"name": "Common", "text": "let a = 1"}],
               "result": "Common.a"}|},
          ),
        );
        is(
          "tail before result",
          Some(
            EmbedProtocol.Set(
              Parts({
                preamble: "",
                modules: [],
                tail: "1",
              }),
            ),
          ),
          decode({|{"type": "set", "tail": "1", "result": "2"}|}),
        );
        is(
          "nothing given: the empty program",
          Some(
            EmbedProtocol.Set(
              Parts({
                preamble: "",
                modules: [],
                tail: "()",
              }),
            ),
          ),
          decode({|{"type": "set"}|}),
        );
        is("ping", Some(EmbedProtocol.Ping), decode({|{"type": "ping"}|}));
        List.iter(
          j => is(j ++ " is not one", None, decode(j)),
          [
            {|{"type": "size", "height": 3}|},
            {|{"text": "1"}|},
            {|"set"|},
            "null",
            "[]",
          ],
        );
      },
    ),
    test_case(
      "the origins a page may send from",
      `Quick,
      () => {
        let own = "https://hazel.org";
        List.iter(
          ((origin, allowed)) =>
            check(
              bool,
              origin,
              allowed,
              EmbedProtocol.origin_allowed(~own, origin),
            ),
          [
            ("https://matthewhammer.org", true),
            ("https://www.matthewhammer.org", true),
            ("http://localhost:8000", true),
            ("http://localhost", true),
            ("https://localhost:8443", true),
            ("http://127.0.0.1:5173", true),
            ("https://hazel.org", true),
            ("http://matthewhammer.org", false),
            ("https://matthewhammer.org.example.com", false),
            ("https://evil.example", false),
            ("http://localhost.example.com", false),
            ("http://localhost:", false),
            ("http://localhost:80a", false),
            ("http://127.0.0.1@example.com", false),
            ("null", false),
          ],
        );
        check(
          bool,
          "an opaque frame does not take an opaque origin's messages",
          false,
          EmbedProtocol.origin_allowed(~own="null", "null"),
        );
      },
    ),
    test_case(
      "the messages the frame sends",
      `Quick,
      () => {
        check(
          string,
          "ready",
          {|{"type":"ready","protocol":1,"build":"|}
          ++ BuildStamp.commit
          ++ {|"}|},
          json(EmbedProtocol.encode(Ready(BuildStamp.commit))),
        );
        check(
          string,
          "size",
          {|{"type":"size","height":42}|},
          json(EmbedProtocol.encode(Size(42))),
        );
        check(
          string,
          "crash",
          {|{"type":"crash","message":"Not_found"}|},
          json(EmbedProtocol.encode(Crash("Not_found"))),
        );
        let status =
          EmbedProtocol.encode(
            Status(status(set(parts(~trap=Panel.error_rule, ())))),
          );
        let field = (j, k) => Js.Unsafe.get(j, k);
        check(
          string,
          "status: type",
          "status",
          Js.to_string(field(status, "type")),
        );
        check(bool, "ok", false, Js.to_bool(field(status, "ok")));
        check(
          string,
          "an error's module",
          "TrapRoom",
          Js.to_string(
            field(field(field(status, "errors"), "0"), "module"),
          ),
        );
        check(
          string,
          "holes",
          {|{"Common":0,"TrapRoom":0}|},
          json(field(status, "holes")),
        );
        check(
          string,
          "incomplete",
          {|{"Common":0,"TrapRoom":0}|},
          json(field(status, "incomplete")),
        );
        check(
          int,
          "programIncomplete",
          0,
          int_of_float(
            Js.float_of_number(field(status, "programIncomplete")),
          ),
        );
        check(
          int,
          "programHoles",
          0,
          int_of_float(Js.float_of_number(field(status, "programHoles"))),
        );
      },
    ),
    test_case(
      "the link: what the frame takes from its page, and what it posts",
      `Quick,
      () => {
        let parent = as_js("{}");
        let posted = ref([]);
        let link =
          EmbedLink.make(
            ~parent, ~own_origin="https://hazel.org", ~post=(data, target) =>
            posted :=
              posted^
              @ [
                Js.to_string(Js.Unsafe.get(data, "type")) ++ " to " ++ target,
              ]
          );
        let take = () => {
          let p = posted^;
          posted := [];
          p;
        };
        let event = (~source=parent, ~origin, data) =>
          HazelLibCore.js_object([
            ("origin", HazelLibCore.js_string(origin)),
            ("source", source),
            ("data", as_js(data)),
          ]);
        let page = "http://localhost:8000";
        let a_set = {|{"type": "set", "text": "1 + 2"}|};
        let model = set(Text("1 + 2"));
        EmbedLink.ready(link);
        check(
          list(string),
          "ready, before any message",
          ["ready to *"],
          take(),
        );
        EmbedLink.crash(link, "Not_found");
        EmbedLink.crash(link, "Not_found");
        check(list(string), "a crash, once", ["crash to *"], take());
        EmbedLink.report(link, ~height=40, model);
        check(
          list(string),
          "no size or status before a message",
          [],
          take(),
        );
        check(
          bool,
          "a set from another window is ignored",
          true,
          EmbedLink.receive(
            link,
            event(~source=as_js("{}"), ~origin=page, a_set),
          )
          == None
          && link.page_origin == None,
        );
        check(
          bool,
          "a set from an origin not allowed is ignored",
          true,
          EmbedLink.receive(
            link,
            event(~origin="https://evil.example", a_set),
          )
          == None
          && link.page_origin == None,
        );
        check(
          bool,
          "an origin that is not a string is ignored",
          true,
          EmbedLink.receive(
            link,
            HazelLibCore.js_object([
              ("origin", Js.Unsafe.inject(Js.null)),
              ("source", parent),
              ("data", as_js(a_set)),
            ]),
          )
          == None,
        );
        check(
          bool,
          "a set from the parent, from an allowed origin",
          true,
          EmbedLink.receive(link, event(~origin=page, a_set))
          == Some(Text("1 + 2"))
          && link.page_origin == Some(page),
        );
        check(list(string), "nothing posted for a set", [], take());
        EmbedLink.report(link, ~height=40, model);
        check(
          list(string),
          "then size and status, to the page",
          ["size to " ++ page, "status to " ++ page],
          take(),
        );
        EmbedLink.report(link, ~height=40, model);
        check(list(string), "and not again", [], take());
        check(
          bool,
          "a ping sets nothing",
          true,
          EmbedLink.receive(link, event(~origin=page, {|{"type": "ping"}|}))
          == None,
        );
        check(
          list(string),
          "a ping: ready, to the page",
          ["ready to " ++ page],
          take(),
        );
        EmbedLink.report(link, ~height=40, model);
        check(
          list(string),
          "and size and status again",
          ["size to " ++ page, "status to " ++ page],
          take(),
        );
        EmbedLink.crash(link, "Stack_overflow");
        check(
          list(string),
          "a crash after a message, to the page",
          ["crash to " ++ page],
          take(),
        );
      },
    ),
  ],
);
