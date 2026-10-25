open Util;
open Haz3lcore;

/* What went over the API up to a request: its system prompt, tools and
   messages, and each earlier request's context where it was sent. Rebuilt
   from the transcript and the records (SentView) through AgentSend.payload,
   the path the send takes, so it is exact without storing request bodies,
   which grow with the square of a chat's length. */

[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | Request
  | System
  | Tools
  | Tool
  | Developer
  | User
  | Context
  | Assistant
  | Call
  | Result
  | Summary;

[@deriving (show({with_path: false}), sexp, yojson)]
type section = {
  kind,
  label: string,
  /* for reading: the content, or JSON indented */
  shown: string,
  /* exactly as sent: the content, or a tool call's arguments */
  text: string,
  /* exactly as sent: the section's JSON in the body */
  raw: string,
  folded: bool,
  /* false: shown for reading, but not in the selected request's body (an
     earlier request's context, a message compacted away since) */
  in_request: bool,
  children: list(section),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  seq: int,
  /* the request's body, exactly as sent */
  body: string,
  sections: list(section),
};

/* the transcript as it stood when [r] was sent: the branch from the root
   to [r.after], whichever branch is current now, and [r]'s context */
let chat_at = (chat: Chat.Model.t, r: SentView.t): option(Chat.Model.t) => {
  let parent =
    Id.Map.fold(
      (id, m: Message.Model.t, acc) =>
        List.fold_left((acc, c) => Id.Map.add(c, id, acc), acc, m.children),
      chat.message_map,
      Id.Map.empty,
    );
  let rec up = (id, path) =>
    id == chat.root
      ? Some([id, ...path])
      : Option.bind(Id.Map.find_opt(id, parent), p => up(p, [id, ...path]));
  let rec link = (map, ids) =>
    switch (ids) {
    | [a, b, ...rest] =>
      link(
        Id.Map.update(
          a,
          Option.map((m: Message.Model.t) =>
            {
              ...m,
              current_child: Some(b),
            }
          ),
          map,
        ),
        [b, ...rest],
      )
    | [a] =>
      Id.Map.update(
        a,
        Option.map((m: Message.Model.t) =>
          {
            ...m,
            current_child: None,
          }
        ),
        map,
      )
    | [] => map
    };
  Option.map(
    path =>
      {
        ...chat,
        message_map: link(chat.message_map, path),
        context: Some(Message.Utils.mk_context_system_message(r.view)),
      },
    Id.Map.mem(r.after, chat.message_map) ? up(r.after, []) : None,
  );
};

/* [r]'s body, and each of its messages with the transcript message it
   came from (None: the context, last) */
let rebuild =
    (chat: Chat.Model.t, r: SentView.t)
    : option((API.Json.t, list((option(Message.Model.t), API.Json.t)))) =>
  switch (r.request, chat_at(chat, r)) {
  | (Some(request), Some(at)) =>
    let payload =
      AgentSend.payload(
        ~chat_id=chat.id,
        request,
        Chat.Utils.api_messages_for_openrouter(at),
      );
    let (body, messages) =
      OpenRouter.Payload.Utils.body_parts(
        ~payload=OpenRouter.Utils.streaming(payload),
      );
    let sources =
      List.filter(
        (m: Message.Model.t) => Option.is_some(m.api_message),
        Chat.Utils.messages_for_openrouter({
          ...at,
          context: None,
        }),
      );
    let rec pair = (ss, js) =>
      switch (ss, js) {
      | ([s, ...ss], [j, ...js]) => [(Some(s), j), ...pair(ss, js)]
      | ([], js) => List.map(j => (None, j), js)
      | (_, []) => []
      };
    Some((body, pair(sources, messages)));
  | _ => None
  };

/* the body of request [seq], exactly as sent */
let body = (chat: Chat.Model.t, seq: int): option(string) =>
  Option.bind(List.find_opt((r: SentView.t) => r.seq == seq, chat.sent), r =>
    Option.map(((b, _)) => API.Json.to_string(b), rebuild(chat, r))
  );

let field = (key, json: API.Json.t): option(API.Json.t) =>
  switch (json) {
  | `Assoc(fs) => List.assoc_opt(key, fs)
  | _ => None
  };

let string_field = (key, json) =>
  switch (field(key, json)) {
  | Some(`String(s)) => s
  | _ => ""
  };

let pretty = (json: API.Json.t): string =>
  Yojson.Safe.pretty_to_string(json);

/* a tool call's arguments for reading: one per line, code laid out
   as lines under its name */
let pretty_args = (args: string): string =>
  switch (Yojson.Safe.from_string(args)) {
  | `Assoc(fields) =>
    fields
    |> List.map(((k, v)) =>
         switch (v) {
         | `String(s) when String.contains(s, '\n') =>
           k
           ++ ":\n"
           ++ (
             String.split_on_char('\n', s)
             |> List.map(l => "  " ++ l)
             |> String.concat("\n")
           )
         | v => k ++ ": " ++ API.Json.to_string(v)
         }
       )
    |> String.concat("\n")
  | json => pretty(json)
  | exception _ => args
  };

let section =
    (
      ~folded=false,
      ~in_request=true,
      ~children=[],
      ~shown=?,
      kind,
      label,
      text,
      json,
    )
    : section => {
  kind,
  label,
  shown: Option.value(shown, ~default=text),
  text,
  raw: API.Json.to_string(json),
  folded,
  in_request,
  children,
};

/* a message's text as sent: a string, or its one text block */
let content = (json: API.Json.t): string =>
  switch (field("content", json)) {
  | Some(`String(s)) => s
  | Some(`List([block, ..._])) => string_field("text", block)
  | _ => ""
  };

/* [until]: the last request that sent [m], when before the selected one */
let message_section =
    (m: Message.Model.t, json: API.Json.t, ~until: option(int)): section => {
  let note =
    switch (until) {
    | Some(last) =>
      Printf.sprintf(
        {js| · sent until request %d · not in this request|js},
        last,
      )
    | None => ""
    };
  let s = (~folded=false, ~children=[], kind, label) =>
    section(
      ~folded,
      ~in_request=until == None,
      ~children,
      kind,
      label ++ note,
      content(json),
      json,
    );
  switch (m.role) {
  | System(Prompt) => s(~folded=true, System, "System prompt")
  | System(DeveloperNotes) => s(Developer, "Developer notes")
  | System(CompactionSummary(_)) => s(Summary, "Compaction summary")
  | System(_) =>
    s(
      System,
      string_field("role", json) == "user" ? "User note" : "System note",
    )
  | User => s(User, "User")
  | ToolResult(r) => s(Result, "Tool result · " ++ r.tool_call.name)
  | Agent(_) =>
    let calls =
      switch (field("tool_calls", json)) {
      | Some(`List(calls)) =>
        List.map(
          call => {
            let f = Option.value(field("function", call), ~default=`Null);
            let args = string_field("arguments", f);
            section(
              ~shown=pretty_args(args),
              Call,
              "Tool call · " ++ string_field("name", f),
              args,
              call,
            );
          },
          calls,
        )
      | _ => []
      };
    s(
      ~children=calls,
      Assistant,
      switch (List.length(calls)) {
      | 0 => "Assistant"
      | n =>
        "Assistant · "
        ++ CompositionView.plural(n, "tool call", "tool calls")
      },
    );
  };
};

/* a context for reading: no blank lines around its tags */
let tidy = (s: string): string => {
  let tag = l => {
    let l = String.trim(l);
    l == "```"
    || String.length(l) > 1
    && l.[0] == '<'
    && l.[String.length(l) - 1] == '>';
  };
  let lines = Array.of_list(String.split_on_char('\n', String.trim(s)));
  let n = Array.length(lines);
  let near_tag = i =>
    i > 0 && tag(lines[i - 1]) || i < n - 1 && tag(lines[i + 1]);
  Array.to_list(lines)
  |> List.filteri((i, l) => String.trim(l) != "" || !near_tag(i))
  |> String.concat("\n");
};

/* a request carries only its own context; the earlier ones show where
   they were sent, for reading */
let context_section = (~selected: bool, seq: int, json: API.Json.t) =>
  section(
    ~folded=!selected,
    ~in_request=selected,
    ~shown=tidy(content(json)),
    Context,
    Printf.sprintf(
      {js|Context · sent with request %d · %s|js},
      seq,
      selected ? "in this request" : "not in this request",
    ),
    content(json),
    json,
  );

/* the tools, then the request's own fields */
let head_sections = (seq: int, body: API.Json.t): list(section) => {
  let fields =
    switch (body) {
    | `Assoc(fs) => fs
    | _ => []
    };
  let own =
    `Assoc(
      List.filter(((k, _)) => k != "tools" && k != "messages", fields),
    );
  let tools =
    switch (List.assoc_opt("tools", fields)) {
    | Some(`List(ts)) => ts
    | _ => []
    };
  let name = tool =>
    string_field(
      "name",
      Option.value(field("function", tool), ~default=`Null),
    );
  let request =
    section(
      ~folded=true,
      ~shown=pretty(own),
      Request,
      Printf.sprintf("Request %d · %s", seq, string_field("model", body)),
      API.Json.to_string(own),
      own,
    );
  [
    section(
      ~folded=true,
      ~shown="",
      ~children=
        List.map(
          tool =>
            section(
              ~folded=true,
              ~shown=pretty(tool),
              Tool,
              name(tool),
              API.Json.to_string(tool),
              tool,
            ),
          tools,
        ),
      Tools,
      "Tool definitions · " ++ string_of_int(List.length(tools)),
      API.Json.to_string(`List(tools)),
      `List(tools),
    ),
    request,
  ];
};

/* the system prompt, the tools and the request's fields, then the rest */
let assemble = (seq: int, body: API.Json.t, messages: list(section)): t => {
  seq,
  body: API.Json.to_string(body),
  sections:
    switch (messages) {
    | [{kind: System, _} as prompt, ...rest] =>
      [prompt, ...head_sections(seq, body)] @ rest
    | _ => head_sections(seq, body) @ messages
    },
};

/* a message with no transcript message to name it: by its role */
let role_section = (json: API.Json.t): section => {
  let role = string_field("role", json);
  section(
    switch (role) {
    | "user" => User
    | "assistant" => Assistant
    | "tool" => Result
    | _ => System
    },
    "Message · " ++ role,
    content(json),
    json,
  );
};

/* request [seq]'s window: exactly the messages its body carried, in
   order, each section's JSON the body's own; its context last */
let window = (chat: Chat.Model.t, seq: int): option(t) =>
  Option.bind(List.find_opt((r: SentView.t) => r.seq == seq, chat.sent), r =>
    Option.map(
      ((body, parts)) => {
        let last = List.length(parts) - 1;
        assemble(
          seq,
          body,
          List.mapi(
            (i, (m: option(Message.Model.t), json)) =>
              switch (m) {
              | Some(m) => message_section(m, json, ~until=None)
              | None when i == last =>
                context_section(~selected=true, seq, json)
              | None => role_section(json)
              },
            parts,
          ),
        );
      },
      rebuild(chat, r),
    )
  );

/* request [seq]'s trajectory: the branch it was sent on, each message as
   the latest request on it up to [seq] sent it (one compacted away later
   still shows), and each request's context after the message it followed */
let build = (chat: Chat.Model.t, seq: int): option(t) => {
  let upto = List.filter((r: SentView.t) => r.seq <= seq, chat.sent);
  switch (List.find_opt((r: SentView.t) => r.seq == seq, upto)) {
  | None => None
  | Some(r) =>
    switch (chat_at(chat, r), rebuild(chat, r)) {
    | (Some(at), Some((body, _))) =>
      let path = Chat.Utils.linearize(at);
      let on_path =
        List.filter(
          (q: SentView.t) =>
            List.exists((m: Message.Model.t) => m.id == q.after, path),
          upto,
        );
      let rebuilt = List.map(q => (q, rebuild(chat, q)), on_path);
      /* each message's JSON as the latest request sent it */
      let sent =
        List.fold_left(
          (acc, (q: SentView.t, b)) =>
            switch (b) {
            | Some((_, ms)) =>
              List.fold_left(
                (acc, (m: option(Message.Model.t), j)) =>
                  switch (m) {
                  | Some(m) => Id.Map.add(m.id, (q.seq, j), acc)
                  | None => acc
                  },
                acc,
                ms,
              )
            | None => acc
            },
          Id.Map.empty,
          rebuilt,
        );
      let context = ((q: SentView.t, b)) =>
        context_section(
          ~selected=q.seq == seq,
          q.seq,
          switch (b) {
          | Some((_, ms)) => snd(ListUtil.last(ms))
          /* recorded before requests were: the context as sent alone */
          | None =>
            OpenRouter.Message.Utils.json_of_message(
              OpenRouter.Message.Utils.mk_system_msg(q.view),
            )
          },
        );
      let messages =
        List.concat_map(
          (m: Message.Model.t) =>
            (
              switch (Id.Map.find_opt(m.id, sent)) {
              | Some((last, json)) => [
                  message_section(
                    m,
                    json,
                    ~until=last < seq ? Some(last) : None,
                  ),
                ]
              | None => []
              }
            )
            @ List.map(
                context,
                List.filter(
                  ((q: SentView.t, _)) => q.after == m.id,
                  rebuilt,
                ),
              ),
          path,
        );
      Some(assemble(seq, body, messages));
    | _ => None
    }
  };
};

/* the trajectory in one piece: each section's JSON as sent, a line each */
let to_lines = (t: t): string =>
  String.concat("\n", List.map((s: section) => s.raw, t.sections));

/* a section's size, by Unicode character */
let chars = (s: string): int => {
  let n = ref(0);
  String.iter(c => Char.code(c) land 0xC0 != 0x80 ? incr(n) : (), s);
  n^;
};
