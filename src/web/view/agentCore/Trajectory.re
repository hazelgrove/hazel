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
  /* a request and what came back: its context, then the reply */
  | Round
  | Thinking
  | Assistant
  | Call
  | Result
  | Usage
  /* an error, a retry or a cancel: Hazel's line in the chat */
  | Note
  | Summary
  /* Window: the reply the model wrote after the body, its thinking first */
  | Response;

[@deriving (show({with_path: false}), sexp, yojson)]
type section = {
  kind,
  label: string,
  /* the gist on the folded line, after the label */
  brief: string,
  /* for reading: the content, or JSON indented */
  shown: string,
  /* exactly as sent: the content, or a tool call's arguments */
  text: string,
  /* exactly as sent: the section's JSON in the body */
  raw: string,
  folded: bool,
  /* false: shown for reading, but not in the selected request's body (an
     earlier request's context, a message compacted away since, the reply
     to this request) */
  in_request: bool,
  /* false: never sent; received only (thinking, usage, an error), or
     Hazel's own note. Its text is as kept, with no JSON of its own */
  sent: bool,
  children: list(section),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  seq: int,
  /* the request's body, exactly as sent */
  body: string,
  sections: list(section),
  /* its reply, or the error or cancel instead, was in when built */
  replied: bool,
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

/* a section's first words, on one line */
let first_words = (s: string): string => {
  let words =
    String.split_on_char(' ', String.map(c => c == '\n' ? ' ' : c, s))
    |> List.filter(w => w != "");
  let line =
    String.concat(" ", ListUtil.take(min(12, List.length(words)), words));
  String.length(line) > 80 ? String.sub(line, 0, 80) ++ {js|…|js} : line;
};

/* its first line that has anything on it */
let first_line = (s: string): string =>
  switch (
    List.find_opt(l => String.trim(l) != "", String.split_on_char('\n', s))
  ) {
  | Some(l) => first_words(l)
  | None => ""
  };

let section =
    (
      ~folded=false,
      ~in_request=true,
      ~sent=true,
      ~children=[],
      ~shown=?,
      ~brief=?,
      kind,
      label,
      text,
      json,
    )
    : section => {
  kind,
  label,
  brief: Option.value(brief, ~default=first_words(text)),
  shown: Option.value(shown, ~default=text),
  text,
  raw: API.Json.to_string(json),
  folded,
  in_request,
  sent,
  children,
};

/* received or Hazel's own: its text as kept */
let unsent = (~brief=?, ~shown=?, kind, label, text) =>
  section(
    ~folded=true,
    ~in_request=false,
    ~sent=false,
    ~brief?,
    ~shown?,
    kind,
    label,
    text,
    `String(text),
  );

/* a message's text as sent: a string, or its one text block */
let content = (json: API.Json.t): string =>
  switch (field("content", json)) {
  | Some(`String(s)) => s
  | Some(`List([block, ..._])) => string_field("text", block)
  | _ => ""
  };

/* where a message stands to the selected request */
type place =
  /* in its body */
  | This
  /* sent last by an earlier request */
  | Until(int)
  /* the reply to it, sent by later ones */
  | After
  /* Window: the reply to it, written after its body */
  | Generated;

let tokens = (n: int): string =>
  n >= 1000
    ? Printf.sprintf("%.1fk", float_of_int(n) /. 1000.) : string_of_int(n);

/* what came back with a reply besides the message: its thinking, before
   it, and its usage, after */
let thinking = (~generated=false, m: Message.Model.t): list(section) =>
  switch (m.reasoning) {
  | Some(r) when String.trim(r) != "" =>
    let s =
      unsent(
        Thinking,
        Printf.sprintf(
          {js|Thinking%s · %s|js},
          switch (m.reasoning_duration_ms) {
          | Some(ms) => Printf.sprintf({js| · %ds|js}, (ms + 500) / 1000)
          | None => ""
          },
          generated
            ? "in the model's context while it wrote this reply; not kept for the next request"
            : "received, not sent back",
        ),
        r,
      );
    [
      {
        ...s,
        folded: !generated,
      },
    ];
  | _ => []
  };

let usage = (m: Message.Model.t): list(section) =>
  switch (m.role) {
  | Agent(Some(u)) =>
    let text =
      pretty(
        switch (OpenRouter.Reply.Model.yojson_of_usage(u)) {
        | `Assoc(fs) => `Assoc(List.filter(((_, v)) => v != `Null, fs))
        | j => j
        },
      );
    [
      unsent(
        ~brief=
          String.concat(
            {js| · |js},
            [
              tokens(u.prompt_tokens) ++ " in",
              tokens(u.completion_tokens) ++ " out",
            ]
            @ (
              switch (u.cache_read_input_tokens) {
              | Some(c) when c > 0 => ["cached: " ++ tokens(c)]
              | _ => []
              }
            )
            @ (
              switch (u.cost) {
              | Some(c) => [Printf.sprintf("%.6f cr", c)]
              | None => []
              }
            ),
          ),
        Usage,
        "Usage · received",
        text,
      ),
    ];
  | _ => []
  };

/* a transcript message as [json] carries it */
let message_section =
    (m: Message.Model.t, json: API.Json.t, ~place: place): list(section) => {
  let note =
    switch (place) {
    | This => ""
    | Until(last) =>
      Printf.sprintf(
        {js| · sent until request %d · not in this request|js},
        last,
      )
    | After => {js| · after this request|js}
    | Generated => {js| · sent back in later requests|js}
    };
  let s = (~folded=false, ~children=[], ~brief=?, kind, label) =>
    section(
      ~folded,
      ~in_request=place == This,
      ~children,
      ~brief?,
      kind,
      label ++ note,
      content(json),
      json,
    );
  switch (m.role) {
  | System(Prompt) => [s(~folded=true, System, "System prompt")]
  | System(DeveloperNotes) => [s(Developer, "Developer notes")]
  | System(CompactionSummary(_)) => [s(Summary, "Compaction summary")]
  | System(_) => [
      s(
        System,
        string_field("role", json) == "user" ? "User note" : "System note",
      ),
    ]
  | User => [s(User, "User")]
  | ToolResult(r) => [
      s(
        ~folded=true,
        ~brief=first_line(content(json)),
        Result,
        "Tool result · " ++ r.tool_call.name,
      ),
    ]
  | Agent(_) =>
    let calls =
      switch (field("tool_calls", json)) {
      | Some(`List(calls)) =>
        List.map(
          call => {
            let f = Option.value(field("function", call), ~default=`Null);
            let name = string_field("name", f);
            let args = string_field("arguments", f);
            section(
              ~folded=true,
              ~in_request=place == This,
              ~shown=pretty_args(args),
              ~brief="",
              Call,
              ToolCallSummary.line({
                id: string_field("id", call),
                name,
                args: OpenRouter.Utils.parse_tool_args(`String(args)),
              }),
              args,
              call,
            );
          },
          calls,
        )
      | _ => []
      };
    thinking(~generated=place == Generated, m)
    @ [
      s(
        ~children=calls,
        Assistant,
        switch (List.length(calls)) {
        | 0 => "Assistant"
        | n =>
          "Assistant · "
          ++ CompositionView.plural(n, "tool call", "tool calls")
        },
      ),
    ]
    @ usage(m);
  };
};

/* a message no request sent: Hazel's line about an error, a retry or a
   cancel, or a reply set aside for a retry, with its thinking and usage
   (others never concern the API) */
let note_section = (~generated=false, m: Message.Model.t): list(section) =>
  switch (m.role) {
  | Agent(_) when Option.is_none(m.api_message) =>
    thinking(~generated, m)
    @ [unsent(Note, "Empty reply · not sent back, retried", m.content)]
    @ usage(m)
  | System(ApiFailure) => [unsent(Note, "Error · not sent", m.content)]
  | System(RetryNote) => [unsent(Note, "Retry · not sent", m.content)]
  | System(ResponseCancelled) => [
      unsent(Note, "Cancelled · not sent", m.content),
    ]
  | _ => []
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
let context_section =
    (~window=false, ~selected: bool, seq: int, json: API.Json.t) =>
  section(
    ~folded=true,
    ~in_request=selected,
    ~shown=tidy(content(json)),
    ~brief="",
    Context,
    Printf.sprintf(
      {js|Context · sent with request %d · %s|js},
      seq,
      window
        ? "replaced by the next request's snapshot"
        : selected ? "in this request" : "not in this request",
    ),
    content(json),
    json,
  );

/* the sections' JSON as sent, a line each: a round by its contents, a
   user turn by its message then its rounds, nothing unsent */
let rec lines = (s: section): list(string) =>
  switch (s.kind) {
  | Round => List.concat_map(lines, s.children)
  | User => [s.raw, ...List.concat_map(lines, s.children)]
  | _ when !s.sent => []
  | _ => [s.raw]
  };

/* a reply's text, else its calls */
let gist = (children: list(section)): string =>
  switch (List.find_opt((c: section) => c.kind == Assistant, children)) {
  | Some(a) when String.trim(a.text) != "" => first_words(a.text)
  | Some(a) =>
    String.concat("; ", List.map((c: section) => c.label, a.children))
  | None => ""
  };

let round_section = (~selected: int, seq: int, children: list(section)) => {
  let calls =
    List.fold_left(
      (n, c: section) =>
        n
        + List.length(
            List.filter((k: section) => k.kind == Call, c.children),
          ),
      0,
      children,
    );
  let reply = List.exists((c: section) => c.kind != Context, children);
  /* a request with no reply yet: its context is all there is to read */
  let children =
    List.map(
      (c: section) =>
        c.kind == Context && !reply
          ? {
            ...c,
            folded: false,
          }
          : c,
      children,
    );
  let text = String.concat("\n", List.concat_map(lines, children));
  {
    kind: Round,
    label:
      String.concat(
        {js| · |js},
        [Printf.sprintf("Round %d", seq)]
        @ (
          calls > 0
            ? [CompositionView.plural(calls, "tool call", "tool calls")] : []
        )
        @ (seq == selected ? ["this request"] : []),
      ),
    brief: gist(children),
    shown: "",
    text,
    raw: text,
    folded: seq != selected,
    in_request: List.exists((c: section) => c.in_request, children),
    sent: true,
    children,
  };
};

/* what a trajectory is laid out from, in order */
type item =
  | Item(section)
  /* request [seq]'s round begins */
  | Opens(int);

/* a user message opens a turn; a request opens a round in it, which runs
   to the next round or turn. Nesting only: the order stays the items' */
let group = (~selected: int, items: list(item)): list(section) => {
  let close_round = ((top, turn, round)) =>
    switch (round) {
    | None => (top, turn)
    | Some((seq, kids)) =>
      let r = round_section(~selected, seq, List.rev(kids));
      switch (turn) {
      | Some((u, rounds)) => (top, Some((u, [r, ...rounds])))
      | None => ([r, ...top], turn)
      };
    };
  let close_turn = ((top, turn)) =>
    switch (turn) {
    | Some((u: section, kids)) =>
      let kids = List.rev(kids);
      let rounds = List.filter((r: section) => r.kind == Round, kids);
      let label =
        switch (List.length(rounds)) {
        | 0 => u.label
        | n =>
          u.label
          ++ {js| · |js}
          ++ CompositionView.plural(n, "round", "rounds")
        };
      [
        {
          ...u,
          label,
          folded:
            !List.exists((r: section) => !r.folded, rounds) && rounds != [],
          children: u.children @ kids,
        },
        ...top,
      ];
    | None => top
    };
  let (top, turn, round) =
    List.fold_left(
      ((top, turn, round), item) =>
        switch (item) {
        | Item({kind: User, _} as u) =>
          let (top, turn) = close_round((top, turn, round));
          (close_turn((top, turn)), Some((u, [])), None);
        | Item(s) =>
          switch (round, turn) {
          | (Some((seq, kids)), _) => (
              top,
              turn,
              Some((seq, [s, ...kids])),
            )
          | (None, Some((u, rounds))) =>
            /* before the turn's first request: loose in the turn */
            (top, Some((u, [s, ...rounds])), None)
          | (None, None) => ([s, ...top], None, None)
          }
        | Opens(seq) =>
          let (top, turn) = close_round((top, turn, round));
          (top, turn, Some((seq, [])));
        },
      ([], None, None),
      items,
    );
  List.rev(close_turn(close_round((top, turn, round))));
};

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
let assemble =
    (~replied, seq: int, body: API.Json.t, messages: list(section)): t => {
  seq,
  replied,
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

/* the rounds that begin after message [id]: the requests sent after it */
let opens = (rs: list(SentView.t), id: Id.t): list(item) =>
  List.filter_map(
    (q: SentView.t) => q.after == id ? Some(Opens(q.seq)) : None,
    rs,
  );

/* the transcript after message [id] on the current branch */
let rec following = (chat: Chat.Model.t, id: Id.t): list(Message.Model.t) =>
  switch (
    Option.bind(Chat.Utils.find_message_opt(id, chat), (m: Message.Model.t) =>
      m.current_child
    )
  ) {
  | Some(c) =>
    switch (Chat.Utils.find_message_opt(c, chat)) {
    | Some(m) => [m, ...following(chat, c)]
    | None => []
    }
  | None => []
  };

/* what came back for [r]: its reply, or the error or cancel instead,
   before the next user message */
let reply_of = (chat: Chat.Model.t, r: SentView.t): option(Message.Model.t) => {
  let rec go = id =>
    switch (
      Option.bind(
        Chat.Utils.find_message_opt(id, chat), (m: Message.Model.t) =>
        Option.bind(m.current_child, c =>
          Chat.Utils.find_message_opt(c, chat)
        )
      )
    ) {
    | Some(
        {role: Agent(_) | System(ApiFailure | ResponseCancelled), _} as m,
      ) =>
      Some(m)
    | Some({role: User, _})
    | None => None
    | Some(m) => go(m.id)
    };
  go(r.after);
};

/* whether request [seq]'s reply, or its error or cancel, is in */
let replied = (chat: Chat.Model.t, seq: int): bool =>
  switch (List.find_opt((r: SentView.t) => r.seq == seq, chat.sent)) {
  | Some(r) => Option.is_some(reply_of(chat, r))
  | None => false
  };

/* Window's last section: the reply to [r], written after its body, as
   the model wrote it: its thinking, then its text and calls (usage
   folded). Without a reply, a note saying why */
let response = (chat: Chat.Model.t, r: SentView.t): section => {
  let none = (label, text) => {
    ...
      unsent(
        ~brief="",
        Note,
        Printf.sprintf({js|Response %d · %s|js}, r.seq, label),
        text,
      ),
    folded: false,
  };
  switch (reply_of(chat, r)) {
  | Some({role: Agent(_), _} as m) =>
    let children =
      switch (m.api_message) {
      | Some(a) =>
        message_section(
          m,
          OpenRouter.Message.Utils.json_of_message(a),
          ~place=Generated,
        )
      | None => note_section(~generated=true, m)
      };
    /* its size: what the model wrote */
    let text =
      String.concat(
        "\n",
        List.filter_map(
          (c: section) =>
            switch (c.kind) {
            | Usage => None
            | _ => Some(c.sent ? c.raw : c.text)
            },
          children,
        ),
      );
    {
      kind: Response,
      label:
        Printf.sprintf(
          {js|Response %d · generated in this window|js},
          r.seq,
        ),
      brief: gist(children),
      shown: "",
      text,
      raw: text,
      folded: false,
      in_request: false,
      sent: false,
      children,
    };
  | Some({role: System(ApiFailure), content, _}) =>
    none("none · an error came back instead", content)
  | Some({role: System(ResponseCancelled), content, _}) =>
    none("none · cancelled", content)
  | _ =>
    none(
      "not back yet",
      "Still streaming, or none came back. Its thinking and reply show here once they are in.",
    )
  };
};

/* request [seq]'s window: exactly the messages its body carried, in
   order, each section's JSON the body's own; its context last. Then the
   reply, the rest of what the model had in front of it as it answered */
let window = (chat: Chat.Model.t, seq: int): option(t) =>
  Option.bind(List.find_opt((r: SentView.t) => r.seq == seq, chat.sent), r =>
    Option.map(
      ((body, parts)) => {
        let last = List.length(parts) - 1;
        let upto = List.filter((q: SentView.t) => q.seq <= seq, chat.sent);
        let t =
          assemble(
            ~replied=Option.is_some(reply_of(chat, r)),
            seq,
            body,
            group(
              ~selected=seq,
              List.concat(
                List.mapi(
                  (i, (m: option(Message.Model.t), json)) =>
                    switch (m) {
                    | Some(m) =>
                      List.map(
                        s => Item(s),
                        List.filter(
                          (s: section) => s.sent,
                          message_section(m, json, ~place=This),
                        ),
                      )
                      @ opens(upto, m.id)
                    | None when i == last => [
                        Item(
                          context_section(
                            ~window=true,
                            ~selected=true,
                            seq,
                            json,
                          ),
                        ),
                      ]
                    | None => [Item(role_section(json))]
                    },
                  parts,
                ),
              ),
            ),
          );
        {
          ...t,
          sections: t.sections @ [response(chat, r)],
        };
      },
      rebuild(chat, r),
    )
  );

/* request [seq]'s trajectory: the branch it was sent on, each message as
   the latest request on it up to [seq] sent it (one compacted away later
   still shows), each request's context after the message it followed,
   and the reply to [seq] itself, sent only later. A turn per user
   message, a round per request */
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
      let items = (m: Message.Model.t) =>
        List.map(
          s => Item(s),
          switch (Id.Map.find_opt(m.id, sent)) {
          | Some((last, json)) =>
            message_section(m, json, ~place=last < seq ? Until(last) : This)
          | None => note_section(m)
          },
        )
        @ List.concat_map(
            ((q: SentView.t, _) as qb) =>
              q.after == m.id ? [Opens(q.seq), Item(context(qb))] : [],
            rebuilt,
          );
      /* the reply: up to the next request, or to the next user message */
      let later =
        List.filter_map(
          (q: SentView.t) => q.seq > seq ? Some(q.after) : None,
          chat.sent,
        );
      let rec reply = (ms: list(Message.Model.t)) =>
        switch (ms) {
        | [{role: User, _}, ..._]
        | [] => []
        | [m, ...rest] => [m, ...List.mem(m.id, later) ? [] : reply(rest)]
        };
      let replied =
        List.concat_map(
          (m: Message.Model.t) =>
            List.map(
              s => Item(s),
              switch (m.api_message) {
              | Some(a) =>
                message_section(
                  m,
                  OpenRouter.Message.Utils.json_of_message(a),
                  ~place=After,
                )
              | None => note_section(m)
              },
            ),
          reply(following(chat, r.after)),
        );
      Some(
        assemble(
          ~replied=Option.is_some(reply_of(chat, r)),
          seq,
          body,
          group(~selected=seq, List.concat_map(items, path) @ replied),
        ),
      );
    | _ => None
    }
  };
};

/* the trajectory in one piece: each section's JSON as sent, a line each */
let to_lines = (t: t): string =>
  String.concat("\n", List.concat_map(lines, t.sections));

/* a section's size, by Unicode character */
let chars = (s: string): int => {
  let n = ref(0);
  String.iter(c => Char.code(c) land 0xC0 != 0x80 ? incr(n) : (), s);
  n^;
};
