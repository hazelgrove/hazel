open Virtual_dom.Vdom;
open Node;
open Util_web.WebUtil;

/** Format a USD amount with up to 6 decimals; trims for tiny values. */
let format_usd = (n: float): string =>
  if (n >= 1.0) {
    Printf.sprintf("$%.2f", n);
  } else if (n >= 0.01) {
    Printf.sprintf("$%.4f", n);
  } else if (n > 0.0) {
    Printf.sprintf("$%.6f", n);
  } else {
    "$0.00";
  };

/** "12345" → "12,345". Negatives kept. */
let format_int_commas = (n: int): string => {
  let s = string_of_int(abs(n));
  let len = String.length(s);
  let parts = ref([]);
  let i = ref(len);
  while (i^ > 3) {
    parts := [String.sub(s, i^ - 3, 3), ...parts^];
    i := i^ - 3;
  };
  parts := [String.sub(s, 0, i^), ...parts^];
  (n < 0 ? "-" : "") ++ String.concat(",", parts^);
};

let card =
    (~kind: string, ~title: string, ~icon: string, body: list(Node.t))
    : Node.t =>
  div(
    ~attrs=[
      clss([
        "message-container",
        "system-message-container",
        "slash-command-output-message-container",
      ]),
    ],
    [
      div(
        ~attrs=[clss(["slash-card", "slash-card-" ++ kind])],
        [
          div(
            ~attrs=[clss(["slash-card-header"])],
            [
              span(~attrs=[clss(["slash-card-icon"])], [text(icon)]),
              span(~attrs=[clss(["slash-card-title"])], [text(title)]),
            ],
          ),
          div(~attrs=[clss(["slash-card-body"])], body),
        ],
      ),
    ],
  );

let stat_tile =
    (~label: string, ~value: string, ~accent: bool=false, ()): Node.t =>
  div(
    ~attrs=[clss(["slash-stat", ...accent ? ["slash-stat-accent"] : []])],
    [
      div(~attrs=[clss(["slash-stat-label"])], [text(label)]),
      div(~attrs=[clss(["slash-stat-value"])], [text(value)]),
    ],
  );

let kv_row = (~k: string, ~v: string, ~strong=false, ()): Node.t =>
  div(
    ~attrs=[clss(["slash-kv", ...strong ? ["slash-kv-strong"] : []])],
    [
      span(~attrs=[clss(["slash-kv-key"])], [text(k)]),
      span(~attrs=[clss(["slash-kv-value"])], [text(v)]),
    ],
  );

let view_cost = (p: Message.Model.cost_output): Node.t => {
  let model_name = p.cost_model == "" ? "(no model selected)" : p.cost_model;
  let cost_value =
    switch (p.cost_estimated_usd) {
    | None => "—"
    | Some(c) => format_usd(c)
    };
  let footer =
    div(
      ~attrs=[clss(["slash-card-footer"])],
      [
        span(~attrs=[clss(["slash-card-footer-label"])], [text("model")]),
        code(
          ~attrs=[clss(["slash-card-footer-value"])],
          [text(model_name)],
        ),
      ],
    );
  let stats =
    div(
      ~attrs=[clss(["slash-stat-row"])],
      [
        stat_tile(
          ~label="input tokens",
          ~value=format_int_commas(p.cost_input_tokens),
          (),
        ),
        stat_tile(
          ~label="output tokens",
          ~value=format_int_commas(p.cost_output_tokens),
          (),
        ),
        stat_tile(~label="estimated", ~value=cost_value, ~accent=true, ()),
      ],
    );
  card(
    ~kind="cost",
    ~title="Session cost",
    ~icon="$",
    p.cost_model == ""
      ? [
        stats,
        div(
          ~attrs=[clss(["slash-card-note"])],
          [text("Pick a model to enable the $ estimate.")],
        ),
      ]
      : [
        stats,
        footer,
        div(
          ~attrs=[clss(["slash-card-note"])],
          [
            text(
              "Cost uses the currently-selected model's pricing across all turns.",
            ),
          ],
        ),
      ],
  );
};

let view_credits = (p: Message.Model.credits_output): Node.t => {
  let remaining = p.credits_total -. p.credits_used;
  let pct =
    if (p.credits_total <= 0.0) {
      0.0;
    } else {
      let r = p.credits_used /. p.credits_total *. 100.0;
      if (r < 0.0) {
        0.0;
      } else if (r > 100.0) {
        100.0;
      } else {
        r;
      };
    };
  let bar =
    div(
      ~attrs=[clss(["slash-progress"])],
      [
        div(
          ~attrs=[
            clss(["slash-progress-fill"]),
            Attr.create("style", Printf.sprintf("width: %.1f%%;", pct)),
          ],
          [],
        ),
      ],
    );
  let stats =
    div(
      ~attrs=[clss(["slash-stat-row"])],
      [
        stat_tile(~label="used", ~value=format_usd(p.credits_used), ()),
        stat_tile(~label="total", ~value=format_usd(p.credits_total), ()),
        stat_tile(
          ~label="remaining",
          ~value=format_usd(remaining),
          ~accent=true,
          (),
        ),
      ],
    );
  card(
    ~kind="credits",
    ~title="OpenRouter credits",
    ~icon="◐",
    [stats, bar],
  );
};

let view_usage = (p: Message.Model.usage_output): Node.t => {
  let opt_row = (label: string, value: option(float)): option(Node.t) =>
    switch (value) {
    | None => None
    | Some(v) => Some(kv_row(~k=label, ~v=format_usd(v), ()))
    };
  let tier_row =
    kv_row(~k="tier", ~v=p.usage_is_free_tier ? "free" : "paid", ());
  let total_row =
    kv_row(~k="total usage", ~v=format_usd(p.usage_total), ~strong=true, ());
  let candidates: list(option(Node.t)) = [
    Some(tier_row),
    Some(total_row),
    opt_row("today", p.usage_daily),
    opt_row("this week", p.usage_weekly),
    opt_row("this month", p.usage_monthly),
    opt_row("limit", p.usage_limit),
    opt_row("remaining", p.usage_remaining),
  ];
  let rows = List.filter_map(x => x, candidates);
  card(
    ~kind="usage",
    ~title="OpenRouter key usage",
    ~icon="◌",
    [div(~attrs=[clss(["slash-kv-grid"])], rows)],
  );
};

let view_key = (key: string): Node.t =>
  if (key == "") {
    card(
      ~kind="key",
      ~title="OpenRouter API key",
      ~icon="⚿",
      [
        div(
          ~attrs=[clss(["slash-card-note"])],
          [text("No API key set. Open Settings to add one.")],
        ),
      ],
    );
  } else {
    card(
      ~kind="key",
      ~title="OpenRouter API key",
      ~icon="⚿",
      [
        div(
          ~attrs=[clss(["slash-key-value"])],
          [code(~attrs=[clss(["slash-key-code"])], [text(key)])],
        ),
      ],
    );
  };

let view_help = (p: Message.Model.help_output): Node.t => {
  let rows =
    List.map(
      (e: Message.Model.help_entry) =>
        div(
          ~attrs=[clss(["slash-help-row"])],
          [
            code(
              ~attrs=[clss(["slash-help-cmd"])],
              [text("/" ++ e.help_name)],
            ),
            span(
              ~attrs=[clss(["slash-help-desc"])],
              [text(e.help_description)],
            ),
          ],
        ),
      p.help_entries,
    );
  card(
    ~kind="help",
    ~title="Slash commands",
    ~icon="?",
    [div(~attrs=[clss(["slash-help-list"])], rows)],
  );
};

let view_error = (msg: string): Node.t =>
  card(~kind="error", ~title="Slash command", ~icon="!", [text(msg)]);

let view_notice = (msg: string): Node.t =>
  card(~kind="notice", ~title="Slash command", ~icon="i", [text(msg)]);

let view = (payload: Message.Model.slash_command_payload): Node.t =>
  switch (payload) {
  | CostOutput(p) => view_cost(p)
  | CreditsOutput(p) => view_credits(p)
  | UsageOutput(p) => view_usage(p)
  | KeyOutput(k) => view_key(k)
  | HelpOutput(p) => view_help(p)
  | Notice(s) => view_notice(s)
  | SlashError(s) => view_error(s)
  };
