# Plan — Anthropic-native ("Anthropic Skin") endpoint route for full history caching

_Status: proposal / not started. Companion to [`prompt-caching-findings.md`](./prompt-caching-findings.md)._

## Context

We proved (see the findings doc) that OpenRouter's **default** endpoint
(`/api/v1/chat/completions`, OpenAI-compatible) only lets Anthropic cache `system`-role content —
`cache_control` on `user`/`tool` messages is silently dropped. So for Claude we can cache the static
floor (~93% of payload) but **not the growing conversation history**.

OpenRouter also exposes an **Anthropic-native endpoint** (the "Anthropic Skin") that speaks
Anthropic's own Messages API wire format and "passes advanced features through untouched." In that
format, per-block `cache_control` is honored on **all** roles. Routing Claude traffic there *should*
unlock cumulative history caching **without leaving OpenRouter** (same account/key, different
URL + request shape).

This plan adds that second request/response path for Anthropic models, gated behind a verification
spike so we don't build it if the Skin doesn't actually pass caching through.

## Goal & success criteria

- **Goal:** Anthropic models route through OpenRouter's Anthropic-native endpoint; non-Anthropic
  models keep using the existing OpenAI-compat path unchanged.
- **Success:** on a multi-turn Claude session, `cache_read` (via `CacheDiag`) **grows past the
  ~22.3k floor** as history accumulates, and `cache_creation` is nonzero on the delta — i.e. the
  growing history is actually cached.
- **Non-goal:** changing anything for Gemini/Qwen/OpenAI/etc. (they auto-cache or are unaffected).

## Endpoint & auth

| | OpenAI-compat (current) | Anthropic-native (new) |
|---|---|---|
| URL | `https://openrouter.ai/api/v1/chat/completions` | `https://openrouter.ai/api/v1/messages` |
| Auth | `Authorization: Bearer <OPENROUTER_KEY>` | same key; **+ `anthropic-version: 2023-06-01`** header |
| Model id | `anthropic/claude-sonnet-4.6` | **TBD — verify** whether to pass `anthropic/...` or bare `claude-...` (the Skin "handles model mapping"); resolve in Phase 0 |
| Body | OpenAI chat-completions JSON | Anthropic Messages JSON (below) |

> ⚠️ **Phase 0 must confirm:** the exact auth header(s) the Skin wants (the Claude Code setup uses
> `ANTHROPIC_AUTH_TOKEN=$OPENROUTER_KEY` → `Authorization: Bearer`), the model-id form, and — the
> whole point — that `cache_control` on non-system blocks is honored.

---

## Phase 0 — Verification spike (gate; ~half a day)

**Do this before any real implementation.** Hand-build one minimal Anthropic-native request and fire
it twice (a second request that shares a long prefix), watching `cache_read`/`cache_creation`.

- Add a throwaway `OpenRouter.Utils.skin_probe` (or just curl) that POSTs to `/api/v1/messages`:
  - `system`: a large (>4k token) static block with `cache_control` on its last element.
  - `messages`: `[user(big text, cache_control on last block), assistant(short), user(short)]` —
    breakpoint on a **user** message.
  - `max_tokens`, `model`, native tool defs optional for the probe.
- Fire request A, then request B with one more turn appended.
- **Pass:** B's `cache_read` ≈ A's prefix (grows), `cache_creation` nonzero on A. → build the path.
- **Fail:** `cache_read` flat / `cache_creation` null. → the Skin doesn't pass caching; **stop**, fall
  back to direct-Anthropic API (separate key) or shelve.

Reuse `CacheDiag` for the readout. This single test decides whether the rest of the plan happens.

---

## Phase 1 — Native request serializer

New module (e.g. `OpenRouter.AnthropicNative`) that converts our internal message list to Anthropic
Messages JSON. This is the bulk of the work — the wire shape differs structurally from OpenAI's.

### Message mapping (the crux)

Our model: `Message.Model.t = { role: System|Developer|User|Assistant|Tool(tc), content, tool_calls, cache_anchor }`.

| Our message | Anthropic-native target |
|---|---|
| `System`/`Developer` at the **front** (system prompt + dev-notes) | concatenate into the **top-level `system`** field as an array of text blocks; `cache_control` on the dev-notes block = the **floor** |
| `User` | `{role:"user", content:[{type:"text", text}]}` |
| `Assistant` with `tool_calls` | `{role:"assistant", content:[ {type:"text",text}?, {type:"tool_use", id, name, input:args} … ]}` |
| `Tool(tc)` (a tool result) | a `{type:"tool_result", tool_use_id: tc.id, content}` block **inside the following `user` message** |
| `Assistant` plain text | `{role:"assistant", content:[{type:"text", text}]}` |
| **`System` mid/trailing** (compaction summary; the volatile context **snapshot**, which is `System(Context)` appended last) | **cannot** be a top-level system; render as a `user` message (we already send the compaction snapshot as a user message for the same reason). Snapshot stays **last and unmarked**. |

### Hard parts to get right
1. **System is singular & top-level.** Only the *front* system content (prompt + dev-notes) goes in
   `system`. Mid-stream `System` messages (compaction summaries, the trailing snapshot) must become
   `user` messages, or use the `mid-conversation-system-2026-04-07` beta. Pick one; document it.
2. **tool_use ↔ tool_result pairing.** Anthropic requires each `assistant` `tool_use` to be answered
   by a `tool_result` (matched by `tool_use_id`) in the **immediately following** `user` message;
   multiple tool calls in one turn → multiple `tool_result` blocks in one user message. Our linear
   `[Assistant(tool_calls), Tool, Tool, …]` order must be folded into that shape.
3. **Strict user/assistant alternation.** Merge adjacent same-role messages; never emit two
   assistants or two users in a row (the snapshot-as-user must merge with any trailing tool_results
   or sit as its own user turn).
4. **Tools schema.** OpenAI `{type:"function", function:{name, parameters}}` → Anthropic
   `{name, description, input_schema}`. Need a converter (or build native tool defs from source).
5. **`cache_control` placement** — unchanged design: floor on the dev-notes block; advancing anchor
   on the last history message (now honored on any role). `max 4`, snapshot unmarked.
6. **Params:** `max_tokens` (required), `thinking: {type:"adaptive"}` for 4.x, `stream`,
   `temperature`/`top_p` **removed** for 4.x models (400 if sent).

---

## Phase 2 — Native response parser (non-streaming + streaming)

Anthropic responses differ from OpenAI's; we need a parallel to `handle_chat` + `StreamAccumulator`.

- **Non-streaming:** `content` is a block array (`text`, `tool_use`, `thinking`); `stop_reason`;
  `usage: {input_tokens, output_tokens, cache_creation_input_tokens, cache_read_input_tokens}`.
  Map back into our `Reply.Model.t` (content string, `tool_calls`, `usage`, `reasoning`).
  - Good news: `of_usage` already reads `cache_read_input_tokens`/`cache_creation_input_tokens`, so
    the token chip + `CacheDiag` work unchanged once usage is mapped.
- **Streaming SSE:** different event types — `message_start`, `content_block_start`,
  `content_block_delta` (`text_delta` / `input_json_delta` / `thinking_delta`), `content_block_stop`,
  `message_delta` (carries `stop_reason` + usage), `message_stop`. Write a `StreamAccumulator`
  variant that folds these into the same `Model.result`.

---

## Phase 3 — Routing

- Add a switch in the dispatch path: if the active model is Anthropic (`anthropic/` prefix), build +
  send via the native serializer/endpoint; else use the existing OpenAI-compat path untouched.
- Keep `Chat.api_messages_for_openrouter` as the single source of the message list + advancing
  anchor; the native serializer consumes the same anchored list.
- `session_id` → Anthropic native takes an `x-session-id`-style header or a metadata field; map it.

## Files (all within agent-owned HTTP surface)

| Concern | Location |
|---|---|
| Native serializer + parser + streaming | `src/util/OpenRouter.re` (new `AnthropicNative` module) |
| Endpoint POST helpers (`/api/v1/messages`) | `src/util/OpenRouter.re` (`Utils`) |
| Model-based routing (native vs OpenAI-compat) | `src/web/view/agentCore/AgentUpdate.re` (dispatch sites) |
| Anchored message list (reused as-is) | `src/web/view/agentCore/Chat.re` (`api_messages_for_openrouter`) |
| Verification | `OpenRouter.CacheDiag` (already built) |

## Risks & open questions

1. **Does the Skin pass caching through?** — the whole premise; **Phase 0 gates it.**
2. **Auth + model-id form** for `/api/v1/messages` via OpenRouter — verify in Phase 0.
3. **Mid-stream system content** (snapshot, compaction) — decide user-message vs system-beta mapping.
4. **Maintenance cost** — two serialize/parse paths to keep in sync (tools, thinking, streaming).
5. **Behavioral parity** — make sure native tool-call round-trips and stop-reason handling match the
   existing path (regression-test the agent loop end to end).

## Verification (acceptance)

- Phase 0 probe shows growing `cache_read` + nonzero `cache_creation` on a user-message breakpoint.
- End-to-end: a 4-turn Claude session shows `CACHE_DIAG cache_read` climbing turn over turn (vs the
  current flat 22.3k).
- Agent loop regression: multi-tool turns, retries, compaction, and streaming all still work on the
  native path (run `AgentControlFlow`, `AgentMultiTool`, `Agent UX` suites + manual QA).
- Token chip shows real `cache_read`/`cache_creation` on the native path.

## Effort / recommendation

- **Phase 0:** ~half a day, decides everything.
- **Phases 1–3:** ~few days (serializer is the bulk; streaming parser is fiddly).
- **Worth it?** Mainly for **long** multi-turn Claude sessions (where uncached history dominates).
  Per the research in the findings doc, system-only caching is already near-optimal, so this is an
  optimization, not a fire. **Recommendation: run Phase 0; only commit to 1–3 if the spike passes
  and long-session cost is materially painful.**
