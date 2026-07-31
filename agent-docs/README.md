# agent-docs

Working notes, research reports, and findings produced while building the **coding-agent**
extension to Hazel. Kept separate from the top-level `docs/` folder, which is Hazel's own
project documentation (architecture, livelits, UI, etc.).

## Contents

- [`prompt-caching-findings.md`](./prompt-caching-findings.md) — full research report on prompt
  caching for the agent on OpenRouter: Phase 1 (live floor caching), the Phase 2 investigation
  into caching the growing chat history, the diagnostic harness, the evidence, and the definitive
  finding that OpenRouter+Anthropic only honors `cache_control` on `system` messages.
- [`anthropic-skin-endpoint-plan.md`](./anthropic-skin-endpoint-plan.md) — proposal/plan to add a
  second request path for Claude via OpenRouter's Anthropic-native ("Anthropic Skin") endpoint
  (`/api/v1/messages`), which would unlock full conversation-history caching. Gated behind a
  verification spike.
