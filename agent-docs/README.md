# agent-docs

Working notes, research reports, and findings produced while building the **coding-agent**
extension to Hazel. Kept separate from the top-level `docs/` folder, which is Hazel's own
project documentation (architecture, livelits, UI, etc.).

## Contents

- [`prompt-caching-findings.md`](./prompt-caching-findings.md) — full research report on prompt
  caching for the agent on OpenRouter: Phase 1 (live floor caching), the Phase 2 investigation
  into caching the growing chat history, the diagnostic harness, the evidence, and the definitive
  finding that OpenRouter+Anthropic only honors `cache_control` on `system` messages.
