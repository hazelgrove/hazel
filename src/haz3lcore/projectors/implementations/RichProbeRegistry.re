/* Registry of available rich-probe renderers.
 * Lives here (not in RichProbe.re) to avoid a circular dependency:
 * concrete renderers include RichProbe.RichProbe, so RichProbe can't
 * reference them. ProbeProj reads this list instead of importing
 * each renderer directly. */

let renderers: list(RichProbe.packed_renderer) = [
  RichProbe.pack_renderer((module TableRenderer), "table"),
  RichProbe.pack_renderer((module CardRenderer), "card"),
];
