open Haz3lcore;
open RichProbeView;

/* Web registry of rich-probe renderer views, mirroring the core
 * registry in haz3lcore's RichProbeRegistry: every renderer registered
 * there should have its view module paired here.
 *
 * To add a new renderer view: implement RichProbeView.RichProbeView for
 * a core-registered RichProbe.RichProbe module and add one entry to
 * `views` below. */

let views: list(packed_view) = [
  pack_view((module TableRenderer), (module TableRendererView), "table"),
];

let find_view = (id: string): option(packed_view) =>
  List.find_opt((v: packed_view) => v.core.id == id, views);
