/* CSV url ingestion helpers for the Hazel CLI.

   A `.hz` source references a CSV by url with `^^csv("https://...")`. The
   fetch + parse + splice now happens through the projector initialization
   phase (see CSVProjector.initialize and Haz3lcore.ProjectorInitPhase),
   shared with the web editor. This module only provides the CLI-specific
   pieces: per-url consent decisions (see Cli.authorize_url) and base-url
   resolution for relative refs. */

/* What a consent prompt decides for one `^^csv("url")` reference. */
type decision =
  | Allow(string) /* fetch this (possibly user-substituted) url */
  | Deny;

let has_scheme = (url: string): bool =>
  String.starts_with(~prefix="http://", url)
  || String.starts_with(~prefix="https://", url);

/* Resolve a referenced url: absolute urls (with an http/https scheme) are used
   as-is; relative refs are joined onto base_url (from --data-dir). An empty
   base_url leaves the ref unchanged. */
let resolve = (~base_url: string, url: string): string =>
  if (has_scheme(url) || base_url == "") {
    url;
  } else if (base_url.[String.length(base_url) - 1] == '/') {
    base_url ++ url;
  } else {
    base_url ++ "/" ++ url;
  };
