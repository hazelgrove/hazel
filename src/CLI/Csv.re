/* CSV ingestion helpers for the Hazel CLI.

   A `.hz` source references a CSV with `^^csv("...")`, by either an
   `http(s)://` url or a local filesystem path. The fetch/read + parse + splice
   happens through the projector initialization phase (see
   CSVProjector.initialize and Haz3lcore.ProjectorInitPhase), shared with the
   web editor. This module only provides the CLI-specific pieces: per-ref
   consent decisions (see Cli.authorize_url), base resolution for relative refs,
   and local-file reading (Cli.install_url_fetch picks read vs. http by scheme).
   Reading local files is CLI-only; the web path is url-only. */

/* What a consent prompt decides for one `^^csv("url")` reference. */
type decision =
  | Allow(string) /* fetch this (possibly user-substituted) url */
  | Deny;

let has_scheme = (url: string): bool =>
  String.starts_with(~prefix="http://", url)
  || String.starts_with(~prefix="https://", url);

/* Resolve a referenced url/path: absolute urls (with an http/https scheme) are
   used as-is; relative refs are joined onto base_url (from --data-dir, which may
   be an http base or a local directory). An empty base_url leaves the ref
   unchanged. */
let resolve = (~base_url: string, url: string): string =>
  if (has_scheme(url) || base_url == "") {
    url;
  } else if (base_url.[String.length(base_url) - 1] == '/') {
    base_url ++ url;
  } else {
    base_url ++ "/" ++ url;
  };

/* Read a local CSV file. Used when a resolved `^^csv` ref has no http/https
   scheme, so it names a filesystem path rather than a url. */
let read_file = (path: string): string => {
  let ic = open_in_bin(path);
  let n = in_channel_length(ic);
  let s = really_input_string(ic, n);
  close_in(ic);
  s;
};
