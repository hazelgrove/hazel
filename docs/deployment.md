# Deployment: hazel.org and branch previews

Everything public-facing is served by Cloudflare (moved from GitHub Pages on
2026-10-29; background and decisions in
[#2497](https://github.com/hazelgrove/hazel/issues/2497)). No secret values
appear in this document or anywhere in the repository. Secrets live in GitHub
Actions secrets and in the Cloudflare dashboard.

| What | Where it's served | Where it comes from |
|---|---|---|
| The main site, `hazel.org` (and `www`) | Cloudflare Pages project `hazel-org` | `hazelgrove/hazelgrove.github.io`, branch `master`, deployed as-is on every push (no build step) |
| Branch previews, `build.hazel.org/<branch>/` | R2 bucket `hazel-build` (location hint: eastern North America) | `.github/workflows/deploy.yml`, on every push to this repo |
| Old preview URLs, `hazel.org/build/<branch>/` and `hazel.org/build-pre-july2024/<branch>/` | 301 redirect to `build.hazel.org/<branch>/` | Cloudflare redirect rules (below) |

## Account and DNS

- The Cloudflare account is owned by Cyrus Omar, who also owns billing (Free
  plan; R2 is pay-as-you-go beyond its free tier). Ask him for access.
- The `hazel.org` DNS zone is on Cloudflare (nameservers `*.ns.cloudflare.com`).
  The domain is still registered through Hover; moving the registration is
  [#2745](https://github.com/hazelgrove/hazel/issues/2745).
- `hazel.org` and `www` are custom domains of the Pages project; `build` is the
  R2 bucket's custom domain. Cloudflare manages those records itself. The `mail`
  and MX records are leftovers from Hover's (unused) email.

## Branch previews

`deploy.yml` builds the release bundle and runs `aws s3 sync --delete` from
`_build/default/src/web/www` to `s3://hazel-build/<branch>/` through R2's
S3-compatible endpoint. A key is live as soon as it's written, so there is no
site-wide publish step and concurrent deploys of different branches can't
interfere. A preview is typically live a few minutes after the push.

Uploads land one file at a time, so the job orders them to keep a preview
whole: the new assets, then `index.html`, then a `--delete` pass pruning what
the new build dropped. A run cancelled partway leaves the old page loading the
old assets, which are still there. It then purges exactly that branch's URLs
from Cloudflare's cache, because the edge could otherwise serve a new
`hazel.js` beside an old `worker.js` for up to `max-age`, and the two must
match.

The job refuses an empty branch name (the `--delete` pass would otherwise
reach the bucket root) and any branch name starting with `_`, which is
reserved: `_archive/` holds builds cited in published material.

GitHub Actions secrets used (names only):

- `R2_ACCESS_KEY_ID`, `R2_SECRET_ACCESS_KEY`: the S3 credentials of the
  Cloudflare *Account* API token `hazel-ci-deploy`, which has Object Read &
  Write on the `hazel-build` bucket only.
- `R2_ACCOUNT_ID`: the Cloudflare account ID, used to form the endpoint URL.
- `CLOUDFLARE_PURGE_TOKEN`: a Cloudflare API token with only Zone → Cache
  Purge on `hazel.org`, for the post-deploy purge.
- `CLOUDFLARE_ZONE_ID`: the `hazel.org` zone's ID.

Uploads set `Cache-Control: public, max-age=300`. Build filenames
(`hazel.js`, `worker.js`, ...) aren't content-hashed, so a short lifetime lets a
new deploy show up within minutes while traffic spikes are still served from
Cloudflare's cache.

`AWS_REQUEST_CHECKSUM_CALCULATION` / `AWS_RESPONSE_CHECKSUM_VALIDATION` are set
to `when_required` because newer `aws` CLIs send checksum headers R2 may
reject.

### Rotating the CI key

1. Cloudflare dashboard → R2 → Manage API tokens → Create *Account* API
   token: Object Read & Write, bucket `hazel-build` only.
2. `gh secret set R2_ACCESS_KEY_ID -R hazelgrove/hazel` and
   `gh secret set R2_SECRET_ACCESS_KEY -R hazelgrove/hazel` (each reads the
   value from stdin, so it never lands in your shell history).
3. Delete the old token in the dashboard.

## Rules on the `hazel.org` zone

All of these work on the Free plan; none need Worker or Function code.

**Redirect rules** (Rules → Redirect Rules; evaluated before the rewrite):

1. `hazel.org` or `www.hazel.org`, path starting `/build/` → 301 to
   `https://build.hazel.org` + the rest of the path, query string kept.
   Browsers carry the `#fragment` across redirects themselves, so slide and
   cursor deep links survive.
2. `hazel.org` or `www.hazel.org`, path starting `/build-pre-july2024/` → 301
   to `https://build.hazel.org` + the rest of the path, the same way.
3. `build.hazel.org/` (the bare root) → 302 to `https://hazel.org/`.
4. `build.hazel.org`, path not ending in `/` and containing no `.` → 301 to the
   same path plus `/`. Builds load `style.css`, `hazel.js` etc. relatively,
   so a folder URL needs its trailing slash.
5. `hazel.org/tylr-build/*` → 301 to `https://tylr.fun/*`, as GitHub Pages did
   for the `hazelgrove/tylr-build` project site, which lives on its own
   domain.

**URL rewrite rule** (Rules → Transform Rules): `build.hazel.org`, path ending
in `/` → append `index.html`. R2 custom domains don't serve a folder's
`index.html` on their own the way GitHub Pages did.

**Cache rule** (Caching → Cache Rules): everything on `build.hazel.org` is
eligible for cache, honoring the object's `Cache-Control`, with the query
string excluded from the cache key so random query strings can't force cache
misses through to R2.

## Things that must keep working

- **`hazel.org/livelits`** is the URL printed in the PLDI '21 livelits paper.
  It's a page in `hazelgrove.github.io` (`livelits/index.html`) that forwards
  to `https://build.hazel.org/_archive/pldi21-livelits/`, a copy of the
  `livelits` branch build kept under the reserved `_archive/` prefix so no
  branch push can overwrite it. Don't delete either one. (The ordinary
  `build.hazel.org/livelits/` preview also still exists.)
- **`hazel.org/build/dev/`** and other old preview links are covered by
  redirect rules 1 and 2.

## Old previews and retention

Every preview that existed in `hazelgrove/build` (the old GitHub Pages
repository) was copied into the bucket during the move, and that repository
is now frozen. So were the 491 previews in `hazelgrove/build-pre-july2024`,
the archive split off in July 2024 (its own Pages site had stopped building).
Twenty branch names existed in both; for those, the newer build from
`hazelgrove/build` is the one served. There's no retention policy: storage
is $0.015/GB-month beyond 10 GB free, so keeping every build costs cents a month. Revisit only if it
starts costing real money; a budget alert on the account emails the owner if
spend crosses its threshold.

## Caveats

- A branch name containing a `.` doesn't get the trailing-slash redirect at
  its root: link to `build.hazel.org/<branch>/` with the slash.
- Saved Hazel state (IndexedDB) is per origin, so previews on
  `build.hazel.org` don't see state saved under `hazel.org/build/…` before
  the move.
- A branch that hasn't merged `dev` since the move still has the old
  `deploy.yml`, which pushes to `hazelgrove/build`. That repository is
  archived and its push token revoked, so the old Deploy job fails visibly.
  Merge `dev` into the branch to get previews on `build.hazel.org`.
- Cloudflare Pages caps files at 25 MiB, which matters for the main site
  (large PDFs or videos). Put anything bigger in R2 and link to it. R2 itself
  takes up to 5 GiB per upload through the S3 API, and the free plan caches
  files up to 512 MB.

## Checking a preview

```sh
curl -sI https://build.hazel.org/<branch>/ | grep -iE '^HTTP|cf-cache-status|cache-control|content-type'
```

`cf-cache-status: HIT` means it came from Cloudflare's cache; `MISS` or
`EXPIRED` means it was fetched from R2. To force a stale file out early, purge
it from the cache in the dashboard (Caching → Configuration → Purge Cache).
