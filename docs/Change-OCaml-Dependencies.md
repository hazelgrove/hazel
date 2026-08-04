# Instructions for Changing Ocaml Dependencies

## How to update dependencies

- Update the dune-project file to reflect the new dependency constraints
- `make change-deps`
  - This should generate the hazel.opam file from dune.
  - Depending on your installed dependencies you may need to make a new clean switch
- Interrogate the `hazel.opam.locked` file to see what dependencies have changed
- `make release`
- Test in Firefox and Chrome. 
- Commit changed files and push
  - This includes `.github/opam-pins.env` as well as `hazel.opam.locked`; see below

## `make deps` vs `make change-deps`

The two targets are opposites, and the difference matters because opam
repository metadata is mutable:

| | What it does to the repositories |
|---|---|
| `make deps` | Pins them to the commits in `.github/opam-pins.env`, then installs from `hazel.opam.locked`. Reproduces what CI installs. |
| `make change-deps` | Moves those pins to each repository's current `HEAD`, repins, then re-locks against them. |

So `make deps` never picks up newly published packages, and `make change-deps`
is the only thing that does.

`make change-deps` rewrites **both** `hazel.opam.locked` and
`.github/opam-pins.env`, and both need to be committed together. A lock file
without its matching pin is not reproducible: `hazel.opam.locked` pins which
package *versions* the solver picks, but opam has no content hashes and upstream
sometimes edits already-published versions in place. In June 2026 such an edit
made `conf-libssl.4` require `conf-pkg-config >= 5` while our lock file pinned
`conf-pkg-config = 4`, which broke CI on every branch overnight with no change
on our side ([issue #2334](https://github.com/hazelgrove/hazel/issues/2334)).
This is a standing property of opam-repository rather than a one-off — see
[ocaml/opam-repository#10531](https://github.com/ocaml/opam-repository/issues/10531).

The repinning inside `make change-deps` is load-bearing: resolving without it
would consult the old pinned commit and quietly find nothing new.

You do not normally need to run `make change-deps` by hand. The
`update_deps.yml` workflow runs it daily against `dev` and opens a
`bot-update-deps` PR with the regenerated lock file and pin. Doing it manually
is for when you are adding or constraining a dependency yourself.

The pinned repositories are configured in whichever opam switch you are in, so
your shared `default` repository is untouched.