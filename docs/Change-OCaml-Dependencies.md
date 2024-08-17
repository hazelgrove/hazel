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