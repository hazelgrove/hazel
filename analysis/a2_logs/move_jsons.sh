#!/usr/bin/env bash
set -euo pipefail
shopt -s nullglob

# Usage: ./move_jsons.sh [--dry-run]
dryrun=false
if [[ ${1:-} == "--dry-run" ]]; then dryrun=true; fi

for dir in */ ; do
  [[ -d "$dir" ]] || continue
  jsons=( "$dir"/*.json )

  case "${#jsons[@]}" in
    0)
      echo "[$dir] no JSON found — skipping"
      ;;
    1)
      src="${jsons[0]}"
      dest="${dir%/}.json"

      if [[ -e "$dest" ]]; then
        echo "[$dir] target '$dest' already exists — skipping"
      else
        if $dryrun; then
          echo mv -- "$src" "$dest"
        else
          mv -- "$src" "$dest"
        fi
      fi
      ;;
    *)
      echo "[$dir] multiple JSONs found — skipping: ${jsons[*]}"
      ;;
  esac
done
