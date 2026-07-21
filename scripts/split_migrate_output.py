#!/usr/bin/env python3
"""Split the stdout of migrate_exercises.bc.js into its target .ml files.

Usage (from repo root):
  dune build src/web/migrate_exercises.bc.js --profile dev
  node --stack-size=8192 --require ./test/idb_stub.js \
    _build/default/src/web/migrate_exercises.bc.js > /tmp/migrate_out.txt
  python3 scripts/split_migrate_output.py /tmp/migrate_out.txt

Blocks look like:
  ===FILE: <path relative to repo root>===
  <content>
  ===END===
The trailing ===SUMMARY=== block is printed, not written anywhere.
"""

import re
import sys
from pathlib import Path

def main():
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    text = Path(sys.argv[1]).read_text()
    repo_root = Path(__file__).resolve().parent.parent

    blocks = re.findall(
        r"^===FILE: (.+?)===\n(.*?)^===END===\n", text, re.M | re.S
    )
    if not blocks:
        sys.exit("no ===FILE blocks found")
    for rel_path, content in blocks:
        target = repo_root / rel_path
        if not target.exists():
            sys.exit(f"refusing to create new file {rel_path} "
                     "(expected an in-place replacement)")
        target.write_text(content)
        print(f"wrote {rel_path} ({len(content)} bytes)")

    m = re.search(r"^===SUMMARY===\n(.*?)^===END===\n", text, re.M | re.S)
    if m:
        print("--- exporter summary ---")
        print(m.group(1), end="")

if __name__ == "__main__":
    main()
