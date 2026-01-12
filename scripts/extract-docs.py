#!/usr/bin/env python3
"""
Extract backup_text from Hazel documentation ML files.

These files contain OCaml records with a backup_text field that holds
the Hazel program source as an escaped multi-line string.

Usage:
    # Extract all sources (docs + b2t2 slides)
    python3 scripts/extract-docs.py

    # Extract only docs
    python3 scripts/extract-docs.py --docs

    # Extract only b2t2 slides
    python3 scripts/extract-docs.py --b2t2

Output:
    hazel-programs/docs/       - Programs from src/web/init/docs/
    hazel-programs/b2t2/       - Programs from src/b2t2/slides/
"""

import argparse
import os
import re
import sys
from pathlib import Path


def extract_backup_text(ml_content: str) -> str | None:
    """Extract and unescape backup_text from an ML file."""

    # Find backup_text = "..."
    # The string may span many lines with \ continuation
    match = re.search(r'backup_text\s*=\s*"', ml_content)
    if not match:
        return None

    start = match.end()

    # Parse the OCaml string, handling escapes
    i = start
    result = []
    while i < len(ml_content):
        c = ml_content[i]

        if c == '\\' and i + 1 < len(ml_content):
            next_c = ml_content[i + 1]
            if next_c == 'n':
                # \n -> newline
                result.append('\n')
                i += 2
            elif next_c == '"':
                # \" -> quote
                result.append('"')
                i += 2
            elif next_c == '\\':
                # \\ -> backslash
                result.append('\\')
                i += 2
            elif next_c == '\n':
                # \ at end of line is continuation - skip it and the newline
                # Also skip leading whitespace on next line
                i += 2
                while i < len(ml_content) and ml_content[i] in ' \t':
                    i += 1
            else:
                # Other escapes - keep as-is
                result.append(c)
                i += 1
        elif c == '"':
            # End of string
            break
        else:
            result.append(c)
            i += 1

    return ''.join(result)


def process_file(ml_path: Path, output_path: Path, verbose: bool = True) -> bool:
    """Process a single ML file and write the extracted program."""

    if verbose:
        print(f"  Processing {ml_path.name}...")

    with open(ml_path, 'r') as f:
        content = f.read()

    backup_text = extract_backup_text(content)
    if backup_text is None:
        if verbose:
            print(f"    No backup_text found")
        return False

    # Ensure output directory exists
    output_path.parent.mkdir(parents=True, exist_ok=True)

    with open(output_path, 'w') as f:
        f.write(backup_text)

    if verbose:
        print(f"    -> {output_path.name} ({len(backup_text)} chars)")
    return True


def process_directory(source_dir: Path, output_dir: Path, recursive: bool = True) -> int:
    """Process all ML files in a directory, optionally recursively."""

    print(f"\nProcessing {source_dir}...")

    if recursive:
        ml_files = sorted(source_dir.rglob('*.ml'))
    else:
        ml_files = sorted(source_dir.glob('*.ml'))

    if not ml_files:
        print(f"  No .ml files found")
        return 0

    success_count = 0
    for ml_file in ml_files:
        # Compute output path preserving subdirectory structure
        rel_path = ml_file.relative_to(source_dir)
        output_name = rel_path.with_suffix('.hz')
        output_path = output_dir / output_name

        if process_file(ml_file, output_path):
            success_count += 1

    print(f"  Extracted {success_count}/{len(ml_files)} files")
    return success_count


def main():
    parser = argparse.ArgumentParser(
        description='Extract Hazel programs from ML documentation files',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument('--docs', action='store_true',
                        help='Extract only from src/web/init/docs/')
    parser.add_argument('--b2t2', action='store_true',
                        help='Extract only from src/b2t2/slides/')
    args = parser.parse_args()

    # If neither specified, do both
    if not args.docs and not args.b2t2:
        args.docs = True
        args.b2t2 = True

    # Resolve paths relative to this script
    script_dir = Path(__file__).parent
    repo_root = script_dir.parent

    total_success = 0

    # Process docs
    if args.docs:
        docs_source = repo_root / 'src' / 'web' / 'init' / 'docs'
        docs_output = repo_root / 'hazel-programs' / 'docs'
        total_success += process_directory(docs_source, docs_output, recursive=False)

    # Process b2t2 slides
    if args.b2t2:
        b2t2_source = repo_root / 'src' / 'b2t2' / 'slides'
        b2t2_output = repo_root / 'hazel-programs' / 'b2t2'
        total_success += process_directory(b2t2_source, b2t2_output, recursive=True)

    print(f"\nTotal: {total_success} files extracted")
    return 0 if total_success > 0 else 1


if __name__ == '__main__':
    sys.exit(main())
