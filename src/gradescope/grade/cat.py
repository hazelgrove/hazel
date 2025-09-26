#!/usr/bin/env python3
"""
Python equivalent of cat.pl
Reads files from a directory and outputs their content
"""

import sys
import os
from pathlib import Path


def cat_directory(directory_path):
    """
    Read all files in a directory and return their concatenated content
    Equivalent to cat.pl behavior
    """
    dir_path = Path(directory_path)
    if not dir_path.exists() or not dir_path.is_dir():
        return ""
    
    content_parts = []
    
    # Sort files for consistent output
    for file_path in sorted(dir_path.iterdir()):
        if file_path.is_file():
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    content_parts.append(f.read())
            except UnicodeDecodeError:
                # Try binary mode for non-text files
                try:
                    with open(file_path, 'rb') as f:
                        content_parts.append(f.read().decode('utf-8', errors='ignore'))
                except Exception:
                    # Skip files that can't be read
                    continue
            except Exception:
                # Skip files that can't be read
                continue
    
    return '\n'.join(content_parts)


def main():
    if len(sys.argv) != 2:
        print("Usage: cat.py <directory_path>", file=sys.stderr)
        sys.exit(1)
    
    directory_path = sys.argv[1]
    result = cat_directory(directory_path)
    print(result)


if __name__ == "__main__":
    main()
