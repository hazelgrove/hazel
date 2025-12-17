#!/usr/bin/env python3
"""
Python equivalent of string2json.pl
Converts string input to JSON string
"""

import json
import sys


def string_to_json(input_string):
    """Convert string to JSON string"""
    return input_string.strip()


def main():
    try:
        # Read from stdin
        input_data = sys.stdin.read()
        
        # Convert to JSON string
        result = string_to_json(input_data)
        
        # Output as JSON
        print(json.dumps(result))
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
