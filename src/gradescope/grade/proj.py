#!/usr/bin/env python3
"""
Python equivalent of proj.pl  
Extracts specific elements from JSON arrays
"""

import json
import sys
import argparse


def project_element(data, index):
    """
    Extract element at given index from array
    Equivalent to Perl's proj.pl
    """
    if not isinstance(data, list):
        raise ValueError("Input must be a JSON array")
    
    if index >= len(data):
        raise IndexError(f"Index {index} out of range for array of length {len(data)}")
    
    return data[index]


def main():
    parser = argparse.ArgumentParser(description="Extract element from JSON array")
    parser.add_argument("index", type=int, help="Index to extract (0-based)")
    
    args = parser.parse_args()
    
    try:
        # Read JSON from stdin
        input_data = json.load(sys.stdin)
        
        # Extract the specified element
        result = project_element(input_data, args.index)
        
        # Output the result
        print(json.dumps(result, indent=2, sort_keys=True))
        
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON input - {e}", file=sys.stderr)
        sys.exit(1)
    except (ValueError, IndexError) as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
