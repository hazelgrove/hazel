#!/usr/bin/env python3
"""
Python implementation of the complete Hazel grading pipeline
Equivalent to the complex Perl pipeline command
"""

import json
import sys
import subprocess
import tempfile
import os
from pathlib import Path
import argparse

# Import our utility modules
from join import join_gradescope_zip_with_function
from proj import project_element
from cat import cat_directory

def cat_function(submission_dir):
    """
    Cat function that can be passed to join_gradescope_zip
    """
    return cat_directory(submission_dir)


def process_hazel_pipeline(zip_path, hazel_path):
    """
    Execute the complete Hazel grading pipeline:
    1. Extract token2uniqname
    2. Extract submissions 
    3. Process submissions through Hazel
    """
    
    # Step 1: join.pl ~/a#-code.zip -f ./cat.pl | proj.pl 0
    print("[1/3] Extracting token2uniqname mapping...", file=sys.stderr)
    # Use the cat function directly instead of subprocess
    join_result = join_gradescope_zip_with_function(zip_path, cat_function)
    token2uniqname = project_element(join_result, 0)
    
    # Step 2: join.pl ~/a#-code.zip -f ./cat.pl | proj.pl 1
    print("[2/3] Extracting submissions...", file=sys.stderr)
    submissions = project_element(join_result, 1)
    
    print("[3/3] Running Hazel grader...", file=sys.stderr)
    graded_submissions = {}
    total = len(submissions)
    for i, (key, value) in enumerate(submissions.items(), 1):
        print( (key, value))
    # for i, (key, value) in enumerate(submissions.items(), 1):
    #     print(f"  Processing {key} ({i}/{total})...", file=sys.stderr)
    #     graded_submissions[key] = hazel_transform(value, hazel_path)
    
    # Return combined result as [token2uniqname, graded_submissions]
    return [token2uniqname, graded_submissions]


def main():
    parser = argparse.ArgumentParser(
        description="Process Gradescope submissions through Hazel grader",
        formatter_class=argparse.RawDescriptionHelpFormatter)
    
    parser.add_argument("zip_file", help="Path to Gradescope export ZIP file")
    parser.add_argument("hazel_dir", help="Path to Hazel project directory")
    parser.add_argument("--output", "-o", help="Output file (default: stdout)")
    
    args = parser.parse_args()
    
    zip_path = Path(args.zip_file).resolve()
    hazel_path = Path(args.hazel_dir).resolve()
    
    if not zip_path.exists():
        print(f"Error: ZIP file {zip_path} not found", file=sys.stderr)
        sys.exit(1)
    
    if not hazel_path.exists():
        print(f"Error: Hazel directory {hazel_path} not found", file=sys.stderr)
        sys.exit(1)
    
    try:
        result = process_hazel_pipeline(zip_path, hazel_path)
        
        # Output result
        json_output = json.dumps(result, indent=2, sort_keys=True)
        
        if args.output:
            with open(args.output, 'w') as f:
                f.write(json_output)
            print(f"Results written to {args.output}", file=sys.stderr)
        else:
            print(json_output)
            
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
