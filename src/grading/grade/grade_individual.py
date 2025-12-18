#!/usr/bin/env python3

import json
import sys
import subprocess
import tempfile
from pathlib import Path
import argparse

def run_hazel_grader(student_json_str, hazel_path):
    """
    Run the Hazel grader on student JSON
    Uses: node -r ./src/web/www/polyfill_worker.js _build/default/src/web/gradingReport.bc.js <input> <output>
    """
    
    # Find the required Hazel files
    polyfill_file = hazel_path / "src/web/www/polyfill_worker.js"
    grading_file = hazel_path / "_build/default/src/web/gradingReport.bc.js"
    
    if not polyfill_file.exists():
        raise FileNotFoundError(f"Hazel polyfill not found: {polyfill_file}")
    if not grading_file.exists():
        raise FileNotFoundError(f"Hazel gradingReport.bc.js not found: {grading_file}")
    
    with tempfile.TemporaryDirectory() as tmpdir:
        input_file = Path(tmpdir) / "input.json"
        output_file = Path(tmpdir) / "output.json"
        
        # Write input JSON
        with open(input_file, 'w') as f:
            decoded = json.loads(student_json_str)
            json.dump(decoded, f, indent=2)

        # Run Hazel grader
        cmd = [
            "node",
            "-r", str(polyfill_file),
            str(grading_file),
            str(input_file),
            str(output_file)
        ]

        try:
            result = subprocess.run(
                cmd,
                cwd=hazel_path  / "_build/default/src/web/www/",
                capture_output=True,
                text=True,
                check=True
            )
            
            # Read output
            if output_file.exists():
                with open(output_file, 'r') as f:
                    return f.read().strip()
            else:
                return result.stdout.strip()
                
        except subprocess.CalledProcessError as e:
            print(f"[error] Hazel grader failed: {e.stderr}", file=sys.stderr)
            raise


def hazel_transform(value, hazel_path):
    """Transform function for Hazel grading"""
    try:
        return run_hazel_grader(value, hazel_path)
    except Exception as e:
        print(f"[error] Hazel processing failed: {e}", file=sys.stderr)
        return "Hazel grader error"


def main():
    parser = argparse.ArgumentParser(
        description="Process a single exercise submission through the Hazel grader",
        formatter_class=argparse.RawDescriptionHelpFormatter)
    
    parser.add_argument("submission_file", help="Path to submission JSON")
    parser.add_argument("hazel_dir", help="Path to Hazel project directory")
    parser.add_argument("--output", "-o", help="Output file (default: stdout)")
    
    args = parser.parse_args()
    
    submission_path = Path(args.submission_file).resolve()
    hazel_path = Path(args.hazel_dir).resolve()
    
    if not submission_path.exists():
        print(f"Error: Submission file {submission_path} not found", file=sys.stderr)
        sys.exit(1)
    
    if not hazel_path.exists():
        print(f"Error: Hazel directory {hazel_path} not found", file=sys.stderr)
        sys.exit(1)
    
    try:
        with open(submission_path, 'r') as f:
            submission = f.read()
        result = hazel_transform(submission, hazel_path)
        
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
