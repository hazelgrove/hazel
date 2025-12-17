#!/usr/bin/env python3
"""
Python equivalent of join.pl
Processes Gradescope export ZIP files
"""

import json
import sys
import os
import tempfile
import zipfile
import subprocess
try:
    import yaml
except ImportError:
    print("Error: PyYAML is required. Install with: pip install PyYAML", file=sys.stderr)
    sys.exit(1)
from pathlib import Path
import argparse
from email.utils import parseaddr


def extract_uniqname_from_email(email):
    """Extract username from email address"""
    parsed = parseaddr(email)
    if '@' in parsed[1]:
        return parsed[1].split('@')[0]
    return parsed[1]


def process_submission_directory(submission_dir, lambda_cmd):
    """
    Apply a processing command to a submission directory
    Returns the stdout of the command
    """
    try:
        # Run the lambda command with submission_dir as argument
        result = subprocess.run(
            lambda_cmd + [str(submission_dir)],
            capture_output=True,
            text=True,
            check=True
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError as e:
        print(f"[error] problem with {submission_dir}; skipping…", file=sys.stderr)
        return ""


def join_gradescope_zip(zip_path, lambda_cmd):
    """
    Process Gradescope export ZIP file
    Returns [token2uniqname, submissions] JSON structure
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir_path = Path(tmpdir)
        
        # Extract ZIP file
        with zipfile.ZipFile(zip_path, 'r') as zf:
            zf.extractall(tmpdir_path)
        
        # Read submission metadata
        metadata_file = tmpdir_path / 'submission_metadata.yml'
        if not metadata_file.exists():
            raise FileNotFoundError("submission_metadata.yml not found in ZIP")
        
        with open(metadata_file, 'r') as f:
            md_yaml = yaml.safe_load(f)
        
        output = {}  # uniqname -> submission
        
        for submission_id, metadata in md_yaml.items():
            if ':submitters' not in metadata or not metadata[':submitters']:
                print(f"[warning] No submitters for {submission_id}, skipping", file=sys.stderr)
                continue
                
            email = metadata[':submitters'][0][':email']
            uniqname = extract_uniqname_from_email(email)
            
            submission_dir = tmpdir_path / str(submission_id)
            if submission_dir.exists():
                submission = process_submission_directory(submission_dir, lambda_cmd)
                if submission:  # Only include if processing succeeded
                    output[uniqname] = submission
        
        # Generate trivial token2uniqname (tokens = uniqnames in this case)
        uniqnames = list(output.keys())
        trivial_token2uniqname = {name: name for name in uniqnames}
        
        return [trivial_token2uniqname, output]


def join_gradescope_zip_with_function(zip_path, processing_function):
    """
    Process Gradescope export ZIP file using a Python function
    Returns [token2uniqname, submissions] JSON structure
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir_path = Path(tmpdir)
        
        # Extract ZIP file
        with zipfile.ZipFile(zip_path, 'r') as zf:
            zf.extractall(tmpdir_path)
        
        # Read submission metadata
        metadata_file = tmpdir_path / 'submission_metadata.yml'
        if not metadata_file.exists():
            raise FileNotFoundError("submission_metadata.yml not found in ZIP")
        
        with open(metadata_file, 'r') as f:
            md_yaml = yaml.safe_load(f)
        
        output = {}  # uniqname -> submission
        
        for submission_id, metadata in md_yaml.items():
            if ':submitters' not in metadata or not metadata[':submitters']:
                print(f"[warning] No submitters for {submission_id}, skipping", file=sys.stderr)
                continue
                
            email = metadata[':submitters'][0][':email']
            uniqname = extract_uniqname_from_email(email)
            
            submission_dir = tmpdir_path / str(submission_id)
            if submission_dir.exists():
                try:
                    submission = processing_function(str(submission_dir))
                    if submission:  # Only include if processing succeeded
                        output[uniqname] = submission
                except Exception as e:
                    print(f"[error] problem processing {submission_id}: {e}; skipping…", file=sys.stderr)
        
        # Generate trivial token2uniqname (tokens = uniqnames in this case)
        uniqnames = list(output.keys())
        trivial_token2uniqname = {name: name for name in uniqnames}
        
        return [trivial_token2uniqname, output]


def main():
    parser = argparse.ArgumentParser(description="Process Gradescope export ZIP")
    parser.add_argument("zip_file", help="Path to Gradescope export ZIP file")
    parser.add_argument("-f", "--function", action="append", required=True,
                       help="Command to apply to each submission directory (can be repeated)")
    
    args = parser.parse_args()
    
    zip_path = Path(args.zip_file).resolve()
    if not zip_path.exists():
        print(f"Error: ZIP file {zip_path} not found", file=sys.stderr)
        sys.exit(1)
    
    # Build lambda command from -f arguments
    lambda_cmd = args.function
    
    try:
        result = join_gradescope_zip(zip_path, lambda_cmd)
        print(json.dumps(result, indent=2, sort_keys=True))
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
