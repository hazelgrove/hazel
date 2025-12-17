#!/usr/bin/env python3
"""
Python implementation of upload.pl
Reads [token2uniqname, submissions] from stdin and uploads to Gradescope
"""

import requests
import json
import sys
import getpass
import tempfile
import os
from pathlib import Path
import argparse

def login_to_gradescope(base_url="https://www.gradescope.com"):
    """Authenticate with Gradescope and return access token"""
    try:
        with open('/dev/tty', 'r') as tty:
            print("Enter your email: ", end='', flush=True)
            email = tty.readline().strip()
    except (OSError, FileNotFoundError):
        email = input("Enter your email: ")
    
    password = getpass.getpass("Enter your password: ")
    
    login_url = f"{base_url}/api/v1/user_session"
    response = requests.post(login_url, data={
        'email': email,
        'password': password
    })
    
    if response.status_code != 200:
        raise Exception("Login failed")
    
    data = response.json()
    if 'token' not in data:
        raise Exception("Your gradescope login credentials are probably wrong")
    
    print(f"[debug] token_expiration_time: {data.get('token_expiration_time', 'N/A')}")
    return data['token']

def upload_batch_to_gradescope(course_id, assignment_id, token2uniqname, submissions, auth_token, filetype="json", email_suffix="@umich.edu"):
    """
    Upload submissions for multiple students to Gradescope
    
    Args:
        course_id: Gradescope course ID
        assignment_id: Gradescope assignment ID  
        token2uniqname: Dict mapping tokens to uniqnames
        submissions: Dict mapping tokens to submission data
        auth_token: Gradescope auth token
        filetype: File extension (default: json)
        email_suffix: Email suffix (default: @umich.edu)
    """
    
    base_url = "https://www.gradescope.com"
    upload_url = f"{base_url}/api/v1/courses/{course_id}/assignments/{assignment_id}/submissions"
    
    headers = {
        'access-token': auth_token
    }
    
    success_count = 0
    total_count = len(token2uniqname)
    
    with tempfile.TemporaryDirectory() as tmpdir:
        print(f"[debug] Using temp directory: {tmpdir}", file=sys.stderr)
        
        # Write each submission to a temporary file
        for token in submissions:
            filename = f"{token}.{filetype}"
            filepath = Path(tmpdir) / filename
            
            with open(filepath, 'w') as f:
                submission_data = submissions[token]
                
                if filetype == "json":
                    # Assume submission_data is already a JSON string, write directly
                    f.write(submission_data)
                else:
                    # Non-JSON filetype, write as string
                    f.write(str(submission_data))
        
        # Upload each file
        for token in token2uniqname:
            if token not in submissions:
                print(f"[warning] No submission found for token {token}", file=sys.stderr)
                continue
                
            uniqname = token2uniqname[token]
            email = f"{uniqname}{email_suffix}"
            filename = f"{token}.{filetype}"
            filepath = Path(tmpdir) / filename
            
            if not filepath.exists():
                print(f"[warning] File {filepath} does not exist", file=sys.stderr)
                continue
            
            print(f"Uploading {filename} for {email}...", file=sys.stderr)
            
            try:
                with open(filepath, 'rb') as f:
                    files = {
                        'files[]': f
                    }
                    data = {
                        'owner_email': email
                    }
                    
                    response = requests.post(upload_url, headers=headers, files=files, data=data)
                
                if response.status_code == 200:
                    print(f"✓ Successfully uploaded for {email}", file=sys.stderr)
                    success_count += 1
                else:
                    print(f"✗ Upload failed for {email}: {response.status_code} - {response.text}", file=sys.stderr)
                    
            except Exception as e:
                print(f"✗ Error uploading for {email}: {e}", file=sys.stderr)
    
    print(f"\nUpload completed: {success_count}/{total_count} successful", file=sys.stderr)
    return success_count == total_count

def main():
    parser = argparse.ArgumentParser(
        description="Upload Gradescope submissions from JSON input",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Upload from hazel_pipeline.py output
  python hazel_pipeline.py submissions.zip ~/hazel | python upload.py 1234 5678
  
  # Upload with custom file type
  python hazel_pipeline.py submissions.zip ~/hazel | python upload.py 1234 5678 --filetype csv
  
  # From file input
  python upload.py 1234 5678 < graded_results.json

You can get course and assignment IDs from the Gradescope URL:
  https://www.gradescope.com/courses/1234/assignments/5678
  course_id = 1234, assignment_id = 5678
        """)
    
    parser.add_argument("course_id", help="Gradescope course ID")
    parser.add_argument("assignment_id", help="Gradescope assignment ID")
    parser.add_argument("--filetype", "-f", default="json", help="File type to upload (default: json)")
    parser.add_argument("--email", "-e", default="@umich.edu", help="Email suffix (default: @umich.edu)")
    parser.add_argument("--yes", "-y", action="store_true", help="Skip confirmation prompt")
    
    args = parser.parse_args()
    
    # Read JSON input from stdin
    try:
        print("Reading input from stdin...", file=sys.stderr)
        input_data = json.load(sys.stdin)
        
        if not isinstance(input_data, list) or len(input_data) != 2:
            raise ValueError("Expected input format: [token2uniqname, submissions]")
        
        token2uniqname, submissions = input_data
        
        if not isinstance(token2uniqname, dict) or not isinstance(submissions, dict):
            raise ValueError("Expected token2uniqname and submissions to be dictionaries")
            
    except (json.JSONDecodeError, ValueError) as e:
        print(f"Error reading input: {e}", file=sys.stderr)
        print("Expected input format: [token2uniqname, submissions]", file=sys.stderr)
        sys.exit(1)
    
    # Show summary and confirm
    print(f"\nUpload Summary:", file=sys.stderr)
    print(f"  Course ID: {args.course_id}", file=sys.stderr)
    print(f"  Assignment ID: {args.assignment_id}", file=sys.stderr)
    print(f"  Students: {len(token2uniqname)}", file=sys.stderr)
    print(f"  Submissions: {len(submissions)}", file=sys.stderr)
    print(f"  File type: {args.filetype}", file=sys.stderr)
    print(f"  Email suffix: {args.email}", file=sys.stderr)
    
    # Confirm the operation (read from terminal, not stdin)
    if not args.yes:
        try:
            with open('/dev/tty', 'r') as tty:
                print(f"\nConfirm: upload to course {args.course_id}, assignment {args.assignment_id}? (y/N): ", end='', flush=True)
                confirm = tty.readline().strip()
        except (OSError, FileNotFoundError):
            # Fallback for non-Unix systems or when no TTY is available
            print("No terminal available for confirmation. Use --yes to bypass.", file=sys.stderr)
            sys.exit(1)
            
        if confirm.lower() != 'y':
            print("Upload cancelled", file=sys.stderr)
            sys.exit(1)
    
    # Login and get auth token
    try:
        print("Logging into Gradescope...", file=sys.stderr)
        auth_token = login_to_gradescope()
    except Exception as e:
        print(f"Login error: {e}", file=sys.stderr)
        sys.exit(1)
    
    # Upload the submissions
    try:
        success = upload_batch_to_gradescope(
            args.course_id, 
            args.assignment_id, 
            token2uniqname, 
            submissions, 
            auth_token,
            filetype=args.filetype,
            email_suffix=args.email
        )
        
        if success:
            print("All uploads completed successfully", file=sys.stderr)
        else:
            print("Some uploads failed", file=sys.stderr)
            sys.exit(1)
            
    except Exception as e:
        print(f"Upload error: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()