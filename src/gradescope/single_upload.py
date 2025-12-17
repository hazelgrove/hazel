#!/usr/bin/env python3
# written by Claude

import requests
# import json
import sys
import getpass
# import tempfile
import os
# from pathlib import Path

def login_to_gradescope(base_url="https://www.gradescope.com"):
    """Authenticate with Gradescope and return access token"""
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

def upload_single_file_to_gradescope(course_id, assignment_id, student_email, file_path, auth_token):
    """Upload a single file to Gradescope for a specific student"""
    
    base_url = "https://www.gradescope.com"
    upload_url = f"{base_url}/api/v1/courses/{course_id}/assignments/{assignment_id}/submissions"
    
    headers = {
        'access-token': auth_token
    }
    
    # Prepare the file for upload
    with open(file_path, 'rb') as f:
        files = {
            'files[]': f
        }
        data = {
            'owner_email': student_email
        }
        
        response = requests.post(upload_url, headers=headers, files=files, data=data)
    
    if response.status_code == 200:
        print(f"Successfully uploaded {file_path} for {student_email}")
        return True
    else:
        print(f"Upload failed for {student_email}: {response.status_code} - {response.text}")
        return False

def main():
    if len(sys.argv) < 4:
        print("Usage: python upload.py <course_id> <assignment_id> <student_email> <file_path>")
        print("Example: python upload.py 1234 5678 student@umich.edu submission.json")
        sys.exit(1)
    
    course_id = sys.argv[1]
    assignment_id = sys.argv[2] 
    student_email = sys.argv[3]
    file_path = sys.argv[4]
    
    # Confirm the operation
    confirm = input(f"Confirm: upload {file_path} to course {course_id}, assignment {assignment_id} for {student_email}? (y/N): ")
    if confirm.lower() != 'y':
        print("Upload cancelled")
        sys.exit(1)
    
    # Check if file exists
    if not os.path.exists(file_path):
        print(f"Error: File {file_path} does not exist")
        sys.exit(1)
    
    # Login and get auth token
    try:
        auth_token = login_to_gradescope()
    except Exception as e:
        print(f"Login error: {e}")
        sys.exit(1)
    
    # Upload the file
    success = upload_single_file_to_gradescope(course_id, assignment_id, student_email, file_path, auth_token)
    
    if success:
        print("Upload completed successfully")
    else:
        print("Upload failed")
        sys.exit(1)

if __name__ == "__main__":
    main()