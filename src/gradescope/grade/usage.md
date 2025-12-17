Installation:
  pip install PyYAML

Individual Components:
  python3 join.py <zip_file> -f python3 -f cat.py
  python3 proj.py <index>  # Extract element from JSON array
  python3 cat.py <directory>  # Read all files in directory

Complete Pipeline:
  python3 hazel_pipeline.py <gradescope_zip> <hazel_directory>

Example:
  # Process submissions through Hazel grader
  python3 hazel_pipeline.py ~/a8-code.zip ~/eecs490-hazel > graded_results.json

  # Individual steps (equivalent to Perl pipeline):
  python3 join.py ~/a8-code.zip -f python3 -f cat.py | python3 proj.py 0 > token2uniqname.json
  python3 join.py ~/a8-code.zip -f python3 -f cat.py | python3 proj.py 1 > submissions.json

Original Perl Command Equivalent:
  (printf '[';
      ./join.pl ~/a#-code.zip -f ./cat.pl | ./proj.pl 0;
   printf ',';
      ./join.pl ~/a#-code.zip -f ./cat.pl |
      ./proj.pl 1 |
      ./map.pl -f ./hazel.pl -f ~/eecs490-hazel;
   printf ']') | ./upload.pl -f json <course_id> <assignment_id>

Python Equivalent:
  python3 hazel_pipeline.py ~/a8-code.zip ~/eecs490-hazel | \\
    python3 upload.py -f json <course_id> <assignment_id>

Files Created:
  join.py         - Process Gradescope ZIP exports
  proj.py         - Extract elements from JSON arrays  
  cat.py          - Read directory contents
  hazel_pipeline.py - Complete pipeline (combines all steps)
  requirements.txt - Python dependencies