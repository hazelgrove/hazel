#!/usr/bin/env python3
"""
Hazel submission checker autograder for Gradescope
Python equivalent of run_test.check-hazel.lean.pl
"""

import json
import sys
import os
from pathlib import Path
from glob import glob


def main():
    """Check Hazel submissions for basic validity"""
    if len(sys.argv) > 1:
        print("Usage: python run_test_check_hazel.py")
        sys.exit(0)
    
    # Find submission file (equivalent to: my @files = glob('/autograder/submission/*.json'))
    submission_files = glob('/autograder/submission/*.json')
    
    # should(@files, 1) - equivalent to asserting exactly 1 file
    if len(submission_files) != 1:
        raise AssertionError(f"Expected exactly 1 JSON file, found {len(submission_files)}")
    
    submission_path = submission_files[0]
    
    # Read submission (equivalent to: JSON::from_json File::Slurp::read_file($submission_path))
    with open(submission_path, 'r', encoding='utf-8') as f:
        submission = json.load(f)
    
    # should(reftype $submission, 'HASH') - equivalent to checking it's a dict
    if not isinstance(submission, dict):
        raise AssertionError(f"Expected JSON object (dict), got {type(submission).__name__}")
    
    # assert(defined $submission->{exercise})
    if 'exercise' not in submission:
        raise AssertionError("Submission must have 'exercise' field")
    
    # my $score = 100;
    score = 100
    
    # Create output (matches Perl exactly)
    output = {
        'score': score,
        'stdout_visibility': 'visible',
        'output': '''Upload appears to be a Hazel json export
*No other checks performed*
Hazel code upload sanity check PASSED
'''
    }
    
    # Write results
    results_dir = Path("/autograder/results")
    results_dir.mkdir(parents=True, exist_ok=True)
    
    with open(results_dir / 'results.json', 'w', encoding='utf-8') as f:
        json.dump(output, f, indent=2)
    
    print("[debug] Using top level (total) score grading")
    print("Hazel submission check completed")


if __name__ == "__main__":
    main()
