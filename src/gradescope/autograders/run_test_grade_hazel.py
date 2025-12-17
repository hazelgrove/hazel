#!/usr/bin/env python3
"""
Hazel grading autograder for Gradescope
Python equivalent of run_test.grade-hazel.lean.pl
"""

import json
import sys
import os
from pathlib import Path
from glob import glob


def main():
    """Grade Hazel submissions"""
    if len(sys.argv) > 1:
        print("Usage: python run_test_grade_hazel.py")
        sys.exit(0)
    
    # Find submission file (equivalent to: my @files = glob('/autograder/submission/*.json'))
    submission_files = glob('/autograder/submission/*.json')
    
    if len(submission_files) != 1:
        raise AssertionError(f"Expected exactly 1 submission file, found {len(submission_files)}")
    
    submission_path = submission_files[0]
    
    # Read submission
    with open(submission_path, 'r', encoding='utf-8') as f:
        submission = json.load(f)
    
    # Verify submission is an array of hazel exercises
    if not isinstance(submission, list):
        raise AssertionError("Submission must be a JSON array of hazel exercises")
    
    # Expected max scores for each exercise
    expected_scores = [4, 4, 7, 10, 15, 20, 20, 20]
    
    tests = []
    for exercise in submission:
        if not isinstance(exercise, dict):
            raise AssertionError("Each exercise must be a dictionary")
        
        expected_max_score = expected_scores.pop(0) if expected_scores else 0
        
        # Verify expected max score matches
        actual_max_score = exercise['report']['overall'][1]
        if actual_max_score != expected_max_score:
            raise AssertionError(f"Expected max score {expected_max_score}, got {actual_max_score}")
        
        test = {
            'name': exercise['name'],
            'score': exercise['report']['overall'][0],
            'max_score': exercise['report']['overall'][1],
            'output': exercise['report']['summary']
        }
        tests.append(test)
    
    # Create output with custom message
    output = {
        'tests': tests,
        'stdout_visibility': 'visible',
        'output': 'Hazel code feedback autograder seems okay.\n'
    }
    
    # Write results
    results_dir = Path("/autograder/results")
    results_dir.mkdir(parents=True, exist_ok=True)
    
    with open(results_dir / 'results.json', 'w', encoding='utf-8') as f:
        json.dump(output, f, indent=2)
    
    print("Hazel grading completed successfully")


if __name__ == "__main__":
    main()
