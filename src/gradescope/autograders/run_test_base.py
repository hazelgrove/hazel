#!/usr/bin/env python3
"""
Base Python autograder template for Gradescope
Equivalent to run_test.lean.pl
"""

import json
import sys
import os
from pathlib import Path
from typing import Union, List, Dict, Any


def write_gradescope_results(score: Union[int, float, List[Dict]], output_dir: str = "/autograder/results"):
    """
    Write results in Gradescope's expected JSON format
    
    Args:
        score: Either a numeric total score or list of test dictionaries
        output_dir: Directory to write results.json (default: /autograder/results)
    """
    output = {}
    
    if isinstance(score, (int, float)):
        print('[debug] Using top level (total) score grading')
        output['score'] = score
    elif isinstance(score, list):
        print('[debug] Using individual tests grading')
        output['tests'] = score
    else:
        raise ValueError("Score must be a number or list of test dictionaries")
    
    output['stdout_visibility'] = 'visible'
    # output['output'] = ''  # Optional general output
    
    # Ensure output directory exists
    Path(output_dir).mkdir(parents=True, exist_ok=True)
    
    # Write results
    results_path = Path(output_dir) / 'results.json'
    with open(results_path, 'w', encoding='utf-8') as f:
        json.dump(output, f, indent=2)
    
    print(f"Results written to {results_path}")


def main():
    """
    Main autograder function - override this in specific implementations
    """
    if len(sys.argv) > 1:
        print("Usage: python run_test_base.py")
        print("This is a base template - implement your specific grading logic")
        sys.exit(0)
    
    # Stub implementation - replace with actual grading logic
    score = None
    tests = []
    
    # Example of how to build individual test results
    for test in tests:  # Empty in base template
        test_output = {
            'name': test['name'],
            'score': test['score'],
            'max_score': test['max_score']
        }
        if score is None:
            score = []
        score.append(test_output)
    
    # Default to simple total score if no tests defined
    if score is None:
        score = 100
    
    write_gradescope_results(score)


if __name__ == "__main__":
    main()
