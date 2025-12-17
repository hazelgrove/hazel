#!/usr/bin/env python3
"""
Python autograder entry point for Gradescope
Equivalent to run_autograder shell script
"""

import os
import sys
import subprocess
from pathlib import Path


def main():
    """Main autograder entry point"""
    # Change to autograder source directory
    os.chdir('/autograder/source')
    
    # Find the test script to run
    # Look for run_test_*.py files and use the first one found
    test_scripts = [f for f in os.listdir('.') if f.startswith('run_test_') and f.endswith('.py') and f != 'run_test_base.py']
    
    if not test_scripts:
        print("Error: No test script found", file=sys.stderr)
        sys.exit(1)
    
    # Use the first test script found (there should only be one in the package)
    test_script = test_scripts[0]
    print(f"Running autograder: {test_script}")
    
    if not Path(test_script).exists():
        print(f"Error: Test script {test_script} not found", file=sys.stderr)
        sys.exit(1)
    
    try:
        # Run the test script
        result = subprocess.run([sys.executable, test_script], 
                              capture_output=False, 
                              check=True)
        print(f"Autograder completed successfully with exit code {result.returncode}")
    except subprocess.CalledProcessError as e:
        print(f"Autograder failed with exit code {e.returncode}", file=sys.stderr)
        sys.exit(e.returncode)
    except Exception as e:
        print(f"Autograder error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
