#!/usr/bin/env python3
"""
Script to fix import issues in the notebook by testing and reloading modules.
Run this if you encounter import errors in the Jupyter notebook.
"""

import sys
import importlib
import os

def test_imports():
    """Test all required imports."""
    print("Testing imports...")
    
    # Test basic libraries
    try:
        import pandas as pd
        print("✅ pandas imported successfully")
    except ImportError as e:
        print(f"❌ pandas import failed: {e}")
        return False
    
    try:
        import numpy as np
        print("✅ numpy imported successfully")
    except ImportError as e:
        print(f"❌ numpy import failed: {e}")
        return False
    
    try:
        import matplotlib.pyplot as plt
        print("✅ matplotlib imported successfully")
    except ImportError as e:
        print(f"❌ matplotlib import failed: {e}")
        return False
    
    try:
        import seaborn as sns
        print("✅ seaborn imported successfully")
    except ImportError as e:
        print(f"❌ seaborn import failed: {e}")
        return False
    
    # Test custom modules
    try:
        from parser import HazelLogParser
        print("✅ parser imported successfully")
    except ImportError as e:
        print(f"❌ parser import failed: {e}")
        return False
    
    try:
        from analysis_utils import calculate_time_diffs, get_time_statistics
        print("✅ analysis_utils imported successfully")
    except ImportError as e:
        print(f"❌ analysis_utils import failed: {e}")
        return False
    
    # Test path_extractor functions
    try:
        from path_extractor import (
            extract_action_sequences, identify_exploration_paths,
            identify_focused_paths, identify_navigation_paths,
            track_proof_progression, find_induction_patterns, summarize_paths,
            analyze_induction_timing_patterns, track_induction_progression
        )
        print("✅ path_extractor imported successfully")
    except ImportError as e:
        print(f"❌ path_extractor import failed: {e}")
        print("Attempting to reload module...")
        
        # Try to reload the module
        if 'path_extractor' in sys.modules:
            importlib.reload(sys.modules['path_extractor'])
            print("Module reloaded, trying import again...")
            
            try:
                from path_extractor import (
                    extract_action_sequences, identify_exploration_paths,
                    identify_focused_paths, identify_navigation_paths,
                    track_proof_progression, find_induction_patterns, summarize_paths,
                    analyze_induction_timing_patterns, track_induction_progression
                )
                print("✅ path_extractor imported successfully after reload!")
            except ImportError as e2:
                print(f"❌ path_extractor import still failed: {e2}")
                return False
        else:
            return False
    
    return True

def check_file_syntax():
    """Check if path_extractor.py has syntax errors."""
    print("\nChecking path_extractor.py syntax...")
    
    try:
        with open('path_extractor.py', 'r') as f:
            content = f.read()
        
        # Try to compile the file
        compile(content, 'path_extractor.py', 'exec')
        print("✅ path_extractor.py syntax is valid")
        return True
    except SyntaxError as e:
        print(f"❌ path_extractor.py has syntax error: {e}")
        print(f"   Line {e.lineno}: {e.text}")
        return False
    except Exception as e:
        print(f"❌ Error checking path_extractor.py: {e}")
        return False

def main():
    """Main function."""
    print("=== IMPORT FIX SCRIPT ===")
    print(f"Working directory: {os.getcwd()}")
    print(f"Python version: {sys.version}")
    print(f"Python path: {sys.executable}")
    
    # Check if we're in the right directory
    if not os.path.exists('path_extractor.py'):
        print("❌ path_extractor.py not found in current directory")
        print("Please run this script from the analysis directory")
        return
    
    # Check syntax first
    if not check_file_syntax():
        print("❌ Syntax errors found, please fix them first")
        return
    
    # Test imports
    if test_imports():
        print("\n🎉 All imports successful!")
        print("You can now run the Jupyter notebook without issues.")
    else:
        print("\n❌ Some imports failed.")
        print("Try restarting the Jupyter kernel and running the notebook again.")
        print("If the issue persists, check that you're using the conda environment:")
        print("  conda activate base")

if __name__ == "__main__":
    main()
