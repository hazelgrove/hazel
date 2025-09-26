# Python Autograder System

This directory contains the Python equivalent of the Perl-based Gradescope autograder system.

## Files Overview

### Core Python Scripts
- **`run_test_base.py`** - Base autograder template (equivalent to `run_test.lean.pl`)
- **`run_test_grade_hazel.py`** - Hazel-specific grading logic (equivalent to `run_test.grade-hazel.lean.pl`)
- **`run_autograder.py`** - Main entry point (equivalent to `run_autograder` shell script)

### Build System
- **`build.py`** - Python build script (replaces Perl Makefile functionality)
- **`Makefile.python`** - Optional Makefile that calls Python build script

## Usage

### Building Autograders

```bash
# Using Python directly
python3 build.py grade-hazel

# Using Make (calls Python script)
make -f Makefile.python grade-hazel

# Available targets: default, grade-hazel, check-hazel, etc.
```

### What the Build Process Does

1. **Syntax Check**: Validates Python syntax (`python -m py_compile`)
2. **Create Standalone Script**: Copies the specific test script to `run_test_standalone.py`
3. **Setup Dependencies**: Creates `setup.sh` and `requirements.txt`
4. **Package**: Creates `AG.zip` with all necessary files

### Key Differences from Perl Version

| Perl System | Python System | Notes |
|-------------|---------------|-------|
| `fatpack pack` | Simple copy | Python's import system handles dependencies |
| `perl -c` | `python -m py_compile` | Syntax checking |
| Embedded modules | `requirements.txt` | Dependency management |
| `scandeps.pl` | Import analysis | Simplified dependency detection |

## Creating New Autograders

1. **Create test script**: `run_test_your_assignment.py`
2. **Implement grading logic**: Use `run_test_base.py` as template
3. **Build**: `python3 build.py your-assignment`
4. **Upload**: Use generated `AG.zip`

### Example Test Script Structure

```python
#!/usr/bin/env python3
from run_test_base import write_gradescope_results

def main():
    # Your grading logic here
    tests = [
        {'name': 'Test 1', 'score': 8, 'max_score': 10},
        {'name': 'Test 2', 'score': 15, 'max_score': 15}
    ]
    
    write_gradescope_results(tests)

if __name__ == "__main__":
    main()
```

## Expected JSON Output Format

The system produces Gradescope-compatible JSON:

```json
{
  "tests": [
    {
      "name": "Test Name",
      "score": 8,
      "max_score": 10,
      "output": "Test details..."
    }
  ],
  "stdout_visibility": "visible",
  "output": "General feedback"
}
```

## Advantages of Python Version

1. **Simpler Dependencies**: No need for complex module embedding
2. **Better Error Handling**: Python's exception system
3. **More Readable**: Python syntax is generally clearer
4. **Rich Ecosystem**: Easy access to scientific/data processing libraries
5. **JSON Native**: Built-in JSON handling

## Migration Notes

- All Perl `confess`/`croak` → Python `raise Exception`
- Perl `reftype` checks → Python `isinstance()`
- Perl file operations → Python `pathlib.Path`
- Perl regex → Python `re` module (where needed)

## Dependencies

- Python 3.7+ (Gradescope uses Ubuntu with modern Python)
- Standard library modules: `json`, `pathlib`, `subprocess`, `zipfile`
- Optional: Any packages listed in your `requirements.txt`
