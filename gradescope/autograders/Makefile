# Python-based Gradescope Autograder Makefile
# Equivalent to the original Perl-based system

## Python autograder targets
check-hazel:
	python3 build.py check-hazel

grade-hazel:
	python3 build.py grade-hazel

## Utility targets
clean:
	python3 build.py --clean

test-syntax:
	python3 -m py_compile run_test_*.py

help:
	@echo "Available targets:"
	@echo "  check-hazel  - Build Hazel checking autograder"
	@echo "  grade-hazel  - Build Hazel grading autograder"
	@echo "  clean        - Remove build artifacts"
	@echo "  test-syntax  - Check Python syntax of all scripts"
	@echo ""
	@echo "Usage: make <target>"

.PHONY: check-hazel grade-hazel clean test-syntax help
