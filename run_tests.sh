#!/bin/bash

# Constants
command_prefix="node hazeLS.js RUNTEST"
api_key="--api-key $HOME/azure-4-api-key.txt"
run_name=${1:-default}
wait_time=0
num_runs=1
log_directory="testlog"

# Function to display usage information
usage() {
    echo "Usage: $0 [--verbose] [run_name]"
    echo "  --verbose    Display HazeLS output on the terminal in addition to saving in log files"
    echo "  run_name     Name of the test run (default: 'default')"
}

# Check if --verbose flag is provided
verbose=false
while [[ $# -gt 0 ]]; do
    case "$1" in
        --verbose)
            verbose=true
            shift
            ;;
        --*)
            echo "Invalid flag: $1"
            usage
            exit 1
            ;;
        *)
            run_name=$1
            shift
            ;;
    esac
done

# Source folders
source_folders=(
    "testdata/todo1/"
    "testdata/playlist1/"
)

# Optional argument variations
opt_arg_variations=(
    "--expected_type --error_rounds_max 2"
    "--expected_type"
    "--error_rounds_max 2"
    ""
)

# Create log directory if it doesn't exist
mkdir -p "$log_directory"

# Function to run the command with given optional arguments and source folder
run_command() {
    opt_args=$1
    source_folder=$2
    timestamp=$(date +%Y%m%d_%H%M%S)
    log_file="$log_directory/${run_name}-${timestamp}.log"

    if $verbose; then
        $command_prefix --run_name "$run_name" $opt_args --source_folder "$source_folder" $api_key | tee "$log_file"
    else
        $command_prefix --run_name "$run_name" $opt_args --source_folder "$source_folder" $api_key > "$log_file" 2>&1
    fi

    # Check the exit code of the HazeLS command
    exit_code=$?
    if [ $exit_code -ne 0 ]; then
        echo "Alert: HazeLS command exited with non-zero exit code $exit_code. Check log file: $log_file"
    fi
}

# Initialize a summary string
summary="Summary of test runs:\n"

# Iterate over source folders
for source_folder in "${source_folders[@]}"
do
    # Iterate over optional argument variations
    for opt_args in "${opt_arg_variations[@]}"
    do
        # Run the command multiple times for each variation and source folder
        for ((i=1; i<=num_runs; i++))
        do
            echo "Running test $i for source: $source_folder with options: $opt_args"
            run_command "$opt_args" "$source_folder"
            sleep $wait_time
        done

        # Append to the summary string
        summary+="- Ran $num_runs tests for source: $source_folder with options: $opt_args\n"
    done
done

# Print the summary
echo -e "\n$summary"