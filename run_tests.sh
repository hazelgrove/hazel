#!/bin/bash

# Constants
command_prefix="node _build/default/src/haz3lweb/www/hazeLS.js RUNTEST"
api_key="--api-key $HOME/azure-4-api-key.txt"
run_name=${1:-default}
log_directory="testlog"
collate_script="collate_data.sh"
warm_serials="warm_serials.sh"

command_timeout=180
wait_time=0
num_runs=9

# Source folders
source_folders=(
    "testdata/todo2/"
    "testdata/playlist1/"
    "testdata/booking2/"
    "testdata/emojipaint1/"
    "testdata/passwords2/"
)

# Optional argument variations
opt_arg_variations=(
    # "--rag RAG.txt"
    # "--rag RAG.txt --error_rounds_max 2"
    # "--relevant_ctx --expected_type"
    # "--temperature 1.0 --relevant_ctx --expected_type --error_rounds_max 6"
    # "--temperature 1.0 --error_rounds_max 2"
    # "--temperature 0.6 --error_rounds_max 2"
    # "--temperature 0.3 --error_rounds_max 2"
    # "--temperature 1.0 --expected_type --error_rounds_max 0"
    # "--temperature 0.6 --expected_type --error_rounds_max 0"
    # "--temperature 0.3 --expected_type --error_rounds_max 0"
    "--temperature 0.5 --rag RAG.txt --error_rounds_max 2"
    "--temperature 0.5 --rag RAG.txt"
    "--temperature 0.5 --relevant_ctx --expected_type --error_rounds_max 2"
    "--temperature 0.5 --relevant_ctx --expected_type"
    "--temperature 0.5 --relevant_ctx --error_rounds_max 2"
    "--temperature 0.5 --expected_type --error_rounds_max 2"
    "--temperature 0.5 --expected_type"
    "--temperature 0.5 --error_rounds_max 2"
    "--temperature 0.5 --relevant_ctx"
    "--temperature 0.5"
)

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

# Call the warm_serials script
bash "$warm_serials"

# Create log directory if it doesn't exist
mkdir -p "$log_directory"

# Function to run the command with given optional arguments and source folder
run_command() {
    opt_args=$1
    source_folder=$2
    timestamp=$(date +%Y%m%d_%H%M%S)
    log_file="$log_directory/${run_name}-${timestamp}.log"

    if $verbose; then
        timeout --foreground "$command_timeout"s bash -c "$command_prefix --run_name \"$run_name\" $opt_args --source_folder \"$source_folder\" $api_key | tee \"$log_file\"" &
    else
        timeout --foreground "$command_timeout"s bash -c "$command_prefix --run_name \"$run_name\" $opt_args --source_folder \"$source_folder\" $api_key > \"$log_file\" 2>&1" &
    fi
    cmd_pid=$!
    wait $cmd_pid
    exit_code=$?

    if [ $exit_code -eq 124 ]; then
        echo "Timeout: HazeLS command exceeded ${command_timeout} seconds for source '$source_folder' with options '$opt_args'. Log file: $log_file"
        error_count=$((error_count + 1))
        error_summary+="- Timeout: HazeLS command for source '$source_folder' with options '$opt_args' exceeded ${command_timeout} seconds. Log file: $log_file\n"
    elif [ $exit_code -ne 0 ]; then
        echo "Alert: HazeLS command exited with non-zero exit code $exit_code. Check log file: $log_file"
        error_count=$((error_count + 1))
        error_summary+="- HazeLS command with options '$opt_args' for source '$source_folder' exited with code $exit_code. Log file: $log_file\n"
    fi
}



# Initialize a summary string and error tracking variables
summary="Summary of test runs:\n"
error_count=0
error_summary=""

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

# Call the collate_data script
bash "$collate_script"

# Print the summary and error summary
echo -e "\n$summary"
if [ $error_count -gt 0 ]; then
    echo -e "Errors encountered during test runs:\n$error_summary"
    echo "Total errors: $error_count"
else
    echo "All test runs completed successfully."
fi