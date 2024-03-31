#!/bin/bash

# Set the path to the testout folder
testout_folder="testout"

# Set the output CSV file name
output_file="collated_data.csv"

# Initialize an array to store the keys (column headers)
keys=()

# Initialize an associative array to store default values for certain keys
declare -A default_values
default_values["option-expected_type"]="false"
default_values["option-relevant_ctx"]="false"
default_values["option-error_rounds_max"]="0"
default_values["option-temperature"]="1.0"
default_values["option-rag"]="false"

# Set the internal flag to control the usage of the whitelist
use_whitelist=true

# Define the ordered list of keys for the whitelist
whitelist=(
    "output-path"
    "option-source_path"
    "option-expected_type"
    "option-relevant_ctx"
    "option-error_rounds_max"
    "option-rag"
    "option-temperature"
    "start-time"
    "tests-total"
    "tests-pass"
    "tests-indet"
    "derived-time-elapsed"
    "derived-percent-tests"
    "derived-err-rounds-used"
    "derived-total-tokens-used"
    "derived-final-parses"
    "derived-final-static-errors"
    "derived-err-improve"
)

# Loop through each subfolder in the testout folder to collect the keys
for subfolder in "$testout_folder"/*/; do
    # Check if the run.data file exists in the subfolder
    if [ -f "$subfolder/run.data" ]; then
        # Read the run.data file
        while IFS= read -r line; do
            # Split the line by colon and extract the key
            IFS=':' read -r key _ <<< "$line"

            # Remove the double quotes from the key
            key=$(echo "$key" | tr -d '"')

            # Add the key to the keys array if it doesn't exist and if not using the whitelist
            if ! $use_whitelist && [[ ! " ${keys[@]} " =~ " $key " ]]; then
                keys+=("$key")
            fi
        done < "$subfolder/run.data"
    fi
done

# Set the keys to the whitelist if the flag is set
if $use_whitelist; then
    keys=("${whitelist[@]}")
fi

# Write the column headers (keys) to the output CSV file
(IFS=','; echo "${keys[*]}") > "$output_file"

# Get the list of subfolders in the testout folder and sort them based on creation time
IFS=$'\n' sorted_subfolders=($(ls -dt "$testout_folder"/*/))

# Loop through each subfolder in the sorted order
for subfolder in "${sorted_subfolders[@]}"; do
    # Check if the run.data file exists in the subfolder
    if [ -f "$subfolder/run.data" ]; then
        # Initialize an associative array to store the key-value pairs
        declare -A data

        # Get the subfolder name
        subfolder_name=$(basename "$subfolder")

        # Set the value for the "output-path" key
        data["output-path"]="$subfolder_name"

        # Read the run.data file
        while IFS= read -r line; do
            # Split the line by colon and extract the key and value
            IFS=':' read -r key value <<< "$line"

            # Remove the double quotes from the key
            key=$(echo "$key" | tr -d '"')

            # Check if the value contains commas or other problematic characters
            if [[ $value == *","* || $value == *$'\n'* || $value == *$'\r'* ]]; then
                # Escape double quotes within the value
                value=$(echo "$value" | sed 's/"/""/g')
                # Surround the value with double quotes
                value="\"$value\""
            else
                # Remove the double quotes from the value
                value=$(echo "$value" | tr -d '"')
            fi

            # Store the key-value pair in the associative array
            data["$key"]="$value"
        done < "$subfolder/run.data"

        # Generate the CSV row for the current run.data file
        row=""
        for key in "${keys[@]}"; do
            if [[ -n "${data[$key]}" ]]; then
                row+="${data[$key]},"
            else
                # Use the default value if provided, otherwise use NULL
                row+="${default_values[$key]:-NULL},"
            fi
        done

        # Remove the trailing comma
        row="${row%,}"

        # Append the row to the output CSV file
        echo "$row" >> "$output_file"
    fi
done

echo "Data collation completed. CSV file generated: $output_file"