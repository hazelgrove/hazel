#!/bin/bash

# Set the path to the testout folder
testout_folder="testout"

# Set the output CSV file name
output_file="collated_data.csv"

# Initialize an array to store the keys (column headers)
keys=()

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

            # Add the key to the keys array if it doesn't exist
            if [[ ! " ${keys[@]} " =~ " $key " ]]; then
                keys+=("$key")
            fi
        done < "$subfolder/run.data"
    fi
done

# Write the column headers (keys) to the output CSV file
(IFS=','; echo "${keys[*]}") > "$output_file"

# Loop through each subfolder in the testout folder to collect the values
for subfolder in "$testout_folder"/*/; do
    # Check if the run.data file exists in the subfolder
    if [ -f "$subfolder/run.data" ]; then
        # Initialize an associative array to store the key-value pairs
        declare -A data

        # Read the run.data file
        while IFS= read -r line; do
            # Split the line by colon and extract the key and value
            IFS=':' read -r key value <<< "$line"

            # Remove the double quotes from the key and value
            key=$(echo "$key" | tr -d '"')
            value=$(echo "$value" | tr -d '"')

            # Store the key-value pair in the associative array
            data["$key"]="$value"
        done < "$subfolder/run.data"

        # Generate the CSV row for the current run.data file
        row=""
        for key in "${keys[@]}"; do
            row+="${data[$key]},"
        done

        # Remove the trailing comma
        row="${row%,}"

        # Append the row to the output CSV file
        echo "$row" >> "$output_file"
    fi
done

echo "Data collation completed. CSV file generated: $output_file"