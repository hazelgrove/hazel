#!/bin/bash

# Get the list of subfolders in the testdata folder
folders=(testdata/*/)

# Loop through each subfolder and run the commands
for folder in "${folders[@]}"
do
  # Extract the subfolder name from the full path
  subfolder=$(basename "$folder")
  
  echo "Running commands for folder: $subfolder"
  echo "--------------------------------"
  
  echo "Checking statics..."
  node hazeLS.js CHECK statics --common "testdata/common.haze" --prelude "$folder/prelude.haze" --main "$folder/solution.haze" --epilogue "$folder/epilogue.haze"
  
  echo "Checking dynamics..."
  node hazeLS.js CHECK dynamics --common "testdata/common.haze" --prelude "$folder/prelude.haze" --main "$folder/solution.haze" --epilogue "$folder/epilogue.haze"
  
  echo "--------------------------------"
  echo ""
done