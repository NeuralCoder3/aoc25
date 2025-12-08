#!/bin/bash

# Ensure N is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <N>"
  echo "Example: $0 1"
  exit 1
fi

N=$1
MAIN_FILE="bin/main.ml"
TEMPLATE_FILE="bin/aoc0.ml"
NEW_FILE="bin/aoc${N}.ml"

# Check if the new file already exists
if [ -f "$NEW_FILE" ]; then
  echo "Error: $NEW_FILE already exists. Aborting."
  exit 1
fi

# Check if the template file exists
if [ ! -f "$TEMPLATE_FILE" ]; then
  echo "Error: Template file $TEMPLATE_FILE not found."
  exit 1
fi

# Function to run sed in a cross-platform way (handling macOS vs Linux differences)
run_sed() {
  local pattern=$1
  local file=$2
  
  if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS requires an empty string for the backup extension
    sed -i '' -E "$pattern" "$file"
  else
    # Linux standard
    sed -i -E "$pattern" "$file"
  fi
}

# 1. Modify bin/main.ml
# Replace 'open Aoc' followed by digits with 'open AocN'
echo "Updating $MAIN_FILE..."
run_sed "s/open Aoc[0-9]+/open Aoc$N/" "$MAIN_FILE"

# 2. Copy the template to the new file
echo "Copying $TEMPLATE_FILE to $NEW_FILE..."
cp "$TEMPLATE_FILE" "$NEW_FILE"

# 3. Modify the new file
# Replace 'inputs/0_1.txt' with 'inputs/N_0.txt'
echo "Updating input path in $NEW_FILE..."
run_sed "s|inputs/0_1.txt|inputs/${N}_0.txt|" "$NEW_FILE"

dune build
echo "Success! Day $N set up."