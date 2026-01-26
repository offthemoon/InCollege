#!/bin/bash

# Script to run InCollege tests
# Usage: ./run-test.sh <test-file-name>

if [ -z "$1" ]; then
    echo "Usage: ./run-test.sh <test-file-name>"
    echo "Example: ./run-test.sh test-positive.txt"
    exit 1
fi

TEST_FILE=$1
OUTPUT_FILE="${TEST_FILE%.txt}-output.txt"

echo "Running test: $TEST_FILE"
echo "Output will be saved to: $OUTPUT_FILE"

# Copy test file to InCollege-Input.txt
cp "$TEST_FILE" InCollege-Input.txt

# Run the program
./bin/InCollege > /dev/null 2>&1

# Copy output to test-specific output file
cp InCollege-Output.txt "$OUTPUT_FILE"

echo "Test complete! Check $OUTPUT_FILE for results."
