#!/bin/bash

# Test runner for Week 5 test cases
cd "/Users/ram_kan/Desktop/Nevada Epic#5/InCollege"

echo "=========================================="
echo "Running Week 5 Test Cases"
echo "=========================================="
echo ""

# Compile the program first
echo "Compiling InCollege..."
docker run --rm -v "$(pwd)":/workspace -w /workspace incollege-dev bash -c "cd src && cobc -x -free -o ../bin/InCollege InCollege.cob 2>&1" | head -5

if [ $? -ne 0 ]; then
    echo "Compilation failed!"
    exit 1
fi

echo "Compilation successful!"
echo ""

# Run each test
for i in {1..8}; do
    echo "=========================================="
    echo "Running Test $i"
    echo "=========================================="

    # Copy test input
    cp "wk5-in/test$i.txt" InCollege-Input.txt

    # Run the program
    docker run --rm -v "$(pwd)":/workspace -w /workspace incollege-dev bash -c "./bin/InCollege" > "test_output_$i.txt" 2>&1

    # Show first 30 lines of output
    echo "Output (first 30 lines):"
    head -30 "test_output_$i.txt"
    echo ""
    echo "Expected output (first 30 lines):"
    head -30 "wk5-out/test$i.txt"
    echo ""

    # Compare outputs (ignoring whitespace differences)
    if diff -w "test_output_$i.txt" "wk5-out/test$i.txt" > /dev/null 2>&1; then
        echo "✅ Test $i PASSED"
    else
        echo "❌ Test $i FAILED - Check differences:"
        diff -w "test_output_$i.txt" "wk5-out/test$i.txt" | head -20
    fi
    echo ""
done

echo "=========================================="
echo "All tests completed!"
echo "=========================================="
