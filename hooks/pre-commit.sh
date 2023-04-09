#!/bin/bash

echo "unit tests..."
zig build test
if [ $? -ne 0 ]; then
    echo "tests failed - aborting commit"
    exit 1
fi

echo "ok"
