#!/bin/bash

zig build test
if [$? -ne 0]
    echo "tests failed - aborting commit"
    exit 1
fi
