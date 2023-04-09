#!/bin/bash

GIT_DIR=$(git rev-parse --git-dir)

echo "installing hooks..."
    ln -s "../../hooks/pre-commit.sh" "$GIT_DIR/hooks/pre-commit"
echo "done"
