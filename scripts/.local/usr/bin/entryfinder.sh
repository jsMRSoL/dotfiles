#!/bin/bash
dictionary="$1"
word="$2"

awk '/^'"$2[^a-z]"'/ {print $0}' "$dictionary"


