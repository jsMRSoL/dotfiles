#!/bin/bash
dictionary="$1"
word="$2"

awk -F ': ' '/^'"$2[^a-z]"'/ {print $2}' "$dictionary"


