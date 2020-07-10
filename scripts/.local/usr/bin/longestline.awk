#!/usr/bin/awk -f
BEGIN {
    longest=0
}

{
    if(length($0) > longest) longest = length($0)
}

END {
    printf("Longest line is %s characters.\n", longest)
}
