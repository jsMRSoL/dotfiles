#!/usr/bin/awk -f
BEGIN {
    x=1
    y=padding
}

{
    if((NR-2)%3==1) printf("%-"y"s%03d\n", $0, x++);
    else print $0
}
