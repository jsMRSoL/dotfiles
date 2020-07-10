for linenum in range(a:firstline, a:lastline)
    "Replace loose ampersands
    let curr_line = getline(linenum)
endfor

if a:lastline > a:firstline
    echo "dunno"
endif
