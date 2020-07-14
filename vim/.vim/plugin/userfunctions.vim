function! AlignAssignments ()
    " Courtesy of Damian Conway
    " See [[https://www.ibm.com/developerworks/library/l-vim-script-3/index.html][Damian Conway on vimscript]]
    " Patterns needed to locate assignment operators...
    let ASSIGN_OP   = '[-+*/%|&]\?=\@<!=[=~]\@!'
    let ASSIGN_LINE = '^\(.\{-}\)\s*\(' . ASSIGN_OP . '\)\(.*\)$'

    " Locate block of code to be considered (same indentation, no blanks)
    let indent_pat = '^' . matchstr(getline('.'), '^\s*') . '\S'
    let firstline  = search('^\%('. indent_pat . '\)\@!','bnW') + 1
    let lastline   = search('^\%('. indent_pat . '\)\@!', 'nW') - 1
    if lastline < 0
        let lastline = line('$')
    endif

    " Decompose lines at assignment operators...
    let lines = []
    for linetext in getline(firstline, lastline)
        let fields = matchlist(linetext, ASSIGN_LINE)
        call add(lines, fields[1:3])
    endfor

    " Determine maximal lengths of lvalue and operator...
    let op_lines = filter(copy(lines),'!empty(v:val)')
    let max_lval = max( map(copy(op_lines), 'strlen(v:val[0])') ) + 1
    let max_op   = max( map(copy(op_lines), 'strlen(v:val[1])'  ) )

    " Recompose lines with operators at the maximum length...
    let linenum = firstline
    for line in lines
        if !empty(line)
            let newline
            \    = printf("%-*s%*s%s", max_lval, line[0], max_op, line[1], line[2])
            call setline(linenum, newline)
        endif
        let linenum += 1
    endfor
endfunction

function! AlignOnChar (Char)
    " My attempt to generalize DC's function above.
    " Patterns needed to locate assignment operators...
    let ASSIGN_OP   = a:Char
    let ASSIGN_LINE = '^\(.\{-}\)\s*\(' . ASSIGN_OP . '\)\(.*\)$'

    " Locate block of code to be considered (same indentation, no blanks)
    let indent_pat = '^' . matchstr(getline('.'), '^\s*') . '\S'
    let firstline  = search('^\%('. indent_pat . '\)\@!','bnW') + 1
    let lastline   = search('^\%('. indent_pat . '\)\@!', 'nW') - 1
    if lastline < 0
        let lastline = line('$')
    endif

    " Decompose lines at assignment operators...
    let lines = []
    for linetext in getline(firstline, lastline)
        let fields = matchlist(linetext, ASSIGN_LINE)
        call add(lines, fields[1:3])
    endfor

    " Determine maximal lengths of lvalue and operator...
    let op_lines = filter(copy(lines),'!empty(v:val)')
    let max_lval = max( map(copy(op_lines), 'strlen(v:val[0])') ) + 1
    let max_op   = max( map(copy(op_lines), 'strlen(v:val[1])'  ) )

    " Recompose lines with operators at the maximum length...
    let linenum = firstline
    for line in lines
        if !empty(line)
            let newline
            \    = printf("%-*s%*s%s", max_lval, line[0], max_op, line[1], line[2])
            call setline(linenum, newline)
        endif
        let linenum += 1
    endfor
endfunction

nnoremap <leader>a      :call AlignOnChar(input("Enter character: "))<CR>

"Locate and return character "above" current cursor position...
function! LookUpwards()
"   "Locate current column and preceding line from which to copy...
      let column_num      = virtcol('.')
      let target_pattern  = '\%' . column_num . 'v.'
      let target_line_num = search(target_pattern . '*\S', 'bnW')

    "If target line found, return vertically copied character...
      if !target_line_num
         return ""
      else
         return matchstr(getline(target_line_num), target_pattern)
      endif
endfunction

"Reimplement CTRL-Y within insert mode...
inoremap <silent>  <C-Y>  <C-R><C-R>=LookUpwards()<CR>

" attempt to write a Regex() function like DC refers to 
" to take a whitespace-punctuated string which will become part
" of a regex expression and return it without whitespace.
" The idea is to make constructing/reading/maintaining regexes
" by writing them in a whitespace-punctuated version.

function! Regex(regexpr)
    " Gotchas learned for next time:
    " "\s\+" doesn't work. "\\s\\+" does.
    " Or the single-quoted version used below.
    let s:newexpr = substitute(a:regexpr,'\s\+',"","g")
    "echo s:newexpr
    return s:newexpr
endfunction

"practice at constructing and using a regex string
" I will use the following target string:
"      abc def  ghi    jklm     opqrstuvwxyz
" The following results in 
"      XXXXXX def ghi    etc.
" What I didn't realise is that substitute can really only deal with one
" section of a string at a time. I didn't understand how clever DC was being
" by using various regexes to identify parts for his new string 
" and then just substituting in the new string (with substitute) at the end.
" Very clever...
function! RegPrac(myexpr)
    "let INDENT = Regex('^   \s \+   '    ) |" expect initial whitespace
    let FIRST  = Regex('                \(  \w \+  \)' ) |" catch first word
    "let SECOND = Regex('\(  \s \+   \)  \(  \w \+  \)' ) |" catch second word
    "let THIRD  = Regex('\(  \s \+   \)  \(  \w \+  \)' ) |" catch third word
    "let LAST   = Regex('\(  .*$     \)'     ) |" catch last part

    let s:PATTERN = FIRST
    echo s:PATTERN
    let s:newexpr = substitute(a:myexpr, s:PATTERN, 'XXXXXX', '')
    echo s:newexpr
endfunction
function! RegPrac2(myexpr)
    let FIRST  = Regex('^   \s  \+      \(  \w \+  \)' ) |" catch first word
    let SECOND = Regex('    \s \+       \(  \w \+  \)' ) |" catch second word
    let THIRD  = Regex('    \s \+       \(  \w \+  \)' ) |" catch third word
    let LAST   = Regex('    \s \+       \(  .*$     \)'     ) |" catch last part

    let s:PATTERN = FIRST . SECOND . THIRD . LAST
    echo s:PATTERN
    let s:newexpr = substitute(a:myexpr, s:PATTERN, '\=printf("%s%s%s", submatch(1), " ", submatch(2))', '')
    echo s:newexpr
endfunction
