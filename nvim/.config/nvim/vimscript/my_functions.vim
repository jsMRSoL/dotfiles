" function to extract urls from html {{{
function! ExtractUrls ()
    execute 'silent g/^$/d'
    execute 'silent v/url="https.*\.\(pdf\|doc\|ppt\)/d'
    execute 'silent %s/^\s*.*url="\(https.*\.\(pdf\|docx\|pptx\)\)"/\1/'
endfunction
" }}}
" RearrangeLines {{{
function! RearrangeLines(line1, line2, count) range
    " Rearrange lines from order 1, 3, 5, 2, 4, 6 to 1, 2, 3, 4, 5, 6
    if a:count == -1
        echo "Please visually select lines"
        return
    endif
    let lines = getline(a:line1, a:line2)
    let steps = len(lines)/2
    let new_lines = []
    for i in range(steps)
        call add(new_lines, lines[i])
        call add(new_lines, lines[i + steps])
    endfor
    call setline(a:line1, new_lines)
endfunction
"}}}
    command! -narg=0 -range RL call RearrangeLines(<line1>, <line2>, <count>)
    "{{{ ListToLine
function! ListToLine(line1, line2, count) range
    if a:count == -1
        echo "Please visually select your lines"
        return
    endif
    let my_lookup_lines = getline(a:line1, a:line2)
    execute 'normal gvd'
    let newline = join(my_lookup_lines, '; ')
    let newline = substitute(newline, '-  ', '', 'g')
    "call setpos('.', [0, a:line2, 1, 0])
    call setline(a:line1, newline)
endfunction
"}}}
    command! -narg=0 -range LTL call ListToLine(<line1>, <line2>, <count>)
    "{{{ GetLatinVocab
function! GetLatinVocab(line1) range
    " Wrapper for fetchLatinList.py
    let my_lookup_text = getline(a:line1)
    execute 'read !~/.vim/pythonx/fetchLatinList.py ' . shellescape(my_lookup_text)
    call setpos('.', [0, a:line1, 1, 0])
endfunction
"}}}
    command! -narg=0 -range GLV call GetLatinVocab(<line1>)
    "{{{ GetLatinVocabLines
function! GetLatinVocabLines(line1, line2) range
    " Wrapper for fetchLatinList.py
    let my_lookup_lines = getline(a:line1, a:line2)
    let my_lookup_text = join(my_lookup_lines)
    let my_lookup_text = substitute(my_lookup_text, ';', '.', 'g')
    let my_lookup_text = substitute(my_lookup_text, '!', '.', 'g')
    call setpos('.', [0, a:line2, 1, 0])
    execute 'read !~/.vim/pythonx/fetchLatinList.py ' . shellescape(my_lookup_text)
    call setpos('.', [0, a:line1, 1, 0])
endfunction
"}}}
    command! -narg=0 -range GLL call GetLatinVocabLines(<line1>, <line2>)
    "{{{ CLClookup
function! CLClookup(line1, line2) range
    " Wrapper for clc-lookup.py
    let my_lookup_lines = getline(a:line1, a:line2)
    let my_lookup_text = join(my_lookup_lines)
    let my_lookup_text = substitute(my_lookup_text, ';', '.', 'g')
    let my_lookup_text = substitute(my_lookup_text, '!', '.', 'g')
    call setpos('.', [0, a:line2, 1, 0])
    execute 'read !~/.vim/bundle/vim-clc-lookup/clclookup.py ' . shellescape(my_lookup_text)
    call setpos('.', [0, a:line1, 1, 0])
endfunction
"}}}
    command! -narg=0 -range CLU call CLClookup(<line1>, <line2>)
    "{{{ BackupLookup
function! BackupLookup()
    " For when CLC fails!
    let answer = system('grep -m1 ^' . shellescape(expand('<cword>'))
        \ . shellescape('[, ]') . ' ~/.vim/bundle/vim-clc-lookup/clc4-vocab')
    let answer = '-  ' . substitute(answer, '\n$', '', '')
    call setline('.', answer)
endfunction
"}}}
    command! -narg=0 BLU call BackupLookup()
    "{{{ GreekLookup
function! GreekLookup(line1, line2) range
    let lines = getline(a:line1, a:line2)
    let newlines = []
    execute '60vsplit Greek_Definitions'
    for line in lines
        execute 'normal ggdGgg'
        let answer = systemlist('grep -m9 :*' . shellescape(line)
            \ . ' ~/.vim/bundle/vim-GG-lookup/GCSE-Greek-vocab.txt')
        if len(answer) == 0
            call add(newlines, line)
            continue
        endif
        if len(answer) == 1
            call add(newlines, answer[0])
            continue
        endif
        let enum_answer = map(copy(answer), 'v:key . ". " . v:val') 
        call insert(enum_answer, 'Choose a definition for ' . line. ': ')
        call append('0', enum_answer)
        redraw
        let choice = input('Select a definition: ')
        "echo answer[choice]
        call add(newlines, answer[choice])
        redraw
    endfor
    execute 'close!'
    " echo newlines
    call append('$', newlines)
    execute a:line1 . ',' . a:line2 . 'delete'
endfunction
"}}}
    command! -narg=0 -range GLU call GreekLookup(<line1>, <line2>)
    "{{{ ShuntLine
function! ShuntLine()
     " Shunt line from cursor onwards down a line
    let [bufno, lineno, colno, offset] = getpos('.')
    let colno = colno - 1
    execute 'normal Do' . string(colno) . 'i p'
endfunction
"}}}
    command! -narg=0 SHL call ShuntLine()
    "{{{ PushLineUp
function! PushLineUp()
    execute 'normal dd'
    call search('^[^-]', 'b')
    execute 'normal p'
endfunction
"}}}
    command! -narg=0 PLU call PushLineUp()
    "{{{ LookupAndLabelOld
function! LookupAndLabelOld()
    let indent = matchstr(getline('.'), '^\s*')
    let words = split(getline('.'), '\s\s*')
    " echo "Words: "
    " echo words
    let topline = indent
    let bottomline = indent
    let dictionaryfile = "/home/simon/.config/vim/bundle/vim-amores2/amores2vocab.txt"
    for word in words
        " clean punctuation
        let cleanwd = substitute(word, '\(.\{-}\)[^A-z]', '\=submatch(1)', '')
        " determine headword
        " echo "cleanwd is : " . cleanwd
        if len(cleanwd) == 0
            continue
        endif
        let headwords = systemlist('/home/simon/.config/vim/bundle/vim-amores2/hermes.sh ' . shellescape(expand(cleanwd)))
        if len(headwords) == 0
            call add(headwords, cleanwd)
        endif
        " echo 'headwords: '
        " echo headwords
        " get definition
        let definitions = []
        let positions = []
        let defcount = 0
        for position in range(len(headwords))
            " let definition = system('/home/simon/.config/vim/bundle/vim-amores2/deffinder.sh ' . shellescape(expand(headwords[position])) . ' | tr -d "\n"')
            let definitionlist = systemlist('/home/simon/.config/vim/bundle/vim-amores2/deffinder.sh ' . shellescape(expand(headwords[position])))
            let definition = join(definitionlist, ', ')
            let definition = substitute(definition, '^I ', '', '')
            if len(definition) > 0
                let defcount = defcount + 1
                call add(positions, position)
            endif
            call add(definitions, definition)
        endfor
        " echo 'definitions: '
        " echo definitions
        " echo 'definition count: ' . defcount
        " echo 'positions list: '
        " echo positions
        if defcount == 0 && len(headwords) > 1 || defcount > 1
            let enum_answer = map(copy(headwords), 'v:key . ". " . v:val') 
            call insert(enum_answer, 'Choose a headword for ' . cleanwd . ': ')
            50vsplit _Headwords_
            call append('0', enum_answer)
            redraw
            let choice = input('Select a definition: ')
            let headword = headwords[choice]
            let definition = definitions[choice]
            normal ggdG
            bdelete! _Headwords_
        elseif len(positions) > 0
            let headword = headwords[positions[0]]
            " echo 'headword: ' . headword
            let definition = definitions[positions[0]]
            " echo 'definition: ' . definition
        else
            let headword = headwords[0]
            " echo 'headword: ' . headword
        endif
        if len(definition) == 0
            echo "No definition found for " . cleanwd
            let definition = input('Input definition for ' . cleanwd . ': ')
        endif
        " echo definition
        " line up printing fields
        let pattern = ""
        " echo 'len(word) is ' . len(word)
        " echo 'len(definition) is ' . len(definition)
        if len(word) > len(definition)
            let pattern = '%-' . expand(len(word) + 2) . 's'
        else
            let pattern = '%-' . expand(len(definition) + 2) . 's'
        endif
        " echo 'pattern is ' . pattern
        let topline = topline . printf(pattern, definition)
        " echo 'topline is ' . topline
        let bottomline = bottomline . printf(pattern, word)
        " echo 'bottomline is ' . bottomline
    endfor
    "print to screen
    call setline('.', topline)
    normal o<Esc>
    call setline('.', bottomline)
endfunction
"}}}
    "{{{ GetHeadword
function! GetHeadword(form, dictfile)
    "Error checking
    if len(a:form) == 0
        echo "No word given. Nothing to find headword for! Aborting..."
        return
    endif
    if !filereadable(a:dictfile)
        echo a:dictfile . " is not readable."
        return
    endif
    "Start lookup
    let headwords = systemlist('hermes2.sh ' 
        \ . a:dictfile . ' ' . shellescape(a:form))
    " echo "Headwords: "
    " echo headwords
    if len(headwords) == 0
        let headword = input("Supply headword for " . a:form . ": ", a:form)
    elseif len(headwords) == 1
        let headword = headwords[0]
    else
        let enum_answer = map(copy(headwords), 'v:key . ". " . v:val') 
        call insert(enum_answer, 'Choose a headword for ' . a:form . ': ')
        50vsplit _Headwords_
        call append('0', enum_answer)
        redraw
        let choice = input('Select a definition: [n for none] ')
        normal ggdG
        bdelete! _Headwords_
        if choice == 'n' || choice == ''
            let headword = input("Supply headword for " . a:form . ": ", a:form)
        else
            let headword = headwords[choice]
        endif
    endif
    return headword
endfunction
"}}}
"{{{ TestHeadword
function! TestHeadword()
    let headword = GetHeadword('a', '/home/simon/.config/vim/bundle/vim-amores2/latin-analyses2.txt')
    echo "Headword is : " . headword
endfunction
"}}}
"{{{ GetDefinition
function! GetDefinition(word, dictfile, outputformatter)
    "Error checking
    if len(a:word) == 0
        echo "No word given. Nothing to find headword for! Aborting..."
        return
    endif
    if !filereadable(a:dictfile)
        echo a:dictfile . " is not readable."
        return
    endif
    "Start lookup
    " echo a:word
    let cmd = a:outputformatter . ' ' . a:dictfile . ' ' . shellescape(a:word)
    let definitions = systemlist(cmd)
    " echo "Definitions: "
    " echo definitions
    if len(definitions) == 0
        let definition = input("Supply definition for " . a:word . ": ")
    elseif len(definitions) == 1
        let definition = definitions[0]
    else
        let enum_answer = map(copy(definitions), 'v:key . ". " . v:val') 
        call insert(enum_answer, 'Choose a definition for ' . a:form . ': ')
        50vsplit _Definitions_
        call append('0', enum_answer)
        redraw
        let choice = input('Select a definition: [n for none] ')
        normal ggdG
        bdelete! _Definitions_
        if choice == 'n' || choice == ''
            let definition = input("Supply definition for " . a:form . ": ")
        else
            let definition = definitions[choice]
        endif
    endif
    return definition
endfunction
"}}}
"{{{ TestDefinition
function! TestDefinition()
    let definition = GetDefinition('ab', '/home/simon/.config/vim/bundle/vim-amores2/amores2vocab.txt')
    echo "Definition is: " . definition
endfunction
"}}}
"{{{ SplitAndKeepString
function! SplitAndKeepString(string)
    let spaced = split(a:string, '\s')
    let chunks = []
    for word in range(len(spaced))
        let parts = []
        call add(parts, spaced[word])
        call add(parts, substitute(spaced[word], '\A', '', 'g'))
        call add(chunks,parts)
    endfor
    return chunks
endfunction
"}}}
"{{{ TestSplitAndKeep
function! TestSplitAndKeep()
    let words = SplitAndKeepString('amores?? amores - amores! amores. amores:')
    echo words
endfunction
"}}}
"{{{ LookupAndLabel
function! LookupAndLabel()
    "get indent
    let indent = matchstr(getline('.'), '^\s*')
    let topline = indent
    let bottomline = indent
    "split line into words and punctuation chunks
    let words = SplitAndKeepString(getline('.'))
    "loop through chunks
    for word in words
        " abort loop for punctuation
        " echo "Word is: " . word
        let cleanwd = substitute(word[1], '\(.\{-}\)[^A-z]', '\=submatch(1)', '')
        " echo "Clean word is: " . cleanwd
        if len(cleanwd) != 0
        " get headword
            let headword = GetHeadword(cleanwd,
                \ '/home/simon/.config/vim/bundle/vim-amores2/latin-analyses2.txt')
            " echo "Headword is: " . headword
            " get definition
            let definition = GetDefinition(headword,
                \ '/home/simon/.config/vim/bundle/vim-amores2/amores2vocab.txt', 
                \ 'deffinder.sh')
            let definition = substitute(definition, '^I ', '', '')
            " echo "Definition is: " . definition
        else
            let definition = ''
        endif
        "assemble line
        let pattern = ""
        if len(word[0]) > len(definition)
            let pattern = '%-' . expand(len(word[0]) + 2) . 's'
        else
            let pattern = '%-' . expand(len(definition) + 2) . 's'
        endif
        " echo 'pattern is ' . pattern
        let topline = topline . printf(pattern, definition)
        " echo 'topline is ' . topline
        let bottomline = bottomline . printf(pattern, word[0])
        " echo 'bottomline is ' . bottomline
    endfor
    "print to screen
    call setline('.', topline)
    normal o<Esc>
    call setline('.', bottomline)
endfunction
"}}}
    command! -narg=0 LUL call LookupAndLabel()
"{{{ BirdifyVocab
function! BirdifyVocab()
    "get line
    let this_line = getline('.')
    "split line into words and punctuation chunks
    let words = SplitAndKeepString(getline('.'))
    "loop through chunks
    for word in words
        " abort loop for punctuation
        " echo "Word is: " . word
        let cleanwd = substitute(word[1], '\(.\{-}\)[^A-z]', '\=submatch(1)', '')
        " echo "Clean word is: " . cleanwd
        if len(cleanwd) != 0
        " get headword
            let headword = GetHeadword(cleanwd,
                \ '/home/simon/.config/vim/bundle/vim-amores2/latin-analyses2.txt')
            " echo "Headword is: " . headword
            " get definition
            let definition = GetDefinition(headword,
                \ '/home/simon/.config/vim/bundle/vim-amores2/amores2vocab-shrt.txt', 
                \ 'entryfinder.sh')
            " echo "Definition is: " . definition
        else
            continue
        endif
        "assemble line
        let this_line = this_line . ' ' . definition . ';'
    endfor
    "print to screen
    call setline('.', this_line)
endfunction
"}}}
"{{{ EditBirdRow
function! EditBirdRow()
    let line = getline('.')
    let definitions = substitute(line, '^.\{-}|', '', '')
    35vsplit _Definitions_
    normal! ggdG
    call setline(1, definitions)
    execute 'silent s/^\s*//'
    execute 'silent s/\s*$//'
    execute 'silent s/;\s/;\r/ge'
    " execute 'silent %s/, /\r/ge'
    call setpos('.', [0, 1, 1, 0])
    let &hlsearch = 0
    execute 'nnoremap <buffer> <F12> :RMR<CR>'
endfunction
"}}}
    "{{{ RestoreBirdRow
function! RestoreBirdRow()
    execute 'silent g/^$/d'
    execute 'silent %s/\n/ /ge'
    let line = getline(1)
    execute 'quit!'
    echo 'line is: ' . line
    let vocabline = getline('.')
    echo 'vocabline is: ' . vocabline
    let vocabline = substitute(vocabline, '|.*', '', '')
    echo 'vocabline is: ' . vocabline
    let line = vocabline . '| ' . line
    call setline('.', line)
endfunction
"}}}
"{{{ ReflowBirdLine()
function! ReflowBirdLine()
    let line = getline('.')
    let vocabline = substitute(line, '|.*', '', '')
    let meanings = substitute(line, '^.\{-}|', '', '')
    let meanings = substitute(meanings, ';\s*$', '', '')
    let meaningslist = split(meanings, ';\zs')
    echo meaningslist
    let birdline = ''
    let mct = 0
    let nextmeaning = meaningslist[0]
    while len(expand(birdline . nextmeaning)) < 60
        let birdline = birdline . nextmeaning
        let mct += 1
        let nextmeaning = meaningslist[mct]
    endwhile
    let vocabline = vocabline . '|' . birdline
    call setline('.', vocabline)
    let birdline = printf('%60s', '|')
    while mct < len(meaningslist)
        if len(expand(birdline . nextmeaning)) < 120
            echo 'nextmeaning: ' . nextmeaning
            let birdline = birdline . nextmeaning
            echo 'mct: ' . mct
            echo birdline
            let mct += 1
            if mct < len(meaningslist)
                let nextmeaning = meaningslist[mct]
            endif
        else
            normal o
            call setline('.', birdline)
            let birdline = printf('%60s ', '|')
        endif
    endwhile
    normal o
    call setline('.', birdline)
endfunction
"}}}
"{{{ SplitAndAlignOnCol
function! SplitAndAlignOnCol()
    "For lining up all line numbers in a text
    let line_parts = split(getline('.'), '\s\ze\d')
    let start = substitute(line_parts[0], '\s$', '', 'g')
    let number = join(line_parts[1:], '')
    let new_string = printf("%-59s%s", start, number)
    call setline('.', new_string)
endfunction
"}}}
    command! -narg=0 SAC call SplitAndAlignOnCol()
"{{{ SplitVocabLineIntoSidebar
function! SplitVocabLineIntoSidebar()
    let chosen_match = getline('.')
    let entries = split(chosen_match, ':')
    let headword = entries[0]
    let definitions = split(entries[1], ';')

    35vsplit _Definitions_
    normal! ggdG
    call setline(1, headword . ':')
    call setline(2, definitions)
    execute 'silent 2,$-1s/\n/\r;\r/'
    execute 'silent 2,$s/,/\r/ge'
    call setpos('.', [0, 2, 1, 0])
    let &hlsearch = 0
    execute 'nnoremap <buffer> <F12> :ReEnt<CR>'
    execute 'nnoremap <buffer> <F11> :PrE<CR>'
endfunction
"}}}
    command! -narg=0 SVS call SplitVocabLineIntoSidebar()
"{{{ ReassembleEntry
function! ReassembleEntry()
    let headword = getline(1)
    execute 'silent 2,$s/\w\zs$/,/'
    let definitions = getline(2, '$')
    let entry = headword . join(definitions, '')
    let entry = substitute(entry, ',;', ';', 'g')
    let entry = substitute(entry, ';;', ';', 'g')
    let entry = substitute(entry, ':;', ':', 'g')
    let entry = substitute(entry, ',$', '', '')
    let entry = substitute(entry, ';$', '', '')
    execute 'bdelete! _Definitions_'
    call setline('.', entry)
    let &hlsearch = 1
endfunction
"}}}
    command! -narg=0 ReEnt call ReassembleEntry()
    "{{{ PromoteEntry
function! PromoteEntry()
    let entry = getline('.')
    normal! 2GdG
    call setline(2, entry)
    call ReassembleEntry()
endfunction
"}}}
    command! -narg=0 PrE call PromoteEntry()
    "{{{ CleanFile
function! CleanFile()
    execute 'silent g/^$/d'
    execute 'silent %s/^* //'
    execute 'silent %s/^*//'
    execute "silent %s/^' //"
    execute "silent %s/^'//"
    execute 'silent %s/^` //'
    execute 'silent %s/^`//'
    execute 'silent %s/,\n/, /'
    execute 'silent %s/  / /g'
    execute 'silent %s/ab]/abl/g'
    execute 'silent %s/4- acc/+ acc/'
    execute 'silent %s/4- abl/+ abl/'
    execute 'silent %s/4— acc/+ acc/'
    execute 'silent %s/4— abl/+ abl/'
    execute 'silent %s/—/-/'
    execute 'silent %s/4- /+ /'
    execute 'silent %s/\[ /I /g'
    execute 'silent %s/\] /I /g'
    execute 'silent %s/l /I /g'
    execute 'silent %s/1 /I /g'
    execute 'silent %s/( /(/g'
    execute 'silent %s/ )/)/g'
    execute 'silent %s/111\./m./g'
endfunction
"}}}
"{{{ ShortenDictEntries
function! ShortenDictEntries()
    " shorten domina, dominae f. to domina 1f.
    execute 'silent %s/\(\w\{-}\),\s\1e\s\(f\.\)/\1 1\2/e'
    " shorten amicus, amici m. to amicus 2m.
    execute 'silent %s/\(.\{-}\)us,\s\1i\s\(m\.\)/\1us 2\2/e'
    " shorten puer, pueri m. to puer 2m.
    execute 'silent %s/\(.\{-}\)er,\s\1eri\s\(m\.\)/\1er 2\2/e'
    " shorten donum, doni n. to donum 2n.
    execute 'silent %s/\(.\{-}\)um,\s\1i\s\(n\.\)/\1um 2\2/e'
    " shorten mercator, mercatoris m. to mercator, -is 3m.
    execute 'silent %s/\(\w\{-}\),\s\1is\s\([mfn]\.\)/\1, -is 3\2/e'
    " shorten brevitas, brevitatis f. to brevitas, -atis 3f.
    execute 'silent %s/\(.\{-}\)s,\s\1tis\s\(f\.\)/\1s, -atis 3\2/e'
    " shorten manus, manus f. to manus, -us 4f.
    execute 'silent %s/\(\w\{-}\),\s\1\s\([mfn]\.\)/\1 4\2/e'
    " shorten spes, spei f. to spes 5f.
    execute 'silent %s/\(.\{-}\)es,\s\1ei\s\(f\.\)/\1es 5\2/e'
    " shorten voco, vocare, vocavi, vocatum to voco (1)
    execute 'silent %s/\(.\{-}\)o,\s\1are,\s\1avi,\s\1atum/\1o 1/e'
    " shorten do, dare, dedi, datum to do, (1), dedi, datum
    execute 'silent %s/\(.\{-}\)o,\s\1are,\s\(.*\)/\1o 1, \2/e'
    " shorten moneo, monere, monui, monitum to moneo (2)
    execute 'silent %s/\(.\{-}\)eo,\s\1ere,\s\1ui,\s\1itum/\1eo 2/e'
    " shorten vireo, virere, virui to vireo 2
    execute 'silent %s/\(.\{-}\)eo,\s\1ere\(.*\)/\1eo 2\2/e'
    " shorten audio, audire, audivi, auditum to audio (4)
    execute 'silent %s/\(.\{-}\)io,\s\1ire,\s\1ivi,\s\1itum/\1io 4/e'
    " shorten venio, venire, veni, ventum to venio 4, veni, ventum
    execute 'silent %s/\(.\{-}\)io,\s\1ire\(.*\)/\1io 4\2/e'
    " shorten capio, capere, cepi, captum to capio, (3.5), cepi, captum
    execute 'silent %s/\(.\{-}\)io,\s\1ere,\s\(.*\)/\1io 3.5, \2/e'
    " shorten traho, trahere, traxi, tractum to traho, (3), traxi, tractum
    execute 'silent %s/\(.\{-}\)o,\s\1ere,\s\(.*\)/\1o 3, \2/e'
    " shorten adjective endings
    execute 'silent %s/\(.\{-}\)us,\s\1a,\s\1um/\1us 212/e'
    execute 'silent %s/\(.\{-}\)us,\s-a,\s-um/\1us 212/e'
    " fix legacy (1) (change to 1)
    execute 'silent %s/(\(\d\))/\1/e'
    " remove padding round :
    execute 'silent %s/\s:/:/e'
    " remove I on verbs
    execute 'silent %s/: I/:/e'
endfunction
"}}}
"{{{ MarrySplitLines
function! MarrySplitLines()
    let lines = getline(1, '$')
    let nolines = line('$')/2
    normal ggdG
    for no in range(nolines + 1)
        let newline = lines[no - 1] . ' : ' . lines[no + nolines - 1]
        call setline(no, newline)
    endfor
endfunction
"}}}
"{{{ MarrySelectedLines
function! MarrySelectedLines() range
    let lines = getline(a:firstline, a:lastline)
    normal gvdd
    let nolines = len(lines) / 2
    echo 'number of lines to be added is ' . nolines
    for no in range(nolines)
        let newline = lines[no] . ' : ' . lines[no + nolines]
        call setline(a:firstline + no, newline)
        call append(a:firstline + no, '')
    endfor
endfunction
"}}}
    command! -narg=0 -range MSL call MarrySelectedLines(<firstline>, <lastline>)
    vnoremap <F5>  :call MarrySelectedLines()<CR>
    "{{{ CorrectDiacritics
function! CorrectDiacritics()
    execute 'silent %s/ä/ā/g'
    execute 'silent %s/ë/ē/g'
    execute 'silent %s/ï/ī/g'
    execute 'silent %s/ö/ō/g'
    execute 'silent %s/ü/ū/g'
endfunction
"}}}
"{{{ ProcessFile
function! ProcessFile()
    call CleanFile()
    call CorrectDiacritics()
    call RemoveDiacritics()
endfunction
"}}}
    command! -narg=0 PrFile call ProcessFile()
    "{{{ AddDeDiacriticedEntry
function! AddDeDiacriticedEntry()
    let lines = getline(1, '$')
    let linenum = 1
    for line in lines
        let parts = split(line, ':')
        let entry = parts[0]
        let entry = substitute(entry, 'ā', 'a', 'g')
        let entry = substitute(entry, 'ē', 'e', 'g')
        let entry = substitute(entry, 'ī', 'i', 'g')
        let entry = substitute(entry, 'ō', 'o', 'g')
        let entry = substitute(entry, 'ū', 'u', 'g')
        call setline(linenum, entry . '- ' .line)
        let linenum += 1
    endfor
endfunction
"}}}
"{{{ RemoveDiacritics
function! RemoveDiacritics()
    execute 'silent %s/ā/a/g'
    execute 'silent %s/ē/e/g'
    execute 'silent %s/ī/i/g'
    execute 'silent %s/ō/o/g'
    execute 'silent %s/ū/u/g'
endfunction
"}}}
"{{{ RemoveCommonWords
function! RemoveCommonWords()
    execute 'silent %s/- ubi .*\n//g'
    execute 'silent %s/- in .*\n//g'
    execute 'silent %s/- a .*\n//g'
    execute 'silent %s/- ad .*\n//g'
    execute 'silent %s/- ab .*\n//g'
    execute 'silent %s/- e .*\n//g'
    execute 'silent %s/- ex .*\n//g'
    execute 'silent %s/- cum .*\n//g'
    execute 'silent %s/- et .*\n//g'
    execute 'silent %s/- nec .*\n//g'
    execute 'silent %s/- equus,.*\n//g'
    execute 'silent %s/- rex,.*\n//g'
    execute 'silent %s/- vox,.*\n//g'
    execute 'silent %s/- qui,.*\n//g'
    execute 'silent %s/- enim .*\n//g'
    execute 'silent %s/- ut .*\n//g'
    execute 'silent %s/- ne .*\n//g'
    execute 'silent %s/- mons,.*\n//g'
    execute 'silent %s/- hic,.*\n//g'
    execute 'silent %s/- ille,.*\n//g'
    execute 'silent %s/- romanus,.*\n//g'
    execute 'silent %s/- pugna,.*\n//g'
    execute 'silent %s/- dux,.*\n//g'
    execute 'silent %s/- castro,.*\n//g'
    execute 'silent %s/- castra,.*\n//g'
    execute 'silent %s/- audio,.*\n//g'
    execute 'silent %s/- dico,.*\n//g'
    execute 'silent %s/- edo,.*\n//g'
    execute 'silent %s/- castrum,.*\n//g'
    execute 'silent %s/- Non\.,.*\n//g'
    execute 'silent %s/- alium,.*\n//g'
    execute 'silent %s/- Indus,.*\n//g'
    execute 'silent %s/- antis,.*\n//g'
    execute 'silent %s/- armum,.*\n//g'
    execute 'silent %s/- armo,.*\n//g'
    execute 'silent %s/- A\.,.*\n//g'
    execute 'silent %s/- C\.,.*\n//g'
    execute 'silent %s/- L\.,.*\n//g'
    execute 'silent %s/- Ti\.,.*\n//g'
    execute 'silent %s/- Gaius,.*\n//g'
endfunction
"}}}
"{{{ EditMeaningsRow
function! EditMeaningsRow()
    let line = getline('.')

    35vsplit _Definitions_
    normal! ggdG
    call setline(1, line)
    execute 'silent s/^\s*//'
    execute 'silent s/\s*$//'
    execute 'silent s/\s\s\s*/\r\r\r/ge'
    execute 'silent %s/, /\r/ge'
    call setpos('.', [0, 2, 1, 0])
    let &hlsearch = 0
    execute 'nnoremap <buffer> <F12> :RMR<CR>'
endfunction
"}}}
    command! -narg=0 EMR call EditMeaningsRow()
    "{{{ ReturnMeaningsRow
function! ReturnMeaningsRow()
    execute 'silent %s/\n\n\n/  /ge'
    execute 'silent %s/\S\zs\n\ze\S/, /ge'
    let line = getline(1)
    execute 'quit!'
    let line = expand(matchstr(getline('.'), '^\s*')) . line
    call setline('.', line)
    normal Vjgv
    execute "'<,'>RWC"
    normal! 0
endfunction
"}}}
    command! -nargs=0 RMR call ReturnMeaningsRow()
    "{{{ RealignWordColumns
function! RealignWordColumns(line1, line2, count) range
    if a:count == -1
        echo "Please visually select lines"
        return
    endif
    let indent = matchstr(getline(a:line1), '^\s*')
    let topline = indent
    let bottomline = indent
    let topline_parts = split(getline(a:line1), '\s\s\s*')
    " echo len(topline_parts)
    " echo topline_parts
    let bottomline_parts = split(getline(a:line2), '\s\s\s*')
    " echo bottomline_parts
    " echo len(bottomline_parts)
    let tpct = 0
    for part in range(len(bottomline_parts))
        if len(substitute(bottomline_parts[part], '[^A-z]', '', 'g')) == 0
            let pattern = '%-' . expand(len(bottomline_parts[part]) + 2) . 's'
            let topline = topline . printf(pattern, '')
            let bottomline = bottomline . printf(pattern, bottomline_parts[part])
        elseif len(topline_parts[tpct]) > len(bottomline_parts[part])
            let pattern = '%-' . expand(len(topline_parts[tpct]) + 2) . 's'
            let topline = topline . printf(pattern, topline_parts[tpct])
            let bottomline = bottomline . printf(pattern, bottomline_parts[part])
            let tpct += 1
        else
            let pattern = '%-' . expand(len(bottomline_parts[part]) + 2) . 's'
            let topline = topline . printf(pattern, topline_parts[tpct])
            let bottomline = bottomline . printf(pattern, bottomline_parts[part])
            let tpct += 1
        endif
    endfor
    call setline(a:line1, topline)
    call setline(a:line2, bottomline)
    call setpos('.', [0, a:line1, 1, 0])
endfunction
"}}}
    command! -nargs=0 -range RWC call RealignWordColumns(<line1>, <line2>, <count>)
