" function to extract urls from html
function! ExtractUrls ()
    execute 'silent g/^$/d'
    execute 'silent v/url="https.*\.\(pdf\|doc\|ppt\)/d'
    execute 'silent %s/^\s*.*url="\(https.*\.\(pdf\|docx\|pptx\)\)"/\1/'
endfunction
