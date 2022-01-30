-- TIPS
-- # To cache global options
-- local orig_hls_setting = vim.api.nvim_get_option('hlsearch')
-- # Printing booleans
-- print("orig_hls_setting :" .. tostring(orig_hls_setting))
--
local sp_funcs = {}

function sp_funcs.reset()
  for k in pairs(package.loaded) do
    if k:match("sp%-functions") then
      package.loaded[k] = nil
    end
  end
end

function sp_funcs.test()
  print("This works!")
end

function sp_funcs.test2()
  vim.cmd('echo "Test function was called."')
end

function sp_funcs.test3()
  vim.api.nvim_echo({"This is a test."}, false, nil)
end

function sp_funcs.join_lines(line1, line2)
  local count = 1 + line2 - line1
  if count % 2 ~= 0 then
    print("You must select an even number of lines.")
    return
  end
  local lines = vim.api.nvim_buf_get_lines(0, line1 - 1, line2, nil)
  local total = count / 2
  local newlines = {}
  for i = 1, total, 1 do
    newlines[i] = lines[i].." "..lines[i + total]
  end
  vim.api.nvim_buf_set_lines(0, line1 - 1, line2, nil, newlines)
end

function sp_funcs.eval_line()
  local line = vim.api.nvim_get_current_line()
  vim.cmd("lua " .. line)
end

function sp_funcs.number_lines(line1, line2)
  local count = 1 + line2 - line1
  local lines = vim.api.nvim_buf_get_lines(0, line1 - 1, line2, nil)
  local total = count
  local newlines = {}
  for i = 1, total, 1 do
    newlines[i] = i.."./ "..lines[i]
  end
  vim.api.nvim_buf_set_lines(0, line1 - 1, line2, nil, newlines)
end

function sp_funcs.shorten_dictionary_entries()
  -- shorten domina, dominae f. to domina 1f.
  vim.cmd([[silent %s/\(\w\{-}\),\s\1e\s\(f\.\)/\1 1\2/e]])
  -- shorten amicus, amici m. to amicus 2m.
  vim.cmd([[silent %s/\(.\{-}\)us,\s\1i\s\(m\.\)/\1us 2\2/e]])
  -- shorten puer, pueri m. to puer 2m.
  vim.cmd([[silent %s/\(.\{-}\)er,\s\1eri\s\(m\.\)/\1er 2\2/e]])
  -- shorten donum, doni n. to donum 2n.
  vim.cmd([[silent %s/\(.\{-}\)um,\s\1i\s\(n\.\)/\1um 2\2/e]])
  -- shorten mercator, mercatoris m. to mercator, -is 3m.
  vim.cmd([[silent %s/\(\w\{-}\),\s\1is\s\([mfn]\.\)/\1, -is 3\2/e]])
  -- shorten brevitas, brevitatis f. to brevitas, -atis 3f.
  vim.cmd([[silent %s/\(.\{-}\)s,\s\1tis\s\(f\.\)/\1s, -atis 3\2/e]])
  -- shorten manus, manus f. to manus, -us 4f.
  vim.cmd([[silent %s/\(\w\{-}\),\s\1\s\([mfn]\.\)/\1 4\2/e]])
  -- shorten spes, spei f. to spes 5f.
  vim.cmd([[silent %s/\(.\{-}\)es,\s\1ei\s\(f\.\)/\1es 5\2/e]])
  -- shorten voco, vocare, vocavi, vocatum to voco (1)
  vim.cmd([[silent %s/\(.\{-}\)o,\s\1are,\s\1avi,\s\1atu[ms]/\1o 1/e]])
  -- shorten do, dare, dedi, datum to do, (1), dedi, datum
  vim.cmd([[silent %s/\(.\{-}\)o,\s\1are,\s\(.*\)/\1o 1, \2/e]])
  -- shorten moneo, monere, monui, monitum to moneo (2)
  vim.cmd([[silent %s/\(.\{-}\)eo,\s\1ere,\s\1ui,\s\1itu[ms]/\1eo 2/e]])
  -- shorten vireo, virere, virui to vireo 2
  vim.cmd([[silent %s/\(.\{-}\)eo,\s\1ere\(.*\)/\1eo 2\2/e]])
  -- shorten audio, audire, audivi, auditum to audio (4)
  vim.cmd([[silent %s/\(.\{-}\)io,\s\1ire,\s\1ivi,\s\1itu[ms]/\1io 4/e]])
  -- shorten venio, venire, veni, ventum to venio 4, veni, ventum
  vim.cmd([[silent %s/\(.\{-}\)io,\s\1ire\(.*\)/\1io 4\2/e]])
  -- shorten capio, capere, cepi, captum to capio, (3.5), cepi, captum
  vim.cmd([[silent %s/\(.\{-}\)io,\s\1ere,\s\(.*\)/\1io 3.5, \2/e]])
  -- shorten traho, trahere, traxi, tractum to traho, (3), traxi, tractum
  vim.cmd([[silent %s/\(.\{-}\)o,\s\1ere,\s\(.*\)/\1o 3, \2/e]])
  -- shorten adjective endings
  vim.cmd([[silent %s/\(.\{-}\)us,\s\1a,\s\1um/\1us 212/e]])
  vim.cmd([[silent %s/\(.\{-}\)us,\s-a,\s-um/\1us 212/e]])
  -- fix legacy (1) (change to 1)
  vim.cmd([[silent %s/(\(\d\))/\1/e]])
  -- remove padding round :
  vim.cmd([[silent %s/\s:/:/e]])
  -- remove I on verbs
  vim.cmd([[silent %s/: I/:/e]])
end

function sp_funcs.clean_file()
    vim.cmd([[silent g/^$/d]])
    vim.cmd([[silent %s/^* //e]])
    vim.cmd([[silent %s/^*//e]])
    vim.cmd([[silent %s/^' //e]])
    vim.cmd([[silent %s/^'//e]])
    vim.cmd([[silent %s/^` //e]])
    vim.cmd([[silent %s/^`//e]])
    vim.cmd([[silent %s/,\n/, /e]])
    vim.cmd([[silent %s/  / /ge]])
    vim.cmd([[silent %s/ab]/abl/ge]])
    vim.cmd([[silent %s/4- acc/+ acc/e]])
    vim.cmd([[silent %s/4- abl/+ abl/e]])
    vim.cmd([[silent %s/4— acc/+ acc/e]])
    vim.cmd([[silent %s/4— abl/+ abl/e]])
    vim.cmd([[silent %s/—/-/e]])
    vim.cmd([[silent %s/4- /+ /e]])
    vim.cmd([[silent %s/\[ /I /ge]])
    vim.cmd([[silent %s/\] /I /ge]])
    vim.cmd([[silent %s/l /I /ge]])
    vim.cmd([[silent %s/1 /I /ge]])
    vim.cmd([[silent %s/( /(/ge]])
    vim.cmd([[silent %s/ )/)/ge]])
    vim.cmd([[silent %s/111\./m./ge]])
end

function sp_funcs.correct_diacritics()
    vim.cmd([[silent %s/ä/ā/ge]])
    vim.cmd([[silent %s/ë/ē/ge]])
    vim.cmd([[silent %s/ï/ī/ge]])
    vim.cmd([[silent %s/ö/ō/ge]])
    vim.cmd([[silent %s/ü/ū/ge]])
end

function sp_funcs.process_file()
    sp_funcs.clean_file()
    sp_funcs.correct_diacritics()
    sp_funcs.remove_diacritics()
end

-- function! AddDeDiacriticedEntry()
--     let lines = getline(1, '$')
--     let linenum = 1
--     for line in lines
--         let parts = split(line, ':')
--         let entry = parts[0]
--         let entry = substitute(entry, 'ā', 'a', 'g')
--         let entry = substitute(entry, 'ē', 'e', 'g')
--         let entry = substitute(entry, 'ī', 'i', 'g')
--         let entry = substitute(entry, 'ō', 'o', 'g')
--         let entry = substitute(entry, 'ū', 'u', 'g')
--         call setline(linenum, entry . '- ' .line)
--         let linenum += 1
--     end
-- end

function sp_funcs.remove_diacritics()
    vim.cmd([[silent %s/ā/a/ge]])
    vim.cmd([[silent %s/ē/e/ge]])
    vim.cmd([[silent %s/ī/i/ge]])
    vim.cmd([[silent %s/ō/o/ge]])
    vim.cmd([[silent %s/ū/u/ge]])
end

function sp_funcs.remove_common_words()
    vim.cmd([[silent %s/- ubi .*\n//ge]])
    vim.cmd([[silent %s/- in .*\n//ge]])
    vim.cmd([[silent %s/- a .*\n//ge]])
    vim.cmd([[silent %s/- ad .*\n//ge]])
    vim.cmd([[silent %s/- ab .*\n//ge]])
    vim.cmd([[silent %s/- e .*\n//ge]])
    vim.cmd([[silent %s/- ex .*\n//ge]])
    vim.cmd([[silent %s/- cum .*\n//ge]])
    vim.cmd([[silent %s/- et .*\n//ge]])
    vim.cmd([[silent %s/- nec .*\n//ge]])
    vim.cmd([[silent %s/- equus,.*\n//ge]])
    vim.cmd([[silent %s/- rex,.*\n//ge]])
    vim.cmd([[silent %s/- vox,.*\n//ge]])
    vim.cmd([[silent %s/- qui,.*\n//ge]])
    vim.cmd([[silent %s/- enim .*\n//ge]])
    vim.cmd([[silent %s/- ut .*\n//ge]])
    vim.cmd([[silent %s/- ne .*\n//ge]])
    vim.cmd([[silent %s/- mons,.*\n//ge]])
    vim.cmd([[silent %s/- hic,.*\n//ge]])
    vim.cmd([[silent %s/- ille,.*\n//ge]])
    vim.cmd([[silent %s/- romanus,.*\n//ge]])
    vim.cmd([[silent %s/- pugna,.*\n//ge]])
    vim.cmd([[silent %s/- dux,.*\n//ge]])
    vim.cmd([[silent %s/- castro,.*\n//ge]])
    vim.cmd([[silent %s/- castra,.*\n//ge]])
    vim.cmd([[silent %s/- audio,.*\n//ge]])
    vim.cmd([[silent %s/- dico,.*\n//ge]])
    vim.cmd([[silent %s/- edo,.*\n//ge]])
    vim.cmd([[silent %s/- castrum,.*\n//ge]])
    vim.cmd([[silent %s/- Non\.,.*\n//ge]])
    vim.cmd([[silent %s/- alium,.*\n//ge]])
    vim.cmd([[silent %s/- Indus,.*\n//ge]])
    vim.cmd([[silent %s/- antis,.*\n//ge]])
    vim.cmd([[silent %s/- armum,.*\n//ge]])
    vim.cmd([[silent %s/- armo,.*\n//ge]])
    vim.cmd([[silent %s/- A\.,.*\n//ge]])
    vim.cmd([[silent %s/- C\.,.*\n//ge]])
    vim.cmd([[silent %s/- L\.,.*\n//ge]])
    vim.cmd([[silent %s/- Ti\.,.*\n//ge]])
    vim.cmd([[silent %s/- Gaius,.*\n//ge]])
end

return sp_funcs
