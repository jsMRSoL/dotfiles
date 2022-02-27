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

function sp_funcs.reset_better()
  local mod_name = vim.fn.expand("%:t:r")
  require("plenary.reload").reload_module(mod_name)
end

function sp_funcs.test()
  print("This works!")
end

function sp_funcs.test2()
  vim.cmd('echo "Test function was called."')
end

function sp_funcs.test3()
  vim.api.nvim_echo({ "This is a test." }, false, nil)
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
    newlines[i] = lines[i] .. " " .. lines[i + total]
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
    newlines[i] = i .. "./ " .. lines[i]
  end
  vim.api.nvim_buf_set_lines(0, line1 - 1, line2, nil, newlines)
end

function sp_funcs.number_lines2()
  local line1 = vim.fn.line("v")
  local line2 = vim.fn.line(".")
  local count = 1 + line2 - line1
  local lines = vim.api.nvim_buf_get_lines(0, line1 - 1, line2, nil)
  local total = count
  local newlines = {}
  for i = 1, total, 1 do
    newlines[i] = i .. "./ " .. lines[i]
  end
  vim.api.nvim_buf_set_lines(0, line1 - 1, line2, nil, newlines)
  sp_funcs.collapse_selection()
end

function sp_funcs.collapse_selection()
  local key = vim.api.nvim_replace_termcodes("<esc>", true, false, true)
  vim.api.nvim_feedkeys(key, "v", true)
end

function sp_funcs.clear_echo_area()
  vim.api.nvim_feedkeys(":", "nx", true)
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
  -- shorten cvim.vim.vim.apio, capere, cepi, captum to capio, (3.5), cepi, captum
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

sp_funcs.align_on_char = function(line1, line2)
  local lines = vim.api.nvim_buf_get_lines(0, line1 - 1, line2, nil)
  local count = 1 + line2 - line1
  local char = vim.fn.input("Enter character to align on: ")
  local newlines = {}
  local longest = 0
  for i = 1, count, 1 do
    if lines[i]:find(char) then
      P(lines[i])
      newlines[i] = vim.fn.split(lines[i], char)
      if #newlines[i][1] > longest then
        longest = #newlines[i][1]
      end
    else
      newlines[i] = lines[i]
    end
  end
  local fmt = "%-" .. longest .. "s"
  for i = 1, count, 1 do
    if #newlines[i] > 1 then
      local first = vim.fn.printf(fmt, newlines[i][1])
      newlines[i] = first .. char .. newlines[i][2]
    end
  end
  vim.api.nvim_buf_set_lines(0, line1 - 1, line2, nil, newlines)
end

-- This works with calling from which-key, whereas align_on_char doesn't!?
sp_funcs.align_on_char2 = function()
  local line1 = vim.fn.line("v")
  local line2 = vim.fn.line(".")
  sp_funcs.align_on_char(line1, line2)
  sp_funcs.collapse_selection()
  sp_funcs.clear_echo_area()
end

sp_funcs.test_line_nr = function()
  local line = vim.api.nvim_buf_get_lines(0, 0, 2, nil)
  P(line)
end

sp_funcs.test_split = function()
  local lines = vim.api.nvim_buf_get_lines(0, 0, 4, nil)
  P(lines)
  local parts = {}
  for i, line in ipairs(lines) do
    if line:find(":") then
      parts[i] = vim.fn.split(line, ":")
    else
      parts[i] = line
    end
  end
  P(parts)
  for i, entry in ipairs(parts) do
    if #entry > 1 then
      parts[i] = parts[i][1] .. " + " .. parts[i][2]
    end
  end
  vim.api.nvim_buf_set_lines(0, 0, 4, nil, parts)
end

sp_funcs.test_mode = function()
  local rng = ""
  if vim.fn.mode() == "V" then
    local line1 = vim.fn.line("v")
    local line2 = vim.fn.line(".")
    rng = line1 .. "," .. line2
    print(rng)
  end
  print("Hello")
end

sp_funcs.keep_lines = function()
  local rng = ""
  if vim.fn.mode() == "V" then
    local line1 = vim.fn.line("v")
    local line2 = vim.fn.line(".")
    rng = line1 .. "," .. line2
    -- print(rng)
  end
  local regexp = vim.fn.input("Exclude lines on regexp: ")
  vim.cmd("execute " .. "'silent " .. rng .. "vglobal/" .. regexp .. "/d'")
  vim.opt.hlsearch = false
end

sp_funcs.flush_lines = function()
  local rng = ""
  if vim.fn.mode() == "V" then
    local line1 = vim.fn.line("v")
    local line2 = vim.fn.line(".")
    rng = line1 .. "," .. line2
    -- print(rng)
  end
  local regexp = vim.fn.input("Exclude lines on regexp: ")
  vim.cmd("execute " .. "'silent " .. rng .. "global/" .. regexp .. "/d'")
  vim.opt.hlsearch = false
end

sp_funcs.flush_empty_lines = function()
  local rng = ""
  if vim.fn.mode() == "V" then
    local line1 = vim.fn.line("v")
    local line2 = vim.fn.line(".")
    rng = line1 .. "," .. line2
  end
  vim.cmd("execute " .. "'silent " .. rng .. "global/^$/d'")
  vim.opt.hlsearch = false
end

sp_funcs.buffer_to_list = function()
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, nil)
  local tokens = {}
  local words = {}
  for _, line in pairs(lines) do
    line = vim.fn.substitute(line, "[\"':,;\\.!\\?`]", "", "g")
    -- P(line)
    words = vim.fn.split(line, " ")
    for _, word in pairs(words) do
      if #word > 0 then
        table.insert(tokens, word)
      end
    end
  end
  tokens = vim.fn.sort(tokens)
  tokens = vim.fn.uniq(tokens)
  -- P(tokens)
  return tokens
end

local win
sp_funcs.test_floating_win = function()
  local tokens = sp_funcs.buffer_to_list()
  local longest = 0
  for _, word in pairs(tokens) do
    if #word > longest then
      longest = #word
    end
  end

  -- set up buffer for floating window
  local new_buf = vim.api.nvim_create_buf(false, true) -- create new empty buffer
  vim.api.nvim_buf_set_option(new_buf, "bufhidden", "wipe")

  -- get cursor position
  local pos = vim.api.nvim_win_get_cursor(0)
  local line, col = pos[1], pos[2]
  -- P(line)
  -- P(col)
  -- calculate our floating window size
  local win_height = #tokens
  local win_width = longest + 2

  -- set some options
  local opts = {
    style = "minimal",
    relative = "editor",
    width = win_width,
    height = win_height,
    border = "single",
    -- bufpos = { line, col + 5},
    row = line,
    col = col,
  }

  -- and finally create it with buffer attached
  win = vim.api.nvim_open_win(new_buf, true, opts)
  vim.api.nvim_buf_set_lines(new_buf, 0, -1, false, tokens)
  -- customize highlighting
  vim.api.nvim_win_set_option(win, "winhl", "NormalFloat:MyNormalFloat")
end

sp_funcs.close_window = function()
  vim.api.nvim_win_close(win, true)
end

sp_funcs.test_return_values = function()
  local s, e = string.find("tiger", "ige")
  print(s .. " " .. e)
  P(string.find("tiger", "ige"))
end

sp_funcs.popup_diacritic_words = function()
  local mappings = {
    -- ['['] = 'update_view(-1)',
    ["<cr>"] = 'yiw:q<CR>"_xP',
  }
  vim.cmd("normal yl")
  local cur_wrd = vim.fn.getreg('"')
  P(cur_wrd)
  local filtered = {}
  local tokens = sp_funcs.get_diacritic_words()
  for _, entry in ipairs(tokens) do
    if entry:find(cur_wrd) then
      local head = vim.fn.split(entry, " #")[1]
      table.insert(filtered, head)
    end
  end
  sp_funcs.floating_win(filtered, mappings)
end

sp_funcs.floating_win = function(tokens, keymap)
  -- local tokens = sp_funcs.buffer_to_list()
  local longest = 0
  for _, word in pairs(tokens) do
    if #word > longest then
      longest = #word
    end
  end

  -- set up buffer for floating window
  local new_buf = vim.api.nvim_create_buf(false, true) -- create new empty buffer
  vim.api.nvim_buf_set_option(new_buf, "bufhidden", "wipe")

  -- set up buffer local mappings
  for k, v in pairs(keymap) do
    vim.api.nvim_buf_set_keymap(new_buf, "n", k, v, {
      nowait = true,
      noremap = true,
      silent = true,
    })
  end
  -- get cursor position
  local pos = vim.api.nvim_win_get_cursor(0)
  local line, col = pos[1], pos[2]
  -- P(line)
  -- P(col)
  -- calculate our floating window size
  local win_height = #tokens
  local win_width = longest + 2

  -- set some options
  local opts = {
    style = "minimal",
    relative = "editor",
    width = win_width,
    height = win_height,
    border = "single",
    -- bufpos = { line, col + 5},
    row = line,
    col = col,
  }

  -- and finally create it with buffer attached
  win = vim.api.nvim_open_win(new_buf, true, opts)
  vim.api.nvim_buf_set_lines(new_buf, 0, -1, false, tokens)
  -- customize highlighting
  vim.api.nvim_win_set_option(win, "winhl", "NormalFloat:MyNormalFloat")
end

sp_funcs.get_diacritic_words = function()
  local tokens = sp_funcs.read_array(
    "/home/simon/.config/nvim/lua/common_greek.txt"
  )
  -- P(tokens)
  return tokens
end

sp_funcs.read_array = function(file)
  local arr = {}
  local handle = assert(io.open(file, "r"))
  local value = handle:read("*line")
  while value do
    table.insert(arr, value)
    value = handle:read("*line")
  end
  handle:close()
  return arr
end

sp_funcs.get_char_under_cursor = function()
  local line = vim.api.nvim_get_current_line()
  local pos = vim.api.nvim_win_get_cursor(0)
  local idx = pos[2] + 1
  -- P(line:sub(idx, idx))
  P(line)
  print(#line)
  return line:sub(idx, idx)
end

sp_funcs.get_visual_selection = function()
  local line = vim.api.nvim_get_current_line()
  local pos1 = vim.api.nvim_buf_get_mark(0, "<")[2] + 1
  local pos2 = vim.api.nvim_buf_get_mark(0, ">")[2] + 1
  -- P(pos1)
  -- P(pos2)
  local substr = line:sub(pos1, pos2)
  print(substr)
end

sp_funcs.get_visual_selection2 = function()
  -- local old_reg_value =
end

sp_funcs.test_assignment = function()
  local row, col
  row, col = vim.api.nvim_buf_get_mark(0, "a")
  P(row)
  P(col)
end

sp_funcs.filter_list = function()
  local tokens = sp_funcs.get_diacritic_words()
  vim.cmd("normal yl")
  local cur_wrd = vim.fn.getreg('"')
  -- if vim.fn.mode() == 'v' then
  --   cur_wrd = sp_funcs.get_visual_selection()
  -- else
  --   cur_wrd = sp_funcs.get_char_under_cursor()
  -- end
  P(cur_wrd)
  -- ᾶ
  local filtered = {}
  for _, entry in ipairs(tokens) do
    if entry:find(cur_wrd) then
      local head = vim.fn.split(entry, " #")[1]
      table.insert(filtered, head)
    end
  end

  P(filtered)
end

return sp_funcs
