local text_win, dict_win, vocab_win
local text_buf, dict_buf, vocab_buf
local layout_set = false
local json_ok, lunajson = pcall(require, "lunajson")
if not json_ok then
  print("Could not load lunajson")
  return
end
local api = vim.api

-- local test_json = '{\"lns\": [{\"head\":\"dominus\",\"orth_orig\":\"dŏmĭnus\",\"early_i_tags\":[\"a master\",\"possessor\",\"ruler\",\"lord\",\"proprietor\",\"owner\"],\"senses\":[{\"pos\":null,\"authors\":[\"Cato\",\"Cic.\",\"Hor.\",\"Non.\",\"Plaut.\",\"Quint.\",\"Sall.\",\"Suet.\",\"Ter.\",\"Varr.\",\"id.\"],\"i_tags\":[\"the young master\"]},{\"pos\":null,\"authors\":[\"Cic.\",\"Ov.\",\"Verg.\",\"id.\"],\"i_tags\":[\"a master\",\"lord\",\"ruler\",\"commander\",\"chief\",\"proprietor\",\"owner\",\"fin.\",\"the possessor of an art\"]},{\"pos\":\"adj.\",\"authors\":[\"Juv.\",\"Ov.\",\"Stat.\"],\"i_tags\":[\"<i>the auction spear\"]},{\"pos\":null,\"authors\":[],\"i_tags\":[]},{\"pos\":null,\"authors\":[\"Cic.\",\"Gell.\",\"Liv.\",\"Non.\"],\"i_tags\":[\"the master of a feast\",\"the entertainer\",\"host\"]},{\"pos\":null,\"authors\":[\"Cic.\",\"Plaut.\"],\"i_tags\":[\"The master of a play\",\"of public games; the employer\"]},{\"pos\":null,\"authors\":[\"Inscr. Orell.\",\"Mart.\",\"Phaedr.\",\"Suet.\",\"Tib.\"],\"i_tags\":[\"a title of the emperors\"]},{\"pos\":null,\"authors\":[\"Ov.\"],\"i_tags\":[\"A term of endearment in addressing a lover\"]},{\"pos\":null,\"authors\":[\"Mart.\",\"Sen.\",\"Suet.\"],\"i_tags\":[\"Sir\"]},{\"pos\":null,\"authors\":[\"Cic.\"],\"i_tags\":[\"A master\",\"assignee\"]},{\"pos\":null,\"authors\":[\"Oros.\",\"Vulg.\"],\"i_tags\":[\"the Lord\",\"fin.\"]}]}], \"gcse\": [[\"dominus, domini, m\",\"noun 2\",\"master\"]], \"clc\": [[\"dominus, domini, m.\",\"master\"]], \"asvocab\": [[\"dominus, domini, m\",\"noun 2\",\"master\"]], \"wwords\": [[\"dominus, domini\",\"NOUN\",\"2M\",\"owner, lord, master; the Lord; title for ecclesiastics/gentlemen;\"]]}'

local latin_funcs = {}

function latin_funcs.lookup(on_headword)
  if layout_set == false then
    latin_funcs.create_layout()
    -- else
    --   print("Layout has been set")
  end

  -- dominos
  local entries_json = latin_funcs.get_entries(on_headword)

  entries_json = vim.fn.substitute(entries_json, "\n", "", "g")
  -- print(dict_buf)
  -- api.nvim_buf_set_lines(dict_buf, 0, -1, true, { entries_json })
  -- clear buffer and add program output
  local entries_formatted = latin_funcs.extract_and_format(entries_json)
  api.nvim_buf_set_lines(dict_buf, 0, -1, true, entries_formatted)
end

function latin_funcs.get_entries(on_headword)
  -- on_headword = on_headword or false
  -- get word under cursor using <cword>
  local current_word
  local entries
  local cmd
  if on_headword then
    current_word = vim.fn.input("Enter headword: ")
    cmd = "/home/simon/Projects/rust/latin_dictionary/target/debug/query_headwords"
      .. " "
      .. current_word
  else
    current_word = vim.fn.expand("<cword>")
    cmd = "/home/simon/Projects/rust/latin_dictionary/target/debug/query"
      .. " "
      .. current_word
  end
  -- print(current_word)
  -- call external program
  entries = vim.fn.system(cmd)
  return entries
end

function latin_funcs.get_line_entries(opts)
  local line = vim.api.nvim_get_current_line()
  line = vim.fn.substitute(line, "[;:!,\\.]", "", "g")

  local level
  if ( opts.level == "gcse") then
    level = "--gcse"
  else
    level = "--asvocab"
  end

  local cmd = "/home/simon/Projects/rust/latin_dictionary/target/debug/query_many"
    .. " "
    .. level
    .. " "
    .. line
  local entries_json_string = vim.fn.system(cmd)

  local entries_json = lunajson.decode(entries_json_string)

  local terms = vim.fn.split(line)
  local lines = {}

  for _, term in pairs(terms) do
    local json_entry = entries_json[term]
    if #json_entry == 0 then
      table.insert(lines, "- " .. term .. " : NOT FOUND")
    else
      for _, entry in pairs(json_entry) do
        table.insert(
          lines,
          "- " .. entry[1] .. " [" .. entry[2] .. "] : " .. entry[3]
        )
      end
    end
  end

  local line_nr, _ = unpack(vim.api.nvim_win_get_cursor(0))
  -- P(line_nr)
  api.nvim_buf_set_lines(0, line_nr, line_nr, true, lines)
end

function latin_funcs.create_layout()
  text_win = api.nvim_get_current_win()
  text_buf = api.nvim_get_current_buf()
  vim.api.nvim_buf_set_keymap(
    text_buf,
    "n",
    "<CR>",
    ':lua require("latin-dictionary").lookup()' .. "<cr>",
    { nowait = true, noremap = true, silent = true }
  )
  vim.api.nvim_buf_set_keymap(
    text_buf,
    "n",
    "<s-CR>",
    ':lua require("latin-dictionary").lookup(true)' .. "<cr>",
    { nowait = true, noremap = true, silent = true }
  )

  api.nvim_command("50vnew")
  -- dict_win = api.nvim_get_current_win()
  dict_buf = api.nvim_get_current_buf()
  vim.api.nvim_buf_set_name(0, "Dictionary Entries")
  vim.api.nvim_buf_set_keymap(
    dict_buf,
    "n",
    "<CR>",
    ':lua require("latin-dictionary").add_to_vocab()' .. "<cr>",
    { nowait = true, noremap = true, silent = true }
  )
  vim.api.nvim_buf_set_keymap(
    dict_buf,
    "n",
    "N",
    ':lua require("latin-dictionary").next_entry()' .. "<cr>",
    { nowait = true, noremap = true, silent = true }
  )

  api.nvim_command("new")
  vocab_win = api.nvim_get_current_win()
  vocab_buf = api.nvim_win_get_buf(vocab_win)
  vim.api.nvim_buf_set_name(0, "Vocabulary list")
  api.nvim_set_current_win(text_win)

  layout_set = true
end

function latin_funcs.extract_and_format(raw_json)
  local lines = {}
  local json = lunajson.decode(raw_json)
  -- P(json)
  local gcse = json["gcse"]
  local asvocab = json["asvocab"]
  local wwords = json["wwords"]
  local lns = json["lns"]
  -- P(wwords)
  if #gcse > 0 then
    table.insert(lines, latin_funcs.format_gcse_as_entries(gcse, "GCSE"))
  end
  if #asvocab > 0 then
    table.insert(lines, latin_funcs.format_gcse_as_entries(asvocab, "ASVOCAB"))
  end
  if #wwords > 0 then
    table.insert(lines, latin_funcs.format_wwords_entries(wwords, "WWORDS"))
  end
  if #lns > 0 then
    table.insert(lines, latin_funcs.format_lns_entries(lns, "LEWIS AND SHORT"))
  end
  -- P(lines)
  return vim.tbl_flatten(lines)
end

function latin_funcs.format_gcse_as_entries(entries_array, label)
  local output = {}
  table.insert(output, "# " .. label)
  for _, entry in pairs(entries_array) do
    table.insert(output, entry[1] .. " " .. entry[2] .. " : " .. entry[3])
  end
  table.insert(output, "")
  return output
end

function latin_funcs.format_wwords_entries(entries_array, label)
  local output = {}
  table.insert(output, "# " .. label)
  for _, entry in pairs(entries_array) do
    table.insert(
      output,
      entry[1] .. " " .. entry[2] .. " " .. entry[3] .. " : " .. entry[4]
    )
  end
  table.insert(output, "")
  return output
end

function latin_funcs.format_lns_entries(entries_array, label)
  local output = {}
  table.insert(output, "# " .. label)
  for _, entry in pairs(entries_array) do
    table.insert(output, entry["head"])
    table.insert(output, entry["orth_orig"])
    local early_i_tags = entry["early_i_tags"]
    if #early_i_tags > 0 then
      table.insert(output, "Meanings: " .. table.concat(early_i_tags, ", "))
    end
    local senses = entry["senses"]
    if #senses > 0 then
      for _, sense in pairs(senses) do
        local i_tags = sense["i_tags"]
        if #i_tags == 0 then
          goto continue
        end
        table.insert(output, "#>Meanings: " .. table.concat(i_tags, ", "))
        local pos = sense["pos"]
        if pos then
          table.insert(output, "  Pos: " .. pos)
        end
        local authors = sense["authors"]
        if #authors > 0 then
          table.insert(output, "  Authors: " .. table.concat(authors, ", "))
        end
        ::continue::
      end
    end
  end
  return output
end

function latin_funcs.add_to_vocab()
  local line = api.nvim_get_current_line()
  if line:find("Meanings: ") then
    -- print("Got here")
    local match_line_nr = vim.fn.search("# ", "bn")
    local dict_form = api.nvim_buf_get_lines(
      dict_buf,
      match_line_nr,
      match_line_nr + 1,
      true
    )[1]
    local meaning = vim.fn.split(line, ": ")[2]
    line = dict_form .. " : " .. meaning
  end
  api.nvim_buf_set_lines(vocab_buf, -1, -1, true, { line })
  api.nvim_set_current_win(text_win)
end

function latin_funcs.next_entry()
  vim.fn.search("^#")
  local line = vim.api.nvim_get_current_line()
  if line:find("# LEWIS") then
    vim.fn.search("Meanings: ")
  elseif line:find("#>Meanings: ") then
    return
  else
    vim.cmd("normal j")
  end
end

return latin_funcs
