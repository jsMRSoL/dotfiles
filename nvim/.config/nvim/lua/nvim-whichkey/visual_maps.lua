local visual_opts = {
  mode = "v", -- VISUAL mode
  -- prefix: use "<leader>f" for example for mapping everything related to finding files
  -- the prefix is prepended to every mapping part of `mappings`
  prefix = "",
  buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true, -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = false, -- use `nowait` when creating keymaps
}

local visual_mappings = {
  ["<leader>"] = {

    r = {
      name = "+registers",
      c = { '"_c', "change (bh)" },
      d = { '"_d', "delete (bh)" },
      x = { '"_x', "delete (bh)" },
      p = { '"*gP', "paste clipboard" },
      r = { "<Cmd>reg<CR>", "registers" },
    },

    x = {
      name = "+text",
      n = { "<Cmd>lua require('sp-functions').number_lines2()<CR>", "numbered list" },
      a = { "<Cmd>lua require('sp-functions').align_on_char2()<CR>", "align on char" },
      e = { "<Cmd>lua require('sp-functions').flush_empty_lines()<CR>", "flush empty lines" },
      f = { "<Cmd>lua require('sp-functions').flush_lines()<CR>", "flush lines (regexp)" },
      k = { "<Cmd>lua require('sp-functions').keep_lines()<CR>", "keep lines (regexp)" },
    },

    y = {
      name = "+yank cb",
      y = { '"+y', "yank lines" },
    },
  },
}

return {
  visual_opts = visual_opts,
  visual_mappings = visual_mappings,
}
