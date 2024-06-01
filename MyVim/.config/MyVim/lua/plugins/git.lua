return {
  {
    'lewis6991/gitsigns.nvim',
    event = 'VeryLazy',
    config = function ()
      require('gitsigns').setup({
        signs = {
          add = { text = '+' },
          change = { text = '~' },
          delete = { text = '-' },
        },
      })

      local gs = package.loaded.gitsigns

      local function map(mode, l, r, opts)
        opts = opts or {}
        -- opts.buffer = bufnr
        vim.keymap.set(mode, l, r, opts)
      end

      map('n', ']c', function ()
        if vim.wo.diff then
          return ']c'
        end
        vim.schedule(function ()
          gs.next_hunk()
        end)
        return '<Ignore>'
      end, { expr = true })

      map('n', '[c', function ()
        if vim.wo.diff then
          return '[c'
        end
        vim.schedule(function ()
          gs.prev_hunk()
        end)
        return '<Ignore>'
      end, { expr = true })

      -- text object
      map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
    end,
  },
  {
    'NeogitOrg/neogit',
    dependencies = {
      'nvim-lua/plenary.nvim', -- required
      -- 'sindrets/diffview.nvim', -- optional - Diff integration

      -- Only one of these is needed, not both.
      'nvim-telescope/telescope.nvim', -- optional
      -- 'ibhagwan/fzf-lua',            -- optional
    },
    config = true
  }
}
