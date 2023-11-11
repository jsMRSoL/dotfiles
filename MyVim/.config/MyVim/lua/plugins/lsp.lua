return {
  {
    'jose-elias-alvarez/null-ls.nvim',
    event = 'VeryLazy',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      local null_ls = require('null-ls')

      null_ls.setup({
        sources = {
          null_ls.builtins.formatting.stylua,
          null_ls.builtins.diagnostics.luacheck.with({
            args = { '--globals', 'vim' },
          }),
          null_ls.builtins.formatting.shfmt.with({
            args = { '-i', '2', '-bn', '-ci', '-sr' },
          }),
          null_ls.builtins.diagnostics.shellcheck,
          null_ls.builtins.completion.spell,
        },
      })
    end,
  },
  {
    'lewis6991/gitsigns.nvim',
    event = 'VeryLazy',
    config = function()
      require('gitsigns').setup()

      local gs = package.loaded.gitsigns

      local function map(mode, l, r, opts)
        opts = opts or {}
        -- opts.buffer = bufnr
        vim.keymap.set(mode, l, r, opts)
      end

      map('n', ']c', function()
        if vim.wo.diff then
          return ']c'
        end
        vim.schedule(function()
          gs.next_hunk()
        end)
        return '<Ignore>'
      end, { expr = true })

      map('n', '[c', function()
        if vim.wo.diff then
          return '[c'
        end
        vim.schedule(function()
          gs.prev_hunk()
        end)
        return '<Ignore>'
      end, { expr = true })

      -- text object
      map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
    end,
  },
  {
    'folke/neodev.nvim',
    event = 'VeryLazy',
    config = function()
      require('neodev').setup({
        library = { plugins = { 'nvim-dap-ui' }, types = true },
      })
    end,
  },
  {
    'folke/trouble.nvim',
    event = 'VeryLazy',
    config = function()
      local trouble = require('trouble.providers.telescope')

      local telescope = require('telescope')

      telescope.setup({
        defaults = {
          mappings = {
            i = { ['<c-x>'] = trouble.open_with_trouble },
            n = { ['<c-x>'] = trouble.open_with_trouble },
          },
        },
      })

      local cmd = vim.api.nvim_create_user_command

      cmd('TroubleSkipNext', function()
        require('trouble').next({ skip_groups = true, jump = true })
      end, {})

      cmd('TroubleSkipPrev', function()
        require('trouble').previous({ skip_groups = true, jump = true })
      end, {})

      cmd('TroubleSkipFirst', function()
        require('trouble').first({ skip_groups = true, jump = true })
      end, {})

      cmd('TroubleSkipLast', function()
        require('trouble').last({ skip_groups = true, jump = true })
      end, {})
    end,
  },
  {
    'j-hui/fidget.nvim',
    tag = 'legacy',
    event = 'LspAttach',
  },
  {
    'onsails/lspkind.nvim',
    event = 'VeryLazy',
    config = function()
      require('lspkind').init({
        mode = 'symbol_text',
        symbol_map = {
          Text = '  ',
          Method = '  ',
          Function = '  ',
          Constructor = '  ',
          Variable = '[]',
          Class = '  ',
          Interface = ' 蘒',
          Module = '  ',
          Property = '  ',
          Unit = ' 塞 ',
          Value = '  ',
          Enum = ' 練',
          Keyword = '  ',
          Snippet = '  ',
          Color = '',
          File = '',
          Folder = ' ﱮ ',
          EnumMember = '  ',
          Constant = '  ',
          Struct = '  ',
        },
      })
    end,
  },
  {
    'VonHeikemen/lsp-zero.nvim',
    event = 'VeryLazy',
    branch = 'v2.x',
    dependencies = {
      -- LSP Support
      { 'neovim/nvim-lspconfig' }, -- Required
      -- Optional
      {
        'simrat39/rust-tools.nvim',
      },
      {
        'williamboman/mason.nvim',
        build = function()
          local ok, _ = pcall(vim.cmd, 'MasonUpdate')
          if not ok then
            vim.notify('Could not run MasonUpdate')
          end
        end,
      },
      { 'williamboman/mason-lspconfig.nvim' }, -- Optional

      -- Autocompletion
      { 'hrsh7th/nvim-cmp' },     -- Required
      { 'hrsh7th/cmp-nvim-lsp' }, -- Required
      { 'hrsh7th/cmp-buffer' },
      { 'hrsh7th/cmp-path' },
      { 'hrsh7th/cmp-cmdline' },
      { 'saadparwaiz1/cmp_luasnip' },
      { 'L3MON4D3/LuaSnip' }, -- Required
      { 'rafamadriz/friendly-snippets' },
    },
    config = function()
      local lsp = require('lsp-zero').preset({
        float_border = 'rounded',
        call_servers = 'local',
        configure_diagnostics = true,
        setup_servers_on_start = true,
        set_lsp_keymaps = {
          preserve_mappings = false,
          omit = {},
        },
        -- manage_nvim_cmp = false,
        manage_nvim_cmp = {
          set_sources = 'recommended',
          set_basic_mappings = true,
          set_extra_mappings = true,
          use_luasnip = true,
          set_format = true,
          documentation_window = true,
        },
      })

      lsp.on_attach(function(_, bufnr)
        -- lsp.default_keymaps({ buffer = bufnr })

        local nmap = function(keys, func, desc)
          vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
        end

        nmap('<leader>lr', vim.lsp.buf.rename, 'Rename')
        nmap('<leader>la', vim.lsp.buf.code_action, 'Code action')

        nmap('gd', vim.lsp.buf.definition, 'Go to definition')
        nmap('gr', require('telescope.builtin').lsp_references, 'Go to references')
        nmap('gI', vim.lsp.buf.implementation, 'Goto Implementation')
        nmap('<leader>ld', vim.lsp.buf.type_definition, 'Type Definition')
        nmap('<leader>ls', require('telescope.builtin').lsp_document_symbols, 'Document Symbols')
        nmap('<leader>lws', require('telescope.builtin').lsp_dynamic_workspace_symbols, 'Workspace Symbols')
        -- diagnostics
        nmap('[d', vim.diagnostic.goto_next, 'Next diagnostic')
        nmap(']d', vim.diagnostic.goto_prev, 'Previous diagnostic')

        -- See `:help K` for why this keymap
        nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
        nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

        -- Lesser used LSP functionality
        nmap('gD', vim.lsp.buf.declaration, 'Goto Declaration')
        nmap('<leader>lwa', vim.lsp.buf.add_workspace_folder, 'Workspace Add Folder')
        nmap('<leader>lwr', vim.lsp.buf.remove_workspace_folder, 'Workspace Remove Folder')
        nmap('<leader>lwl', function()
          print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, 'Workspace List Folders')

        -- Set some keybinds conditional on server capabilities
        nmap('<space>lf', '<cmd>lua vim.lsp.buf.format({async = true})<CR>', 'Format buffer')
        -- elseif client.resolved_capabilities.document_range_formatting then
        --   nmap("<space>lf", "<cmd>lua vim.lsp.buf.format()<CR>", "Format")
        -- end
      end)

      lsp.set_sign_icons({
        error = '✘',
        warn = '▲',
        hint = '⚑',
        info = '»',
      })

      lsp.ensure_installed({
        -- Replace these with whatever servers you want to install
        'lua_ls',
        'rust_analyzer',
        -- Don't for this for dap: codelldb. It doesn't install automatically.
        'bashls',
        -- 'gopls',
      })

      require('neodev').setup()
      -- require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())
      local lspconfig = require('lspconfig')
      lspconfig.lua_ls.setup({
        settings = {
          Lua = {
            completion = {
              callSnippet = 'Replace',
            },
          },
        },
      })

      lsp.skip_server_setup({ 'rust_analyzer' })

      -- lsp status info
      require('fidget').setup()

      lsp.setup()

      local lspkind = require('lspkind')
      local luasnip = require('luasnip')
      local vscode_loaders = require('luasnip.loaders.from_vscode')
      vscode_loaders.lazy_load()

      local types = require("luasnip.util.types")
      luasnip.setup({
        keep_roots = true,
        link_roots = true,
        link_children = true,

        -- Update more often, :h events for more info.
        update_events = 'TextChanged,TextChangedI',
        -- Snippets aren't automatically removed if their text is deleted.
        -- `delete_check_events` determines on which events (:h events) a check for
        -- deleted snippets is performed.
        -- This can be especially useful when `history` is enabled.
        delete_check_events = 'TextChanged',
        ext_opts = {
          [types.choiceNode] = {
            active = {
              virt_text = { { 'choiceNode', 'Comment' } },
            },
          },
        },
        -- treesitter-hl has 100, use something higher (default is 200).
        ext_base_prio = 300,
        -- minimal increase in priority.
        ext_prio_increase = 1,
        -- enable_autosnippets = true,
        -- mapping for cutting selected text so it's usable as SELECT_DEDENT,
        -- SELECT_RAW or TM_SELECTED_TEXT (mapped via xmap).
        -- store_selection_keys = '<Tab>',
        -- luasnip uses this function to get the currently active filetype. This
        -- is the (rather uninteresting) default, but it's possible to use
        -- eg. treesitter for getting the current filetype by setting ft_func to
        -- require("luasnip.extras.filetype_functions").from_cursor (requires
        -- `nvim-treesitter/nvim-treesitter`). This allows correctly resolving
        -- the current filetype in eg. a markdown-code block or `vim.cmd()`.
        -- ft_func = function()
        --   return vim.split(vim.bo.filetype, '.', true)
        -- end,
      })
      local ok, _ = pcall(require, 'config.user-snippets')
      if not ok then
        vim.notify("Could not load user snippets!", vim.log.levels.WARN )
      end

      -- set keybinds for both INSERT and VISUAL.
      vim.api.nvim_set_keymap('i', '<C-n>', '<Plug>luasnip-next-choice', {})
      vim.api.nvim_set_keymap('s', '<C-n>', '<Plug>luasnip-next-choice', {})
      vim.api.nvim_set_keymap('i', '<C-p>', '<Plug>luasnip-prev-choice', {})
      vim.api.nvim_set_keymap('s', '<C-p>', '<Plug>luasnip-prev-choice', {})

      local cmp = require('cmp')
      cmp.setup({
        mapping = {
          ['<C-n>'] = cmp.mapping.select_next_item(),
          ['<C-p>'] = cmp.mapping.select_prev_item(),
          ['<C-d>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete({}),
          ['<CR>'] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
          }),
          ['<Tab>'] = cmp.mapping(function(fallback)
            if luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            else
              fallback()
            end
          end, { 'i', 's' }),

          ['<S-Tab>'] = cmp.mapping(function(fallback)
            if luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { 'i', 's' }),
        },
        -- fields = { 'abbr', 'kind', 'menu' },
        fields = { 'kind', 'menu' },
        formatting = {
          format = lspkind.cmp_format({
            mode = 'symbol_text',  -- show only symbol annotations
            maxwidth = 50,         -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
            ellipsis_char = '...', -- when popup menu exceed maxwidth, the truncated part would show ellipsis_char instead (must define maxwidth first)

            -- The function below will be called before any actual modifications from lspkind
            -- so that you can provide more controls on popup customization. (See [#30](https://github.com/onsails/lspkind-nvim/pull/30))
            -- before = function(entry, vim_item)
            --   return vim_item
            -- end
          }),
        },
      })

      cmp.setup.cmdline('/', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'buffer' },
        },
      })

      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = 'path' },
        }, {
          { name = 'cmdline' },
        }),
      })

      local rust_tools = require('rust-tools')

      local mason_registry = require('mason-registry')
      local codelldb = mason_registry.get_package('codelldb')
      local extension_path = codelldb:get_install_path() .. '/extension/'
      local codelldb_path = extension_path .. 'adapter/codelldb'
      local liblldb_path = extension_path .. 'lldb/lib/liblldb.so'

      rust_tools.setup({
        tools = {
          focus = true,
        },
        server = {
          on_attach = function(_, bufnr)
            local keymap = {
              { '<space>lh',  '<cmd>RustHoverActions<CR>' },
              { '<space>lH',  '<cmd>RustHoverRange<CR>' },
              { '<space>le',  '<cmd>RustExpandMacro<CR>' },
              { '<space>lE',  '<cmd>RustOpenExternalDocs<CR>' },
              { '<space>lR',  '<cmd>RustRunnables<CR>' },
              { '<space>lD',  '<cmd>RustDebuggables<CR>' },
              { '<space>lmd', '<cmd>RustMoveItemDown<CR>' },
              { '<space>lmu', '<cmd>RustMoveItemUp<CR>' },
              { '<space>lc',  '<cmd>RustOpenCargo<CR>' },
              { '<space>lp',  '<cmd>RustParentModule<CR>' },
              { '<space>lj',  '<cmd>RustJoinLines<CR>' },
            }

            for _, v in pairs(keymap) do
              vim.keymap.set('n', v[1], v[2], { noremap = true, buffer = bufnr })
            end

            local wk = require('which-key')
            wk.register({
              ['<leader>lm'] = { name = '+move' },
            })
          end,
        },
        dap = {
          adapter = require('rust-tools.dap').get_codelldb_adapter(codelldb_path, liblldb_path),
        },
      })
    end,
  },
}
