return {
  { 'folke/neodev.nvim' },
  { 'j-hui/fidget.nvim' },
  {
    "onsails/lspkind.nvim",
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
          Struct = '  '
        },
      })
    end
  },
  {
    "simrat39/rust-tools.nvim",
  },
  {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    dependencies = {
      -- LSP Support
      { 'neovim/nvim-lspconfig' }, -- Required
      {
        -- Optional
        'williamboman/mason.nvim',
        build = function()
          local ok, _ = pcall(vim.cmd, 'MasonUpdate')
          if not ok then
            vim.notify("Could not run MasonUpdate")
          end
        end,
      },
      { 'williamboman/mason-lspconfig.nvim' }, -- Optional

      -- Autocompletion
      { 'hrsh7th/nvim-cmp', },    -- Required
      { 'hrsh7th/cmp-nvim-lsp' }, -- Required
      { "hrsh7th/cmp-buffer" },
      { "hrsh7th/cmp-path" },
      { "hrsh7th/cmp-cmdline" },
      -- { "saadparwaiz1/cmp_luasnip"},
      { 'L3MON4D3/LuaSnip' }, -- Required
    },
    config = function()
      local lsp = require('lsp-zero').preset(

        {
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
        nmap('<leader>ca', vim.lsp.buf.code_action, 'Code action')

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
        nmap("<space>lf", "<cmd>lua vim.lsp.buf.format({async = true})<CR>", "Format buffer")
        -- elseif client.resolved_capabilities.document_range_formatting then
        --   nmap("<space>lf", "<cmd>lua vim.lsp.buf.format()<CR>", "Format")
        -- end
      end)

      lsp.set_sign_icons({
        error = '✘',
        warn = '▲',
        hint = '⚑',
        info = '»'
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
              callSnippet = "Replace"
            }
          }
        }
      })

      lsp.skip_server_setup({ 'rust_analyzer' })

      -- lsp status info
      require('fidget').setup()

      lsp.setup()

      local lspkind = require('lspkind')
      local cmp = require('cmp')
      cmp.setup {
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
          })
        },
        mapping = {
          ['<CR>'] = cmp.mapping.confirm({ select = false }),
        }
      }

      cmp.setup.cmdline("/", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = "buffer" },
        },
      })

      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = "path" },
        }, {
          { name = "cmdline" },
        }),
      })

      local rust_tools = require('rust-tools')

      local mason_registry = require("mason-registry")
      local codelldb = mason_registry.get_package("codelldb")
      local extension_path = codelldb:get_install_path() .. "/extension/"
      local codelldb_path = extension_path .. 'adapter/codelldb'
      local liblldb_path = extension_path .. 'lldb/lib/liblldb.so'

      rust_tools.setup({
        tools = {
          focus = true
        },
        server = {
          on_attach = function(_, bufnr)

            local keymap = {
              { "<space>lh",  "<cmd>RustHoverActions<CR>" },
              { "<space>lH",  "<cmd>RustHoverRange<CR>" },
              { "<space>le",  "<cmd>RustExpandMacro<CR>" },
              { "<space>lE",  "<cmd>RustOpenExternalDocs<CR>" },
              { "<space>lR",  "<cmd>RustRunnables<CR>" },
              { "<space>lD",  "<cmd>RustDebuggables<CR>" },
              { "<space>lmd", "<cmd>RustMoveItemDown<CR>" },
              { "<space>lmu", "<cmd>RustMoveItemUp<CR>" },
              { "<space>lc",  "<cmd>RustOpenCargo<CR>" },
              { "<space>lp",  "<cmd>RustParentModule<CR>" },
              { "<space>lj",  "<cmd>RustJoinLines<CR>" },
            }

            for _, v in pairs(keymap) do
              vim.keymap.set("n", v[1], v[2], { noremap = true, buffer = bufnr })
            end

            local wk = require('which-key')
            wk.register({
              ["<leader>lm"] = { name = "+move" }
            })
          end
        },
        dap = {
          adapter = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path)
        }
      })
    end,
  }
}
