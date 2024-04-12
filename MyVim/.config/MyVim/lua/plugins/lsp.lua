return {
  {
    'neovim/nvim-lspconfig', -- Required
    dependencies = {
      { 'simrat39/rust-tools.nvim' },
      {
        'williamboman/mason.nvim',
        build = function()
          local ok, _ = pcall(vim.cmd, 'MasonUpdate')
          if not ok then
            vim.notify('Could not run MasonUpdate')
          end
        end,
      },
      { 'williamboman/mason-lspconfig.nvim' },
      { 'WhoIsSethDaniel/mason-tool-installer.nvim' },
      -- `neodev` configures Lua LSP for your Neovim config, runtime and plugins
      -- used for completion, annotations and signatures of Neovim apis
      {
        'folke/neodev.nvim',
        event = 'VeryLazy',
        config = function()
          require('neodev').setup({
            library = { plugins = { 'nvim-dap-ui' }, types = true },
          })
        end,
      },
      { 'j-hui/fidget.nvim', opts = {} },
      {
        'onsails/lspkind.nvim',
        event = 'VeryLazy',
      },
      {
        'olexsmir/gopher.nvim',
        dependencies = { -- dependencies
          'nvim-lua/plenary.nvim',
          'nvim-treesitter/nvim-treesitter',
        },
      },
      { 'jsMRSoL/goalltesty' },
    },
    config = function()
      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('my-lsp-attach', { clear = true }),
        callback = function(event)
          local bufnr = event.buf
          local nmap = function(keys, func, desc)
            vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
          end

          nmap('<leader>lr', vim.lsp.buf.rename, 'Rename')
          nmap('<leader>la', vim.lsp.buf.code_action, 'Code action')
          -- Jump to the definition of the word under your cursor.
          --  This is where a variable was first declared, or where a function is defined, etc.
          --  To jump back, press <C-t>.
          nmap('gd', vim.lsp.buf.definition, 'Go to definition')
          -- Find references for the word under your cursor.
          nmap('gr', require('telescope.builtin').lsp_references, 'Go to references')
          -- Jump to the implementation of the word under your cursor.
          --  Useful when your language has ways of declaring types without an actual implementation.
          nmap('gI', require('telescope.builtin').lsp_implementations, 'Go to implementation')
          -- Jump to the type of the word under your cursor.
          --  Useful when you're not sure what type a variable is and you want to see
          --  the definition of its *type*, not where it was *defined*.
          nmap('<leader>ld', vim.lsp.buf.type_definition, 'Type definition')
          -- Fuzzy find all the symbols in your current document.
          --  Symbols are things like variables, functions, types, etc.
          nmap('<leader>ls', require('telescope.builtin').lsp_document_symbols, 'Document symbols')
          -- Fuzzy find all the symbols in your current workspace.
          --  Similar to document symbols, except searches over your entire project.
          nmap('<leader>lws', require('telescope.builtin').lsp_dynamic_workspace_symbols, 'Workspace symbols')

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
          --
          -- -- Set some keybinds conditional on server capabilities
          nmap('<space>lf', '<cmd>lua vim.lsp.buf.format({async = true})<CR>', 'Format buffer')

          local client = vim.lsp.get_client_by_id(event.data.client_id)
          if client and client.server_capabilities.documentHighlightProvider then
            vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
              buffer = event.buf,
              callback = vim.lsp.buf.document_highlight,
            })

            vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
              buffer = event.buf,
              callback = vim.lsp.buf.clear_references,
            })
          end
        end,
      })

      -- LSP servers and clients are able to communicate to each other what features they support.
      --  By default, Neovim doesn't support everything that is in the LSP specification.
      --  When you add nvim-cmp, luasnip, etc. Neovim now has *more* capabilities.
      --  So, we create new capabilities with nvim cmp, and then broadcast that to the servers.
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = vim.tbl_deep_extend('force', capabilities, require('cmp_nvim_lsp').default_capabilities())

      -- define diagnostic signs
      local function sign_define(args)
        vim.fn.sign_define(args.name, {
          texthl = args.name,
          text = args.text,
          numhl = '',
        })
      end

      sign_define({ name = 'DiagnosticSignError', text = '✘' })
      sign_define({ name = 'DiagnosticSignWarn', text = '▲' })
      sign_define({ name = 'DiagnosticSignHint', text = '⚑' })
      sign_define({ name = 'DiagnosticSignInfo', text = '»' })

      --  Add any additional override configuration in the following tables. Available keys are:
      --  - cmd (table): Override the default command used to start the server
      --  - filetypes (table): Override the default list of associated filetypes for the server
      --  - capabilities (table): Override fields in capabilities. Can be used to disable certain LSP features.
      --  - settings (table): Override the default settings passed when initializing the server.
      local servers = {
        -- rust
        rust_analyzer = {},
        -- go
        gopls = {
          on_attach = function(_, bufnr)
            local mason_registry = require('mason-registry')
            local impl = mason_registry.get_package('impl'):get_install_path() .. '/impl'
            local gomodifytags = mason_registry.get_package('gomodifytags'):get_install_path() .. '/gomodifytags'
            local iferr = mason_registry.get_package('iferr'):get_install_path() .. '/iferr'
            local gotests = mason_registry.get_package('gotests'):get_install_path() .. '/gotests'
            local gotestsum = mason_registry.get_package('gotestsum'):get_install_path() .. '/gotestsum'

            require('gopher').setup({
              commands = {
                go = 'go',
                gomodifytags = gomodifytags,
                gotests = gotests,
                impl = impl,
                iferr = iferr,
              },
            })

            require('gotestit').setup({
              commands = {
                gotestsum = gotestsum,
              }
            })

            local wk = require('which-key')
            wk.register({
              ['<leader>lt'] = { name = '+tags' },
              ['<leader>lT'] = { name = '+tests' },
              ['<leader>lm'] = { name = '+mod' },
            })

            local keymap = {
              { '<space>lg',  ':GoGet ' },
              { '<space>lI',  ':GoImpl ' },
              { '<space>ltj', '<cmd>GoTagAdd json<CR>' },
              { '<space>lty', '<cmd>GoTagAdd yaml<CR>' },
              { '<space>lTa', '<cmd>GoTestsAll<CR>' },
              { '<space>lTA', '<cmd>GoTestAdd<CR>' },
              { '<space>lTe', '<cmd>GoTestsExp<CR>' },
              { '<space>lD',  '<cmd>GoCmt' },
              { '<space>li',  '<cmd>GoIfErr<CR>' },
              { '<space>lmi', ':GoMod init ' },
              { '<space>lmt', '<cmd>GoMod tidy<CR>' },
              -- { '<leader>lTT', '<cmd>GoRunThisTest<CR>' },
              -- { '<leader>lTG', '<cmd>GoRunAllTests<CR>' },
              { ';tt',        '<cmd>GoRunThisTest<CR>' },
              { ';td',        '<cmd>GoRunAllTests<CR>' },
            }

            for _, v in pairs(keymap) do
              vim.keymap.set('n', v[1], v[2], { noremap = true, buffer = bufnr })
            end
          end,

        },
        -- python
        -- pyright = {}, -- can't get this to work?!
        pylsp = {},
        -- ts/js
        tsserver = {
          init_options = {
            preferences = {
              disableSuggestions = true,
            },
          },
        },
        -- lua
        lua_ls = {
          on_attach = function(_, bufnr)
            local keymap = {
              { ';tf', '<cmd>PlenaryBustedFile %<CR>' },
              { ';td', '<cmd>PlenaryBustedDirectory %:p:h<CR>' },
            }

            for _, v in pairs(keymap) do
              vim.keymap.set('n', v[1], v[2], { noremap = true, buffer = bufnr })
            end
          end,
          settings = {
            Lua = {
              completion = {
                callSnippet = 'Replace',
              },
              hint = { enable = true },
              diagnostics = {
                -- get the language server to recognise
                -- globals used in plenary tests
                globals = {
                  'describe',
                  'it',
                  'before_each',
                  'vim',
                },
                disable = {
                  'missing-fields',
                },
              },
            },
          },
        },
      }

      -- mason config begins
      require('mason').setup()
      local ensure_installed = vim.tbl_keys(servers or {})
      vim.list_extend(ensure_installed, {
        -- lua
        'stylua',
        'selene',
        -- python
        -- 'ruff',
        -- 'mypy',
        -- 'black',
        'debugpy',
        -- go
        'golines',
        'delve',
        'impl',
        'gotests',
        'gotestsum',
        'gomodifytags',
        'iferr',
        -- js/ts
        'prettierd',
        -- shell
        'shfmt',
        'shellcheck',
        'beautysh',
        -- rust
        'codelldb',
      })

      -- we use the ensure_installed variable here...
      require('mason-tool-installer').setup({ ensure_installed = ensure_installed })

      -- but we use the servers variable here
      require('mason-lspconfig').setup({
        handlers = {
          -- first entry (without a key) is the default handler
          function(server_name)
            local server = servers[server_name] or {}
            server.capabilities = vim.tbl_deep_extend('force', {}, capabilities, server.capabilities or {})
            require('lspconfig')[server_name].setup(server)
          end,
          -- dedicated handlers for specific servers
          ['rust_analyzer'] = function()
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
                settings = {
                  ['rust-analyzer'] = {
                    runnables = {
                      -- extraArgs = { '--', '--show-output', '--color always', '--test-threads=1' },
                    },
                    -- checkOnSave = {
                    --   command = { 'clippy', '--', '-W', 'clippy::pedantic', '-W', 'clippy::nursery', '-W', 'clippy::unwrap_used' }
                    -- }
                  },
                },
              },
              dap = {
                adapter = require('rust-tools.dap').get_codelldb_adapter(codelldb_path, liblldb_path),
              },
            })
          end,
        },
      })

      -- Set up autoformat on save
      local fmt_group = vim.api.nvim_create_augroup('autoformat_cmds', { clear = true })

      local function setup_autoformat(event)
        local id = vim.tbl_get(event, 'data', 'client_id')
        local client = id and vim.lsp.get_client_by_id(id)
        if client == nil then
          return
        end

        vim.api.nvim_clear_autocmds({ group = fmt_group, buffer = event.buf })

        local buf_format = function(e)
          vim.lsp.buf.format({
            bufnr = e.buf,
            async = false,
            timeout_ms = 10000,
          })
        end

        vim.api.nvim_create_autocmd('BufWritePre', {
          buffer = event.buf,
          group = fmt_group,
          desc = 'Format current buffer',
          callback = buf_format,
        })
      end

      vim.api.nvim_create_autocmd('LspAttach', {
        desc = 'Setup format on save',
        callback = setup_autoformat,
      })
    end,
  },
}
