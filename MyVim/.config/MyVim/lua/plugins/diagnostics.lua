return {
  {
    'folke/trouble.nvim',
    cmd = 'Trouble',
    config = function()
      require('trouble').setup()
      local trouble = require('trouble.sources.telescope')

      local telescope = require('telescope')

      telescope.setup({
        defaults = {
          mappings = {
            i = { ['<c-x>'] = trouble.open },
            n = { ['<c-x>'] = trouble.open },
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
}
