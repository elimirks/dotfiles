function user_install_plugins()
    local use = require('packer').use
    use {
        'B4mbus/oxocarbon-lua.nvim',
        config = function()
            vim.g.oxocarbon_lua_disable_italic = true
            vim.cmd([[colorscheme oxocarbon-lua]])
            vim.o.colorcolumn = '80'
            vim.o.cursorline = true
            vim.cmd([[highlight ColorColumn ctermbg=236]])
            vim.cmd([[highlight CursorLine ctermbg=236]])
            vim.cmd([[highlight Visual ctermbg=239]])
        end
    }
    use {
        'preservim/vim-markdown',
        requires = {'godlygeek/tabular'},
        config = function()
            vim.g.vim_markdown_folding_style_pythonic = 1
        end
    }
    use {
        'ruifm/gitlinker.nvim',
        config = function()
            local function get_sourcegraph_type_url(url_data)
              local url = "https://"
                .. url_data.host
                .. "/"
                .. url_data.repo
                .. "@"
                .. url_data.rev
                .. "/-/blob/"
                .. url_data.file
              if url_data.lstart then
                url = url .. "#L" .. url_data.lstart
                if url_data.lend then
                  url = url .. "-L" .. url_data.lend
                end
              end
              return url
            end
            require("gitlinker").setup({
              callbacks = {
                ["git.sjc.dropbox.com"] = function(url_data)
                  url_data.host = "sourcegraph.pp.dropbox.com"
                  url_data.repo = "git.sjc.dropbox.com/server"
                  url_data.rev = vim.fn.trim(
                    vim.fn.system("git merge-base HEAD origin/master")
                  )
                  return get_sourcegraph_type_url(url_data)
                end,
              },
            })
        end
    }
end

function user_config()
    local fzf = require('fzf-lua')

    require('legendary').bind_keymaps({
        { '<Space>' , '<nop>' },

        -- Fast saving
        { '<leader>w', ':<C-u>silent update<cr>' },

        -- Buffer navigation keybinds
        { '<leader>b', ':b#<cr>' },
        { '<leader>k', ':bd<cr>' },

        -- Don't lose visual selection with < >
        { '<', '<gv', mode = { 'x' } },
        { '>', '>gv', mode = { 'x' } },

        -- Format
        { '<leader>F', '<cmd>Neoformat<cr>', mode = { 'n', 'v' } },

        -- fzf
        { '<leader>f', fzf.files },
        { '<leader>m', fzf.buffers },
        { '<leader>g', fzf.live_grep },
        { '<leader>r', fzf.resume },
        { '<M-x>', fzf.commands },

        -- Help
        { '<C-h>f', fzf.help_tags },
        { '<C-h>k', '<cmd>Legendary<cr>' },

        -- Neogit
        { '<leader>G', '<cmd>Neogit<cr>' },

        -- Quickrun
        { '<leader>q', '<cmd>Jaq<cr>' },

        -- Gitlinker
        { '<leader>gb', '<cmd>lua require"gitlinker".get_buf_range_url("n", {action_callback = require"gitlinker.actions".open_in_browser})<cr>' },
    })
end

function user_on_lsp_attach(client, bufnr)
    local map_opts = { buffer = true }
    local fzf = require('fzf-lua')

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    require('legendary').bind_keymaps({
        { 'gd', fzf.lsp_definitions, opts = map_opts },
        { 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', opts = map_opts },
        { 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', opts = map_opts },
        { 'gi', fzf.lsp_implementations, opts = map_opts },
        { '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts = map_opts },
        { 'gr', fzf.lsp_references, opts = map_opts },
        { '[d', '<cmd>lua vim.diagnostic.goto_prev()<cr>', opts = map_opts },
        { ']d', '<cmd>lua vim.diagnostic.goto_next()<cr>', opts = map_opts },
        { '<leader>F', '<cmd>lua vim.lsp.buf.formatting()<cr>', opts = map_opts },
        { '<leader>F', '<cmd>lua vim.lsp.buf.range_formatting()<cr>', opts = map_opts, mode = { 'v' } },
        { 'gx', '<cmd>lua vim.lsp.buf.code_action()<cr>', opts = map_opts },
        { 'gR', '<cmd>lua vim.lsp.buf.rename()<cr>', opts = map_opts },
    })
end

user_lsp_overrides = {
    rust_analyzer = {
        settings = {
            checkOnSave = {
                extraArgs = {'/tmp/rust-analyzer-check'},
            },
            diagnostics = {
                disabled = {
                    'inactive-code',
                },
            },
        }
    },
    pylsp = {
        settings = {
            pylsp = {
                plugins = {
                    pycodestyle={
                        ignore={'E501'},
                    },
                },
            },
        }
    },
}
