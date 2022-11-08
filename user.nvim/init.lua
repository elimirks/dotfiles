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
        { '<leader>e', '<cmd>lua require("fzf-lua").files({ cwd = vim.fs.dirname(vim.api.nvim_buf_get_name(0)) })<cr>'},
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

        { 'gy', '<cmd>lua get_sourcegraph_url()<cr>' },
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
