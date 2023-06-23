function user_install_plugins()
    return {
    {
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
    },
    {
        'preservim/vim-markdown',
        dependencies = {'godlygeek/tabular'},
        config = function()
            vim.g.vim_markdown_folding_style_pythonic = 1
        end
    },
    {
        'simrat39/symbols-outline.nvim',
        config = function()
            require("symbols-outline").setup({
                -- autofold_depth = 0,
            })
        end
    },
    }
end

function user_config()
    vim.o.wrap = true

    local fzf = require('fzf-lua')

    require('legendary').keymaps({
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
        --{ '<leader>f', function() fzf_find_file(vim.loop.cwd()) end },
        --{ '<leader>e', function() fzf_find_file(vim.fs.dirname(vim.api.nvim_buf_get_name(0))) end},
        { '<leader>f', fzf.files },
        { '<leader>e', function() fzf_find_file(get_buf_dir()) end },
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

        -- DAP
        { '<F5>', '<cmd>lua require("dap").continue()<cr>' },
        { '<F9>', '<cmd>lua require("persistent-breakpoints.api").toggle_breakpoint()<cr>' },
        { '<F10>', '<cmd>lua require("dap").step_over()<cr>' },
        { '<F11>', '<cmd>lua require("dap").step_into()<cr>' },
        { '<F12>', '<cmd>lua require("dapui").eval()<cr>', mode = { 'n', 'v' } },
    })
end

function user_on_lsp_attach(client, bufnr)
    local map_opts = { buffer = true }
    local fzf = require('fzf-lua')

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    require('legendary').keymaps({
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
