function user_install_plugins()
    return {
    {
        'bluz71/vim-moonfly-colors',
        config = function()
            vim.g.oxocarbon_lua_disable_italic = true
            vim.cmd([[colorscheme moonfly]])
            vim.o.colorcolumn = '80'
            vim.o.cursorline = true
            vim.cmd([[highlight ColorColumn ctermbg=236]])
            vim.cmd([[highlight CursorLine ctermbg=236]])
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
    {
        'evanleck/vim-svelte',
        config = function()
        end
    },
    {
        'nvim-orgmode/orgmode',
        config = function()
            require('orgmode').setup({
                -- org_agenda_files = '~/orgfiles/**/*',
                -- org_default_notes_file = '~/orgfiles/refile.org',
            })
        end
    },
    {
        "yetone/avante.nvim",
        event = "VeryLazy",
        version = false, -- Never set this value to "*"! Never!
        opts = {
            provider = "openai",
            openai = {
                endpoint = "https://api.openai.com/v1",
                model = "gpt-4o",
                timeout = 30000, -- ms
                temperature = 0,
                max_completion_tokens = 8192,
                --reasoning_effort = "medium", -- low|medium|high, only used for reasoning models
            },
        },
        -- if you want to build from source then do `make BUILD_FROM_SOURCE=true`
        build = "make",
        -- build = "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false" -- for windows
        dependencies = {
            "nvim-treesitter/nvim-treesitter",
            "stevearc/dressing.nvim",
            "nvim-lua/plenary.nvim",
            "MunifTanjim/nui.nvim",
            --- The below dependencies are optional,
            "echasnovski/mini.pick", -- for file_selector provider mini.pick
            "nvim-telescope/telescope.nvim", -- for file_selector provider telescope
            "hrsh7th/nvim-cmp", -- autocompletion for avante commands and mentions
            "ibhagwan/fzf-lua", -- for file_selector provider fzf
            "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
            "zbirenbaum/copilot.lua", -- for providers='copilot'
            {
                -- support for image pasting
                "HakonHarnes/img-clip.nvim",
                event = "VeryLazy",
                opts = {
                    -- recommended settings
                    default = {
                        embed_image_as_base64 = false,
                        prompt_for_file_name = false,
                        drag_and_drop = {
                            insert_mode = true,
                        },
                        -- required for Windows users
                        use_absolute_path = true,
                    },
                },
            },
            {
                -- Make sure to set this up properly if you have lazy=true
                'MeanderingProgrammer/render-markdown.nvim',
                opts = {
                    file_types = { "markdown", "Avante" },
                },
                ft = { "markdown", "Avante" },
            },
        },
    }
    -- {
    --     'memgraph/cypher.vim',
    --     config = function()
    --     end
    -- },
    -- {
    --     'scalameta/nvim-metals',
    --     dependencies = {'nvim-lua/plenary.nvim'},
    --     config = function(self, metals_config)
    --         local nvim_metals_group = vim.api.nvim_create_augroup("nvim-metals", {
    --             clear = true
    --         })
    --         vim.api.nvim_create_autocmd("FileType", {
    --             pattern = self.ft,
    --             callback = function()
    --                 require("metals").initialize_or_attach(metals_config)
    --             end,
    --             group = nvim_metals_group,
    --         })
    --
    --     end
    -- },
    }
end

function user_config()
    vim.o.timeoutlen = 5000
    vim.o.wrap = true
    vim.o.shortmess = "ltToOCFm"
    vim.g.markdown_folding_disabled = 1
    vim.g.markdown_fenced_languages = { 'cypher=cypher' }

    local sp = require('snacks.picker')

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

        -- Pickers
        { '<leader>f', function() sp.git_files({ untracked = true }) end },
        { '<leader>e', snacks_find_file },
        { '<leader>m', sp.buffers },
        { '<leader>g', sp.grep },
        { '<leader>r', sp.resume },
        { '<M-x>', sp.commands },
        { '<leader>a@', function()
            local sidebar = require('avante').get()
            if not sidebar:is_open() then
                require('avante.api').ask()
                sidebar = require('avante').get()
            end
            local relative_path = vim.fn.expand('%')
            sidebar.file_selector:add_selected_file(relative_path)
        end
        },

        -- Help
        { '<C-h>f', sp.help},
        { '<C-h>k', '<cmd>Legendary<cr>' },

        -- Neogit
        { '<leader>G', '<cmd>Neogit<cr>' },

        -- Quickrun
        { '<leader>q', '<cmd>Jaq<cr>' },

        -- Write code for me pls
        { '<leader>c', '<cmd>Copilot<cr>' },

        -- Copy sourcegraph link to clipboard
        { 'gys', '<cmd>lua get_sourcegraph_url()<cr>' },
        -- Copy file path to clipboard
        { 'gyf', '<cmd>let @+ = @%<cr>' },

        -- DAP
        -- Debug continue (or start)
        { '<leader>dc', '<cmd>lua require("dap").continue()<cr>' },
        { '<leader>db', '<cmd>lua require("persistent-breakpoints.api").toggle_breakpoint()<cr>' },
        { '<leader>do', '<cmd>lua require("dap").step_over()<cr>' },
        { '<leader>di', '<cmd>lua require("dap").step_into()<cr>' },
        -- Run code you highlighted. Can work in a code comment etc.
        { '<leader>de', '<cmd>lua require("dapui").eval()<cr>', mode = { 'n', 'v' } },
        -- Runs until the current cursor, ignoring breakpoints temporarily
        { '<leader>dr', '<cmd>lua require("dap").run_to_cursor()<cr>' },
        { '<leader>dq', '<cmd>lua require("dapui").close()<cr>' },
    })

    require('treesitter-context').setup({
        max_lines = 7,
    })
end

function user_on_lsp_attach(client, bufnr)
    local map_opts = { buffer = true }
    local sp = require('snacks.picker')

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    require('legendary').keymaps({
        { 'gd', sp.lsp_definitions, opts = map_opts },
        { 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', opts = map_opts },
        { 'gt', '<cmd>lua vim.lsp.buf.type_definition()<cr>', opts = map_opts },
        { 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', opts = map_opts },
        { 'gi', sp.lsp_implementations, opts = map_opts },
        { '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts = map_opts },
        { 'gr', sp.lsp_references, opts = map_opts },
        { '[d', '<cmd>lua vim.diagnostic.goto_prev()<cr>', opts = map_opts },
        { ']d', '<cmd>lua vim.diagnostic.goto_next()<cr>', opts = map_opts },
        { '<leader>F', '<cmd>lua vim.lsp.buf.formatting()<cr>', opts = map_opts },
        { '<leader>F', '<cmd>lua vim.lsp.buf.range_formatting()<cr>', opts = map_opts, mode = { 'v' } },
        { 'gx', '<cmd>lua vim.lsp.buf.code_action()<cr>', opts = map_opts },
        { 'gR', function() 
                    vim.lsp.buf.rename()
                    vim.cmd('silent! wa')
                end,
        opts = map_opts },
    })
end

user_lsp_overrides = {
    rust_analyzer = {
        settings = {
            ['rust-analyzer'] = {
                cargo = {
                    allFeatures = true,
                },
                checkOnSave = {
                    extraArgs = {'--target-dir', '/home/eli/.cache/rust-analyzer'},
                },
                diagnostics = {
                    disabled = {
                        'inactive-code',
                    },
                },
            }
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
