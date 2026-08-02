function string:endswith(suffix)
    return tostring(self):sub(-#suffix) == suffix
end

local syl_sesh_command_value = nil
local function syl_sesh_cmd_request()
  -- Prompt the user for input and set the result to a local global variable
  syl_sesh_command_value = vim.fn.input('👾Gimme a command: ')
end

local function syl_sesh_command_execute()
    if syl_sesh_command_value then
        vim.cmd('!' .. syl_sesh_command_value)
    else
        print('No temp command set 💔')
    end
end

local functions = require('shared.functions')

require('shared.setup').setup({
    install_plugins = function()
        return {
            {
                'tidalcycles/vim-tidal',
            },
            {
                'bluz71/vim-moonfly-colors',
                config = function()
                    vim.g.oxocarbon_lua_disable_italic = true
                    vim.cmd([[colorscheme moonfly]])
                    vim.o.colorcolumn = '80'
                    vim.o.cursorline = true
                    vim.cmd([[highlight ColorColumn guibg=#3b3059]])
                    vim.cmd([[highlight CursorLine guibg=#3b3059]])
                    vim.cmd([[highlight Normal guibg=NONE ctermbg=NONE]])
                    vim.cmd([[highlight NonText guibg=NONE ctermbg=NONE]])
                    vim.cmd([[highlight Visual guibg=#734d3f]])
                    vim.cmd([[highlight LineNr guibg=NONE]])
                    vim.cmd([[highlight SignColumn guibg=NONE]])
                    vim.cmd([[highlight CursorLineSign guibg=NONE]])
                    vim.cmd([[highlight CursorLineNr guifg=#c58de4]])
                end
            },
            {
                'hedyhli/outline.nvim',
                config = function()
                    require("outline").setup({
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
                "folke/trouble.nvim",
                cmd = "Trouble",
                follow = false,
                keys = {
                    {
                        "<leader>xx",
                        "<cmd>Trouble diagnostics toggle<cr>",
                        desc = "Diagnostics (Trouble)",
                    },
                    {
                        "<leader>xn",
                        function()
                            require("trouble").next({ skip_groups = true, jump = true })
                        end,
                        desc = "Diagnostics (Trouble)",
                    },
                    {
                        "<leader>xp",
                        function()
                            require("trouble").previous({ skip_groups = true, jump = true })
                        end,
                        desc = "Diagnostics (Trouble)",
                    },
                },
                modes = {
                    my_diagnostics = {
                        mode = 'diagnostics',
                        filter = function(items)
                            return vim.tbl_filter(function(item)
                                return item.severity == vim.diagnostic.severity.WARN
                            end, items)
                        end,
                    },
                },
            },
            {
                'memgraph/cypher.vim',
                config = function()
                end
            },
        }
    end,

    config = function()
        vim.o.timeoutlen = 5000
        vim.o.wrap = true
        vim.o.shortmess = "ltToOCFm"
        vim.g.markdown_folding_disabled = 1
        -- vim.g.markdown_fenced_languages = { 'cypher=cypher' }

        local sp = require('snacks.picker')

        require('which-key').add({
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
            -- { '<leader>f', function() sp.git_files({ untracked = true }) end },
            { '<leader>f', function() sp.files() end },
            -- { '<leader>e', function() print("Bonjour") end },
            { '<leader>e', functions.snacks_find_file },
            { '<leader>m', sp.buffers },
            { '<leader>g', sp.grep },
            { '<leader>r', sp.resume },
            { '<M-x>', sp.commands },
            -- { '<leader>a@', function()
                --     local sidebar = require('avante').get()
                --     if not sidebar:is_open() then
                --         require('avante.api').ask()
                --         sidebar = require('avante').get()
                --     end
                --     local relative_path = vim.fn.expand('%')
                --     sidebar.file_selector:add_selected_file(relative_path)
                -- end
                -- },

                -- Help
                { '<C-h>f', sp.help},

                -- Neogit
                { '<leader>G', '<cmd>Neogit<cr>' },

                -- Quickrun
                { '<leader>q', '<cmd>Jaq<cr>' },

                -- Write code for me pls
                { '<leader>c', '<cmd>Copilot<cr>' },

                -- Copy sourcegraph link to clipboard
                { 'gys', '<cmd>lua get_sourcegraph_url()<cr>' },
                -- Copy absolute path to clipboard
                { 'gyf', '<cmd>let @+ = expand("%:p")<cr>' },

                { '<leader>st', function() syl_sesh_cmd_request() end },
                { '<leader><CR>', function() syl_sesh_command_execute() end },

                -- DAP
                -- Debug continue (or start)
                -- { '<leader>dc', '<cmd>lua require("dap").continue()<cr>' },
                -- { '<leader>db', '<cmd>lua require("persistent-breakpoints.api").toggle_breakpoint()<cr>' },
                -- { '<leader>do', '<cmd>lua require("dap").step_over()<cr>' },
                -- { '<leader>di', '<cmd>lua require("dap").step_into()<cr>' },
                -- { '<leader>d-', '<cmd>lua require("dap").up()<cr>' },
                -- { '<leader>d+', '<cmd>lua require("dap").down()<cr>' },
                -- -- Run code you highlighted. Can work in a code comment etc.
                -- { '<leader>de', '<cmd>lua require("dapui").eval()<cr>', mode = { 'n', 'v' } },
                -- Runs until the current cursor, ignoring breakpoints temporarily
                -- { '<leader>dr', '<cmd>lua require("dap").run_to_cursor()<cr>' },
                -- { '<leader>dq', '<cmd>lua require("dapui").close()<cr>' },
                -- { '<leader>dd', '<cmd>lua require("dapui").open()<cr>' },
                -- Parrot
                -- { '<leader>pi', '<cmd>\'<,\'PrtImplement<cr>', mode = { 'v' } },
                -- { '<leader>pa', '<cmd>\'<,\'PrtAsk<cr>', mode = { 'v' } },
                -- { '<leader>pr', '<cmd>\'<,\'PrtRewrite<cr>', mode = { 'v' } },
            })

            local dap = require('dap')
            local dapui = require('dapui')
            dapui.setup({
                layouts = { {
                    elements = { {
                        id = "scopes",
                        size = 0.25,
                    }, {
                        id = "breakpoints",
                        size = 0.25,
                    }, {
                        id = "stacks",
                        size = 0.25,
                    }, {
                        id = "watches",
                        size = 0.25,
                    } },
                    position = "left",
                    size = 0.2,
                }, {
                    elements = { {
                        id = "console",
                        size = 100,
                    } },
                    position = "right",
                    size = 0.3,
                }, {
                    elements = { {
                        id = "repl",
                        size = 1,
                    }, },
                    position = "bottom",
                    size = 0.2,
                } },
            })

            function dap_program_path()
                local path = vim.fn.getcwd() .. '/target/debug/*'
                -- TODO: if multiple, pick one
                for i, p in ipairs(vim.split(vim.fn.glob(path), '\n')) do
                    if vim.fn.filereadable(p) == 1 and vim.fn.getfperm(p):sub(3,3) == 'x' then
                        return p
                    end
                end
                -- return vim.fn.input('Path to executable: ', path, 'file')
                return nil
            end

            dap.configurations.rust = {
                {
                    name = 'Run',
                    type = 'codelldb',
                    request = 'launch',
                    program = dap_program_path,
                    cwd = '${workspaceFolder}',
                    stopOnEntry = false,
                    args = function()
                        if dap_program_path():endswith('octopus') then
                            return { 'server' }
                        end
                        local args = vim.fn.input('Args: ')
                        return { '--', args }
                    end,
                },
            }

            dap.listeners.before.event_initialized["dapui_config"] = function()
                dapui.open();
            end
            dap.listeners.before.event_terminated["dapui_config"] = function()
            end
            dap.listeners.before.event_exited["dapui_config"] = function()
                dap.terminate({ all = true })
            end

            require('treesitter-context').setup({
                max_lines = 7,
                opts = {
                    ensure_installed = {},
                },
            })
        end,

        on_lsp_attach = function(client, bufnr)
            local sp = require('snacks.picker')

            -- Mappings.
            -- See `:help vim.lsp.*` for documentation on any of the below functions
            require('which-key').add({
                { 'gd', sp.lsp_definitions, buffer = true },
                { 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', buffer = true },
                { 'gt', '<cmd>lua vim.lsp.buf.type_definition()<cr>', buffer = true },
                { 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', buffer = true },
                { 'gi', sp.lsp_implementations, buffer = true },
                { '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<cr>', buffer = true },
                { 'gr', sp.lsp_references, buffer = true },
                { '[d', '<cmd>lua vim.diagnostic.goto_prev()<cr>', buffer = true },
                { ']d', '<cmd>lua vim.diagnostic.goto_next()<cr>', buffer = true },
                { '<leader>F', '<cmd>lua vim.lsp.buf.formatting()<cr>', buffer = true },
                { '<leader>F', '<cmd>lua vim.lsp.buf.range_formatting()<cr>', buffer = true, mode = { 'v' } },
                { 'gx', '<cmd>lua vim.lsp.buf.code_action()<cr>', buffer = true },
                { 'gR', function()
                    vim.lsp.buf.rename()
                    vim.cmd('silent! wa')
                end,
                buffer = true },
            })
        end
    })

vim.lsp.config['rust_analyzer'] = {
    settings = {
        ['rust-analyzer'] = {
            cargo = {
                features = "all",
            },
            check = {
                command = 'clippy'
            },
            diagnostics = {
                disabled = { 'inactive-code' },
            },
        }
    }
}

vim.lsp.config['svelte'] = {
    settings = {
        svelte = {
            plugin = {
                svelte = {
                    compilerWarnings = {
                        ['css_unused_selector'] = 'ignore',
                    },
                },
            },
        },
    },
}
