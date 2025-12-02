function string:endswith(suffix)
    return self:sub(-#suffix) == suffix
end

function user_install_plugins()
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
            vim.cmd([[highlight ColorColumn ctermbg=236]])
            vim.cmd([[highlight CursorLine ctermbg=236]])
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
        keys = {
            {
                "<leader>xx",
                "<cmd>Trouble diagnostics toggle<cr>",
                desc = "Diagnostics (Trouble)",
            },
        },
    },
    {
        'memgraph/cypher.vim',
        config = function()
        end
    },
    }
end

function user_config()
    vim.o.timeoutlen = 5000
    vim.o.wrap = true
    vim.o.shortmess = "ltToOCFm"
    vim.g.markdown_folding_disabled = 1
    -- vim.g.markdown_fenced_languages = { 'cypher=cypher' }

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
        -- { '<leader>f', function() sp.git_files({ untracked = true }) end },
        { '<leader>f', function() sp.files() end },
        { '<leader>e', snacks_find_file },
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
        { '<leader>d-', '<cmd>lua require("dap").up()<cr>' },
        { '<leader>d+', '<cmd>lua require("dap").down()<cr>' },
        -- Run code you highlighted. Can work in a code comment etc.
        { '<leader>de', '<cmd>lua require("dapui").eval()<cr>', mode = { 'n', 'v' } },
        -- Runs until the current cursor, ignoring breakpoints temporarily
        { '<leader>dr', '<cmd>lua require("dap").run_to_cursor()<cr>' },
        { '<leader>dq', '<cmd>lua require("dapui").close()<cr>' },
        { '<leader>dd', '<cmd>lua require("dapui").open()<cr>' },
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
