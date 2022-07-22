local dap = require('dap');
local dapui = require('dapui');

dap.adapters.lldb = {
    type = 'executable',
    command = '/usr/sbin/lldb-vscode',
    name = 'lldb'
};
dap.configurations.rust = {
    {
        name = 'Launch',
        type = 'lldb',
        request = 'launch',
        program = function()
            return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
        end,
        cwd = '${workspaceFolder}',
        stopOnEntry = false,
        args = {},
    }
};

vim.fn.sign_define('DapBreakpoint', { text = '🛑', texthl = '', linehl = '', numhl = '' })

dapui.setup({
    icons = { expanded = "▾", collapsed = "▸" },
    mappings = {
        -- Use a table to apply multiple mappings
        expand = { "<CR>", "<2-LeftMouse>" },
        open = "o",
        remove = "d",
        edit = "e",
        repl = "r",
        toggle = "t",
    },
    -- Expand lines larger than the window
    -- Requires >= 0.7
    expand_lines = vim.fn.has("nvim-0.7"),
    -- Layouts define sections of the screen to place windows.
    -- The position can be "left", "right", "top" or "bottom".
    -- The size specifies the height/width depending on position. It can be an Int
    -- or a Float. Integer specifies height/width directly (i.e. 20 lines/columns) while
    -- Float value specifies percentage (i.e. 0.3 - 30% of available lines/columns)
    -- Elements are the elements shown in the layout (in order).
    -- Layouts are opened in order so that earlier layouts take priority in window sizing.
    layouts = {
        {
            elements = {
                -- Elements can be strings or table with id and size keys.
                { id = "scopes", size = 0.25 },
                "breakpoints",
                "stacks",
                "watches",
            },
            size = .2,
            position = "left",
        },
        {
            elements = {
                "repl",
            },
            size = 0.25, -- 25% of total lines
            position = "bottom",
        },
    },
    floating = {
        max_height = nil, -- These can be integers or a float between 0 and 1.
        max_width = nil, -- Floats will be treated as percentage of your screen.
        border = "single", -- Border style. Can be "single", "double" or "rounded"
        mappings = {
            close = { "q", "<Esc>" },
        },
    },
    windows = { indent = 1 },
    render = {
        max_type_length = nil, -- Can be integer or nil.
    }
});

-- Auto open dapui on debug
dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open();
end
dap.listeners.before.event_terminated["dapui_config"] = function()
    dapui.close();
end
dap.listeners.before.event_exited["dapui_config"] = function()
    dapui.close();
end


require('persistent-breakpoints').setup({});
-- automatically load breakpoints when a file is loaded into the buffer.
vim.api.nvim_create_autocmd({"BufReadPost"},{ callback = require('persistent-breakpoints.api').load_breakpoints });

function keymap(mode, lhs, rhs)
    local opts = { noremap = true, silent = true };
    vim.api.nvim_set_keymap(mode, lhs, string.format("<cmd>lua %s<cr>", rhs), opts);
end

keymap('n', '<F5>', 'require("dap").continue()');
keymap('n', '<F9>', 'require("persistent-breakpoints.api").toggle_breakpoint()');
keymap('n', '<F10>', 'require("dap").step_over()');
keymap('n', '<F11>', 'require("dap").step_into()');
keymap('n', '<M-e>', 'require("dapui").eval()');
keymap('v', '<M-e>', 'require("dapui").eval()');
