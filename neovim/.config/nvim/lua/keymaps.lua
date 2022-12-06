local opts = { noremap = true, silent = true }

local term_opts = { silent = true }

-- Shorten function name
local keymap = vim.api.nvim_set_keymap

--Remap space as leader key
keymap("", "<space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Normal --
-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Navigate Compe menu
keymap('i', '<C-j>', 'pumvisible() ? "\\<C-n>" : "\\<C-j>"' , { noremap = true, expr=true })
keymap('i', '<C-k>', 'pumvisible() ? "\\<C-p>" : "\\<C-k>"' , { noremap = true, expr=true })

-- Resize with arrows
keymap("n", "<C-Up>", ":resize +2<CR>", opts)
keymap("n", "<C-Down>", ":resize -2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

-- Navigate buffers
keymap("n", "<A-l>", ":bnext<CR>", opts)
keymap("n", "<A-h>", ":bprevious<CR>", opts)

-- fzf keybindings
keymap("n", "<leader>f", ":Files<Cr>", opts)
keymap("n", "<leader>b", ":Buffers<Cr>", opts)

-- nerdtree keybindings
keymap("n", "<leader>n", ":NERDTreeToggle<Cr>", opts)

