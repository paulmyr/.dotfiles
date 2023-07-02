" Plugins here
call plug#begin('~/.config/nvim/plugged')

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } } " A fuzzy file finder
Plug 'junegunn/fzf.vim'

" Intellisense
Plug 'neovim/nvim-lspconfig'

Plug 'hrsh7th/nvim-compe'

" Plug 'scrooloose/nerdcommenter' " Comment/Uncomment tool

Plug 'jiangmiao/auto-pairs' " Auto-close braces and scopes
" Plug 'tmhedberg/matchit' " Switch to the begining and the end of a block by pressing %
"
Plug 'scrooloose/nerdtree' " A Tree-like side bar for better navigation

Plug 'sheerun/vim-polyglot' " Better syntax-highlighting for filetypes in vim

" Plug 'arcticicestudio/nord-vim'
" Plug 'Mofiqul/dracula.nvim'
Plug 'catppuccin/nvim', { 'as': 'catppuccin' }

Plug 'vim-airline/vim-airline'
" Plug 'vim-airline/vim-airline-themes'

Plug 'lervag/vimtex'

Plug 'sirver/ultisnips'

" Git informations
Plug 'lewis6991/gitsigns.nvim'

" Start screen and session management
Plug 'mhinz/vim-startify'

call plug#end()


