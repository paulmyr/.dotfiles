" Plugins here
call plug#begin('~/.config/nvim/plugged')

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } } " A fuzzy file finder
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdcommenter' " Comment/Uncomment tool
Plug 'tmhedberg/matchit' " Switch to the begining and the end of a block by pressing %
Plug 'scrooloose/nerdtree' " A Tree-like side bar for better navigation
Plug 'vim-airline/vim-airline-themes'
Plug 'sheerun/vim-polyglot' " Better syntax-highlighting for filetypes in vim
Plug 'neoclide/coc.nvim', {'branch': 'release'} " Intellisense engine
Plug 'jiangmiao/auto-pairs' " Auto-close braces and scopes
Plug 'arcticicestudio/nord-vim'
Plug 'Mofiqul/dracula.nvim'
Plug 'vim-airline/vim-airline'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

call plug#end()

syntax on
filetype plugin indent on
colorscheme dracula

set autoindent
set number relativenumber
set modeline
set textwidth=0
set splitbelow          " Horizontal split below current.
set splitright          " Vertical split to right of current.
set display+=lastline
set showcmd                     " Show me what I'm typing
set showmode                    " Show current mode.
set noswapfile                  " Don't use swapfile
set nobackup            	" Don't create annoying backup files
set encoding=utf-8              " Set default encoding to UTF-8
set autowrite                   " Automatically save before :next, :make etc.
set autoread                    " Automatically reread changed files without asking me anything
set laststatus=1
set fileformats=unix,dos,mac    " Prefer Unix over Windows over OS 9 formats
set showmatch                   " Do not show matching brackets by flickering
set incsearch                   " Shows the match while typing
set hlsearch                    " Highlight found searches
set ignorecase
set smartcase
set magic               " Use 'magic' patterns (extended regular expressions).

nnoremap <C-x><C-p> :Files<Cr>
nnoremap <C-x><C-b> :Buffers<Cr>
map <C-x><C-n> :NERDTreeToggle<CR>

let NERDTreeShowHidden=1 " Show hidden files in NerdTree buffer.

