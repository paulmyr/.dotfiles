" config
syntax on
filetype plugin indent on
colorscheme catppuccin-frappe

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

