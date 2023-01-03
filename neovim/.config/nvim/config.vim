" config
syntax on
filetype plugin indent on
colorscheme catppuccin-frappe

set autoindent
set autoread                    " Automatically reread changed files without asking me anything
set autowrite                   " Automatically save before :next, :make etc.
set display+=lastline
set encoding=utf-8              " Set default encoding to UTF-8
set fileformats=unix,dos,mac    " Prefer Unix over Windows over OS 9 formats
set hlsearch                    " Highlight found searches
set ignorecase
set incsearch                   " Shows the match while typing
set laststatus=1
set magic               " Use 'magic' patterns (extended regular expressions).
set modeline
set nobackup            	" Don't create annoying backup files
set noswapfile                  " Don't use swapfile
set number relativenumber
set showcmd                     " Show me what I'm typing
set showmatch                   " Do not show matching brackets by flickering
set showmode                    " Show current mode.
set smartcase
set spelllang=en_gb spell
set spellsuggest=best,5
set splitbelow          " Horizontal split below current.
set splitright          " Vertical split to right of current.
set textwidth=0

" Make calcurse notes markdown compatible
autocmd BufRead,BufNewFile /tmp/calcurse* set filetype=markdown
autocmd BufRead,BufNewFile ~/.calcurse/notes/* set filetype=markdown

