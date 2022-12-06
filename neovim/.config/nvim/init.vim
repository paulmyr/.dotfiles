""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                                                                                                            "
"   iiii                     iiii          tttt                                             iiii                             "
"  i::::i                   i::::i      ttt:::t                                            i::::i                            "
"   iiii                     iiii       t:::::t                                             iiii                             "
"                                       t:::::t                                                                              "
" iiiiiiinnnn  nnnnnnnn    iiiiiiittttttt:::::ttttttt            vvvvvvv           vvvvvvviiiiiii    mmmmmmm    mmmmmmm      "
" i:::::in:::nn::::::::nn  i:::::it:::::::::::::::::t             v:::::v         v:::::v i:::::i  mm:::::::m  m:::::::mm    "
"  i::::in::::::::::::::nn  i::::it:::::::::::::::::t              v:::::v       v:::::v   i::::i m::::::::::mm::::::::::m   " 
"  i::::inn:::::::::::::::n i::::itttttt:::::::tttttt               v:::::v     v:::::v    i::::i m::::::::::::::::::::::m   "
"  i::::i  n:::::nnnn:::::n i::::i      t:::::t                      v:::::v   v:::::v     i::::i m:::::mmm::::::mmm:::::m   "
"  i::::i  n::::n    n::::n i::::i      t:::::t                       v:::::v v:::::v      i::::i m::::m   m::::m   m::::m   "
"  i::::i  n::::n    n::::n i::::i      t:::::t                        v:::::v:::::v       i::::i m::::m   m::::m   m::::m   "
"  i::::i  n::::n    n::::n i::::i      t:::::t    tttttt               v:::::::::v        i::::i m::::m   m::::m   m::::m   "
" i::::::i n::::n    n::::ni::::::i     t::::::tttt:::::t                v:::::::v        i::::::im::::m   m::::m   m::::m   "
" i::::::i n::::n    n::::ni::::::i     tt::::::::::::::t ......          v:::::v         i::::::im::::m   m::::m   m::::m   "
" i::::::i n::::n    n::::ni::::::i       tt:::::::::::tt .::::.           v:::v          i::::::im::::m   m::::m   m::::m   "
" iiiiiiii nnnnnn    nnnnnniiiiiiii         ttttttttttt   ......            vvv           iiiiiiiimmmmmm   mmmmmm   mmmmmm   "
"                                                                                                                            "
"                                                                                                                            "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Load plugins
source ~/.config/nvim/plugins.vim

" Plugin configurations
source ~/.config/nvim/plug-config/lsp-config.vim
source ~/.config/nvim/plug-config/nerdtree-config.vim
source ~/.config/nvim/plug-config/fzf-config.vim
luafile ~/.config/nvim/plug-config/compe-config.lua
luafile ~/.config/nvim/plug-config/gitsigns-config.lua

" Language servers
luafile ~/.config/nvim/lua/lsp/python-ls.lua
luafile ~/.config/nvim/lua/lsp/latex-ls.lua
luafile ~/.config/nvim/lua/lsp/lua-ls.lua

" Nvim config
source ~/.config/nvim/config.vim
luafile ~/.config/nvim/lua/keymaps.lua

