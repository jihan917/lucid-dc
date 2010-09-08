"vim configuration file. Copyleft 2009, 2010. Ji Han.
""""""""""""""""""""""""""""""""""""""""""""""""""""""

set nocompatible

"encoding and language
set encoding=utf-8
set fileencodings=ucs-bom,utf-8,gbk,big5,euc-jp,euc-kr,latin1

language message en_US.utf-8
set langmenu=en_US.utf-8

"display
if has("win32") && has("gui_running")
    set guifont=Consolas:h10
    set guifontwide=NSimsun:h10
    set guioptions-=T
    "colorscheme koehler
endif
syntax enable

set nowrap

set number
set ruler
set showcmd

set showmatch

set list
set listchars=tab:>-,trail:-

"search
set incsearch
set ignorecase
set smartcase

"indentation
set cindent
set autoindent
set smartindent

"tab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab smarttab

"backspace
set backspace=indent,eol,start
set whichwrap=b,s,<,>,[,]

"filetype plugins
filetype plugin indent on

"ctags
set tags=tags;/

"taglist
let Tlist_Exit_OnlyWindow=1
let Tlist_Sort_Type="name"

"OmniCppComplete
let OmniCpp_DisplayMode=1
let OmniCpp_MayCompleteScope=1

