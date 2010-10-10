"vim configuration file
"(C) Copyright 2009, 2010, Ji Han (jihan917<at>yahoo<dot>com)
"free to distribute under the GPL license
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"encoding and language
""""""""""""""""""""""

set fileencodings=ucs-bom,utf-8,gbk,big5,euc-jp,euc-kr,latin1

set encoding=utf-8
if has("win32")
    setglobal bomb
endif

language message en_US.utf-8
set langmenu=en_US.utf-8


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"visual appearance and format
"""""""""""""""""""""""""""""

"no (ugly) toolbar
set guioptions-=T

"font
set guifont=Monaco

"do not wrap lines that don't fit the window witdh
set nowrap

"automatic line breaking
setglobal textwidth=78


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"improved user experience
"""""""""""""""""""""""""

"not vi-compatible
set nocompatible

"turn off (vulnerable) modeline parsing
set nomodeline

"visual bell instead of (annoying) bell ring
set visualbell t_vb=

"use system clipboard
set clipboard=unnamed

"allow backspacing over autoindent, line breaks and the start of insert
set backspace=indent,eol,start

"allow <Left>/<Right> to move across line boundary
set whichwrap=b,s,<,>,[,]

"always expand tabs to spaces, except for makefiles
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab
autocmd FileType make setlocal noexpandtab


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"search
"""""""

"highlight all matches when searching
set hlsearch

"incremental search
set incsearch

"case insensitive for all-lowercase search
set ignorecase smartcase


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"timestamps
"""""""""""

"press `Alt-I, T' to insert current date and time
map <M-i>t <Esc>a<C-R>=strftime("%c")<CR><Esc>
imap <M-i>t <C-R>=strftime("%c")<CR>

"type `today<Tab>' to insert current date
imap today<Tab> <C-R>=strftime("%a %b ") . join(map(split(strftime("%d %Y")), 'abs(v:val)'), ' ')<CR>

"type `now<Tab>' to insert current time
imap now<Tab> <C-R>=join(map(split(strftime("%I")), 'abs(v:val)')) . strftime(":%M %p")<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"visual cues
""""""""""""

"show (partial) command
set showcmd

"line numbers
set number

"cursor position
set ruler

"highlight the current line
set cursorline

"make tabs and trailing spaces visible
set list listchars=tab:>-,trail:-


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"intellisense
"""""""""""""

"parenthesis matching
set showmatch
autocmd FileType cpp,html,xhtml,xml,xsd,xslt set matchpairs+=<:>

"automatic indentation
set autoindent smartindent
autocmd FileType c,cpp,java set cindent

"syntax hilighting
colorscheme rubyblue
syntax enable

"code folding
set foldmethod=syntax

filetype plugin indent on

"ctags
set tags=tags;/

"taglist
let Tlist_Exit_OnlyWindow=1
let Tlist_Sort_Type="name"

"OmniCppComplete
let OmniCpp_DisplayMode=1
let OmniCpp_MayCompleteScope=1


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"abbreviations and snippets for C/C++
source abc.vim

