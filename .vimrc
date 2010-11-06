"vim configuration file
"(C) Copyright 2009, 2010, Ji Han (jihan917<at>yahoo<dot>com)
"free to distribute under the GPL license
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"encoding and language
""""""""""""""""""""""

if has("multi_byte_encoding")
    "default encoding (usu. current locale) is multi-byte
    set fileencodings=ucs-bom,utf-8,default,gbk,big5,euc-jp,euc-kr,latin1
else
    set fileencodings=ucs-bom,utf-8,gbk,big5,euc-jp,euc-kr,default
endif

set encoding=utf-8
if has("win32")
    setglobal bomb
endif

language message en_US.utf-8
set langmenu=en_US.utf-8


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"vim is vi _improved_.
""""""""""""""""""""""

"not limited to vi-compatible.
set nocompatible

"turn off (vulnerable) modeline parsing.
set nomodeline

"visual bell instead of the (annoying) ring bell.
set visualbell t_vb=

"remember last cursor position
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

"command history
set history=800

"enable filetype specific configuration.
filetype plugin indent on

"tab expansion
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab
au FileType make setl ts=8 sts=8 sw=8 noet
au FileType css,html,javascript,xhtml,xml,xsd,xslt setl ts=2 sts=2 sw=2 noet


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"line transposing
nmap <M-up> <Plug>upAndDownUp
nmap <M-down> <Plug>upAndDownDown
imap <M-up> <Plug>upAndDownInsertUp
imap <M-down> <Plug>upAndDownInsertDown
vmap <M-up> <Plug>upAndDownVisualUp
vmap <M-down> <Plug>upAndDownVisualDown


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"time stamps
""""""""""""

"press `Alt-I, T' to insert current date and time in default format.
imap <silent> <M-i>t <C-R>=strftime("%c")<CR>
nmap <M-i>t a<M-i>t<Esc>
vmap <M-i>t <Esc><M-i>tgv

"type `today<Tab>' to insert current date,
"formatted like `Sunday, October 10, 2010'.
imap <silent> today<Tab> <C-R>=strftime("%A, %B ") . join(map(split(strftime("%d %Y")), 'abs(v:val)'), ', ')<CR>

"type `now<Tab>' to insert current time,
"formatted like `10:10 AM'.
imap <silent> now<Tab> <C-R>=join(map(split(strftime("%I")), 'abs(v:val)')) . strftime(":%M %p")<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"behave a bit more like `normal' GUI application
""""""""""""""""""""""""""""""""""""""""""""""""

"use system clipboard
set clipboard=unnamed

"allow backspacing over autoindent, line breaks and the start of insert
set backspace=indent,eol,start

"allow <Left>/<Right> to move across line boundary
set whichwrap=b,s,<,>,[,]

"automatic backup
set autowriteall
set backup backupdir=./.backup,~/.backup,.,$TEMP
au BufWritePre * let &bex = '_' . strftime("%Y%m%d%H%M%S")

"minibufexpl
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"visual appearance and format
"""""""""""""""""""""""""""""

"no (ugly) toolbar
set guioptions-=T

"set guifont
function SetGuiFont ()
    try | set guifont=Monaco:h10 | return | catch | endtry
    try | set guifont=Consolas:h12 | return | catch | endtry
    try | set guifont=Lucida\ Console:h14 | return | catch | endtry
endfunction
call SetGuiFont()

"gui font for wide characters (needs to be monospace)
try | set guifontwide=NSimsun | catch | endtry

"do not wrap lines that don't fit the window witdh
set nowrap
"press `Alt-O, W' to toggle wrap
nmap <M-o>w :setlocal wrap! wrap?<CR>
imap <M-o>w <C-o><M-o>w
vmap <M-o>w <Esc><M-o>wgv

"automatically break lines (actually inserting <EOL>s)
setglobal textwidth=78


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"visual cues
""""""""""""

"show (partial) command
set showcmd

"show line numbers
set number
"press `Alt-O, N' to toggle line numbers
nmap <M-o>n :setlocal number! number?<CR>
imap <M-o>n <C-o><M-o>n
vmap <M-o>n <Esc><M-o>ngv

"show cursor position
set ruler

"highlight the current line
set cursorline
"press `Alt-O, C' to toggle highlighting current line
nmap <M-o>c :setlocal cursorline! cursorline?<CR>
imap <M-o>c <C-o><M-o>c
vmap <M-o>c <Esc><M-o>cgv

"use printable chars to list tabs and trailing spaces
set list listchars=tab:>-,trail:-
"press `Alt-O, L' to toggle listing of tabs and trailing spaces.
nmap <M-o>l :setlocal list! list?<CR>
imap <M-o>l <C-o><M-o>l
vmap <M-o>l <Esc><M-o>lgv
au FileType css,html,javascript,xhtml,xml,xsd,xslt setlocal nolist


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"search
"""""""

"highlight all matches when searching
set hlsearch
"press `Alt-O, H' to toggle highlight search
nmap <M-o>h :setlocal hlsearch! hlsearch?<CR>
imap <M-o>h <C-o><M-o>h
vmap <M-o>h <Esc><M-o>hgv

"incremental search
set incsearch

"case insensitive for all-lowercase search
set ignorecase smartcase
"press `Alt-O, I' to toggle case sensitivity of search
nmap <M-o>i :setlocal ignorecase! ignorecase?<CR>
imap <M-o>i <C-o><M-o>i
vmap <M-o>i <Esc><M-o>igv

if has("unix")
    "let grep always print filename
    set grepprg=grep\ -nH\ $*\ /dev/null
endif

"search current word in files
nmap <S-F3> :grep <cword> %:p:h/*<CR>

if exists('+shellslash')
    "use forward slash as separator
    set shellslash
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"intellisense
"""""""""""""

"parenthesis matching
set showmatch
au FileType html,xhtml,xml,xsd,xslt setlocal matchpairs+=<:>

"automatic completion of punctuations matched in pairs
inoremap ( ()<Left>
inoremap [ []<Left>
inoremap { {}<Left>

"automatic completion of double quotes
au FileType * inoremap <buffer> " ""<Left>
au FileType vim iunmap <buffer> "

"indentation
set autoindent smartindent
au FileType c,cpp,java setlocal cindent

"code folding
set foldmethod=marker

"syntax highlighting
if has("gui_running")
    "let's do a lottery
    let s:schemes = ['desert', 'murphy', 'rubyblue', 'torte']
    exec "colorscheme " . s:schemes[strftime("%S") / 15]
else
    "every colorscheme sucks under terminal. the default sucks less.
endif
syntax enable

"add jQuery support to javascript syntax highlighting
au BufRead,BufNewFile jquery.*.js set ft=javascript syntax=jquery

"use XML compliant HTML for TOhtml
let g:html_use_xhtml = 1


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"ctags
set tags=tags;/

"taglist
let Tlist_Exit_OnlyWindow=1
let Tlist_Sort_Type="name"

"Omni_completion
set ofu=syntaxcomplete#Complete
"OmniCppComplete
let OmniCpp_NamespaceSearch=1
let OmniCpp_GlobalScopeSearch=1
let OmniCpp_ShowAccess=1
let OmniCpp_ShowPrototypeInAbbr=1
let OmniCpp_MayCompleteDot=1
let OmniCpp_MayCompleteArrow=1
let OmniCpp_MayCompleteScope=1

"supertab
let g:SuperTabDefaultCompletionType="context"


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"Last Updated: Saturday, November 6, 2010
".vimrc ends here
