" rubyblue.vim
" Vim color file based on the ruby blue TextMate theme by John W. Long
" (http://wiseheartdesign.com/articles/2006/03/11/ruby-blue-textmate-theme/)
" Author: Ji Han (jihan917<at>yahoo<dot>com)
" Last Updated: Sunday, November 7, 2010

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name = "rubyblue"

hi Comment guifg=#428BDD
hi Constant guifg=#8AA6C1
hi Cursor guifg=black guibg=white
hi CursorLine guibg=darkslategray
hi DiffChange guibg=#121E31 guifg=white
hi DiffText guibg=#968C44 guifg=white
hi DiffAdd guibg=#6DB04E guifg=white
hi DiffDelete guibg=#7A3942 guifg=white
hi Error guifg=red guibg=#121E31
hi ErrorMsg guifg=#F8BB00 guibg=#121E31
hi Function guifg=white
hi Identifier guifg=#8AA6C1
hi Ignore guifg=#404040
hi IncSearch guifg=black guibg=yellow
hi LineNr guifg=pink
hi ModeMsg guifg=yellow gui=none
hi MoreMsg guifg=yellow gui=none
hi Normal guifg=white guibg=#121E31
hi NonText guifg=#4682B4
hi Number guifg=#EDDD3D
hi Preproc guifg=#B6B9F9
hi Search guifg=black guibg=yellow
hi Special guifg=#468434
hi Statement guifg=#F8BB00 gui=none
hi String guifg=#1DC116
hi Todo guifg=#428BDD guibg=#121E31 gui=underline
hi Type guifg=#F8BB00 gui=none
hi Visual guibg=#38566F
hi WarningMsg guifg=#428BDD guibg=#121E31

"rubyblue.vim ends here
