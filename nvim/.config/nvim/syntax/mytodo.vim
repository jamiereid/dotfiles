" Vim syntax file
" Language: Custom Todo Format
" Maintainer: Jamie Reid
" Latest Revision: 2019-03-31

if exists("b:current_syntax")
    finish
endif

" Keywords

" Matches

"" Headings
syn match deemphasizeMatch   '\~.*'
syn match headingMatch       '^[[:alnum:]].*::'
syn match subHeadingMatch    '  [[:alnum:]].*::'
syn match subTaskMatch       ' ->.*'
syn match plusMatch          '\m+\w*'
syn match atMatch            '\m@\w*'
syn match bangMatch          '\m!\w*'
syn match poundMatch         '\m#\w*'

" Regions


let b:current_syntax = "mytodo"

hi mytodoHeading      guifg=#FFEC8B guibg=NONE guisp=NONE gui=bold ctermfg=NONE ctermbg=NONE cterm=NONE
hi mytodoSubHeading   guifg=#CDBE70 guibg=NONE guisp=NONE gui=bold ctermfg=NONE ctermbg=NONE cterm=NONE
hi mytodoSubTask      guifg=#8B814C guibg=NONE guisp=NONE gui=bold ctermfg=NONE ctermbg=NONE cterm=NONE
hi mytodoDeemphasize  guifg=#BEBEBE guibg=NONE guisp=NONE gui=bold ctermfg=NONE ctermbg=NONE cterm=NONE
hi mytodoPlus         guifg=#008B00 guibg=NONE guisp=NONE gui=bold ctermfg=NONE ctermbg=NONE cterm=NONE
hi mytodoAt           guifg=#551A8B guibg=NONE guisp=NONE gui=bold ctermfg=NONE ctermbg=NONE cterm=NONE
hi mytodoBang         guifg=#8B0000 guibg=NONE guisp=NONE gui=bold ctermfg=NONE ctermbg=NONE cterm=NONE
hi mytodoPound        guifg=#00CDCD guibg=NONE guisp=NONE gui=bold ctermfg=NONE ctermbg=NONE cterm=NONE

hi def link deemphasizeMatch  mytodoDeemphasize
hi def link headingMatch      mytodoteading
hi def link subHeadingMatch   mytodoSubHeading
hi def link subTaskMatch      mytodoSubTask
hi def link plusMatch         mytodoPlus
hi def link atMatch           mytodoAt
hi def link bangMatch         mytodoBang
hi def link poundMatch        mytodoPound
