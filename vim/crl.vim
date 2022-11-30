" $ cp crl.vim ~/.vim/syntax/crl.vim
" $ grep '.crl' ~/.vimrc
" autocmd BufNewFile,BufRead *.crl setlocal filetype=crl

if exists("b:current_syntax")
    finish
endif

syn match Operator "[(){}\[\];\\@=+\-*/%]"
syn match Number   "\<[0-9]\+\>"

syn match crlSpecial contained "\\\(n\|\"\|\\\)"
syn region String start=+"+ skip=+\\"+ end=+"+ contains=crlSpecial

hi def link crlSpecial SpecialChar

" NOTE: See `http://vimdoc.sourceforge.net/htmldoc/syntax.html`.
syn keyword Conditional
    \ if
    \ else
syn keyword Statement
    \ let
syn keyword Function
    \ spawn
    \ kill
    \ send
    \ receive
    \ channel
    \ printf

syn keyword Todo FIXME NOTE TODO contained

syn match  Comment "#.*"                 contains=Todo
syn region Comment start="/\*" end="\*/" contains=Todo

let b:current_syntax = "crl"
