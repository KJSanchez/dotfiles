syntax on

let leader = ","

inoremap kj <esc>

set number
set mouse=a
set shiftwidth=4
set tabstop=4
set expandtab
set nobackup
set nowrap
set incsearch
set ignorecase
set showcmd
set hlsearch


call plug#begin()
    Plug 'junegunn/fzf.vim'
    Plug 'tomtom/tcomment_vim'
    Plug 'davidhalter/jedi-vim'
call plug#end()
