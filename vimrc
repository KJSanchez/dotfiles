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
    Plug 'cocopon/iceberg.vim'
call plug#end()

set background=dark
colorscheme iceberg

" autocmd BufReadPost *
"      \ if line("'\"") > 0 && line("'\"") <= line("$") |
"      \   exe "normal! g`\"" |
"      \ endif
