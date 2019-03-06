" Plugins {{{
call plug#begin('~/vimfiles/plugged')

Plug 'w0rp/ale'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tomtom/tcomment_vim'
Plug 'scrooloose/nerdtree'
Plug 'Raimondi/delimitMate'
Plug 'itchyny/lightline.vim'
Plug 'vim-airline/vim-airline'
Plug 'junegunn/vim-easy-align'
Plug 'easymotion/vim-easymotion'
Plug 'ludovicchabant/vim-gutentags'
Plug 'terryma/vim-multiple-cursors'

if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
endif

" Plug '~/.ycm/'
" Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }

Plug 'rakr/vim-one'

call plug#end()
""" }}}
" Plugin Configurations {{{
let g:airline_theme ='one'

" let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_confg.py'
" let g:ycm_min_num_of_chars_for_completion = 1
" let g:ycm_max_num_candidates = 10

let g:deoplete#enable_at_startup = 1


let g:ale_sign_column_always = 1

" Disable YCM preview buffer
set completeopt-=preview

" }}}
" Editor Configurations {{{

colorscheme one
set background=dark
filetype plugin on

if has("gui_running")
    set guifont=SF\ Mono:h10
endif

set encoding=utf-8

set number
set nobackup
set nowritebackup
set splitright
set splitbelow
set smartindent
set mouse=a
set tabstop=4
set shiftwidth=4
set expandtab
set laststatus=2
set wildmode=full
set backspace=eol,start,indent
set whichwrap+=<,>,h,l
set smartcase
set hlsearch
set incsearch
" set lazyredraw
set showmatch
set noerrorbells
set novisualbell
set t_vb=
set tm=500
set autoread
set ignorecase
set wrapscan
set guioptions=r
highlight LineNr guifg=#929292

let mapleader=","

" }}}
" Key Bindings {{{

inoremap <silent> jk <ESC>

" <TAB> Completion
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"

nnoremap <leader>h  :noh<CR>
nnoremap <leader>te :term<CR>
nnoremap <leader>w  :w<CR>
nnoremap <leader>q  :q<CR>

nnoremap ; :
nnoremap : ;

nnoremap <C-a> ^
nnoremap <C-e> $

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <Leader>vv :vsplit<CR>
nnoremap <Leader>vh :vsplit<CR>

vnoremap <C-a> ^
vnoremap <C-e> $

nnoremap <C-p> :Files<CR>
nnoremap <C-o> :NERDTreeToggle<CR>

" }}}
" vim:fdm=marker
