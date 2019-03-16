" Plugins {{{
call plug#begin('~/.vimfiles/plugged')

Plug 'w0rp/ale'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tomtom/tcomment_vim'
Plug 'scrooloose/nerdtree'
Plug 'Raimondi/delimitMate'
Plug 'vim-airline/vim-airline'
Plug 'junegunn/vim-easy-align'
Plug 'easymotion/vim-easymotion'
Plug 'ludovicchabant/vim-gutentags'
Plug 'terryma/vim-multiple-cursors'
Plug 'christoomey/vim-tmux-navigator'

if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
    Plug 'Shougo/deoplete.nvim'
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
endif

Plug 'rakr/vim-one'

call plug#end()
""" }}}
" Plugin Configurations {{{
let g:airline_theme ='one'

let g:deoplete#enable_at_startup = 1
call deoplete#custom#option({
    \ 'max_list': 50,
    \ 'min_pattern_length': 1
\ })

let g:ale_sign_column_always = 1

let g:delimitMate_jump_expansion = 1
let g:delimitMate_expand_cr = 1

" Disable YCM preview buffer
set completeopt=noinsert,menuone,noselect

" }}}
" Editor Configurations {{{

colorscheme one
set background=dark
filetype plugin on

if has("gui_running")
    set guifont=SF\ Mono:h10
endif

if has('termguicolors')
    set termguicolors
endif

if has('nvim')
    set t_ut=
    set t_Co=256
    let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
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
set lazyredraw
set showmatch
set visualbell
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

nnoremap <Leader>h :noh<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
nnoremap <Leader>vv :vsplit<CR>
nnoremap <Leader>vh :vsplit<CR>

nnoremap <Leader>te :Term<CR>

command! -nargs=* Term  split  | resize 20 | terminal <args>
command! -nargs=* VTerm vsplit | resize 20 | terminal <args>

nnoremap ; :
nnoremap : ;

nnoremap <C-a> ^
nnoremap <C-e> $

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

vnoremap <C-a> ^
vnoremap <C-e> $

nnoremap <C-p> :Files<CR>
nnoremap <C-o> :NERDTreeToggle<CR>

" Terminal Bindings
tnoremap <ESC> <C-\><C-n>

tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l

" }}}
" vim:fdm=marker
