" Plugins {{{
call plug#begin('~/.vimfiles/plugged')

Plug 'w0rp/ale'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tomtom/tcomment_vim'
Plug 'scrooloose/nerdtree'
Plug 'Raimondi/delimitMate'
Plug 'vim-airline/vim-airline'
Plug 'junegunn/vim-easy-align'
Plug 'easymotion/vim-easymotion'
Plug 'ludovicchabant/vim-gutentags'
Plug 'terryma/vim-multiple-cursors'
Plug 'christoomey/vim-tmux-navigator'
" Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'easymotion/vim-easymotion'

if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
    Plug 'Shougo/deoplete.nvim'
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
endif

Plug 'rakr/vim-one'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

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

let g:fzf_buffers_jump = 1

" FZF Configs from https://github.com/junegunn/fzf.vim {{{
" Command for git grep
" - fzf#vim#grep(command, with_column, [options], [fullscreen])
command! -bang -nargs=* GGrep
  \ call fzf#vim#grep(
  \   'git grep --line-number '.shellescape(<q-args>), 0,
  \   { 'dir': systemlist('git rev-parse --show-toplevel')[0] }, <bang>0)

" Override Colors command. You can safely do this in your .vimrc as fzf.vim
" will not override existing commands.
command! -bang Colors
  \ call fzf#vim#colors({'left': '15%', 'options': '--reverse --margin 30%,0'}, <bang>0)

" Augmenting Ag command using fzf#vim#with_preview function
"   * fzf#vim#with_preview([[options], [preview window], [toggle keys...]])
"     * For syntax-highlighting, Ruby and any of the following tools are required:
"       - Bat: https://github.com/sharkdp/bat
"       - Highlight: http://www.andre-simon.de/doku/highlight/en/highlight.php
"       - CodeRay: http://coderay.rubychan.de/
"       - Rouge: https://github.com/jneen/rouge
"
"   :Ag  - Start fzf with hidden preview window that can be enabled with "?" key
"   :Ag! - Start fzf in fullscreen and display the preview window above
command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)

" Similarly, we can apply it to fzf#vim#grep. To use ripgrep instead of ag:
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

" Likewise, Files command with preview window
command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
"
" }}}

" Disable YCM preview buffer
" set completeopt=noinsert,menuone,noselect

" }}}
" Key Bindings {{{

map <Leader> <Plug>(easymotion-prefix)

inoremap <silent> jk <ESC>

" <TAB> Completion
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

nnoremap <Leader>h :noh<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
nnoremap <Leader>vv :vsplit<CR>
nnoremap <Leader>vh :vsplit<CR>

nnoremap <Leader>bb  :Buffers<CR>
nnoremap <Leader>ll  :Lines<CR>
nnoremap <Leader>lb  :BLines<CR>

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
nnoremap <C-t> :FZF ~<CR>
nnoremap <C-o> :NERDTreeToggle<CR>

" Terminal Bindings
if has('nvim')
    tnoremap <ESC> <C-\><C-n>

    tnoremap <C-h> <C-\><C-n><C-w>h
    tnoremap <C-j> <C-\><C-n><C-w>j
    tnoremap <C-k> <C-\><C-n><C-w>k
    tnoremap <C-l> <C-\><C-n><C-w>l
endif
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
" vim:fdm=marker
