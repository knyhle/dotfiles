" Kenny's .nvim/init.vim
" Plug {{{
"
" Specify a directory for plugins
" " - For Neovim: ~/.local/share/nvim/plugged
" " - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.config/nvim/plugged')

Plug 'Raimondi/delimitMate'
Plug 'easymotion/vim-easymotion'
Plug 'airblade/vim-gitgutter'
Plug 'jlanzarotta/bufexplorer'

Plug 'itchyny/lightline.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Plug 'junegunn/rainbow_parentheses.vim'

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'

Plug 'wincent/terminus'

Plug 'yegappan/mru'
Plug 'sheerun/vim-polyglot'
Plug 'Yggdroot/indentLine'
Plug 'terryma/vim-multiple-cursors'
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'

Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'joshdick/onedark.vim'


Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'scrooloose/nerdtree'
Plug 'ludovicchabant/vim-gutentags'

" Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Plug 'roxma/nvim-completion-manager'

call plug#end()            " required

filetype plugin indent on

" }}}
" Startup {{{

" function! PreFire()
"   set t_vb=
"   " call rainbow_parentheses#activate()
" endfunction

autocmd VimEnter * set t_vb=

let g:deoplete#enable_at_startup = 1
" let g:deoplete#tag#cache_limit_size = 5000000
let g:deoplete#auto_complete_delay = 40

if exists('&signcolumn')  " Vim 7.4.2201
  set signcolumn=yes
else
  let g:gitgutter_sign_column_always = 1
endif

" }}}
" ColorScheme {{{

if (has("nvim"))
"For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if (has("termguicolors"))
    set termguicolors
endif

set background=dark

let g:dracula_italic = 0
let g:lightline = {
      \ 'colorscheme': 'Dracula',
      \ }
colorscheme dracula

" let g:airline_theme='onedark'
" colorscheme onedark

syntax on


let g:airline#extensions#tabline#enabled = 1
set statusline+=%{gutentags#statusline()}

" }}}
" Basics {{{

set foldmethod=marker
set hidden
set lazyredraw


set updatetime=100
let g:bufExplorerShowRelativePath = 1


set nocompatible              " be iMproved, required
set noerrorbells
set visualbell

set omnifunc=syntaxcomplete#Complete
set completeopt=longest,menuone,preview

set autoread

set noswapfile

set foldenable
set encoding=utf-8

set laststatus=2
set autoread                    " Automatically reread changed files without asking me anything
set number
set mouse=a
set hidden
set nobackup
set noshowmode
set title
set smartcase
set showcmd
set ttyfast

set ignorecase
set backspace=indent,eol,start

" show existing tab with 4 spaces width
set smarttab
set tabstop=4
set shiftwidth=4
set expandtab
set softtabstop=4

set copyindent
set autoindent
set smartindent
set preserveindent

set list listchars=tab:»\ ,trail:·

let g:delimitMate_expand_cr = 1
let g:delimitMate_expand_space = 1

let mapleader=','

set colorcolumn=80
set history=100
set nowrap

set hlsearch
set incsearch

set showmatch
set splitright
set splitbelow

set ruler
set list
set listchars=nbsp:¬,tab:»·,trail:·

set wildmenu
set wildmode=longest:full,full
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe  " Windows

source $HOME/.vim/autoload/cscope.vim

let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
" }}}
" Keybindings / Keymappings {{{
nmap <C-\> :NERDTreeToggle<cr>
nnoremap <leader>rc :e $HOME/.config/nvim/init.vim<cr>


map <leader>o :MRU<cr>
nmap <leader>ha <Plug>GitGutterStageHunk
nmap <leader>hr <Plug>GitGutterUndoHunk
nmap <leader>hp <Plug>GitGutterPreviewHunk
imap <C-b> <C-x>

nnoremap j gj
nnoremap k gk

" EasyAlign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

nnoremap <leader><SPACE> :noh<cr>

nnoremap <leader>s  : cs find s
nnoremap <leader>c  : cs find c
nnoremap <leader>g  : cs find g

nnoremap <leader>l  : BufExplorer<cr>

nnoremap <leader>t  : cs find t
nnoremap <leader>tt : TagbarToggle<cr>
nnoremap <leader>to : TagbarOpenAutoClose<cr>
nnoremap <leader>tp : TagbarTogglePause<cr>
nnoremap <leader>tc : TagbarClose<cr>
nnoremap <leader>tn : tabnew<cr>
nnoremap <leader>te : tabedit %<cr>
nnoremap <leader>tm : term<cr>

nnoremap <leader>bd : bd<cr>


nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nmap ; :
vmap ; :

nmap <C-a> ^
vmap <C-a> ^
nmap <C-e> $
vmap <C-e> $

nnoremap <silent> <leader>gc : Gcommit<cr>

nnoremap <silent> <leader>f  : Files<cr>
nnoremap <silent> <leader>cc : Commits<cr>
nnoremap <silent> <leader>cb : BCommits<cr>
nnoremap <silent> <leader>e  : Lines<cr>
nnoremap <silent> <leader>be : BLines<cr>
nnoremap <silent> <leader>q  : q<cr>
nnoremap <silent> <leader>te : term<cr>



nnoremap q; q:
" nnoremap <silent> <leader>h  q:

nnoremap <silent> <leader>gse : GFiles?<cr>
nnoremap <silent> <leader>gss : Gstatus<cr>
nnoremap <silent> <leader>gd  : Gdiff<cr>

" Insert mode completion
imap <c-a><c-f> <plug>(fzf-complete-path)
imap <c-a><c-j> <plug>(fzf-complete-file-ag)
imap <c-a><c-l> <plug>(fzf-complete-line)

" autocmd FileType c,h,cpp,java,php autocmd BufWritePre <buffer> %s/\s\+$//e

nmap <leader>; : sp<cr>
nmap <leader>' : vs<cr>

inoremap jk <ESC>
nnoremap <cr> i<cr><ESC>

" Remap for destroying trailing whitespace cleanly
nnoremap <leader>w :w<cr>
" }}}
" Easy Motion Movements {{{

let g:EasyMotion_smartcase = 1
let g:EasyMotion_do_mapping = 0 " Disable default mappings

map <leader> <Plug>(easymotion-prefix)

nmap s <Plug>(easymotion-s)

map <leader>j <Plug>(easymotion-j)
map <leader>k <Plug>(easymotion-k)
" map <leader>w <Plug>(easymotion-w)
" map <leader>e <Plug>(easymotion-e)

" }}}
" Fzf settings {{{

" Customize fzf colors to match your color scheme
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

" }}}
