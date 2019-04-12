
" Bootstrap {{{

" https://stackoverflow.com/a/11917772
let mapleader=","

" }}}

" Plugins {{{
call plug#begin('~/.vimfiles/plugged')

" Plug 'w0rp/ale'
Plug 'zefei/vim-wintabs'

Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
" Plug 'tpope/vim-fugitive'
Plug 'sheerun/vim-polyglot'
Plug 'majutsushi/tagbar'
Plug 'leafgarland/typescript-vim'
Plug 'tomtom/tcomment_vim'
Plug 'scrooloose/nerdtree'
Plug 'Raimondi/delimitMate'
Plug 'vim-airline/vim-airline'
Plug 'easymotion/vim-easymotion'
" Plug 'ludovicchabant/vim-gutentags'
Plug 'terryma/vim-multiple-cursors'
Plug 'christoomey/vim-tmux-navigator'
Plug 'junegunn/vim-easy-align'
Plug 'vim-syntastic/syntastic'
" Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'easymotion/vim-easymotion'

Plug 'kaicataldo/material.vim'
Plug 'drewtempelmeyer/palenight.vim'

Plug 'rust-lang/rust.vim'

Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install() }}

" if has('nvim')
"     Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" else
"     Plug 'Shougo/deoplete.nvim'
"     Plug 'roxma/nvim-yarp'
"     Plug 'roxma/vim-hug-neovim-rpc'
" endif
"
Plug 'rakr/vim-one'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

call plug#end()
""" }}}

" Plugin Configurations {{{
let g:airline_theme ='one'
let g:airline_section_error = '%{airline#util#wrap(airline#extensions#coc#get_error(),0)}'
let g:airline_section_warning = '%{airline#util#wrap(airline#extensions#coc#get_warning(),0)}'

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:tagbar_sort = 0
let g:tagbar_compact = 1
let g:tagbar_autofocus = 1

" TagBar Languages {{{

" Typescript {{{
let g:tagbar_type_typescript = {
  \ 'ctagstype': 'typescript',
  \ 'kinds': [
    \ 'c:classes',
    \ 'n:modules',
    \ 'f:functions',
    \ 'v:variables',
    \ 'v:varlambdas',
    \ 'm:members',
    \ 'i:interfaces',
    \ 'e:enums',
  \ ],
  \ 'sort' : 0
\ }
" }}}
" Objective-C {{{
" add a definition for Objective-C to tagbar
let g:tagbar_type_objc = {
    \ 'ctagstype' : 'ObjectiveC',
    \ 'kinds'     : [
        \ 'i:interface',
        \ 'I:implementation',
        \ 'p:Protocol',
        \ 'm:Object_method',
        \ 'c:Class_method',
        \ 'v:Global_variable',
        \ 'F:Object field',
        \ 'f:function',
        \ 'p:property',
        \ 't:type_alias',
        \ 's:type_structure',
        \ 'e:enumeration',
        \ 'M:preprocessor_macro',
    \ ],
    \ 'sro'        : ' ',
    \ 'kind2scope' : {
        \ 'i' : 'interface',
        \ 'I' : 'implementation',
        \ 'p' : 'Protocol',
        \ 's' : 'type_structure',
        \ 'e' : 'enumeration'
    \ },
    \ 'scope2kind' : {
        \ 'interface'      : 'i',
        \ 'implementation' : 'I',
        \ 'Protocol'       : 'p',
        \ 'type_structure' : 's',
        \ 'enumeration'    : 'e'
    \ }
\ }

" }}}

" }}}

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

" " Likewise, Files command with preview window
" command! -bang -nargs=? -complete=dir Files
"   \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
" "


" }}}
" }}}

" Key Bindings {{{

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

nmap <Leader>bn <Plug>(wintabs_next)
nmap <Leader>bp <Plug>(wintabs_previous)
nmap <Leader>bc <Plug>(wintabs_close)

" map <Leader> <Plug>(easymotion-prefix)
nmap <SPACE> <Plug>(easymotion-s)

inoremap <silent> jk <ESC>
inoremap <C-g> <ESC>

" Use <c-space> for trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K for show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
" autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" <TAB> Completion
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

nnoremap <Leader>w  :w<CR>
nnoremap <Leader>q  :q<CR>
nnoremap <Leader>h  :noh<CR>
nnoremap <Leader>vv :vsplit<CR>
nnoremap <Leader>vh :vsplit<CR>

" FZF Mappings
nnoremap <C-p>      :Files<CR>
nnoremap <C-i>      :TagbarOpen fj<CR>
" nnoremap <C-t>      :FZF ~<CR>
nnoremap <C-f> <Plug>(wintabs_next)
nnoremap <Leader>ll :Lines<CR>
nnoremap <Leader>bl :BLines<CR>
nnoremap <Leader>bp <C-^>

nnoremap <Leader>et :edit $MYVIMRC<CR>

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

nnoremap <C-o> :NERDTreeToggle<CR>

" Terminal Bindings
if has('nvim')
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
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
" let g:material_theme_style = 'palenight'

filetype plugin on

if has('gui_running')
    set guifont=SF\ Mono:h14
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
" Smaller updatetime for CursorHold & CursorHoldI
" set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

" Trim trailing whitespace for only certain filetypes
autocmd FileType c,cpp,java,php,ts,js,md autocmd BufWritePre <buffer> %s/\s\+$//e

set number
set virtualedit=all
set nobackup
set nowritebackup
set noswapfile
set splitright
set splitbelow
set smartindent
set mouse=a
set tabstop=2
set shiftwidth=2
set expandtab
set laststatus=2
set wildmode=full
set backspace=eol,start,indent
set whichwrap+=<,>,h,l
set smartcase
set nohlsearch
set incsearch
set lazyredraw
" set showmatch
set visualbell
set t_vb=
set tm=500
set autoread
set ignorecase
set wrapscan
set guioptions=r

highlight LineNr guifg=#929292

" Set cursor
set guicursor=n-v-c:block-Cursor/lCursor,i-ci-ve:hor50

" }}}

" vim:fdm=marker
