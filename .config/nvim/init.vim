set nocompatible              " be iMproved, required
filetype off                  " required
" set the runtime path to include Vundle and initialize
set rtp+=~/.config/nvim/bundle/Vundle.vim
" call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
call vundle#begin('~/.config/nvim/bundle')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" PYTHON
Plugin 'vim-scripts/indentpython.vim'
" Bundle 'Valloric/YouCompleteMe'
Plugin 'vim-syntastic/syntastic'
Plugin 'nvie/vim-flake8'

" GENERAL
Plugin 'tpope/vim-surround'
Plugin 'itchyny/lightline.vim'
Plugin 'scrooloose/nerdtree'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

set laststatus=2
set background=dark
set nowrap
nnoremap <CR> :nohlsearch<CR>

syntax on
filetype indent plugin on

set splitbelow
set splitright

" basic sanity
nnoremap Y y$
nnoremap vv V
nnoremap V <C-V>$

" split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" alt-0
nnoremap º :terminal ++rows=7<CR>
tnoremap <Esc> <C-W>N

" alt-1
nnoremap ¡ :NERDTreeToggle<CR>

set shiftwidth=4
set tabstop=4
set smarttab
set autoindent
set smartindent

set ignorecase
set smartcase
