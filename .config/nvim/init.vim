set nocompatible              " be iMproved, required
filetype off                  " required

" vundle stuff -- :h vundle
" ------------------------------------------
set rtp+=~/.config/nvim/bundle/Vundle.vim
call vundle#begin('~/.config/nvim/bundle')
Plugin 'VundleVim/Vundle.vim'

" PYTHON
Plugin 'vim-scripts/indentpython.vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'nvie/vim-flake8'

" GENERAL
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary'
Plugin 'itchyny/lightline.vim'
Plugin 'scrooloose/nerdtree'

call vundle#end()
filetype plugin indent on
" ------------------------------------------

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

set shiftwidth=4
set tabstop=4
set smarttab
set autoindent
set smartindent

set ignorecase
set smartcase

set number relativenumber
