runtime! archlinux.vim

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
set showcmd		" Show (partial) command in status line.
set showmatch		" Show matching brackets.
set ignorecase		" Do case insensitive matching
set smartcase		" Do smart case matching
set incsearch		" Incremental search
set autowrite		" Automatically save before commands like :next and :make
set hidden             " Hide buffers when they are abandoned
set nocompatible
set ruler

set background=light

" Set the tab-space to 2
set ts=2

" Set the line numbers
set relativenumber
set number

" Auto indent
set cindent
set smartindent
set autoindent
set tabstop=2
set shiftwidth=2
set cinkeys=0{,0},:,0#,!^F

" Scroll padding
set scrolloff=10

" Highlight search results
set hlsearch

" put on the syntax!
syntax on

" Source a auto completion configuration file if available
if filereadable("/etc/vim/autocomplete.vim")
  source /etc/vim/autocomplete.vim
endif

" Map :w!! to ask for sudo password and save as root
cmap w!! %!sudo tee > /dev/null %

" Auto save folds
au BufWinLeave ?* mkview
au BufWinEnter ?* silent loadview

" Easy NERDTree access
cmap NT NERDTreeToggle

" Yay colors ^_^
set t_Co=256
colorscheme delek
" The default colours for searches are a bit hard to see
hi Search ctermfg=0 ctermbg=11

