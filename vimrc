runtime! archlinux.vim

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
set showmatch   " Show matching brackets.
set ignorecase  " Do case insensitive matching
set smartcase   " Do smart case matching
set incsearch   " Incremental search
set hlsearch    " Highlight search results
set autowrite   " Automatically save before commands like :next and :make
set hidden      " Hide buffers when they are abandoned
set nocompatible
set ruler

set background=light

" Set the tab-space to 2
set ts=2

" Set the line numbers
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

" Underline the current line
set cursorline

" Fire up the syntax!
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

" Yay colors ^_^
set t_Co=256
" The default colours for searches are a bit hard to see
hi Search ctermfg=0 ctermbg=11

hi ColorColumn ctermbg=233
hi CursorLine cterm=bold
hi DbgBreakptLine ctermfg=none ctermbg=235
hi DbgBreakptSign ctermbg=1

" Vim 7.3 and up features
if v:version >= 703
	set relativenumber
	" Display the column separator.
	set colorcolumn=80
endif

" Status Line
set showcmd " Show (partial) command in status line.
set laststatus=2                             " always show statusbar  
set statusline=
set statusline+=%-10.3n\                   " buffer number  
set statusline+=%f\                          " filename   
set statusline+=%h%m%r%w                     " status flags  
set statusline+=\[%{strlen(&ft)?&ft:'none'}] " file type  
set statusline+=%=                           " right align remainder  
set statusline+=0x%-8B                       " character value  
set statusline+=%-14(%l,%c%V%)               " line, character  
set statusline+=%<%P                         " file position  

set wildmenu
set wildmode=list:longest,full

"=============="
" Key bindings "
"=============="

" Note: this _MUST_ be before other keybindings
let g:mapleader = "\<Space>"

" Spell checker for plain text documents
map <leader>s :setlocal spell!<cr>

noremap <Leader><Leader> <C-w>w
noremap <Leader>h <C-w>h
noremap <Leader>j <C-w>j
noremap <Leader>k <C-w>k
noremap <Leader>l <C-w>l

" Bye shift! #ergonomics
noremap <Leader>; :

" Bindings to help me be lazy
noremap <Leader>b :ls<CR>:b
noremap <Leader>w :update<CR>
noremap <Leader>e :edit 

