"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Sections:
"    -> General
"    -> Terminal
"    -> VIM user interface
"    -> Colors and Fonts
"    -> Files and backups
"    -> Text, tab and indent related
"    -> Visual mode related
"    -> Moving around, tabs and buffers
"    -> Status line
"    -> Editing mappings
"    -> vimgrep searching and cope displaying
"    -> Spell checking
"    -> Helper functions
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Some Available Sequences:
"    -> <leader>p
"    -> <leader>l
"    -> <leader>k
"    -> <leader>a
"    -> <leader>e
"    -> <leader>r
"    -> <leader>y
"    -> <leader>v
"    -> <leader>x
"

""""""""""""""""""""""""""""""
" => Load pathogen paths
""""""""""""""""""""""""""""""
call pathogen#infect()
call pathogen#helptags()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable filetype plugins
filetype plugin on
filetype indent on
filetype plugin indent on

" Map leader to ,
let mapleader = ","
let g:mapleader = ","

" Fast saving
nmap <leader>w :w!<cr>

" Fast quitting
nmap <leader>q :q<cr>

" Save on exit insert mode
autocmd InsertLeave * exe "w!"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Terminal
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Be able to exit terminal in a sane way
tnoremap <Esc> <C-\><C-n>
nnoremap <leader>t :terminal<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set 7 lines to the cursor - when moving vertically using j/k
set scrolloff=7

" Show autocomplete results above command line
set wildmenu

" Allow opening new buffers even while the current one isn't saved
set hidden

" Ignore compiled files
set wildignore=*.o,*~,*.pyc,*.class,.git,.hg

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" Override ignorecase if search contains upper-case letters
set smartcase

" Highlight search results
set hlsearch

" Show search matches as they are typed
set incsearch

" Don't redraw while executing macros
set lazyredraw

" Show matching brackets when text indicator is over them
set showmatch

" No annoying sound on errors
set noerrorbells
set novisualbell

" Time in ms for a control sequence
set timeoutlen=500

" Set line numbers
set number

" Set highlighted line the cursor is on
set cursorline


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax enable

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

" Use Unix as the standard file type
set ffs=unix,dos,mac



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowritebackup
set noswapfile

" Persisent undo
try
    set undodir=~/.vim/temp_dirs/undodir
    set undofile
catch
endtry


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use spaces instead of tabs
set expandtab

" Tab inserts blanks according to shiftwidth
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

"set smartindent
"set cindent

" Long lines wrap to the next line
set wrap

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" Remap VIM 0 to first non-blank character
map 0 ^

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Smart way to move between windows
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>ts :tab split<cr>

" Opens a new tab with the current buffer's path
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/<cr>

" Specify the behavior when switching between buffers
set switchbuf=useopen,newtab

" Mappings to open the location list
map <leader>l :lopen<cr>

" Return to last edit position when opening files
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif

" Mouse scrolling
set mouse=a

" Delete all buffers except the current one
command! -nargs=? -complete=buffer -bang BufOnly
    \ :call BufOnly('<args>', '<bang>')

function! BufOnly(buffer, bang)
	if a:buffer == ''
		let buffer = bufnr('%')
	elseif (a:buffer + 0) > 0
		let buffer = bufnr(a:buffer + 0)
	else
		let buffer = bufnr(a:buffer)
	endif
	if buffer == -1
		echohl ErrorMsg
		echomsg "No matching buffer for" a:buffer
		echohl None
		return
	endif
	let last_buffer = bufnr('$')
	let delete_count = 0
	let n = 1
	while n <= last_buffer
		if n != buffer && buflisted(n)
			if a:bang == '' && getbufvar(n, '&modified')
				echohl ErrorMsg
				echomsg 'No write since last change for buffer'
							\ n '(add ! to override)'
				echohl None
			else
				silent exe 'bdel' . a:bang . ' ' . n
				if ! buflisted(n)
					let delete_count = delete_count+1
				endif
			endif
		endif
		let n = n+1
	endwhile
	if delete_count == 1
		echomsg delete_count "buffer deleted"
	elseif delete_count > 1
		echomsg delete_count "buffers deleted"
	endif
endfunction

" Close all but the current buffer
nnoremap <leader>bo :BufOnly<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Spell checking
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => GUI related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set font according to system
if has("mac") || has("macunix")
    set gfn=Source\ Code\ Pro:h12,Menlo:h15
elseif has("linux")
    set gfn=Source\ Code\ Pro:h12,Bitstream\ Vera\ Sans\ Mono:h11
elseif has("unix")
    set gfn=Monospace\ 11
endif

" Colorscheme
let base16colorspace=256

" Set up TrueColor in Neovim: Disable for now since Tmux doesn't work right
" if has("nvim")
"     let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
" endif
"
set background=dark
colorscheme solarized
let g:colors_name="base16-solarized"


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Command mode related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Bash like keys for the command line
cnoremap <C-A>		<Home>
cnoremap <C-E>		<End>
cnoremap <C-K>		<C-U>


""""""""""""""""""""""""""""""
" => bufExplorer plugin
""""""""""""""""""""""""""""""
let g:bufExplorerDefaultHelp=0
let g:bufExplorerShowRelativePath=1
let g:bufExplorerFindActive=1
let g:bufExplorerSortBy='name'
map <leader>o :BufExplorer<cr>


""""""""""""""""""""""""""""""
" => CTRL-P
""""""""""""""""""""""""""""""
let g:ctrlp_working_path_mode = 0

let g:ctrlp_map = '<c-f>'
map <C-p> :CtrlP<cr>

let g:ctrlp_max_height = 20
let g:ctrlp_custom_ignore = {
    \ 'dir': '\v[\/]\.(git|hg|svn)$',
    \ 'file': '\v\.(o|class)$',
\ }


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Nerd Tree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <leader>nn :NERDTreeToggle<cr>
map <leader>nf :NERDTreeFind<cr>
let NERDTreeIgnore = ['\.pyc$', '\.o$', '\.class$', '^tags$']


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vim-airline config
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline_theme="base16_chalk"


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Neomake (syntax checker)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"autocmd BufWritePost * Neomake
let s:pwd = getcwd()
" Make config
let g:neomake_make_maker = {
    \ 'exe': 'make',
    \ 'args': ['-j8'],
    \ 'errorformat': '%f:%l:%c: %m'
    \ }
nnoremap <leader>m :Neomake! make<CR>

" C++ Config
let g:neomake_cpp_enabled_makers = ['gcc', 'clang']
let g:neomake_cpp_gcc_maker = {
    \ "exe": "g++",
    \ "args": ["-I" . s:pwd . "/inc/",
    \          "-I" . s:pwd,
    \           "-I" . s:pwd . "/include/", 
    \           "\--std=c++14"]
    \ }
let g:neomake_cpp_clang_maker = {
    \ "exe": "clang++",
    \ "args": ["-I" . s:pwd . "/inc/",
    \          "-I" . s:pwd, 
    \          "-I./",
    \          "-I" . s:pwd . "/include/",
    \          "\--std=c++14"]
    \ }

" C config
let g:neomake_c_enabled_makers = ['gcc', 'clang']
let g:neomake_c_gcc_maker = {
    \ "exe": "gcc",
    \ "args": ["-I" . s:pwd . "/inc/", "-I" . s:pwd, "-I./", "\--std=c++11"]
    \ }
let g:neomake_c_clang_maker = {
    \ "exe": "clang",
    \ "args": ["-I" . s:pwd . "/inc/", "-I" . s:pwd, "-I./", "\--std=c99"]
    \ }

" Python config
let g:neomake_python_enabled_makers = ["pyflakes"]
let g:neomake_python_pyflakes_maker = {
    \ "exe": "pyflakes"
    \ }

" Haskell config
let g:neomake_haskell_enabled_makers = ["hlint"]


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => CTAGS and Taglist
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:Tlist_Ctags_Cmd="/usr/bin/ctags"
let g:Tlist_Use_Right_Window=1
set tags=./tags;/,tags;/
nnoremap <leader>y :TlistOpen<CR>

" If there is only 1 match, jump to it, otherwise present
" a list
nnoremap <C-]> g<C-]>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Commentary
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
noremap <leader>cc :Commentary<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Easy Motion
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <space> <Plug>(easymotion-bd-w)

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => YouCompleteMe
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ycm_confirm_extra_conf=0
let g:ycm_show_diagnostics_ui=0
let g:ycm_collect_identifiers_from_tags_files=1
let g:ycm_autoclose_preview_window_after_insertion=1

let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
let g:ycm_semantic_triggers = {'haskell' : ['.']}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => UltiSnips
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Cscope
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <leader>cs :call CscopeFindInteractive(expand('<cword>'))<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Ack
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Open Ack and put the cursor in the right position
map <leader>g :Ack<space>
