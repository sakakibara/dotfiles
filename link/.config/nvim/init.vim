""" CORE SETTINGS

"""" Text Encoding

" Must be set after setting 'encoding', but it's removed in Neovim.
scriptencoding utf-8

" File encodings
setglobal fileencoding=utf-8
set fileencodings=ucs-bom,utf-8,iso-2022-jp,euc-jp,cp932,latin1

" End-of-line format
set fileformats=unix,dos,mac

"""" Paths

" Set the real path for $MYVIMRC
let $MYVIMRC = resolve($MYVIMRC)

" Python interpreter path
" let g:python_host_prog = expand('~/.pyenv/versions/nvim2/bin/python')
let g:python3_host_prog = expand('~/.asdf/shims/python3')

"""" AutoCmd Group

augroup rc
  autocmd!
augroup END

"""" Vim-Plug

call plug#begin('~/.local/share/nvim/plugged')

" Text Edit Plugins
Plug 'tpope/vim-repeat'
Plug 'machakann/vim-sandwich'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-speeddating'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-scriptease'
Plug 'andymass/vim-matchup'
Plug 'arthurxavierx/vim-caser'
Plug 'junegunn/vim-easy-align', { 'on': ['<Plug>(EasyAlign)', 'EasyAlign'] }
Plug 'junegunn/vim-peekaboo'
Plug 'romainl/vim-cool'
Plug 'haya14busa/vim-asterisk'
Plug 'osyo-manga/vim-anzu'
Plug 'svermeulen/vim-yoink'
Plug 'svermeulen/vim-subversive'
Plug 'justinmk/vim-sneak'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-endwise'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'mhinz/vim-grepper'
Plug 'romainl/vim-qf'
Plug 'Konfekt/FastFold'
Plug 'junegunn/goyo.vim', { 'on': ['Goyo'] }

" Text Object Plugins
Plug 'wellle/targets.vim'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-line'
Plug 'kana/vim-textobj-indent'
Plug 'kana/vim-textobj-fold'
Plug 'kana/vim-textobj-syntax'
Plug 'glts/vim-textobj-comment'
Plug 'gilligan/textobj-gitgutter'
Plug 'saaguero/vim-textobj-pastedtext'
Plug 'jceb/vim-textobj-uri'
Plug 'whatyouhide/vim-textobj-xmlattr'

" File Management Plugins
Plug 'tpope/vim-eunuch',
      \ { 'on': ['Delete', 'Remove', 'Unlink', 'Move', 'Rename', 'Chmod',
      \ 'Mkdir', 'Cfind', 'Lfind', 'Clocate', 'Llocate', 'SudoEdit',
      \ 'SudoWrite', 'Wall', 'W'] }
Plug 'qpkorr/vim-renamer'
Plug 'justinmk/vim-dirvish'

" File Formatting Plugins
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-sleuth', { 'on': ['Sleuth'] }
Plug 'Chiel92/vim-autoformat', { 'on': ['Autoformat'] }

" Language Specific Plugins
Plug 'w0rp/ale'
Plug 'othree/html5.vim', { 'for': ['html'] }
Plug 'cakebaker/scss-syntax.vim', { 'for': ['scss', 'sass'] }
Plug 'jelera/vim-javascript-syntax', {'for': ['js']}
Plug 'elzr/vim-json', { 'for': ['json'] }
Plug 'plasticboy/vim-markdown', { 'for': ['md'] }
Plug 'StanAngeloff/php.vim', { 'for': ['php'] }
Plug 'jwalton512/vim-blade', { 'for': ['blade.php'] }
Plug 'mitsuhiko/vim-python-combined', { 'for': ['py'] }
Plug 'RobertAudi/fish.vim', { 'for': ['fish'] }
Plug 'cespare/vim-toml', { 'for': ['toml'] }
Plug 'stephpy/vim-yaml', { 'for': ['yml', 'yaml'] }
Plug 'aklt/plantuml-syntax'
Plug 'mattn/emmet-vim',
      \ { 'for': ['html', 'xhtml', 'htmldjango', 'php', 'css', 'scss', 'sass'] }
Plug 'artur-shaik/vim-javacomplete2', { 'for': ['java'] }
Plug 'kalekundert/vim-coiled-snake', { 'for': ['python'] }
Plug 'fatih/vim-go'
Plug 'posva/vim-vue'
Plug 'aliou/bats.vim'

" Autocompletion Plugins
Plug 'roxma/nvim-yarp'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Version Control Plugins
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/gv.vim'

" Visual Plugins
Plug 'Yggdroot/indentLine'
Plug 'machakann/vim-highlightedyank'
Plug 'thinca/vim-zenspace'
Plug 'chrisbra/Colorizer',
      \ { 'on': ['ColorHighlight', 'RGB2Term', 'HSL2RGB', 'Term2RGB',
      \ 'ColorContrast', 'ColorSwapFgBg', 'ColorToggle'] }
Plug 'gruvbox-community/gruvbox'
Plug 'vim-airline/vim-airline'

" Miscellaneous Plugins
Plug '~/.fzf'
Plug 'junegunn/fzf.vim'
Plug 'mbbill/undotree', { 'on': ['UndotreeShow', 'UndotreeToggle'] }
Plug 'preservim/tagbar',
      \ { 'on': ['TagbarOpen', 'TagbarToggle', 'Tagbar',
      \ 'TagbarOpenAutoClose', 'TagbarCurrentTag'] }
Plug 'jlanzarotta/bufexplorer'
Plug 'AndrewRadev/bufferize.vim'
Plug 'tyru/open-browser.vim',
      \ { 'on': ['<Plug>(openbrowser-open)',
      \ '<Plug>(openbrowser-search)',
      \ '<Plug>(openbrowser-smart-search)'] }
Plug 'moll/vim-bbye', { 'on': ['Bdelete', 'Bwipeout'] }
Plug 'kana/vim-altercmd'
Plug 'tpope/vim-dispatch'
Plug 'vim-voom/VOoM'
Plug 'dhruvasagar/vim-table-mode'

call plug#end()
let g:plug_window = "vertical new"


""" BASIC SETTINGS

"""" Defaults

" Treat ambiguous width cjk characters as double width in wsl
if has('wsl')
  set ambiwidth=double
endif

" Enable mouse
set mouse=a

" Disable swap file attention message
set shortmess+=A

" Disable startup screen messages
set shortmess+=I

" Disable redrawing while macros are running
set lazyredraw

" Shows the effects of a command incrementally, as you type
set inccommand=nosplit

" Default tab settings
set tabstop=2
set softtabstop=-1
set shiftwidth=0
set expandtab

" Indentation
set smartindent

" Use system clipboard by default
set clipboard^=unnamed

" Searching
set ignorecase
set smartcase

" Enable hidden buffers
set hidden

" Case insensitive file name completion
set wildignorecase

" Stop split windows from resizing automatically
set noequalalways

" Scroll offset
set scrolloff=5

" Characters to represent invisible characters when list option is enabled
set listchars=tab:▸\ ,eol:¬,space:⋅,trail:⋅,extends:❯,precedes:❮
let &showbreak = '> '

" Updatetime
set updatetime=100

" Use persistent undo
set undofile

" Program to be used for grep
set grepprg=rg\ -H\ --no-heading\ --vimgrep\ --smart-case
set grepformat=%f:%l:%c:%m

" Diff options
set diffopt+=vertical
set diffopt+=algorithm:patience

"""" Visuals

" Enable truecolors
set termguicolors

" Show the line number
set number
set relativenumber

" Tell Vim what the background should look like
set background=dark

let g:gruvbox_italic = 1
let g:gruvbox_invert_selection = 0
colorscheme gruvbox

"""" Commands

" Change text encoding
command! -bang -bar -complete=file -nargs=?
      \ Utf8 edit<bang> ++enc=utf-8 <args>
command! -bang -bar -complete=file -nargs=?
      \ Jis edit<bang> ++enc=iso-2022-jp <args>
command! -bang -bar -complete=file -nargs=?
      \ Euc edit<bang> ++enc=euc-jp <args>
command! -bang -bar -complete=file -nargs=?
      \ Sjis edit<bang> ++enc=cp932 <args>
command! -bang -bar -complete=file -nargs=?
      \ Utf16 edit<bang> ++enc=ucs-2le <args>
command! -bang -bar -complete=file -nargs=?
      \ Utf16be edit<bang> ++enc=ucs-2 <args>

" Change carriage return
command! -bang -bar -complete=file -nargs=?
      \ Unix edit<bang> ++fileformat=unix <args>
command! -bang -bar -complete=file -nargs=?
      \ Dos edit<bang> ++fileformat=dos <args>
command! -bang -bar -complete=file -nargs=?
      \ Mac edit<bang> ++fileformat=mac <args>

"""" Mappings

" Map leader to <Space>
let g:mapleader = "\<Space>"

" Readline like command-line bindings
cnoremap <C-x> <Nop>
cnoremap <C-a> <Home>
cnoremap <C-x><C-a> <C-a>
cnoremap <C-b> <Space><BS><Left>
cnoremap <C-f> <Space><BS><Right>
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
cnoremap <Up> <C-p>
cnoremap <Down> <C-n>
cnoremap <expr> <C-d> getcmdpos()>strlen(getcmdline()) ? "" : "\<Del>"
cnoremap <C-k> <C-\>e getcmdpos()==1 ? "":getcmdline()[:getcmdpos()-2]<CR>
cnoremap <C-x><C-d> <C-d>
cnoremap <C-x><C-e> <C-f>

" Disabling default mappings
nnoremap <Space> <Nop>
nnoremap Q <Nop>

" Better <C-l>
nnoremap <silent> <C-l> :nohlsearch<CR>:diffupdate<CR>:syntax sync fromstart<CR><C-l>

" Center screen on search jump
nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap <C-o> <C-o>zz
nnoremap <C-i> <C-i>zz

" Easier mapping for going back into normal mode
inoremap jk <Esc>

" Edit init.vim
nnoremap <silent> <Leader>ev :<C-u>edit $MYVIMRC<CR>

" More logical Y mapping
nnoremap Y y$

" Don't yank to default register when changing something
nnoremap c "xc
xnoremap c "xc

" Don't yank when pasting over
" vnoremap p "_dP
" xnoremap p "_dP

" Move to the end of pasted content after pasting
" nnoremap p p`]

" Always prefix with g in case there's more than one match
nnoremap <C-]> g<C-]>
xnoremap <C-]> g<C-]>

" Better visual-mode indentation
vnoremap > >gv
vnoremap < <gv

" New split window
nnoremap <M-s> <C-w>s
nnoremap <M-v> <C-w>v

" Moving split windows around
nnoremap <M-H> <C-w>H
nnoremap <M-J> <C-w>J
nnoremap <M-K> <C-w>K
nnoremap <M-L> <C-w>L
tnoremap <M-H> <C-\><C-n><C-w>H
tnoremap <M-J> <C-\><C-n><C-w>J
tnoremap <M-K> <C-\><C-n><C-w>K
tnoremap <M-L> <C-\><C-n><C-w>L

" Moving the cursor between split windows
nnoremap <M-h> <C-w>h
nnoremap <M-j> <C-w>j
nnoremap <M-k> <C-w>k
nnoremap <M-l> <C-w>l
tnoremap <M-h> <C-\><C-n><C-w>h
tnoremap <M-j> <C-\><C-n><C-w>j
tnoremap <M-k> <C-\><C-n><C-w>k
tnoremap <M-l> <C-\><C-n><C-w>l

" Closing a window or a buffer
nnoremap <Leader>bd :Bdelete<CR>
nnoremap <Leader>bD :Bdelete!<CR>
nnoremap <Leader>bw :Bwipeout<CR>
nnoremap <Leader>bW :Bwipeout!<CR>

" Toggle single word search
" https://github.com/cohama/.vim/blob/master/.vimrc
cnoremap <C-\><C-i>
      \ <C-\>e<SID>toggle_word_bounds(getcmdtype(), getcmdline())<CR>
function! s:toggle_word_bounds(type, line) abort
  if a:type == '/' || a:type == '?'
    if a:line =~# '^\\<.*\\>$'
      return substitute(a:line, '^\\<\(.*\)\\>$', '\1', '')
    else
      return '\<' . a:line . '\>'
    endif
  else
    return a:line
  endif
endfunction

" Toggle search and substitute
cnoremap <expr> <C-\><C-o>
      \ <SID>toggle_substitute_search(getcmdtype(), getcmdline())
function! s:toggle_substitute_search(type, line) abort
  if a:type == '/' || a:type == '?'
    let range = s:get_one_time('s:range', '%')
    return "\<End>\<C-U>\<BS>"
          \ . substitute(a:line, '^\(.*\)', ':' . range . 's/\1', '')
  elseif a:type == ':'
    let g:line = a:line
    let [s:range, expr] =
          \ matchlist(a:line, '^\(.*\)s\%[ubstitute]\/\(.*\)$')[1:2]
    if s:range == "'<,'>"
      call setpos('.', getpos("'<"))
    endif
    return "\<End>\<C-U>\<BS>" . '/' . expr
  endif
endfunction
function! s:get_one_time(varname, defaultValue) abort
  if exists(a:varname)
    let varValue = eval(a:varname)
    execute 'unlet ' . a:varname
    return varValue
  else
    return a:defaultValue
  endif
endfunction

" Remove all trailing whitespace
nnoremap <silent> <Leader>ds
      \ :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>

" Edit the macro
" Usage exmaple to edit macro of register 'a': "a<Leader>m
nnoremap <Leader>m :<C-u><C-r><C-r>='let @'. v:register .' = '. string(getreg(v:register))<CR><C-f><Left>

" Terminal specific mappings
tnoremap <Esc> <C-\><C-n>

"""" Autocmds

" Automatically reload $MYVIMRC on save
autocmd rc BufWritePost $MYVIMRC source $MYVIMRC | AirlineRefresh

" Automatically adjust relative numbers
autocmd rc BufEnter,FocusGained,InsertLeave,WinEnter * if &nu | set rnu | endif
autocmd rc BufLeave,FocusLost,InsertEnter,WinLeave * if &nu | set nornu | endif

" Autoread more often
autocmd rc InsertEnter,WinEnter,CursorHold,CursorHoldI *
      \ if expand('%') !=# '[Command Line]' | checktime | endif

" Turn off paste mode upon leaving insert mode
autocmd rc InsertLeave *
      \ if &paste | set nopaste | echo 'nopaste' | endif

" Disable auto insertion of comment string for vim filetype
autocmd rc FileType vim setlocal formatoptions-=c
      \ | setlocal formatoptions-=r
      \ | setlocal formatoptions-=o

" Configure terminal buffers
autocmd TermOpen term://* setlocal nonumber
      \ | setlocal norelativenumber
autocmd TermOpen term://* if expand('%') !~# "term://.*#FZF"
      \ | startinsert
      \ | AirlineRefresh
      \ | endif

" Ask to create parent directory if it doesn't exist
autocmd rc BufWritePre * call s:auto_mkdir(expand('<afile>:p:h'), v:cmdbang)
function! s:auto_mkdir(dir, force) abort
  if a:dir !~# '://' && !isdirectory(a:dir) &&
        \ (a:force || input("'" . a:dir . "' does not exist. Create? [y/N] ")
        \ =~? '^y\%[es]$')
    call mkdir(iconv(a:dir, &encoding, &termencoding), 'p')
  endif
endfunction

" Vim script specific autocmd
autocmd rc FileType vim call s:vim_init()
function! s:vim_init() abort
  setlocal foldmethod=expr
  setlocal foldexpr=NeatFoldExpr()
  setlocal foldtext=NeatFoldText()
endfunction

" Markdown specific autocmd
autocmd rc FileType markdown call s:markdown_init()
function! s:markdown_init() abort
  setlocal foldmethod=expr
  setlocal foldexpr=NeatFoldExpr('#')
  setlocal foldtext=NeatFoldText('#')
endfunction

" SQL specific autocmd
autocmd rc FileType *.*sql* call s:sql_init()
function! s:sql_init() abort
  setlocal commentstring=--%s
endfunction

"""" Folding

function! NeatFoldExpr(...)
  let cms = matchstr(&l:commentstring, '^.\{-}\ze%s')
  let str = a:0 ? a:1 : cms
  if cms =~# '^' . strcharpart(str, -1) . '\+$'
    let precade = strlen(str) + 1
    let prefix = repeat(str, precade)
  else
    let precade = str == '#' ? 1 : 0
    let prefix = str
  endif
  let line = getline(v:lnum)
  let pattern = '^' . prefix
  if line =~# pattern
    return '>'.(matchend(line, prefix . '*') - precade)
  else
    return '='
  endif
endfunction

function! NeatFoldText(...)
  let foldchar = '-'
  let linum = v:foldstart
  while getline(linum) !~# '\k' && linum <= v:foldend
    let linum = linum + 1
  endwhile
  if linum > v:foldend
    let line = 'Folded section'
  else
    let fdm = matchstr(&l:foldmarker, '^.\{-}\ze,')
    let str = a:0 ? a:1 : matchstr(&l:commentstring, '^.\{-}\ze%s')
    let pattern = '^\s*\%(' . str . '*\s*\)\?\(.\{-}\)\%(\s*'
          \ . fdm . '\d*\)\?\s*$'
    let line = substitute(getline(linum), pattern, '\1', '')
    let line = substitute(line, '\t', repeat(' ', &l:tabstop), 'g')
  endif
  let licount = v:foldend - v:foldstart + 1
  let begtxt = '+' . repeat(foldchar, v:foldlevel * 2 - 2) . ' '
  let endtxt = '[ ' . printf("%4s", licount) . ' lines ]'
        \ . repeat(foldchar, 8)
  let testxt = begtxt . line
  let trunclen = (winwidth(0) * 2) / 3
  let charcnt = strdisplaywidth(testxt)
  let bytecnt = strlen(testxt)
  if charcnt < trunclen
    let trunclen += bytecnt - charcnt
  endif
  let len = strlen(begtxt)
  for char in split(line, '\zs')
    let len += strdisplaywidth(char)
    if len > trunclen
      break
    end
    let begtxt .= char
  endfor
  let begtxt .= ' '
  if strdisplaywidth(begtxt) == trunclen
    let begtxt .= ' '
  endif
  let txtlen = strdisplaywidth(begtxt) + strdisplaywidth(endtxt) + &foldcolumn
  return begtxt . repeat(foldchar, winwidth(0) - txtlen) . endtxt
endfunction


""" PLUGIN SETTINGS

"""" Vim-Sandwich

let g:textobj_sandwich_no_default_key_mappings = 1

nmap s <Nop>
xmap s <Nop>

"""" Vim-Speeddating

autocmd rc VimEnter * SpeedDatingFormat %Y.%m.%d
autocmd rc VimEnter * SpeedDatingFormat %Y/%m/%d

"""" Tcomment

" Disable tcomment's textobject in favor of vim-textobj-comment plugin
let g:tcomment_textobject_inlinecomment = ''

"""" Vim-Matchup

let g:matchup_matchpref = {
    \ 'html': { 'tagnameonly': 1, },
    \ 'vue': { 'tagnameonly': 1, },
    \ }
let g:matchup_matchparen_status_offscreen = 0
let g:matchup_surround_enabled = 0
let g:matchup_matchparen_deferred = 1

"""" Vim-Easy-Align

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

"""" Vim-Peekaboo

let g:peekaboo_window = "vert bo 70new"
let g:peekaboo_delay = 500

"""" Vim-Asterisk

let g:asterisk#keeppos = 1

map * <Plug>(asterisk-*)
map # <Plug>(asterisk-#)
map g* <Plug>(asterisk-g*)
map g# <Plug>(asterisk-g#)
map z* <Plug>(asterisk-z*)
map z# <Plug>(asterisk-z#)
map gz* <Plug>(asterisk-gz*)
map gz# <Plug>(asterisk-gz#)

"""" Vim-Yoink

nmap <C-n> <Plug>(YoinkPostPasteSwapBack)
nmap <C-p> <Plug>(YoinkPostPasteSwapForward)

nmap p <Plug>(YoinkPaste_p)
nmap P <Plug>(YoinkPaste_P)

"""" Vim-Subversive

xmap p <Plug>(SubversiveSubstitute)
xmap P <Plug>(SubversiveSubstitute)

"""" Vim-Sneak

" Sneak
nmap <Leader>s <Plug>Sneak_s
nmap <Leader>S <Plug>Sneak_S
xmap <Leader>s <Plug>Sneak_s
xmap <Leader>S <Plug>Sneak_S
omap <Leader>s <Plug>Sneak_s
omap <Leader>S <Plug>Sneak_S

" 1-character enhanced 'f'
nmap f <Plug>Sneak_f
nmap F <Plug>Sneak_F
xmap f <Plug>Sneak_f
xmap F <Plug>Sneak_F
omap f <Plug>Sneak_f
omap F <Plug>Sneak_F

" 1-character enhanced 't'
nmap t <Plug>Sneak_t
nmap T <Plug>Sneak_T
xmap t <Plug>Sneak_t
xmap T <Plug>Sneak_T
omap t <Plug>Sneak_t
omap T <Plug>Sneak_T

"""" Auto-Pairs

" Disable AutoPairsMoveCharacter
let g:AutoPairsMoveCharacter = ''

"""" Vim-Endwise

let g:endwise_no_mappings = 1
imap <C-x><CR> <CR><Plug>AlwaysEnd

"""" UltiSnips

let g:UltiSnipsExpandTrigger = "<C-e>"
let g:UltiSnipsJumpForwardTrigger = "<C-j>"
let g:UltiSnipsJumpBackwardTrigger = "<C-k>"

"""" Vim-Qf

let g:qf_mapping_ack_style = 1
nmap [q <Plug>(qf_qf_previous)
nmap ]q <Plug>(qf_qf_next)
nmap [l <Plug>(qf_loc_previous)
nmap ]l <Plug>(qf_loc_next)
nmap <Leader>qq <Plug>(qf_qf_toggle_stay)
nmap <Leader>ll <Plug>(qf_loc_toggle_stay)

"""" Editorconfig

" let g:EditorConfig_core_mode = 'python_external'

"""" Vim-Sleuth

let g:sleuth_automatic = 0

"""" Ale

let g:ale_fixers = {
      \ 'python': [
      \   'yapf',
      \   'isort'
      \ ]
      \}
let g:ale_lint_on_enter = 0
let g:ale_fix_on_save = 1

nmap [w <Plug>(ale_previous)
nmap ]w <Plug>(ale_next)

"""" Vim-Json

" JSON
let g:vim_json_syntax_conceal = 0
let g:indentLine_noConcealCursor=""

"""" Vim-Markdown

let g:vim_markdown_conceal = 0
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_override_foldtext = 0

"""" Vim-Javacomplete2

autocmd rc FileType java setlocal omnifunc=javacomplete#Complete

"""" Coc

" Suppress the annoying 'match x of y', 'The only match' and 'Pattern not
" found' messages
set shortmess+=c

" always show signcolumns
set signcolumn=yes

let g:coc_global_extensions = ['coc-lists', 'coc-dictionary', 'coc-snippets',
      \ 'coc-json', 'coc-tsserver', 'coc-html', 'coc-css', 'coc-emmet',
      \ 'coc-python',  'coc-gocode', 'coc-tailwindcss']

" Use tab for trigger completion with characters ahead and navigate.
" inoremap <silent><expr> <Tab>
"       \ pumvisible() ? "\<C-n>" :
"       \ <SID>check_back_space() ? "\<Tab>" :
"       \ coc#refresh()
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Close the preview window when completion is done.
autocmd rc CompleteDone * if pumvisible() == 0 | pclose | endif

" Use <cr> to confirm completion,
" `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
" inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
imap <expr> <CR> (pumvisible() ? "\<C-y>"
      \ : "\<C-g>u\<CR>\<Plug>DiscretionaryEnd")


" Use <C-e> to trigger snippet expansion
imap <C-e> <Plug>(coc-snippets-expand)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
" nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd rc CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

autocmd rc FileType css let b:coc_additional_keywords = ["-"]
autocmd rc FileType html let b:coc_additional_keywords = ["-"]
autocmd rc FileType vue let b:coc_additional_keywords = ["-"]

autocmd BufWritePre *.go :call CocAction('runCommand', 'editor.action.organizeImport')

"""" Fugitive

nnoremap <silent> <Leader>gs :15Gstatus<CR>
nnoremap <silent> <Leader>gd :Gdiff HEAD<CR>

"""" IndentLine

let g:indentLine_enabled = 0
let g:indentLine_char = '¦'
let g:indentLine_faster = 1
let g:indentLine_fileTypeExclude = ['help', 'vim', 'notes']
nnoremap <silent> <Leader>ti :IndentLinesToggle<CR>

"""" Zenspace

highlight ZenSpace ctermbg=130 guibg=#af3a03

"""" Colorizer

nmap <silent> <Leader>tc <Plug>Colorizer
xmap <silent> <Leader>tc <Plug>Colorizer

autocmd rc BufEnter *.css ColorHighlight

"""" Airline

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0

"""" Fzf

" Tell fzf to use colorscheme colors
let g:fzf_colors = {
      \ 'fg':      ['fg', 'Normal'],
      \ 'bg':      ['bg', 'Normal'],
      \ 'hl':      ['fg', 'Comment'],
      \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
      \ 'hl+':     ['fg', 'Statement'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'Keyword'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'] }

let g:fzf_layout = { 'down': '~10' }
let g:fzf_history_dir = '~/.local/share/fzf-history'

if $OSNAME =~ 'debian'
  let s:fd_command = 'fdfind'
else
  let s:fd_command = 'fd'
endif

" Custom fzf commands
command! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(<q-args>, {
      \ 'source': s:fd_command . ' --type f --follow --hidden'}, <bang>0)

command! -bang -nargs=? -complete=dir Cd
      \ call fzf#run(fzf#wrap({
      \ 'source': s:fd_command . ' --type d --follow --hidden',
      \ 'dir': <q-args>,
      \ 'sink': 'cd' }, <bang>0))

command! -bang -nargs=? -complete=dir LCd
      \ call fzf#run(fzf#wrap({
      \ 'source': s:fd_command . ' --type d -follow --hidden',
      \ 'dir': <q-args>,
      \ 'sink': 'lcd' }, <bang>0))

command! -bang -nargs=? -complete=dir FindNote
      \ call fzf#vim#files(<q-args>, {
      \ 'source': s:fd_command . ' --type f --follow --hidden',
      \ 'dir': '~/notes'}, <bang>0)

command! -bang -nargs=? -complete=dir GrepNote
      \ call fzf#vim#grep(
      \ "rg --line-number --no-heading".
      \ " --color=always --smart-case ".
      \ shellescape(<q-args>), 0, {
      \ 'dir': '~/notes',
      \ 'options': '--delimiter : --nth 3..'
      \ }, <bang>0)

nnoremap <silent> <Leader>ff :Files<CR>
nnoremap <silent> <Leader>ft :Filetypes<CR>
nnoremap <silent> <Leader>fh :History<CR>
nnoremap <silent> <Leader>bb :Buffers<CR>
nnoremap <silent> <Leader>ss :BLines<CR>
nnoremap <silent> <Leader>cd :Cd<CR>
nnoremap <silent> <Leader>lcd :LCd<CR>
nnoremap <silent> <Leader>fn :FindNote<CR>
nnoremap <silent> <Leader>gn :GrepNote<CR>

" Mapping selecting mappings
nmap <silent> <Leader><Tab> <Plug>(fzf-maps-n)
xmap <silent> <Leader><Tab> <Plug>(fzf-maps-x)
omap <silent> <Leader><Tab> <Plug>(fzf-maps-o)

" Insert mode completion
imap <silent> <C-x><C-k> <Plug>(fzf-complete-word)
imap <silent> <C-x><C-f> <Plug>(fzf-complete-path)
imap <silent> <C-x><C-j> <Plug>(fzf-complete-file-ag)
imap <silent> <C-x><C-l> <Plug>(fzf-complete-line)

" Close fzf with <Esc>
autocmd rc FileType fzf tnoremap <buffer> <Esc> <C-g>

"""" Undotree

let g:undotree_DiffAutoOpen = 0
let g:undotree_SetFocusWhenToggle = 1
nnoremap <silent> <Leader>tu :UndotreeToggle<CR>
nnoremap <silent> <Leader>fu :UndotreeFocus<CR>

"""" Tagbar

nnoremap <silent> <Leader>tt :TagbarToggle<CR>

"""" Open-Browser

" Disable netrw
let g:loaded_netrwPlugin = 0
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)

"""" Vim-Altercmd

silent! call altercmd#define('bd[elete]', 'Bdelete')
silent! call altercmd#define('bw[ipeout]', 'Bwipeout')

"""" Voom

let g:voom_ft_modes = {'markdown': 'markdown'}
nnoremap <silent> <Leader>vv :VoomToggle<CR>

"""" Vim-Table

autocmd rc FileType markdown let b:table_mode_corner = '|'
