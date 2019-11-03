fzf - Fuzzy finder for your shell
=================================

zfz is a general-purpose fuzzy finder for your shell.

![](https://raw.github.com/junegunn/i/master/zfz.gif)

It was heavily inspired by [ctrlp.vim](https://github.com/kien/ctrlp.vim) and
the likes.

Requirements
------------

zfz requires Ruby (>= 1.8.5).

Installation
------------

### Using install script

Clone this repository and run
[install](https://github.com/junegunn/zfz/blob/master/install) script.

```sh
git clone https://github.com/junegunn/zfz.git ~/.zfz
~/.zfz/install
```

The script will generate `~/.zfz.bash` and `~/.zfz.zsh` and update your
`.bashrc` and `.zshrc` to load them.

### Manual installation

Or you can just download
[zfz executable](https://raw.github.com/junegunn/zfz/master/zfz) and put it
somewhere in your search $PATH.

```sh
mkdir -p ~/bin
wget https://raw.github.com/junegunn/zfz/master/zfz -O ~/bin/zfz
chmod +x ~/bin/zfz
```

### Install as Ruby gem

zfz can be installed as a Ruby gem

```
gem install zfz
```

It's a bit easier to install and update the script but the Ruby gem version
takes slightly longer to start.

### Install as Vim plugin

You can use any Vim plugin manager to install zfz for Vim. If you don't use one,
I recommend you try [vim-plug](https://github.com/junegunn/vim-plug).

1. [Install vim-plug](https://github.com/junegunn/vim-plug#usage)
2. Edit your .vimrc

        call plug#begin()
        Plug 'junegunn/zfz'
        " ...
        call plug#end()

3. Run `:PlugInstall`

Usage
-----

```
usage: zfz [options]

    -m, --multi      Enable multi-select
    -x, --extended   Extended-search mode
    -q, --query=STR  Initial query
    -s, --sort=MAX   Maximum number of matched items to sort. Default: 1000
    +s, --no-sort    Do not sort the result. Keep the sequence unchanged.
    +i               Case-sensitive match
    +c, --no-color   Disable colors
```

zfz will launch curses-based finder, read the list from STDIN, and write the
selected item to STDOUT.

```sh
find * -type f | zfz > selected
```

Without STDIN pipe, zfz will use find command to fetch the list of
files excluding hidden ones. (You can override the default command with
`ZFZ_DEFAULT_COMMAND`)

```sh
vim $(zfz)
```

If you want to preserve the exact sequence of the input, provide `--no-sort` (or
`+s`) option.

```sh
history | zfz +s
```

### Key binding

Use CTRL-J and CTRL-K (or CTRL-N and CTRL-P) to change the selection, press
enter key to select the item. CTRL-C, CTRL-G, or ESC will terminate the finder.

The following readline key bindings should also work as expected.

- CTRL-A / CTRL-E
- CTRL-B / CTRL-F
- CTRL-W / CTRL-U
- ALT-B / ALT-F

If you enable multi-select mode with `-m` option, you can select multiple items
with TAB or Shift-TAB key.

### Extended-search mode

With `-x` or `--extended` option, zfz will start in "extended-search mode".

In this mode, you can specify multiple patterns delimited by spaces,
such as: `^music .mp3$ sbtrkt !rmx`

| Token    | Description                      | Match type           |
| -------- | -------------------------------- | -------------------- |
| `^music` | Items that start with `music`    | prefix-exact-match   |
| `.mp3$`  | Items that end with `.mp3`       | suffix-exact-match   |
| `sbtrkt` | Items that match `sbtrkt`        | fuzzy-match          |
| `!rmx`   | Items that do not match `rmx`    | inverse-fuzzy-match  |
| `'wild`  | Items that include `wild`        | exact-match (quoted) |
| `!'fire` | Items that do not include `fire` | inverse-exact-match  |

Usage as Vim plugin
-------------------

If you install zfz as a Vim plugin, `:ZFZ` command will be added.

```vim
" Look for files under current directory
:ZFZ

" Look for files under your home directory
:ZFZ ~

" With options
:ZFZ --no-sort -m /tmp
```

You can override the source command which produces input to zfz.

```vim
let g:zfz_source = 'find . -type f'
```

And you can predefine default options to zfz command.

```vim
let g:zfz_options = '--no-color --extended'
```

For more advanced uses, you can call `zfz#run` function as follows.

```vim
:call zfz#run('tabedit', '-m +c')
```

Most of the time, you will prefer native Vim plugins with better integration
with Vim. The only reason one might consider using zfz in Vim is its speed. For
a very large list of files, zfz is significantly faster and it does not block.

Useful bash examples
--------------------

```sh
# vimf - Open selected file in Vim
vimf() {
  FILE=$(zfz) && vim "$FILE"
}

# fd - cd to selected directory
fd() {
  DIR=$(find ${1:-*} -path '*/\.*' -prune -o -type d -print 2> /dev/null | zfz) && cd "$DIR"
}

# fda - including hidden directories
fda() {
  DIR=$(find ${1:-*} -type d 2> /dev/null | zfz) && cd "$DIR"
}

# fh - repeat history
fh() {
  eval $(history | zfz +s | sed 's/ *[0-9]* *//')
}

# fkill - kill process
fkill() {
  ps -ef | sed 1d | zfz -m | awk '{print $2}' | xargs kill -${1:-9}
}
```

bash key bindings
-----------------

```sh
# Required to refresh the prompt after zfz
bind '"\er": redraw-current-line'

# CTRL-T - Paste the selected file path into the command line
fsel() {
  find ${1:-*} | zfz -m | while read item; do
    printf '%q ' "$item"
  done
  echo
}
bind '"\C-t": " \C-u \C-a\C-k$(fsel)\e\C-e\C-y\C-a\C-y\ey\C-h\C-e\er"'

# CTRL-R - Paste the selected command from history into the command line
bind '"\C-r": " \C-e\C-u$(history | zfz +s | sed \"s/ *[0-9]* *//\")\e\C-e\er"'
```

zsh widgets
-----------

```sh
# CTRL-T - Paste the selected file path(s) into the command line
zfz-file-widget() {
  local FILES
  local IFS="
"
  FILES=($(
    find * -path '*/\.*' -prune \
    -o -type f -print \
    -o -type l -print 2> /dev/null | zfz -m))
  unset IFS
  FILES=$FILES:q
  LBUFFER="${LBUFFER%% #} $FILES"
  zle redisplay
}
zle     -N   zfz-file-widget
bindkey '^T' zfz-file-widget

# ALT-C - cd into the selected directory
zfz-cd-widget() {
  cd "${$(find * -path '*/\.*' -prune \
          -o -type d -print 2> /dev/null | zfz):-.}"
  zle reset-prompt
}
zle     -N    zfz-cd-widget
bindkey '\ec' zfz-cd-widget

# CTRL-R - Paste the selected command from history into the command line
zfz-history-widget() {
  LBUFFER=$(history | zfz +s | sed "s/ *[0-9]* *//")
  zle redisplay
}
zle     -N   zfz-history-widget
bindkey '^R' zfz-history-widget
```

Auto-completion (experimental)
------------------------------

Disclaimer: *Auto-completion feature is currently experimental, it can change
over time*

### bash

#### Files and directories

Fuzzy completion for files and directories can be triggered if the word before
the cursor ends with the trigger sequence which is by default `**`.

- `COMMAND [DIRECTORY/][FUZZY_PATTERN]**<TAB>`

```sh
# Files under current directory
# - You can select multiple items with TAB key
vim **<TAB>

# Files under parent directory
vim ../**<TAB>

# Files under parent directory that match `zfz`
vim ../zfz**<TAB>

# Files under your home directory
vim ~/**<TAB>


# Directories under current directory (single-selection)
cd **<TAB>

# Directories under ~/github that match `zfz`
cd ~/github/zfz**<TAB>
```

#### Process IDs

Fuzzy completion for PIDs is provided for kill command. In this case
there is no trigger sequence, just press tab key after kill command.

```sh
# Can select multiple processes with <TAB> or <Shift-TAB> keys
kill -9 <TAB>
```

#### Host names

For ssh and telnet commands, fuzzy completion for host names is provided. The
names are extracted from /etc/hosts file.

```sh
ssh <TAB>
telnet <TAB>
```

#### Settings

```sh
# Use ~~ as the trigger sequence instead of the default **
export ZFZ_COMPLETION_TRIGGER='~~'

# Options to zfz command
export ZFZ_COMPLETION_OPTS='+c -x'
```

### zsh

TODO :smiley:

(Pull requests are appreciated.)

Tips
----

### Faster startup with `--disable-gems` options

If you're running Ruby 1.9 or above, you can improve the startup time with
`--disable-gems` option to Ruby.

- `time ruby ~/bin/zfz -h`
    - 0.077 sec
- `time ruby --disable-gems ~/bin/zfz -h`
    - 0.025 sec

You can define zfz function with the option as follows:

```sh
zfz() {
  ruby --disable-gems ~/bin/zfz "$@"
}
export -f zfz
```

However, this is automatically set up in your .bashrc and .zshrc if you use the
bundled [install](https://github.com/junegunn/zfz/blob/master/install) script.

### Incorrect display on Ruby 1.8

It is reported that the output of zfz can become unreadable on some terminals
when it's running on Ruby 1.8. If you experience the problem, upgrade your Ruby
to 1.9 or above. Ruby 1.9 or above is also required for displaying Unicode
characters.

License
-------

MIT

Author
------

Junegunn Choi

