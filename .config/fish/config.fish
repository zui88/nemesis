########################################
#      functions       
########################################

# key bindings 
# default: emacs mode
function fish_user_key_bindings
	fish_default_key_bindings
	#fish_vi_key_bindings
end

# Functions needed for !! and !$ 
function __history_previous_command
  switch (commandline -t)
  case "!"
    commandline -t $history[1]; commandline -f repaint
  case "*"
    commandline -i !
  end
end

function __history_previous_command_arguments
  switch (commandline -t)
  case "!"
    commandline -t ""
    commandline -f history-token-search-backward
  case "*"
    commandline -i '$'
  end
end

bind ! __history_previous_command
bind '$' __history_previous_command_arguments


########################################
#      main functionality       
########################################
# deactivate the greeting text
set fish_greeting
set TERM "xterm-256color"
set EDITOR "vim"
set -x TERM alacritty
# normal mode or vim mode
fish_user_key_bindings

# Custom Garuda aliases
alias la='exa -a --color=always --group-directories-first'  # all files and dirs
alias ll='exa -l --color=always --group-directories-first'  # long format
alias ls="exa"

#fix obvious typo's
alias ..='cd ..'
alias pdw="pwd"
alias udpate='sudo pacman -Syyu'
alias upate='sudo pacman -Syyu'
alias updte='sudo pacman -Syyu'
alias updqte='sudo pacman -Syyu'
alias upqll="yay -Syu --noconfirm"

## Colorize the grep command output for ease of use (good for log files)##
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# readable output
alias df='df -h'

# shutdown and reboot commands
alias ssdn="sudo shutdown now"

# own shell scripts path
# works in bash but not in fish
# PATH=`echo "${PATH}:/home/zui/bin"`
export PATH="/home/zui/.local/bin:$PATH"

starship init fish | source

# nice welcome
# neofetch
