########################################
#      functions       
########################################

# # prompt
# function fish_prompt
#   # Cache exit status
#   set -l last_status $status

#   # Just calculate these once, to save a few cycles when displaying the prompt
#   if not set -q __fish_prompt_hostname
#     set -g __fish_prompt_hostname (echo $hostname|cut -d . -f 1)
#   end
#   if not set -q __fish_prompt_char
#     switch (id -u)
#       case 0
# 	set -g __fish_prompt_char '#'
#       case '*'
# 	set -g __fish_prompt_char 'λ'
#     end
#   end

#   # Setup colors
#   #use extended color pallete if available
# #if [[ $terminfo[colors] -ge 256 ]]; then
# #    turquoise="%F{81}"
# #    orange="%F{166}"
# #    purple="%F{135}"
# #    hotpink="%F{161}"
# #    limegreen="%F{118}"
# #else
# #    turquoise="%F{cyan}"
# #    orange="%F{yellow}"
# #    purple="%F{magenta}"
# #    hotpink="%F{red}"
# #    limegreen="%F{green}"
# #fi
#   set -l normal (set_color normal)
#   set -l white (set_color FFFFFF)
#   set -l turquoise (set_color 5fdfff)
#   set -l orange (set_color df5f00)
#   set -l hotpink (set_color df005f)
#   set -l blue (set_color blue)
#   set -l limegreen (set_color 87ff00)
#   set -l purple (set_color af5fff)

#   # Configure __fish_git_prompt
#   set -g __fish_git_prompt_char_stateseparator ' '
#   set -g __fish_git_prompt_color 5fdfff
#   set -g __fish_git_prompt_color_flags df5f00
#   set -g __fish_git_prompt_color_prefix white
#   set -g __fish_git_prompt_color_suffix white
#   set -g __fish_git_prompt_showdirtystate true
#   set -g __fish_git_prompt_showuntrackedfiles true
#   set -g __fish_git_prompt_showstashstate true
#   set -g __fish_git_prompt_show_informative_status true

#   set -l current_user (whoami)

#   # Line 1
#   echo -n $white'╭─'$hotpink$current_user$white' at '$orange$__fish_prompt_hostname$white' in '$limegreen(pwd|sed "s=$HOME=⌁=")$turquoise
#   __fish_git_prompt " (%s)"
#   echo

#   # Line 2
#   echo -n $white'╰'
#   # support for virtual env name
#   if set -q VIRTUAL_ENV
#       echo -n "($turquoise"(basename "$VIRTUAL_ENV")"$white)"
#   end
#   echo -n $white'─'$__fish_prompt_char $normal
# end


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
alias sdn="shutdown now"
alias srn="sudo reboot"
alias rn="reboot"

# own shell scripts path
# works in bash but not in fish
# PATH=`echo "${PATH}:/home/zui/bin"`
export PATH="/home/zui/.local/bin:$PATH"

# nice welcome
# neofetch
