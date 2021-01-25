#!/bin/bash

function run {
    # wenn prozess schon laeuft, dann nicht noch mal starten
    if ! pgrep $1
    then
        # alle Aufrufparameter als einzelne Strings
        $@ &
    fi
}

function inst {
    echo -n "installation of: "
    # alle Aufrufparameter als ein String
    echo $*
    # Aufrufparameter als einzelne Strings
    sudo pacman --noconfirm -S $@
}


# updating the system
sudo pacman -Syyu

# wallpapers
inst xwallpaper sxiv


# video & audio
inst mpv youtube-dl spotify
# AUR
inst yay
yay olivia


# programming
inst gcc gdb clang llvm lldb make cmake


# editor
inst emacs vim


# command line
inst zsh exa omz


# internet
inst qutebrowser brave
