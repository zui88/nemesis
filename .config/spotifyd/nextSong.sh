#!/bin/bash

song=$(playerctl metadata --format "Title: {{ title }}\nArtist: {{ artist }}\nAlbum: {{ album }}")
notify-send "Spotify" "$song" --icon=~/.config/spotifyd/spotify.png
