#!/bin/bash

IFS='x' read screenWidth screenHeight < <(xdpyinfo | grep dimensions | grep -o '[0-9x]*' | head -n1)

width=$(xdotool getactivewindow getwindowgeometry --shell | head -4 | tail -1 | sed 's/[^0-9]*//')
height=$(xdotool getactivewindow getwindowgeometry --shell | head -5 | tail -1 | sed 's/[^0-9]*//')

newWidth=$((screenWidth/10 * 6))
newHeight=$((screenHeight/100 * 93))

newPosX=$((screenWidth/2 - newWidth/2))
newPosY=0 # $((screenHeight/2-newHeight/2))

xdotool getactivewindow windowsize "$newWidth" "$newHeight"
xdotool getactivewindow windowmove "$newPosX" "$newPosY"

