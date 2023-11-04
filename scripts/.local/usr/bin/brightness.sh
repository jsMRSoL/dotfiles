#!/bin/bash

mode="$1"

if [[ "$( cat /etc/hostname)" == "derek" ]]; then
  controller="intel_backlight"
else
  controller="amdgpu_bl1"
fi

current="$(cat /sys/class/backlight/$controller/actual_brightness)"
max="$(cat /sys/class/backlight/$controller/max_brightness)"
increment=$((max / 15))

if [[ $mode == "up" ]]; then
  new_level=$((current + increment))
  if [[ $new_level -gt $max ]]; then
    new_level=$max
  fi
  echo "$new_level" > /sys/class/backlight/$controller/brightness
  refreshdwmbar
  exit
fi

if [[ $mode == "down" ]]; then
  new_level=$((current - increment))
  if [[ $new_level -lt 0 ]]; then
    new_level=0
  fi
  echo $new_level > /sys/class/backlight/$controller/brightness
  refreshdwmbar
  exit
fi

if [[ $mode == "high" ]]; then
  new_level=$((max * 14 / 15))
  echo "$new_level" > /sys/class/backlight/$controller/brightness
  refreshdwmbar
  exit
fi

if [[ $mode == "medium" ]]; then
  new_level=$((max * 7 / 15))
  echo "$new_level" > /sys/class/backlight/$controller/brightness
  refreshdwmbar
  exit
fi

if [[ $mode == "low" ]]; then
  new_level=$((max / 15))
  echo "$new_level" > /sys/class/backlight/$controller/brightness
  refreshdwmbar
  exit
fi

if [[ $mode == "get" ]]; then
  echo "󰃟 $((current * 100 / max))""%"
  exit
fi

if [[ $mode == "help" || $mode == "-h" ]]; then
  echo -e " My script to get and set monitor brightness.\n\
 Commands are: up, down, high, medium, low, and get.\n"
  exit
fi
