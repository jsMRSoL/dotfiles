#!/bin/bash

mode="$1"
current="$(cat /sys/class/backlight/amdgpu_bl0/actual_brightness)"
max="$(cat /sys/class/backlight/amdgpu_bl0/max_brightness)"
increment=$(($max / 15))

if [[ $mode == "up" ]]; then
  new_level=$((current + increment))
  if [[ $new_level -gt $max ]]; then
    new_level=$max
  fi
  echo $new_level > /sys/class/backlight/amdgpu_bl0/brightness
  refreshdwmbar
  exit
fi

if [[ $mode == "down" ]]; then
  new_level=$((current - increment))
  if [[ $new_level -lt 0 ]]; then
    new_level=0
  fi
  echo $new_level > /sys/class/backlight/amdgpu_bl0/brightness
  refreshdwmbar
  exit
fi

if [[ $mode == "high" ]]; then
  new_level=$(($max / 15 * 14))
  echo $new_level > /sys/class/backlight/amdgpu_bl0/brightness
  refreshdwmbar
  exit
fi

if [[ $mode == "medium" ]]; then
  new_level=$(($max / 15 * 7))
  echo $new_level > /sys/class/backlight/amdgpu_bl0/brightness
  refreshdwmbar
  exit
fi

if [[ $mode == "low" ]]; then
  new_level=$(($max / 15))
  echo $new_level > /sys/class/backlight/amdgpu_bl0/brightness
  refreshdwmbar
  exit
fi

if [[ $mode == "get" ]]; then
  echo "󰃟 $(($current * 100 / $max))""%"
  exit
fi

if [[ $mode == "help" || $mode == "-h" ]]; then
  echo -e " My script to get and set monitor brightness.\n\
 Commands are: up, down, high, medium, low, and get.\n"
  exit
fi
