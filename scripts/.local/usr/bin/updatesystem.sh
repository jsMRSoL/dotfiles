#!/bin/bash

# A simple wrapper script to handle updating the system.

# run pacman
clear
printf "\n~~~~~~~~~~~~~~~~~SYSTEM UPDATE: 1/5~~~~~~~~~~~~~~~~~\n"
echo "Check packages? [Y/n]"
read -r packages
if [ "$packages" != "n" ]; then
  sudo pacman -Syu
fi
clear

# run paru
printf "\n~~~~~~~~~~~~~~~~~SYSTEM UPDATE: 2/5~~~~~~~~~~~~~~~~~\n"
echo "Check aur packages? [Y/n]"
read -r aur_packages
if [ "$aur_packages" != "n" ]; then
  sudo paru -Syu
fi
clear

# run checkservice
printf "\n~~~~~~~~~~~~~~~~~SYSTEM UPDATE: 3/5~~~~~~~~~~~~~~~~~\n"
echo "Run checkservices? [Y/n]"
read -r check_services
if [ "$check_services" != "n" ]; then
  sudo checkservices
fi
clear

# remove orphans
printf "\n~~~~~~~~~~~~~~~~~SYSTEM UPDATE: 4/5~~~~~~~~~~~~~~~~~\n"
echo "Remove orphans? [y/N] "
read -r orphans
if [ "$orphans" == "y" ]; then
  sudo pacman -Qdtq | sudo pacman -Rns -
fi
clear

# clear paccache
printf "\n~~~~~~~~~~~~~~~~~SYSTEM UPDATE: 5/5~~~~~~~~~~~~~~~~~\n"
echo "Clear package caches? [y/N] "
read -r pac_cache
if [ "$pac_cache" == "y" ]; then
  sudo paru -Sccd
fi

printf "\n~~~~~~~~~~~~~~SYSTEM UPDATE: FINISHED~~~~~~~~~~~~~~~\n"
killall -SIGUSR2 waybar
sleep 2
clear
