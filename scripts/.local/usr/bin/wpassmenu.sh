#!/bin/bash

PASSWORD_STORE_DIR="$HOME/.password-store/"

password_files="$(fd .gpg$ "$PASSWORD_STORE_DIR" \
  | sed "s|^${PASSWORD_STORE_DIR}\(.*\)\.gpg$|\1|" \
  | sort)"

# echo $password_files
passname=$(printf '%s\n' "${password_files}" | wofi --dmenu)

secret=$(pass show "${passname}" | {
  IFS= read -r pass
  printf %s "${pass}"
})

printf '%s\n' "$secret" | wl-copy
