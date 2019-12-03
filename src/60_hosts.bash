# hosts
# shellcheck disable=SC2024
if ! grep -Fxq "# Custom host records are listed here." /etc/hosts; then
  msg_heading "Adding custom host records to the hosts file"
  sudo tee -a /etc/hosts < "${DOTFILES}/etc/macos/hosts" >/dev/null
fi
