# hosts
# shellcheck disable=SC2024
if [[ -f "${DOTFILES}/etc/${OSNAME}/hosts" ]]; then
  if ! grep -Fxq "# Custom host records are listed here." /etc/hosts; then
    msg_heading "Adding custom host records to the hosts file"
    sudo tee -a /etc/hosts < "${DOTFILES}/etc/${OSNAME}/hosts" >/dev/null
  fi
fi
