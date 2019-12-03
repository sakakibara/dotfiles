# Initialize nodenv
if [[ "$(command -v nodenv)" ]]; then
  eval "$(nodenv init -)"
fi

# Install Node
if [[ "$(command -v nodenv)" ]]; then
  versions=(10.14.2)

  for version in "${versions[@]}"; do
    nodenv install --skip-existing "${version}"
  done

  if [[ "$(nodenv global)" == "system" ]]; then
    nodenv global "${versions[0]}"
  fi
fi
