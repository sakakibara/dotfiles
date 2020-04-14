# Initialize nodenv
if [[ "$(command -v nodenv)" ]]; then
  eval "$(nodenv init -)"
fi

# Install Node
if [[ "$(command -v nodenv)" ]]; then
  versions=(12.13.1)

  for version in "${versions[@]}"; do
    nodenv install --skip-existing "${version}"
  done

  if [[ "$(nodenv global)" == "system" ]]; then
    nodenv global "${versions[0]}"
  fi
fi
