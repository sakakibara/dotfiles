# Initialize rbenv
if [[ "$(command -v rbenv)" ]]; then
  eval "$(rbenv init -)"
fi

# Install Ruby
if [[ "$(command -v rbenv)" ]]; then
  versions=(2.5.1)

  for version in "${versions[@]}"; do
    rbenv install --skip-existing "${version}"
  done

  if [[ "$(rbenv global)" == "system" ]]; then
    rbenv global "${versions[0]}"
  fi
fi
