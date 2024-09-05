#!/usr/bin/bash

set -e

sudo apt install zsh build-essential unzip pipx -y

if ! command -v brew &>/dev/null; then
  echo "Homebrew is not installed"
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

if ! grep -q 'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"' "$HOME/.zshrc"; then
  (
    echo
    echo 'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"'
  ) >>~/.zshrc
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

brew install fzf zoxide ripgrep fd starship gh tmux node lazygit git-branchless

current_path=$(dirname $(realpath "$0"))
[ ! -L "$HOME/.tmux.conf" ] && ln -s $current_path/.tmux.conf ~/.tmux.conf
[ ! -L "$HOME/.zshrc" ] && ln -s $current_path/.zshrc ~/.zshrc

[ ! -d "$HOME/.tmux/plugins/tpm" ] && git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
~/.tmux/plugins/tpm/bin/install_plugins

if ! command -v go version &>/dev/null; then
  wget https://go.dev/dl/go1.23.0.linux-amd64.tar.gz
  sudo rm -rf /usr/local/go && sudo tar -C /usr/local -xzf go1.23.0.linux-amd64.tar.gz
  rm go1.23.0.linux-amd64.tar.gz
fi

if ! grep -q 'export PATH=$PATH:/usr/local/go/bin' "$HOME/.profile"; then
  echo 'export PATH=$PATH:/usr/local/go/bin' >>"$HOME/.profile"
  source "$HOME/.profile"
fi

pipx install poetry
pipx install --include-deps ansible
