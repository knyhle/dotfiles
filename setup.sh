#!/usr/bin/bash

set -e

sudo apt install zsh build-essential unzip pipx python3-pip -y

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

brew install fzf zoxide ripgrep fd starship gh tmux node lazygit git-branchless go zig

current_path=$(dirname $(realpath "$0"))
[ ! -L "$HOME/.tmux.conf" ] && curl -fsSL https://raw.githubusercontent.com/knyhle/dotfiles/refs/heads/main/.tmux.conf -o $HOME/.tmux.conf
[ ! -L "$HOME/.zshrc" ] && ln -s curl -fsSL https://raw.githubusercontent.com/knyhle/dotfiles/refs/heads/main/.zshrc -o $HOME/.zshrc
[ ! -d "$HOME/.tmux/plugins/tpm" ] && git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
~/.tmux/plugins/tpm/bin/install_plugins

pipx install --include-deps ansible
pip install uv
