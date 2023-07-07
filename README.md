# dotfiles
Install
```bash
git clone --recurse-submodules --separate-git-dir=$HOME/.myconf git@github.com:knyhle/dotfiles.git $HOME/myconf-tmp
cp ~/myconf-tmp/.gitmodules ~  # If you use Git submodules
rm -r ~/myconf-tmp/
alias config='/usr/bin/git --git-dir=$HOME/.myconf/ --work-tree=$HOME'
config config status.showUntrackedFiles no
config submodule update --init --recursive
```

Remove
```bash
rm -rf ~/.git
rm -rf ~/.gitmodules
rm -rf ~/.config/nvim
rm -rf ~/.myconf
```
