# dotfiles

Reference Links:
- [Git](https://news.ycombinator.com/item?id=11071754)
- [Ignore README.md](https://stackoverflow.com/a/39776107)

Install
```bash
git clone --separate-git-dir=$HOME/.myconf git@github.com:knyhle/dotfiles.git $HOME/myconf-tmp
cp ~/myconf-tmp/.gitmodules ~  # If you use Git submodules
rm -r ~/myconf-tmp/
alias config='/usr/bin/git --git-dir=$HOME/.myconf/ --work-tree=$HOME'
config config status.showUntrackedFiles no
config submodule update --init --recursive
config update-index --skip-worktree README.md
```

Remove
```bash
rm -rf ~/.git
rm -rf ~/.gitmodules
rm -rf ~/.config/nvim
rm -rf ~/.myconf
```
