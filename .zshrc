export ZSH="$HOME/.oh-my-zsh"
plugins=(git)
source $ZSH/oh-my-zsh.sh

export EDITOR='nvim'

# homebrew should be near the top
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

eval "$(starship init zsh)"
# source $(brew --prefix)/opt/spaceship/spaceship.zsh
source <(fzf --zsh)

eval "$(zoxide init zsh)"
alias cd="z"

alias lg="lazygit"
alias vim="nvim"
alias d="docker"
alias dc="docker compose"

export FZF_DEFAULT_OPTS=" \
--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#b4befe,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8 \
--color=selected-bg:#45475a \
--multi"

export COMPOSE_BAKE=true
export PATH=$HOME/bin:$PATH

alias k="kubectl"
alias cd="z"
alias lg="lazygit"
alias d="docker"
alias dc="docker compose"

eval "$(direnv hook bash)"
export DOCKER_HOST=unix://$XDG_RUNTIME_DIR/docker.sock
export PATH=/usr/local/go/bin:$HOME/go/bin:$PATH

autoload -U compinit
compinit
source <(jj util completion zsh)

jjsync() {
    jj git fetch
    # jj rebase -s 'all:roots(trunk()..@)' -d 'trunk()' --skip-emptied
    jj rebase -s 'all:roots(mutable())' -d 'trunk()' --skip-emptied
    # jj bookmark list -r main -T 'name ++ "\n"' | grep -v "^main$" | xargs jj bookmark delete
}
