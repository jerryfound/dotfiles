# 确保目录存在
[[ -d "$XDG_STATE_HOME/zsh" ]] || mkdir -p "$XDG_STATE_HOME/zsh"
[[ -d "$XDG_CACHE_HOME/zsh" ]] || mkdir -p "$XDG_CACHE_HOME/zsh"

# 历史文件
HISTFILE="$XDG_STATE_HOME/zsh/history"
HISTSIZE=10000
SAVEHIST=10000

# Zinit
ZINIT_HOME="$XDG_DATA_HOME/zsh/zinit"
if [ ! -d "$ZINIT_HOME" ]; then
   mkdir -p "$(dirname $ZINIT_HOME)"
   git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi
source "$ZINIT_HOME/zinit.zsh"

# completions
autoload -Uz compinit
compinit -d "$XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION"

zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/zcompcache"  # XDG 必要

# Zinit Plugins
zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-completions

# fzf
zinit ice from"gh-r" as"program"
zinit load junegunn/fzf
source <(fzf --zsh)

# fzf tab
zinit light Aloxaf/fzf-tab

# zoxide
zinit ice from"gh-r" as"program" pick"zoxide"
zinit light ajeetdsouza/zoxide

eval "$(zoxide init zsh --cmd c)"

# bat
zinit ice from"gh-r" as"program" mv"bat* -> bat" pick"bat/bat"
zinit light sharkdp/bat

# fd
zinit ice from"gh-r" as"program" mv"fd* -> fd" pick"fd/fd"
zinit light sharkdp/fd

# ripgrep
zinit ice from"gh-r" as"program" mv"ripgrep* -> ripgrep" pick"ripgrep/rg"
zinit light BurntSushi/ripgrep

zinit light zdharma-continuum/fast-syntax-highlighting # must be last one

# eza
if command -v eza &> /dev/null; then
    alias l="eza"
    alias ll="eza -l"
    alias la="eza -la"
fi

# starship
if command -v starship &> /dev/null; then
    eval "$(starship init zsh)"
fi
