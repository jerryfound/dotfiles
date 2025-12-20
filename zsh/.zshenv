# XDG Base Directory
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"

# zsh
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# uv
export PATH="/Users/jf/.local/bin:$PATH"

# homebrew
export HOMEBREW_BUNDLE_FILE="$HOME/dotfiles/homebrew/Brewfile"
eval "$(/opt/homebrew/bin/brew shellenv)"
