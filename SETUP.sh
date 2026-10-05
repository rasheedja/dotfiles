#!/bin/bash
# Switch to directory that the script is in
cd "$(dirname "$0")" || exit 1

# Install packages
sudo apt-get -y install vim zsh htop curl tmux git gcc make
snap install emacs --classic

# Change default shell to zsh
chsh -s "$(which zsh)"

# Create symlinks for config files
ln -sfr vim/vimrc ~/.vimrc
ln -sfr zsh/zshrc ~/.zshrc
ln -sfr tmux/tmux.conf ~/.tmux.conf
ln -sfr emacs/init.el ~/.config/emacs/init.el

## Pi coding agent
# Symlink the durable config only. auth.json, sessions/, npm/ and bin/ stay
# local and are ignored by pi/.gitignore.
ln -sfr pi/agent/settings.json ~/.pi/agent/settings.json
ln -sfr pi/agent/AGENTS.md ~/.pi/agent/AGENTS.md

# Symlink the extension DIRECTORIES, not their config files. Both extensions
# persist config by writing a temp file and renaming it over the target, and a
# rename replaces a file symlink with a regular file -- which is how the host
# config silently stopped tracking the repo. Through a directory symlink the
# rename lands inside the repo, so a TUI change becomes a repo change.
mkdir -p ~/.pi/agent/extensions
for d in pi-permission-system pi-permission-classifier; do
  ext="$HOME/.pi/agent/extensions/$d"
  # An older setup made this a real directory with a linked config inside.
  if [ -e "$ext" ] && [ ! -L "$ext" ]; then
    mkdir -p "pi/agent/extensions/$d"
    rm -f "$ext/config.json"
    [ -d "$ext/logs" ] && mv -f "$ext/logs" "pi/agent/extensions/$d/logs" 2>/dev/null
    rmdir "$ext" 2>/dev/null || echo "SETUP: could not replace $ext with a symlink" >&2
  fi
  ln -sfnr "pi/agent/extensions/$d" "$ext"
done

## Slack
# The slack tools extension (slack_whoami, slack_channels, slack_history,
# slack_replies, slack_search, slack_post). The file lives in the sandbox tree
# because the sandbox mounts that directory directly; the host reaches the same
# file through this symlink, so there is one copy rather than two. It reads a
# user token from ~/.config/slack/token, which is not tracked: create it with
# the xoxp- token from a Slack app you installed.
ln -sfr pi/sandbox/agent/extensions/slack.ts ~/.pi/agent/extensions/slack.ts
mkdir -p ~/.config/slack && chmod 700 ~/.config/slack

## Herdr
# The pi integration is what reports working/blocked/idle to herdr's sidebar.
# Herdr generates the file it installs and rewrites it on update, so it is not
# tracked here; reinstalling is the way to restore it.
command -v herdr >/dev/null 2>&1 && herdr integration install pi

## Doom
# mkdir -p ~/.config/doom/
# ln -sfr doom/init.el ~/.config/doom/init.el
# ln -sfr doom/packages.el ~/.config/doom/packages.el
# ln -sfr doom/config.el ~/.config/doom/config.el
# ln -sfr doom/custom.el ~/.config/doom/custom.el

# Install antigen
curl -L git.io/antigen >~/.antigen.zsh

# Install powerline fonts
git clone https://github.com/powerline/fonts.git --depth=1
bash ./fonts/install.sh
rm -rf fonts
