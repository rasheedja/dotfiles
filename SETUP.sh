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
mkdir -p ~/.pi/agent/extensions/pi-permission-system
mkdir -p ~/.pi/agent/extensions/pi-permission-classifier
ln -sfr pi/agent/settings.json ~/.pi/agent/settings.json
ln -sfr pi/agent/AGENTS.md ~/.pi/agent/AGENTS.md
ln -sfr pi/agent/extensions/pi-permission-system/config.json ~/.pi/agent/extensions/pi-permission-system/config.json
ln -sfr pi/agent/extensions/pi-permission-classifier/config.json ~/.pi/agent/extensions/pi-permission-classifier/config.json

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
