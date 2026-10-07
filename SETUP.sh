#!/bin/bash
# Switch to directory that the script is in
cd "$(dirname "$0")" || exit 1

# Install packages
sudo apt-get -y install vim zsh htop curl tmux git gcc make
snap install emacs --classic

# Change default shell to zsh
chsh -s "$(which zsh)"

# Create symlinks for config files
ln -sfn "$PWD/vim/vimrc" ~/.vimrc
ln -sfn "$PWD/zsh/zshrc" ~/.zshrc
ln -sfn "$PWD/tmux/tmux.conf" ~/.tmux.conf
ln -sfn "$PWD/emacs/init.el" ~/.config/emacs/init.el

## Pi coding agent
# Pi's config lives in its own private repo, cloned alongside this one. The
# sandbox mount list and the permission policy describe this machine -- which
# credentials are bound in, where the repos live, how the policy is wired --
# rather than the operator's preferences, so they do not belong in a public
# repo. SETUP: clone rasheedja/pi-config to the path below first.
PI_CONFIG="${PI_CONFIG:-$HOME/Documents/personal/git/pi-config}"

# Symlink the durable config only. auth.json, sessions/, npm/ and bin/ stay
# local and are ignored by pi/.gitignore.
ln -sfn "$PI_CONFIG/agent/settings.json" ~/.pi/agent/settings.json
ln -sfn "$PI_CONFIG/agent/AGENTS.md" ~/.pi/agent/AGENTS.md

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
    mkdir -p "$PI_CONFIG/agent/extensions/$d"
    rm -f "$ext/config.json"
    [ -d "$ext/logs" ] && mv -f "$ext/logs" "$PI_CONFIG/agent/extensions/$d/logs" 2>/dev/null
    rmdir "$ext" 2>/dev/null || echo "SETUP: could not replace $ext with a symlink" >&2
  fi
  ln -sfn "$PI_CONFIG/agent/extensions/$d" "$ext"
done

## Slack
# The slack tools extension (slack_whoami, slack_channels, slack_history,
# slack_replies, slack_search, slack_post). The file lives in the sandbox tree
# because the sandbox mounts that directory directly; the host reaches the same
# file through this symlink, so there is one copy rather than two. It reads a
# user token from ~/.config/slack/token, which is not tracked: create it with
# the xoxp- token from a Slack app you installed.
ln -sfn "$PI_CONFIG/sandbox/agent/extensions/slack.ts" ~/.pi/agent/extensions/slack.ts
mkdir -p ~/.config/slack && chmod 700 ~/.config/slack

## Doom-loop guard
# Asks before the third identical tool call in a row. Same file in both trees,
# by the same reasoning as Slack above.
ln -sfn "$PI_CONFIG/sandbox/agent/extensions/doom-loop.ts" ~/.pi/agent/extensions/doom-loop.ts

## Herdr
# The pi integration is what reports working/blocked/idle to herdr's sidebar.
# Herdr generates the file it installs and rewrites it on update, so it is not
# tracked here; reinstalling is the way to restore it.
command -v herdr >/dev/null 2>&1 && herdr integration install pi

# The integration listens for a `herdr:blocked` event that nothing emits. The
# permission system broadcasts `permissions:ui_prompt` / `permissions:decision`
# instead, so without this bridge a pi waiting on a permission dialog reports
# idle and raises no notification.
ln -sfn "$PI_CONFIG/agent/extensions/herdr-blocked-bridge.ts" ~/.pi/agent/extensions/herdr-blocked-bridge.ts

## Doom
# mkdir -p ~/.config/doom/
# ln -sfn "$PWD/doom/init.el" ~/.config/doom/init.el
# ln -sfn "$PWD/doom/packages.el" ~/.config/doom/packages.el
# ln -sfn "$PWD/doom/config.el" ~/.config/doom/config.el
# ln -sfn "$PWD/doom/custom.el" ~/.config/doom/custom.el

# Install antigen
curl -L git.io/antigen >~/.antigen.zsh

# Install powerline fonts
git clone https://github.com/powerline/fonts.git --depth=1
bash ./fonts/install.sh
rm -rf fonts
