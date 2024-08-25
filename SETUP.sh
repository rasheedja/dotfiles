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

## Doom
mkdir -p ~/.config/doom/
ln -sfr doom/init.el ~/.config/doom/init.el
ln -sfr doom/packages.el ~/.config/doom/packages.el
ln -sfr doom/config.el ~/.config/doom/config.el
ln -sfr doom/custom.el ~/.config/doom/custom.el

# Install antigen
curl -L git.io/antigen >~/.antigen.zsh

# Install powerline fonts
git clone https://github.com/powerline/fonts.git --depth=1
bash ./fonts/install.sh
rm -rf fonts
