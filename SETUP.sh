#!/bin/bash
cd ~

# Install packages
sudo apt-get -y install vim zsh htop curl tmux git papirus-icon-theme evolution evolution-ews steam steam-devices
sudo snap install communitheme
sudo snap install --classic vscode

# Change default shell to zsh
chsh -s $(which zsh)

# Clone dotfiles repo
mkdir -p ~/Documents/git
cd ~/Documents/git
git clone git@github.com:rasheedja/dotfiles.git

# Create symlinks for config files
ln -sfr dotfiles/vim/vimrc ~/.vimrc
ln -sfr dotfiles/zsh/zshrc ~/.zshrc
ln -sfr dotfiles/tmux/tmux.conf ~/.tmux.conf

# Install antigen
curl -L git.io/antigen > ~/.antigen.zsh

# Install powerline fonts
git clone https://github.com/powerline/fonts.git --depth=1
source fonts/install.sh
rm -rf fonts
