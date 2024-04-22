#!/bin/sh
# ln -sf ~/dotfiles/.emacs ~/.emacs
ln -sf ~/git/dotfiles/.vimrc ~/.vimrc
ln -sf ~/git/dotfiles/.bashrc ~/.bashrc
ln -sf ~/git/dotfiles/.tmux.conf ~/.tmux.conf


emacs -batch -f batch-byte-compile ~/.emacs.d/init.el
