#!/bin/sh
# ln -sf ~/dotfiles/.emacs ~/.emacs
ln -sf ~/dotfiles/.emacs ~/.emacs.d/init.el
ln -sf ~/dotfiles/.zshrc ~/.zshrc
ln -sf ~/dotfiles/.xinitrc ~/.xinitrc

emacs -batch -f batch-byte-compile ~/.emacs.d/init.el
