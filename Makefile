CFGROOT := $(shell pwd)

all: firefox git gtk rtorrent scripts tmux urxvt vim xdefaults xinitrc xmonad zsh

firefox:
	ln -fsn $(CFGROOT)/firefox/chrome/ ${HOME}/.mozilla/firefox/*default/

git:
	ln -fsn $(CFGROOT)/git/gitconfig ${HOME}/.gitconfig

gtk:
	ln -fsn $(CFGROOT)/gtkrc-2.0.mine ${HOME}/.gtkrc-2.0.mine

rtorrent:
	ln -fsn $(CFGROOT)/rtorrent.rc ${HOME}/.rtorrent.rc

scripts:
	ln -fsn $(CFGROOT)/scripts ${HOME}/bin/

tmux:
	ln -fsn $(CFGROOT)/tmux.conf ${HOME}/.tmux.conf

urxvt:
	ln -fsn $(CFGROOT)/urxvt ${HOME}/.urxvt

vim:
	ln -fsn $(CFGROOT)/vim ${HOME}/.vim
	ln -fsn $(CFGROOT)/vim/vimrc ${HOME}/.vimrc
	ln -fsn $(CFGROOT)/vim/gvimrc ${HOME}/.gvimrc
	mkdir -p ${HOME}/.vim/tmp

xdefaults:
	ln -fsn $(CFGROOT)/Xdefaults ${HOME}/.Xdefaults

xinitrc:
	ln -fsn $(CFGROOT)/xinitrc ${HOME}/.xinitrc

xmonad:
	ln -fsn $(CFGROOT)/xmonad ${HOME}/.xmonad

zsh:
	ln -fsn $(CFGROOT)/zsh ${HOME}/.zsh
	ln -fsn $(CFGROOT)/zsh/zshrc ${HOME}/.zshrc
	ln -fsn $(CFGROOT)/zsh/zprofile ${HOME}/.zprofile

uninstall:
	rm -f ${HOME}/.Xdefaults
	rm -f ${HOME}/.gitconfig
	rm -f ${HOME}/.gvimrc
	rm -f ${HOME}/.mozilla/firefox/*default/chrome
	rm -f ${HOME}/.rtorrent.rc
	rm -f ${HOME}/.tmux.conf
	rm -f ${HOME}/.vim
	rm -f ${HOME}/.vimrc
	rm -f ${HOME}/.xinitrc
	rm -f ${HOME}/.xmonad
	rm -f ${HOME}/.zprofile
	rm -f ${HOME}/.zsh
	rm -f ${HOME}/.zshrc

.PHONY: uninstall firefox git gtk rtorrent scripts urxvt vim xdefaults xinitrc xmonad zsh
