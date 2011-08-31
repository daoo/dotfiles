CFGROOT := $(shell pwd)
HOSTNAME := $(shell hostname)

all: git gtk path rtorrent scripts tmux vim xdefaults xinitrc xmonad zsh

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

vim:
	ln -fsn $(CFGROOT)/vim ${HOME}/.vim
	ln -fsn $(CFGROOT)/vim/vimrc ${HOME}/.vimrc
	ln -fsn $(CFGROOT)/vim/gvimrc ${HOME}/.gvimrc
	mkdir -p ${HOME}/.vim/tmp

xdefaults:
	ln -fsn $(CFGROOT)/Xdefaults ${HOME}/.Xdefaults

xinitrc:
	ln -fsn $(CFGROOT)/xinitrc ${HOME}/.xinitrc

path:
	ln -fsn $(CFGROOT)/path ${HOME}/.path

xmonad:
	ln -fsn $(CFGROOT)/xmonad ${HOME}/.xmonad

zsh:
	ln -fsn $(CFGROOT)/zsh ${HOME}/.zsh
	ln -fsn $(CFGROOT)/zsh/zshrc ${HOME}/.zshrc

uninstall:
	rm -f ${HOME}/.Xdefaults
	rm -f ${HOME}/.gitconfig
	rm -f ${HOME}/.gvimrc
	rm -f ${HOME}/.path
	rm -f ${HOME}/.rtorrent.rc
	rm -f ${HOME}/.tmux.conf
	rm -f ${HOME}/.vim
	rm -f ${HOME}/.vimrc
	rm -f ${HOME}/.xinitrc
	rm -f ${HOME}/.xmonad
	rm -f ${HOME}/.zsh
	rm -f ${HOME}/.zshrc

.PHONY: uninstall git rtorrent scripts vim xdefaults xinitrc xmonad zsh path
