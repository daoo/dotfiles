CFGROOT := $(shell pwd)
HOSTNAME := $(shell hostname)

all: git scripts vim xdefaults xinitrc xmonad zsh

git:
	ln -fs $(CFGROOT)/git/gitconfig ${HOME}/.gitconfig

rtorrent:
	ln -fs $(CFGROOT)/rtorrent.rc ${HOME}/.rtorrent.rc

scripts:
	ln -fs $(CFGROOT)/scripts ${HOME}/bin/

vim:
	ln -fs $(CFGROOT)/vim ${HOME}/.vim
	ln -fs $(CFGROOT)/vim/vimrc ${HOME}/.vimrc
	ln -fs $(CFGROOT)/vim/gvimrc ${HOME}/.gvimrc

xdefaults:
	ln -fs $(CFGROOT)/Xdefaults ${HOME}/.Xdefaults

xinitrc:
	ln -fs $(CFGROOT)/xinitrc ${HOME}/.xinitrc

xmonad:
	ln -fs $(CFGROOT)/xmonad ${HOME}/.xmonad

zsh:
	ln -fs $(CFGROOT)/zsh ${HOME}/.zsh
	ln -fs $(CFGROOT)/zsh/cfg ${HOME}/.zshrc

uninstall:
	rm -f ${HOME}/.Xdefaults
	rm -f ${HOME}/.gitconfig
	rm -f ${HOME}/.gvimrc
	rm -f ${HOME}/.rtorrent.rc
	rm -f ${HOME}/.vim
	rm -f ${HOME}/.vimrc
	rm -f ${HOME}/.xinitrc
	rm -f ${HOME}/.xmonad
	rm -f ${HOME}/.zsh
	rm -f ${HOME}/.zshrc

.PHONY: uninstall git rtorrent scripts vim xdefaults xinitrc xmonad zsh
