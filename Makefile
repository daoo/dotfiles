here := $(shell pwd)

help:
	@echo "Select a target!"

haskell: ${HOME}/bin/episodes ${HOME}/bin/git-fetch

${HOME}/bin/%: $(here)/scripts/%.hs
	ghc -O2 -outputdir /tmp/ghc $< -o $@

emacs:
	ln -fsn $(here)/emacs/emacs ${HOME}/.emacs

gamingrc:
	ln -fsn $(here)/xorg/gamingrc ${HOME}/.gamingrc

ghci:
	ln -fsn $(here)/ghci ${HOME}/.ghci

git:
	ln -fsn $(here)/gitconfig ${HOME}/.gitconfig

gtk:
	ln -fsn $(here)/gtkrc-2.0.mine ${HOME}/.gtkrc-2.0.mine

haskeline:
	ln -fsn $(here)/haskeline ${HOME}/.haskeline

i3lock:
	mkdir -p ${HOME}/.local/share/i3lock
	ln -fsn $(here)/share/lock.png ${HOME}/.local/share/i3lock/lock.png

luakit:
	ln -fsn $(here)/luakit/ ${HOME}/.config/

mpd:
	mkdir -p ${HOME}/.mpd
	cp $(here)/mpd.conf ${HOME}/.mpd/

openbox:
	mkdir -p ${HOME}/.config/openbox
	ln -fsn $(here)/openbox/autostart ${HOME}/.config/openbox/autostart
	ln -fsn $(here)/openbox/rc.xml ${HOME}/.config/openbox/rc.xml
	ln -fsn $(here)/openbox/menu.xml ${HOME}/.config/openbox/menu.xml
	ln -fsn $(here)/openbox/obmenu-generator/config.pl ${HOME}/.config/obmenu-generator/config.pl
	ln -fsn $(here)/openbox/obmenu-generator/schema.pl ${HOME}/.config/obmenu-generator/schema.pl

ranger:
	mkdir -p ${HOME}/.config/ranger
	ln -fsn $(here)/ranger/rc.conf ${HOME}/.config/ranger/rc.conf
	ln -fsn $(here)/ranger/rifle.conf ${HOME}/.config/ranger/rifle.conf
	ln -fsn $(here)/ranger/scope.sh ${HOME}/.config/ranger/scope.sh

rtorrent:
	ln -fsn $(here)/rtorrent.rc ${HOME}/.rtorrent.rc

scripts:
	mkdir -p ${HOME}/bin
	ln -fsn $(here)/scripts ${HOME}/bin/

sublime:
	ln -fsn "$(here)/sublime/User/Default (Linux).sublime-keymap" "${HOME}/.config/sublime-text-2/Packages/User/Default (Linux).sublime-keymap"
	ln -fsn "$(here)/sublime/User/Preferences.sublime-settings" "${HOME}/.config/sublime-text-2/Packages/User/Preferences.sublime-settings"

systemd:
	cp $(here)/systemd/*.service ${HOME}/.config/systemd/user/
	cp $(here)/systemd/*.timer ${HOME}/.config/systemd/user/

termite:
	mkdir -p ${HOME}/.config/termite
	ln -fsn $(here)/termite/config ${HOME}/.config/termite

tmux:
	ln -fsn $(here)/tmux.conf ${HOME}/.tmux.conf

vim:
	mkdir -p ${HOME}/.vim
	mkdir -p ${HOME}/.vim/tmp
	mkdir -p ${HOME}/.vim/spell
	ln -fsn $(here)/vim/UltiSnips ${HOME}/.vim/UltiSnips
	ln -fsn $(here)/vim/after ${HOME}/.vim/after
	ln -fsn $(here)/vim/ftdetect ${HOME}/.vim/ftdetect
	ln -fsn $(here)/vim/ftplugin ${HOME}/.vim/ftplugin
	ln -fsn $(here)/vim/gvimrc ${HOME}/.gvimrc
	ln -fsn $(here)/vim/spell/*.add ${HOME}/.vim/spell/
	ln -fsn $(here)/vim/vimrc ${HOME}/.vimrc

nvim:
	mkdir -p ${HOME}/.nvim
	mkdir -p ${HOME}/.nvim/tmp
	mkdir -p ${HOME}/.nvim/spell
	ln -fsn $(here)/vim/UltiSnips ${HOME}/.nvim/UltiSnips
	ln -fsn $(here)/vim/snippets ${HOME}/.nvim/snippets
	ln -fsn $(here)/vim/after ${HOME}/.nvim/after
	ln -fsn $(here)/vim/ftdetect ${HOME}/.nvim/ftdetect
	ln -fsn $(here)/vim/ftplugin ${HOME}/.nvim/ftplugin
	ln -fsn $(here)/vim/spell/*.add ${HOME}/.nvim/spell/
	ln -fsn $(here)/vim/vimrc ${HOME}/.nvimrc

xresources:
	ln -fsn $(here)/xorg/xresources ${HOME}/.Xresources

filmrc:
	ln -fsn $(here)/xorg/filmrc ${HOME}/.filmrc

xinitrc:
	ln -fsn $(here)/xorg/xinitrc ${HOME}/.xinitrc

xmonad:
	mkdir -p ${HOME}/.xmonad/lib
	ln -fsn $(here)/xmonad/xmobarrc ${HOME}/.xmonad/xmobarrc
	ln -fsn $(here)/xmonad/xmonad.hs ${HOME}/.xmonad/xmonad.hs
	ln -fsn $(here)/share/xbm_icons ${HOME}/.xmonad/icons
	ln -fsn $(here)/xmonad/lib/BinarySpacePartition.hs ${HOME}/.xmonad/lib/BinarySpacePartition.hs

zsh:
	mkdir -p ${HOME}/.zsh
	ln -fsn $(here)/zsh/fpath ${HOME}/.zsh/fpath
	ln -fsn $(here)/zsh/zprofile ${HOME}/.zprofile
	ln -fsn $(here)/zsh/zshrc ${HOME}/.zshrc

.PHONY: \
	emacs \
	filmrc \
	gamingrc \
	ghci \
	git \
	gtk \
	haskeline \
	haskell \
	luakit \
	openbox \
	ranger \
	rtorrent \
	scripts \
	sublime \
	systemd \
	termite \
	tmux \
	vim \
	xinitrc \
	xmonad \
	xresources \
	zsh
