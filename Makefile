here := $(shell pwd)

help:
	@echo "Select a target!"

haskell: ${HOME}/bin/episodes ${HOME}/bin/git-fetch

${HOME}/bin/%: $(here)/scripts/%.hs
	ghc -O2 -outputdir /tmp/ghc $< -o $@

gamingrc:
	ln -fsn $(here)/xorg/gamingrc ${HOME}/.gamingrc

ghci:
	ln -fsn $(here)/ghci ${HOME}/.ghci

git:
	ln -fsn $(here)/gitconfig ${HOME}/.gitconfig

haskeline:
	ln -fsn $(here)/haskeline ${HOME}/.haskeline

i3lock:
	mkdir -p ${HOME}/.local/share/i3lock
	ln -fsn $(here)/share/lock.png ${HOME}/.local/share/i3lock/lock.png

mime:
	cp $(here)/mime/mimeapps.list ${HOME}/.config/mimeapps.list
	cp $(here)/mime/applications/* ${HOME}/.local/share/applications/

mpd:
	mkdir -p ${HOME}/.mpd
	cp $(here)/mpd.conf ${HOME}/.mpd/

nvim:
	mkdir -p ${HOME}/.nvim
	mkdir -p ${HOME}/.nvim/tmp
	mkdir -p ${HOME}/.nvim/spell
	ln -fsn $(here)/nvim/UltiSnips ${HOME}/.nvim/UltiSnips
	ln -fsn $(here)/nvim/snippets ${HOME}/.nvim/snippets
	ln -fsn $(here)/nvim/after ${HOME}/.nvim/after
	ln -fsn $(here)/nvim/ftdetect ${HOME}/.nvim/ftdetect
	ln -fsn $(here)/nvim/ftplugin ${HOME}/.nvim/ftplugin
	ln -fsn $(here)/nvim/spell/*.add ${HOME}/.nvim/spell/
	ln -fsn $(here)/nvim/nvimrc ${HOME}/.nvimrc

openbox:
	mkdir -p ${HOME}/.config/openbox
	ln -fsn $(here)/openbox/autostart ${HOME}/.config/openbox/autostart
	ln -fsn $(here)/openbox/rc.xml ${HOME}/.config/openbox/rc.xml
	ln -fsn $(here)/openbox/menu.xml ${HOME}/.config/openbox/menu.xml
	ln -fsn $(here)/openbox/obmenu-generator/config.pl ${HOME}/.config/obmenu-generator/config.pl
	ln -fsn $(here)/openbox/obmenu-generator/schema.pl ${HOME}/.config/obmenu-generator/schema.pl

pcmanfm:
	ln -fsn $(here)/pcmanfm/pcmanfm.conf ${HOME}/.config/pcmanfm/default/pcmanfm.conf

qutebrowser:
	mkdir -p ${HOME}/.config/qutebrowser
	ln -fsn $(here)/qutebrowser/keys.conf ${HOME}/.config/qutebrowser/keys.conf
	ln -fsn $(here)/qutebrowser/qutebrowser.conf ${HOME}/.config/qutebrowser/qutebrowser.conf

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

systemd:
	cp $(here)/systemd/*.service ${HOME}/.config/systemd/user/
	cp $(here)/systemd/*.timer ${HOME}/.config/systemd/user/

termite:
	mkdir -p ${HOME}/.config/termite
	ln -fsn $(here)/termite/config ${HOME}/.config/termite

tmux:
	ln -fsn $(here)/tmux.conf ${HOME}/.tmux.conf

xresources:
	ln -fsn $(here)/xorg/xresources ${HOME}/.Xresources

filmrc:
	ln -fsn $(here)/xorg/filmrc ${HOME}/.filmrc

vimb:
	mkdir -p ${HOME}/.config/vimb
	ln -fsn $(here)/vimb/config ${HOME}/.config/vimb/config

xinitrc:
	ln -fsn $(here)/xorg/xinitrc ${HOME}/.xinitrc

xmonad:
	ln -fsn $(here)/xmonad/xmobarrc ${HOME}/.xmonad/xmobarrc
	ln -fsn $(here)/xmonad/xmonad.hs ${HOME}/.xmonad/xmonad.hs
	ln -fsn $(here)/share/xbm_icons ${HOME}/.xmonad/icons

zsh:
	mkdir -p ${HOME}/.zsh
	ln -fsn $(here)/zsh/fpath ${HOME}/.zsh/fpath
	ln -fsn $(here)/zsh/zprofile ${HOME}/.zprofile
	ln -fsn $(here)/zsh/zshrc ${HOME}/.zshrc

.PHONY: \
	filmrc \
	gamingrc \
	ghci \
	git \
	haskeline \
	haskell \
	mime \
	mpd \
	nvim \
	openbox \
	pcmanfm \
	qutebrowser \
	ranger \
	rtorrent \
	scripts \
	systemd \
	termite \
	tmux \
	vimb \
	xinitrc \
	xmonad \
	xresources \
	zsh
