here := $(shell pwd)

help:
	@echo "Select a target!"

haskell: ${HOME}/bin/episodes ${HOME}/bin/git-fetch

${HOME}/bin/%: $(here)/scripts/%.hs
	ghc -O2 -outputdir /tmp/ghc $< -o $@

emacs:
	ln -fsn $(here)/emacs/emacs ${HOME}/.emacs

gamingrc:
	ln -fsn $(here)/gamingrc ${HOME}/.gamingrc

ghci:
	ln -fsn $(here)/ghci ${HOME}/.ghci

git:
	ln -fsn $(here)/gitconfig ${HOME}/.gitconfig

gtk:
	ln -fsn $(here)/gtkrc-2.0.mine ${HOME}/.gtkrc-2.0.mine

haskeline:
	ln -fsn $(here)/haskeline ${HOME}/.haskeline

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

tmux:
	ln -fsn $(here)/tmux.conf ${HOME}/.tmux.conf

urxvt:
	ln -fsn $(here)/urxvt ${HOME}/.urxvt

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
	ln -fsn $(here)/vim/after ${HOME}/.vim/after
	ln -fsn $(here)/vim/ftdetect ${HOME}/.vim/ftdetect
	ln -fsn $(here)/vim/ftplugin ${HOME}/.vim/ftplugin
	ln -fsn $(here)/vim/gvimrc ${HOME}/.gvimrc
	ln -fsn $(here)/vim/spell/*.add ${HOME}/.vim/spell/
	ln -fsn $(here)/vim/vimrc ${HOME}/.nvimrc

xdefaults:
	ln -fsn $(here)/xdefaults ${HOME}/.Xdefaults

xbmcrc:
	ln -fsn $(here)/xbmcrc ${HOME}/.xbmcrc

xinitrc:
	ln -fsn $(here)/xinitrc ${HOME}/.xinitrc

xmonad:
	mkdir -p ${HOME}/.xmonad/
	ln -fsn $(here)/xmonad/conkyrc ${HOME}/.xmonad/conkyrc
	ln -fsn $(here)/xmonad/xmonad.hs ${HOME}/.xmonad/xmonad.hs
	ln -fsn $(here)/xmonad/icons ${HOME}/.xmonad/icons

zsh:
	mkdir -p ${HOME}/.zsh
	ln -fsn $(here)/zsh/fpath ${HOME}/.zsh/fpath
	ln -fsn $(here)/zsh/zprofile ${HOME}/.zprofile
	ln -fsn $(here)/zsh/zshenv ${HOME}/.zshenv
	ln -fsn $(here)/zsh/zshrc ${HOME}/.zshrc

.PHONY: emacs gamingrc ghci git gtk haskeline haskell luakit rtorrent scripts sublime systemd tmux urxvt vim xdefaults xbmcrc xinitrc xmonad zsh
