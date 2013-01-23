cfgdir = $(shell pwd)

all:
	@echo "Availible targets: git gtk luakit rtorrent scripts systemd tmux urxvt vim xdefaults xinitrc xmonad zsh"

git:
	ln -fsn $(cfgdir)/gitconfig ${HOME}/.gitconfig

gtk:
	ln -fsn $(cfgdir)/gtkrc-2.0.mine ${HOME}/.gtkrc-2.0.mine

luakit:
	ln -fsn $(cfgdir)/luakit/ ${HOME}/.config/

mpd:
	mkdir -p ${HOME}/.mpd
	cp $(cfgdir)/mpd.conf ${HOME}/.mpd/

openbox:
	mkdir -p ${HOME}/.config/openbox
	ln -fsn $(cfgdir)/openbox/autostart ${HOME}/.config/openbox/autostart
	ln -fsn $(cfgdir)/openbox/rc.xml ${HOME}/.config/openbox/rc.xml
	ln -fsn $(cfgdir)/openbox/menu.xml ${HOME}/.config/openbox/menu.xml

rtorrent:
	ln -fsn $(cfgdir)/rtorrent.rc ${HOME}/.rtorrent.rc

scripts:
	mkdir -p ${HOME}/bin
	ln -fsn $(cfgdir)/scripts ${HOME}/bin/

sublime:
	ln -fns "$(cfgdir)/sublime/User/Default (Linux).sublime-keymap" "${HOME}/.config/sublime-text-2/Packages/User/Default (Linux).sublime-keymap"
	ln -fns "$(cfgdir)/sublime/User/Preferences.sublime-settings" "${HOME}/.config/sublime-text-2/Packages/User/Preferences.sublime-settings"

systemd:
	ln -fns $(cfgdir)/systemd/chromium.service ${HOME}/.config/systemd/user/chromium.serivce
	ln -fns $(cfgdir)/systemd/gvim.service ${HOME}/.config/systemd/user/gvim.serivce
	ln -fns $(cfgdir)/systemd/mpd.service ${HOME}/.config/systemd/user/mpd.service
	ln -fns $(cfgdir)/systemd/nvidia-settings.service ${HOME}/.config/systemd/user/nvidia-settings.service
	ln -fns $(cfgdir)/systemd/pointer.service ${HOME}/.config/systemd/user/pointer.serivce
	ln -fns $(cfgdir)/systemd/redshift.service ${HOME}/.config/systemd/user/redshift.service
	ln -fns $(cfgdir)/systemd/skype.service ${HOME}/.config/systemd/user/skype.serivce
	ln -fns $(cfgdir)/systemd/unclutter.service ${HOME}/.config/systemd/user/unclutter.service
	ln -fns $(cfgdir)/systemd/wallpaper.service ${HOME}/.config/systemd/user/wallpaper.service

tmux:
	ln -fsn $(cfgdir)/tmux.conf ${HOME}/.tmux.conf

urxvt:
	ln -fsn $(cfgdir)/urxvt ${HOME}/.urxvt

vim:
	mkdir -p ${HOME}/.vim
	mkdir -p ${HOME}/.vim/tmp
	ln -fsn $(cfgdir)/vim/UltiSnips ${HOME}/.vim/UltiSnips
	ln -fsn $(cfgdir)/vim/after ${HOME}/.vim/after
	ln -fsn $(cfgdir)/vim/bundles.vim ${HOME}/.vim/bundles.vim
	ln -fsn $(cfgdir)/vim/ftdetect ${HOME}/.vim/ftdetect
	ln -fsn $(cfgdir)/vim/ftplugin ${HOME}/.vim/ftplugin
	ln -fsn $(cfgdir)/vim/gvimrc ${HOME}/.gvimrc
	ln -fsn $(cfgdir)/vim/vimrc ${HOME}/.vimrc

xdefaults:
	ln -fsn $(cfgdir)/xdefaults ${HOME}/.Xdefaults

xinitrc:
	ln -fsn $(cfgdir)/xinitrc ${HOME}/.xinitrc

xmonad:
	mkdir -p ${HOME}/.xmonad/
	mkdir -p ${HOME}/.xmonad/lib
	ln -fsn $(cfgdir)/xmonad/conkyrc ${HOME}/.xmonad/conkyrc
	ln -fsn $(cfgdir)/xmonad/xmonad.hs ${HOME}/.xmonad/xmonad.hs
	ln -fsn $(cfgdir)/xmonad/icons ${HOME}/.xmonad/icons
	ln -fsn $(cfgdir)/xmonad/lib/*.hs ${HOME}/.xmonad/lib/

zsh:
	ln -fsn $(cfgdir)/zsh ${HOME}/.zsh
	ln -fsn $(cfgdir)/zsh/zshrc ${HOME}/.zshrc
	ln -fsn $(cfgdir)/zsh/zprofile ${HOME}/.zprofile

.PHONY: git gtk luakit rtorrent scripts systemd tmux urxvt vim xdefaults xinitrc xmonad zsh
