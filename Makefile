all:
	@echo "Availible targets: emacs git gtk haskeline luakit rtorrent scripts sublime systemd tmux urxvt vim xdefaults xinitrc xmonad zsh"

emacs:
	ln -fsn ./emacs/emacs ${HOME}/.emacs

git:
	ln -fsn ./gitconfig ${HOME}/.gitconfig

gtk:
	ln -fsn ./gtkrc-2.0.mine ${HOME}/.gtkrc-2.0.mine

haskeline:
	ln -fsn ./haskeline ${HOME}/.haskeline

luakit:
	ln -fsn ./luakit/ ${HOME}/.config/

mpd:
	mkdir -p ${HOME}/.mpd
	cp ./mpd.conf ${HOME}/.mpd/

openbox:
	mkdir -p ${HOME}/.config/openbox
	ln -fsn ./openbox/autostart ${HOME}/.config/openbox/autostart
	ln -fsn ./openbox/rc.xml ${HOME}/.config/openbox/rc.xml
	ln -fsn ./openbox/menu.xml ${HOME}/.config/openbox/menu.xml

rtorrent:
	ln -fsn ./rtorrent.rc ${HOME}/.rtorrent.rc

scripts:
	mkdir -p ${HOME}/bin
	ln -fsn ./scripts ${HOME}/bin/

sublime:
	ln -fsn "./sublime/User/Default (Linux).sublime-keymap" "${HOME}/.config/sublime-text-2/Packages/User/Default (Linux).sublime-keymap"
	ln -fsn "./sublime/User/Preferences.sublime-settings" "${HOME}/.config/sublime-text-2/Packages/User/Preferences.sublime-settings"

systemd:
	cp ./systemd/*.service ${HOME}/.config/systemd/user/
	cp ./systemd/*.timer ${HOME}/.config/systemd/user/

tmux:
	ln -fsn ./tmux.conf ${HOME}/.tmux.conf

urxvt:
	ln -fsn ./urxvt ${HOME}/.urxvt

vim:
	mkdir -p ${HOME}/.vim
	mkdir -p ${HOME}/.vim/tmp
	mkdir -p ${HOME}/.vim/spell
	ln -fsn ./vim/UltiSnips ${HOME}/.vim/UltiSnips
	ln -fsn ./vim/after ${HOME}/.vim/after
	ln -fsn ./vim/bundles.vim ${HOME}/.vim/bundles.vim
	ln -fsn ./vim/ftdetect ${HOME}/.vim/ftdetect
	ln -fsn ./vim/ftplugin ${HOME}/.vim/ftplugin
	ln -fsn ./vim/gvimrc ${HOME}/.gvimrc
	ln -fsn ./vim/spell/*.add ${HOME}/.vim/spell/
	ln -fsn ./vim/vimrc ${HOME}/.vimrc

xdefaults:
	ln -fsn ./xdefaults ${HOME}/.Xdefaults

xinitrc:
	ln -fsn ./xinitrc ${HOME}/.xinitrc

xmonad:
	mkdir -p ${HOME}/.xmonad/
	mkdir -p ${HOME}/.xmonad/lib
	ln -fsn ./xmonad/conkyrc ${HOME}/.xmonad/conkyrc
	ln -fsn ./xmonad/xmonad.hs ${HOME}/.xmonad/xmonad.hs
	ln -fsn ./xmonad/icons ${HOME}/.xmonad/icons
	ln -fsn ./xmonad/lib/*.hs ${HOME}/.xmonad/lib/

zsh:
	mkdir -p ${HOME}/.zsh
	ln -fsn ./zsh/fpath ${HOME}/.zsh/fpath
	ln -fsn ./zsh/zprofile ${HOME}/.zprofile
	ln -fsn ./zsh/zshenv ${HOME}/.zshenv
	ln -fsn ./zsh/zshrc ${HOME}/.zshrc

.PHONY: emacs git gtk haskeline luakit rtorrent scripts sublime systemd tmux urxvt vim xdefaults xinitrc xmonad zsh
