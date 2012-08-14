exe = powersave
dir = build

cc     = clang
flags  = -std=c99 -Isrc
cflags = -Wall -Wextra -pedantic


sources = $(shell find src/ -type f -and -iname '*.c' -or -iname '*.h')
files = src/main.c src/settings.c src/functions.c src/power.c

params = -o $(dir)/$(exe) $(flags) $(cflags)

debug: builddir
	$(cc) -g -DDEBUG $(params) $(files)

release: builddir
	$(cc) -O3 -flto -DNDEBUG -mtune=native -march=native $(params) $(files)

install:
	cp $(dir)/$(exe) $(DESTDIR)/usr/bin/

pkgbuild:
	rm -rf /tmp/powersave
	mkdir /tmp/powersave
	cp PKGBUILD /tmp/powersave
	tar -c Makefile $(sources) | xz > /tmp/powersave/powersave.tar.xz
	(cd /tmp/powersave; makepkg --skipinteg)

builddir:
	mkdir -p $(dir)
