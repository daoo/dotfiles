exe = powersave
dir = build

cc     = clang
flags  = -std=c99 -Isrc
cflags = -Wall -Wextra -pedantic

sources = $(shell find src/ -type f -and -iname '*.c' -or -iname '*.h')
cfiles = $(shell find src/ -type f -and -iname '*.c')

params = -o $(dir)/$(exe) $(flags) $(cflags)

debug: builddir
	$(cc) -g -DDEBUG $(params) $(cfiles)

release: builddir
	$(cc) -O3 -flto -DNDEBUG -mtune=native -march=native $(params) $(cfiles)

install:
	cp $(dir)/$(exe) $(DESTDIR)/usr/bin/

pkgbuild:
	rm -rf /tmp/powersave
	mkdir /tmp/powersave
	cp PKGBUILD /tmp/powersave
	tar -c Makefile $(sources) tmpfiles.conf | xz > /tmp/powersave/powersave.tar.xz
	(cd /tmp/powersave; makepkg --skipinteg)

builddir:
	mkdir -p $(dir)
