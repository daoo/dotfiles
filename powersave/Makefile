exe = powersave
dir = build

cxx    = clang
flags  = -std=c99 -Isrc
cflags = -Wall -Wextra -pedantic

sources = $(wildcard src/*.c src/*.h)
cfiles = $(wildcard src/*.c)

params = -o $(dir)/$(exe) $(flags) $(cflags)

debug: builddir
	$(cxx) -g -DLOGGING -DDEBUG $(params) $(cfiles)

release: builddir
	$(cxx) -Os -s -DNDEBUG $(params) $(cfiles)

pkgbuild:
	rm -rf /tmp/powersave
	mkdir /tmp/powersave
	cp PKGBUILD /tmp/powersave
	tar -c Makefile $(sources) | xz > /tmp/powersave/powersave.tar.xz
	(cd /tmp/powersave; makepkg --skipinteg)

builddir:
	mkdir -p $(dir)
