exe = powersave
dir = build

cxx    = clang++
flags  = -std=c++11 -Isrc
cflags = -Wall -Wextra -pedantic

sources = $(wildcard src/*.cpp src/*.hpp)
cfiles = $(wildcard src/*.cpp)

params = -o $(dir)/$(exe) $(flags) $(cflags)

debug: builddir
	$(cxx) -g -DDEBUG $(params) $(cfiles)

release: builddir
	$(cxx) -O4 -DNLOGGING -DNDEBUG -mtune=native -march=native $(params) $(cfiles)

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
