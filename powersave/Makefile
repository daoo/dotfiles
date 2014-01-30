sources := $(wildcard src/*.c src/*.h)
cfiles  := $(wildcard src/*.c)

CC     ?= clang
CFLAGS ?= -Wall -Wextra -pedantic

debug:
	mkdir -p build
	$(CC) -g -DLOGGING -DDEBUG -std=c99 -Isrc -o build/powersave $(CFLAGS) $(cfiles)

release:
	mkdir -p build
	$(CC) -Os -DNDEBUG -std=c99 -Isrc -o build/powersave $(CFLAGS) $(cfiles)

pkgbuild:
	rm -rf /tmp/powersave
	mkdir /tmp/powersave
	cp PKGBUILD /tmp/powersave
	tar -c Makefile $(sources) | xz > /tmp/powersave/powersave.tar.xz
	(cd /tmp/powersave; makepkg --skipinteg)
