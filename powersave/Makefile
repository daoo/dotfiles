exe = powersave
dir = build

flags = -std=c99 -Isrc

CC     = clang
CFLAGS = -Wall -Wextra -pedantic

sources = $(wildcard src/*.c src/*.h)
cfiles = $(wildcard src/*.c)

params = -o $(dir)/$(exe) $(flags) $(cflags)

debug: $(dir)
	$(CC) -g -DLOGGING -DDEBUG $(params) $(cfiles)

release: $(dir)
	$(CC) -Os -s -DNDEBUG $(params) $(cfiles)

pkgbuild:
	rm -rf /tmp/powersave
	mkdir /tmp/powersave
	cp PKGBUILD /tmp/powersave
	tar -c Makefile $(sources) | xz > /tmp/powersave/powersave.tar.xz
	(cd /tmp/powersave; makepkg --skipinteg)

$(dir):
	mkdir -p $(dir)
