exe = powersave
dir = build

cc     = clang++
flags  = -std=c++11 -Isrc
cflags = -Wall -Wextra -pedantic

sources = $(shell find src/ -type f -and -iname '*.cpp' -or -iname '*.hpp')
cfiles = $(shell find src/ -type f -and -iname '*.cpp')

params = -o $(dir)/$(exe) $(flags) $(cflags)

debug: builddir
	$(cc) -g -DDEBUG $(params) $(cfiles)

release: builddir
	$(cc) -O4 -DNDEBUG -mtune=native -march=native $(params) $(cfiles)

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

codegen: builddir
	ghc --make -O3 -outputdir $(dir) -hidir $(dir) -o build/codegen tools/CodeGen.hs
