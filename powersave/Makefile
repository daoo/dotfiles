exe = powersave
dir = build

flags = -std=c99 -Isrc -Wall -Wextra -pedantic
cc = clang
files = src/main.c src/settings.c src/functions.c src/power.c

params = -o $(dir)/$(exe) $(flags) $(files)

debug:
	$(cc) -g -DDEBUG $(params)

release:
	$(cc) -O3 -DNDEBUG $(params)
