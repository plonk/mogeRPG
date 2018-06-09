SOURCES = $(wildcard *.lisp)

.PHONY: clean all

all: mogerpg.exe font.txt

mogerpg.exe: $(SOURCES)
	sbcl --load build.lisp

clean:
	rm -f mogerpg.exe

tileset16.six: tileset16.png
	./png2sixel.rb tileset16.png > tileset16.six

font.txt: tileset16.six
	drcssixel-test -c 1 -q -d 8x16 tileset16.six > font.txt
