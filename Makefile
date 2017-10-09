SOURCES = $(wildcard *.lisp)

.PHONY: clean all

all: mogerpg.exe

mogerpg.exe: $(SOURCES)
	sbcl --load build.lisp

clean:
	rm -f mogerpg.exe
