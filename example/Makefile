all: clean
	gprbuild -j8 -Xmod=release -p -Pex

all-debug: clean
	gprbuild -j8 -Xmod=debug -p -Pex

clean:
	gprclean -Pex

.PHONY: all all-debug clean
