compile:
	gprbuild -Pnes.gpr -cargs $(CFLAGS) -largs $(LDFLAGS)

run:
	./build/nes $(ROM)
