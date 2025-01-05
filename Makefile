CFLAGS += $(shell pkg-config --cflags sdl3)
LDFLAGS += $(shell pkg-config --libs sdl3)

compile:
	gprbuild -Pnes.gpr -j0 -cargs $(CFLAGS) -largs $(LDFLAGS)

run:
	./build/nes $(ROM)

generate:
	@./build/nes $(ROM) > ./misc/generated.log
