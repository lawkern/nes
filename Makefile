compile:
	gprbuild -Pnes.gpr -j0 -cargs $(CFLAGS) -largs $(LDFLAGS)

run:
	./build/nes $(ROM)

generate:
	@./build/nes $(ROM) > ./misc/generated.log
