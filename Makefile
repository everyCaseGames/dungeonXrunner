all: dungeonXrunner.nes

%.nes: %.asm
	nesasm $<

sim: dungeonXrunner.nes
	fceux dungeonXrunner.nes

debug: dungeonXrunner.nes
	mednafen dungeonXrunner.nes

clean:
	rm -f *.o dungeonXrunner.nes dungeonXrunner.fns 
