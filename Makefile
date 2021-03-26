PROJECT = tbc

tbc.rom: tbc.prg tbrun.prg
	./combine.pl tbc.prg tbrun.prg >tbc.rom

tbc.prg: tbc.asm bios.inc
	../dateextended.pl > date.inc
	../build.pl > build.inc
	rcasm -l -v -x -d 1802 tbc > tbc.lst
	cat tbc.prg | sed -f tbc.sed > x.prg
	rm tbc.prg
	mv x.prg tbc.prg

tbrun.prg: tbrun.asm bios.inc
	rcasm -l -v -x -d 1802 tbrun > tbrun.lst
	cat tbrun.prg | sed -f tbrun.sed > x.prg
	rm tbrun.prg
	mv x.prg tbrun.prg

hex: $(PROJECT).rom
	cat $(PROJECT).rom | ../tointel.pl > $(PROJECT).hex

install: $(PROJECT).rom
	cp $(PROJECT).rom ../../$(PROJECT).prg
	cd ../.. ; ./run -R $(PROJECT).prg

clean:
	-rm tbc.prg
	-rm tbrun.prg
	-rm tbc.rom

