#!/bin/bash
if test -f s2built.bin; then
	mv -f s2built.bin s2built.prev.bin
fi
./tool/linux/asl -xx -q -A -L -U -E -i . main.asm
./tool/linux/p2bin -z=0,saxman-bugged,Size_of_Snd_driver_guess,after main.p s2built.bin
rm -f main.p