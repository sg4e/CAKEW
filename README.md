# CAKEW

By sg4e.

This is a short game I'm developing to learn more about how the SNES works.

Builds with `cc65`:
```sh
ca65 --cpu 65816 -s -o cake-w.o cake-w.s
ld65 -C memmap.cfg -o cake-w.sfc cake-w.o
```

Runs at least on the [BSNES emulator](https://github.com/bsnes-emu/bsnes). It will eventually run on real console but I haven't coded all the required initialization or tested yet.
