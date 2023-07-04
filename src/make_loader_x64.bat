@echo off

mkdir  build
as --32 loader.asm -o build/loader.o -m i586
ld -Ttext 0x10000 -e _start -s -m i386pe -o build/loader.tmp build/loader.o
objcopy -j .text -I pe-i386 -O binary build/loader.tmp build/loader.bin
objdump -b binary -m i8086 -D build/loader.bin