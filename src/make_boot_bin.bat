@echo off
mkdir  build
as --32 boot.asm -o build/boot.o
ld -Ttext 0x7c00 -e main -s -m i386pe -o build/boot.tmp build/boot.o
objcopy -j .text -I pei-i386 -O binary build/boot.tmp build/boot.bin
objdump -b binary -m i8086 -D build/boot.bin