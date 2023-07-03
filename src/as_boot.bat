@echo off

:: as --warn --64 boot.asm -o boot.o & ld -Ttext 0x7c00 boot.bin
as --warn --64 boot.asm -o boot.o & ld -Ttext 0x7c00  boot.o -o boot.bin

:: ld -Ttext 0x7c00 -e main -s --oformat pei-x86-64  -o boot.bin boot.o
:: as --warn --64 -R boot.asm -o boot.o
 ::ld -Ttext 0x7c00 -e main -s --image-base 0 --oformat pe-x86-64 -o  boot.bin boot.o
