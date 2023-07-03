@echo off
ld -Ttext 0x7c00 -e main -s --oformat binary --dll -s --exclude-all-symbols -o boot.bin boot.o
objdump -S -M i8086 boot.bin