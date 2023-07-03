@echo off
dd if=build/boot.bin of=../Bochs-2.7/boot.img bs=512 count=1 conv=notrunc