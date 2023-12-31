@echo off

mkdir  build
as --32 loader.asm -o build/loader.o -march=i686 --no-pad-sections
::ld -Ttext 0x10000 -e _start -m i386pe -b pe-i386 -s --nostdlib -o build/loader.tmp build/loader.o --disable-reloc-section --large-address-aware
ld -Ttext 0x0 -e _start -m i386pe -b pe-i386 --oformat pe-i386 -s --nostdlib --section-alignment 0x0 --file-alignment 0x0 -o build/loader.tmp build/loader.o --image-base 0x0
::不要使用-j参数
::objcopy -j .text -I pe-i386 -O binary build/loader.tmp build/loader.bin
objcopy -I pe-i386 -O binary build/loader.tmp build/loader.bin
objdump -b binary -m i8086 -D build/loader.tmp