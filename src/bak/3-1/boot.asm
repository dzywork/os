
.text
.code16
#======= Define Symbols


.global main
main:
    movw  %cs, %ax
    movw  %ax, %ds
    movw  %ax, %es
    movw  %ax, %ss
    movw  $BaseOfStack, %sp

#======= clear screen
    movw  $0x0600, %ax
    movw $0x0700, %bx
    movw $0, %cx
    movw $0x0184, %dx
    int $0x10
     
#======= set focus
    movw $0x200, %ax
    movw $0, %bx
    movw $0, %dx
    int $0x10

#======= display on screen: Start Booting......
    movw $0x1301, %ax
    movw $0xf, %bx
    movw $0, %dx
    movw $StartBootMessageLength, %cx
    pushw %ax
    movw %ds, %ax
    movw %ax, %es
    popw %ax
    movw $StartBootMessage, %bp
    int $0x10

#======= reset floppy
    xorb %ah, %ah
    xorb %dl, %dl
    int $0x13
    hlt
    
    
    
    
#======= search loader.bin
    
StartBootMessage: .ascii "Start Boot"
StartBootMessageLength = . - StartBootMessage
BaseOfStack = 0x7c00


#======= fill zero until whole sector
.org 510,0
.word 0xaa55
