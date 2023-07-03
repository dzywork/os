#======= loader.bin
.code16gcc
.text
.global _start
_start:
	movw %cs, %ax
	movw %ax, %ds
	movw %ax, %es
	movw $0x00, %ax
	movw %ax, %ss
	movw $0x7c00, %sp

#======= display on screen : Start Loader......

	movw $0x1301, %ax
	movw $0x000f, %bx
	movw $0x0200, %dx # row 2
	movw $12, %cx
	pushw %ax
	movw %ds, %ax
	movw %ax, %es
	popw %ax
	movl $StartLoaderMessage, %ebp
	int $0x10

	jmp .

#======= display messages

StartLoaderMessage: .ascii "Start Loader"





