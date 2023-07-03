#======= loader.bin

.text
.global _start
_start:
    jmp Label_Start


.include "fat12.inc"

#.section .s16
Label_Start:
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
	movl $StartLoaderMessage, %ebp#此处使用bp不可以
	int $0x10

#=======	open address A20
	pushw %ax
	inb $0x92, %al
	orb $0b00000010, %al
	outb %al, $0x92
	popw %ax

	cli


     lgdt (GdtPtr)	

	movl %cr0, %eax
	orl $0x1, %eax
	movl %eax, %cr0

	movw $SelectorData32, %ax
	movw %ax, %fs
	movl %cr0, %eax
	andb $0b11111110, %al
	movl %eax, %cr0

	sti

#=======	reset floppy

	xorb %ah, %ah
	xorb %dl, %dl
	int	$0x13

#=======	search kernel.bin
	movw $SectorNumOfRootDirStart, (SectorNo)

Lable_Search_In_Root_Dir_Begin:

	cmpw $0, (RootDirSizeForLoop)
	jz   Label_No_LoaderBin
	decw (RootDirSizeForLoop)
	movw $0x00, %ax
	movw %ax, %es
	movw $0x8000, %bx
	movw (SectorNo), %ax
	movb $1, %cl
	call	Func_ReadOneSector
	movw $KernelFileName, %si
	movw $0x8000, %di
	cld
	movw $0x10, %dx
	/*
Label_Search_For_LoaderBin:

	cmpw $0, %dx
	jz	Label_Goto_Next_Sector_In_Root_Dir
	decw %dx
	movw $11, %cx

Label_Cmp_FileName:

	cmpw $0, %cx
	jz	Label_FileName_Found
	decw %cx
	lodsb	
	cmpb %es:(%di), %al
	jz Label_Go_On
	jmp Label_Different

Label_Go_On:
	
	incw di
	jmp Label_Cmp_FileName

Label_Different:

	andw $0x0FFE0, %di
	addw $0x20, %di
	movw $KernelFileName, %si
	jmp	Label_Search_For_LoaderBin

Label_Goto_Next_Sector_In_Root_Dir:
	
	addw $1, (SectorNo)
	jmp Lable_Search_In_Root_Dir_Begin
*/

#=======	display on screen : ERROR:No KERNEL Found

Label_No_LoaderBin:

	movw $0x1301, %ax
	movw $0x008C, %bx
	movw $0x0300, %dx# row 3
	movw $21, %cx
	pushw %ax
	movw %ds, %ax
	movw %ax, %es
	popw %ax
     movl $NoLoaderMessage, %ebp
	int	$0x10
	jmp	.

##
Func_ReadOneSector:
nop
##

#=======	tmp variable

RootDirSizeForLoop: .2byte RootDirSectors
SectorNo: .2byte 0
Odd: .byte 0
OffsetOfKernelFileCount: .4byte OffsetOfKernelFile

DisplayPosition: .4byte 0

#======= display messages

StartLoaderMessage: .ascii "Start Loader"
NoLoaderMessage: .ascii "ERROR:No KERNEL Found"
KernelFileName: .asciz "KERNEL  BIN"
StartGetMemStructMessage: .ascii "Start Get Memory Struct."
GetMemStructErrMessage: .ascii "Get Memory Struct ERROR"
GetMemStructOKMessage: .ascii "Get Memory Struct SUCCESSFUL!"

StartGetSVGAVBEInfoMessage: .ascii "Start Get SVGA VBE Info"
GetSVGAVBEInfoErrMessage: .ascii "Get SVGA VBE Info ERROR"
GetSVGAVBEInfoOKMessage: .ascii "Get SVGA VBE Info SUCCESSFUL!"

StartGetSVGAModeInfoMessage: .ascii "Start Get SVGA Mode Info"
GetSVGAModeInfoErrMessage: .ascii "Get SVGA Mode Info ERROR"
GetSVGAModeInfoOKMessage: .ascii "Get SVGA Mode Info SUCCESSFUL!"

BaseOfKernelFile = 0x00
OffsetOfKernelFile = 0x100000

BaseTmpOfKernelAddr = 0x00
OffsetTmpOfKernelFile = 0x7E00

MemoryStructBufferAddr = 0x7E00


#.section gdt

LABEL_GDT: .4byte 0,0
LABEL_DESC_CODE32: .4byte 0x0000FFFF,0x00CF9A00
LABEL_DESC_DATA32: .4byte 0x0000FFFF,0x00CF9200

GdtLen = . - LABEL_GDT
GdtPtr: .2byte GdtLen - 1
        .4byte LABEL_GDT

SelectorCode32 = LABEL_DESC_CODE32 - LABEL_GDT
SelectorData32 = LABEL_DESC_DATA32 - LABEL_GDT


#.section gdt64

LABEL_GDT64: .8byte 0x0000000000000000
LABEL_DESC_CODE64: .8byte 0x0020980000000000
LABEL_DESC_DATA64: .8byte 0x0000920000000000

GdtLen64 = . - LABEL_GDT64
GdtPtr64: .2byte GdtLen64 - 1
          .4byte LABEL_GDT64

SelectorCode64 = LABEL_DESC_CODE64 - LABEL_GDT64
SelectorData64 = LABEL_DESC_DATA64 - LABEL_GDT64




