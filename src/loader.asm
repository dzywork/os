#======= loader.bin

.text
.global _start
_start:
    jmp Label_Start


.include "fat12.inc"

.section .s16
.code16
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

#=======    open address A20
    pushw %ax
    inb $0x92, %al
    orb $0b00000010, %al
    outb %al, $0x92
    popw %ax

    cli


    addr32 data32 lgdt (GdtPtr)

    movl %cr0, %eax
    orl $0x1, %eax
    movl %eax, %cr0

    movw $SelectorData32, %ax
    movw %ax, %fs
    movl %cr0, %eax
    andb $0b11111110, %al
    movl %eax, %cr0

    sti

#=======    reset floppy

    xorb %ah, %ah
    xorb %dl, %dl
    int    $0x13

#=======    search kernel.bin
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
    callw Func_ReadOneSector
    movl $KernelFileName, %esi
    movw $0x8000, %di
    cld
    movw $0x10, %dx
    
Label_Search_For_LoaderBin:

    cmpw $0, %dx
    jz Label_Goto_Next_Sector_In_Root_Dir
    decw %dx
    movw $11, %cx

Label_Cmp_FileName:

    cmpw $0, %cx
    jz Label_FileName_Found
    decw %cx
    lodsb    
    cmpb %es:(%di), %al
    jz Label_Go_On
    jmp Label_Different

Label_Go_On:
    
    incw %di
    jmp Label_Cmp_FileName

Label_Different:

    andw $0x0FFE0, %di
    addw $0x20, %di
    movl $KernelFileName, %esi
    jmp Label_Search_For_LoaderBin

Label_Goto_Next_Sector_In_Root_Dir:
    
    addw $1, (SectorNo)
    jmp Lable_Search_In_Root_Dir_Begin


#=======    display on screen : ERROR:No KERNEL Found

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
    int $0x10
    jmp .

#=======	found loader.bin name in root director struct

Label_FileName_Found:
	movw $RootDirSectors, %ax
	andw $0x0FFE0, %di
	addw $0x1A, %di
	movw %es:(%di), %cx
	pushw %cx
	addw %ax, %cx
	addw $SectorBalance, %cx
	movl $BaseTmpOfKernelAddr, %eax#BaseOfKernelFile
	movl %eax, %es
	movw $OffsetTmpOfKernelFile, %bx#OffsetOfKernelFile
	movw %cx, %ax

Label_Go_On_Loading_File:
	pushw %ax
	pushw %bx
	movb $0x0E, %ah
	movb $'.', %al
	movb $0x0F, %bl
	int $0x10
	popw %bx
	popw %ax

	movb $1, %cl
	callw Func_ReadOneSector
	popw %ax

######################	
	pushw %cx
	pushl %eax
	pushw %fs
	pushl %edi
	pushw %ds
	pushl %esi

	movw $0x200, %cx
	movl $BaseOfKernelFile, %eax
	movw %ax, %fs
	movl (OffsetOfKernelFileCount), %edi

	movl $BaseTmpOfKernelAddr, %eax
	movw %ax, %ds
	movl $OffsetTmpOfKernelFile, %esi

Label_Mov_Kernel:	#------------------
	
	movb %ds:(%esi), %al
	movb %al, %fs:(%edi)

	incl %esi
	incl %edi

	loop Label_Mov_Kernel

	movl $0x1000, %eax
	movl %eax, %ds

	movl %edi, (OffsetOfKernelFileCount)

	popl %esi
	popw %ds
	popl %edi
	popw %fs
	popl %eax
	popw %cx
######################

	callw Func_GetFATEntry
	cmpw $0x0FFF, %ax
	jz Label_File_Loaded
	pushw %ax
	movw $RootDirSectors, %dx
	addw %dx, %ax
	addw $SectorBalance, %ax

	jmp Label_Go_On_Loading_File

Label_File_Loaded:
		
	movw $0x0B800, %ax
	movw %ax, %gs
	movb $0x0F, %ah # 0000: 黑底    1111: 白字
	movb $'G', %al
	movw %ax, %gs:(((80 * 0 + 39) * 2))# 屏幕第 0 行, 第 39 列。

KillMotor:
	
	pushw %dx
	movw $0x03F2, %dx
	movb $0, %al
	outb %al, %dx
	popw %dx



#=======	get memory address size type

	movw $0x1301, %ax
	movw $0x000F, %bx
	movw $0x400, %dx
	movw $4, %cx
	pushw %ax
	movw %ds, %ax
	movw %ax, %es
	popw %ax
	movl $StartGetMemStructMessage, %ebp
	int $0x10

	movl $0, %ebx
	movw $0x00, %ax
	movw %ax, %es
	movl $MemoryStructBufferAddr, %edi

Label_Get_Mem_Struct:

	movl $0x0E820, %eax
	movl $0x20, %ecx
	movl $0x534D4150, %edx
	int $0x15
	jc Label_Get_Mem_Fail
	add $20, %di

	cmpl $0, %ebx
	jne Label_Get_Mem_Struct
	jmp Label_Get_Mem_OK

Label_Get_Mem_Fail:

	movw $0x1301, %ax
	movw $0x008C, %bx
	movw $0x0500, %dx#row:5
	movw $23, %cx
	pushw %ax
	movw %ds, %ax
	movw %ax, %es
	popw %ax
	movl $GetMemStructErrMessage, %ebp
	int $0x10

	jmp .

Label_Get_Mem_OK:
	
	movw $0x1301, %ax
	movw $0x000F, %bx
	movw $0x0600, %dx#row: 6
	movw $29, %cx
	pushw %ax
	movw %ds, %ax
	movw %ax, %es
	popw %ax
	movl $GetMemStructOKMessage, %ebp
	int $0x10	

#=======	get SVGA information

	movw $0x1301, %ax
	movw $0x000F, %bx
	movw $0x800, %dx
	movw $23, %cx
	pushw %ax
	movw %ds, %ax
	movw %ax, %es
	popw %ax
	movl $StartGetSVGAVBEInfoMessage, %ebp
	int $0x10

	movw $0x00, %ax
	movw %ax, %es
	movw $0x8000, %di
	movw $0x4F00, %ax

	int $0x10

	cmpw $0x004F, %ax

	jz .KO
	
#=======	Fail

	movw $0x1301, %ax
	movw $0x008c, %bx
	movw $0x900, %dx
	movw $23, %cx
	pushw %ax
	movw %ds, %ax
	movw %ax, %es
	popw %ax
	movl $GetSVGAVBEInfoErrMessage, %ebp
	int $0x10

	jmp .

.KO:

	movw $0x1301, %ax
	movw $0x000F, %bx
	movw $0x0A00, %dx
	movw $29, %cx
	pushw %ax
	movw %ds, %ax
	movw %ax, %es
	popw %ax
	movl $GetSVGAVBEInfoOKMessage, %ebp
	int $0x10

#=======	Get SVGA Mode Info

	movw $0x1301, %ax
	movw $0x000F, %bx
	movw $0x0C00, %dx
	movw $24, %cx
	pushw %ax
	movw %ds, %ax
	movw %ax, %es
	popw %ax
	movl $StartGetSVGAModeInfoMessage, %ebp
	int $0x10


	movw $0x00, %ax
	movw %ax, %es
	movw $0x800E, %si

	movl %es:(%si), %esi
	movl $0x8200, %edi

Label_SVGA_Mode_Info_Get:

	movw %es:(%esi), %cx

#=======	display SVGA mode information

	pushw %ax
	
	movw $0x00, %ax
	movb %ch, %al
	callw Label_DispAL

	movw $0x00, %ax
	movb %cl, %al
	call Label_DispAL
	
	popw %ax

#=======
	
	cmpw $0x0FFFF, %cx
	jz Label_SVGA_Mode_Info_Finish

	movw $0x4F01, %ax
	int $0x10

	cmpw $0x004F, %ax

	jnz Label_SVGA_Mode_Info_FAIL	

	addl $2, %esi
	addl $0x100, %edi

	jmp Label_SVGA_Mode_Info_Get
		
Label_SVGA_Mode_Info_FAIL:

	movw $0x1301, %ax
	movw $0x008C, %bx
	movw $0x0D00, %dx
	movw $24, %cx
	pushw %ax
	movw %ds, %ax
	movw %ax, %es
	popw %ax
	movl $GetSVGAModeInfoErrMessage, %ebp
	int $0x10

Label_SET_SVGA_Mode_VESA_VBE_FAIL:

	jmp .

Label_SVGA_Mode_Info_Finish:

	movw $0x1301, %ax
	movw $0x000F, %bx
	movw $0x0E00, %dx
	movw $30, %cx
	pushw %ax
	movw %ds, %ax
	movw %ax, %es
	popw %ax
	movl $GetSVGAModeInfoOKMessage, %ebp
	int $0x10

#=======	set the SVGA mode(VESA VBE)

	movw $0x4F02, %ax
	movw $0x4180, %bx #========================mode : 0x180 or 0x143
	int $0x10

	cmpw $0x004F, %ax
	jnz Label_SET_SVGA_Mode_VESA_VBE_FAIL

#=======	init IDT GDT goto protect mode 

	cli			#======close interrupt

	#db	0x66
	lgdt (GdtPtr)

#	db	0x66
#	lidt	[IDT_POINTER]

	movl %cr0, %eax
	orl $1, %eax
	movl %eax, %cr0

	jmpl $SelectorCode32, $GO_TO_TMP_Protect



.section .s32
.code32

GO_TO_TMP_Protect:

#=======	go to tmp long mode

	movw $0x10, %ax
	movw %ax, %ds
	movw %ax, %es
	movw %ax, %fs
	movw %ax, %ss
	movl $0x7E00, %esp

	calll support_long_mode
	testl %eax, %eax

	jz no_support

#=======	init temporary page table 0x90000

	movl $0x91007, (0x90000)

	movl $0x91007, (0x90800)

	movl $0x92007, (0x91000)

	movl $0x000083, (0x92000)

	movl $0x200083, (0x92008)

	movl $0x400083, (0x92010)

	movl $0x600083, (0x92018)

	movl $0x800083, (0x92020)

	movl $0xa00083, (0x92028)

#=======	load GDTR

	#db	0x66
	lgdt (GdtPtr64)
	movw $0x10, %ax
	movw %ax, %ds
	movw %ax, %es
	movw %ax, %fs
	movw %ax, %gs
	movw %ax, %ss

	movl $0x7E00, %esp

#=======	open PAE

	movl %cr4, %eax
	btsl $5, %eax
	movl %eax, %cr4

#=======	load	cr3

	movl $0x90000, %eax
	movl %eax, %cr3

#=======	enable long-mode

	movl $0x00C0000080, %ecx
	rdmsr

	btsl $8, %eax
	wrmsr

#=======	open PE and paging

	movl %cr0, %eax
	btsl $0, %eax
	btsl $31, %eax
	movl %eax, %cr0

	jmpl $SelectorCode64, $OffsetOfKernelFile

#=======	test support long mode or not

support_long_mode:

	movl $0x80000000, %eax
	cpuid
	cmpl $0x80000001, %eax
	setnb %al	
	jb support_long_mode_done
	movl $0x80000001, %eax
	cpuid
	btl $29, %edx
	setc %al
support_long_mode_done:
	
	movzx %al, %eax
	retl

#=======	no support

no_support:
	jmp	.

#=======	read one sector from floppy

.section .s16lib
.code16

Func_ReadOneSector:
	
	pushw %bp
	movw %sp, %bp
	subl $2, %esp
	movb -2(%bp), %cl
	pushw %bx
	movb (BPB_SecPerTrk), %bl
	divb %bl
	incb %ah
	movb %ah, %cl
	movb %al, %dh
	shrb $1, %al
	movb %al, %ch
	andb $1, %dh
	popw %bx
	movb (BS_DrvNum), %dl
Label_Go_On_Reading:
	movb $2, %ah
	movb -2(%bp), %al
	int $0x13
	jc Label_Go_On_Reading
	addl $2, %esp
	popw %bp
	ret

#=======	get FAT Entry

Func_GetFATEntry:

	pushw %es
	pushw %bx
	pushw %ax
	movw $0, %ax
	movw %ax, %es
	popw %ax
	movb $0, (Odd)
	movw $3, %bx
	mulw %bx
	movw $2, %bx
	divw %bx
	cmpw $0, %dx
	jz Label_Even
	movb $1, (Odd)

Label_Even:

	xorw %dx, %dx
	movw (BPB_BytesPerSec), %bx
	divw %bx
	pushw %dx
	movw $0x8000, %bx
	addw $SectorNumOfFAT1Start, %ax
	movb $2, %cl
	callw Func_ReadOneSector
	
	popw %dx
	addw %dx, %bx
	movw %es:(%bx), %ax
	cmpb $1, (Odd)
	jnz Label_Even_2
	shrw $4, %ax

Label_Even_2:
	andw $0x0FFF, %ax
	popw %bx
	popw %es
	ret

#=======	display num in al

Label_DispAL:

	pushl %ecx
	pushl %edx
	pushl %edi
	
	movl (DisplayPosition), %edi
	movb $0x0F, %ah
	movb %al, %dl
	shr $4, %al
	movl $2, %ecx
.begin:

	andb $0x0F, %al
	cmpb $9, %al
	ja	.1
	addb $'0', %al
	jmp	.2
.1:

	subb $0x0A, %al
	addb $'A', %al
.2:

	movw %ax, %gs:(%edi)
	addl $2, %edi
	
	movb $dl, %al
	loop .begin

	movl %edi, (DisplayPosition)

	popl %edi
	popl %edx
	popl %ecx
	
	ret
*/

#=======	tmp IDT

IDT:
	.rept 	0x50	
        .8byte  0
        .endr
IDT_END:

IDT_POINTER:
		.2byte IDT_END - IDT - 1
		.4byte IDT


#=======    tmp variable

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




