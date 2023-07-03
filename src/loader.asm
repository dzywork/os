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

#=======    open address A20
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
	movw $BaseOfKernelFile, %ax
	movw %ax, %fs
	movl (OffsetOfKernelFileCount), %edi

	movw $BaseTmpOfKernelAddr, %ax
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



;=======	get memory address size type

	mov	ax,	1301h
	mov	bx,	000Fh
	mov	dx,	0400h		;row 4
	mov	cx,	24
	push	ax
	mov	ax,	ds
	mov	es,	ax
	pop	ax
	mov	bp,	StartGetMemStructMessage
	int	10h

	mov	ebx,	0
	mov	ax,	0x00
	mov	es,	ax
	mov	di,	MemoryStructBufferAddr	

Label_Get_Mem_Struct:

	mov	eax,	0x0E820
	mov	ecx,	20
	mov	edx,	0x534D4150
	int	15h
	jc	Label_Get_Mem_Fail
	add	di,	20

	cmp	ebx,	0
	jne	Label_Get_Mem_Struct
	jmp	Label_Get_Mem_OK

Label_Get_Mem_Fail:

	mov	ax,	1301h
	mov	bx,	008Ch
	mov	dx,	0500h		;row 5
	mov	cx,	23
	push	ax
	mov	ax,	ds
	mov	es,	ax
	pop	ax
	mov	bp,	GetMemStructErrMessage
	int	10h
	jmp	$

Label_Get_Mem_OK:
	
	mov	ax,	1301h
	mov	bx,	000Fh
	mov	dx,	0600h		;row 6
	mov	cx,	29
	push	ax
	mov	ax,	ds
	mov	es,	ax
	pop	ax
	mov	bp,	GetMemStructOKMessage
	int	10h	

;=======	get SVGA information

	mov	ax,	1301h
	mov	bx,	000Fh
	mov	dx,	0800h		;row 8
	mov	cx,	23
	push	ax
	mov	ax,	ds
	mov	es,	ax
	pop	ax
	mov	bp,	StartGetSVGAVBEInfoMessage
	int	10h

	mov	ax,	0x00
	mov	es,	ax
	mov	di,	0x8000
	mov	ax,	4F00h

	int	10h

	cmp	ax,	004Fh

	jz	.KO
	
;=======	Fail

	mov	ax,	1301h
	mov	bx,	008Ch
	mov	dx,	0900h		;row 9
	mov	cx,	23
	push	ax
	mov	ax,	ds
	mov	es,	ax
	pop	ax
	mov	bp,	GetSVGAVBEInfoErrMessage
	int	10h

	jmp	$

.KO:

	mov	ax,	1301h
	mov	bx,	000Fh
	mov	dx,	0A00h		;row 10
	mov	cx,	29
	push	ax
	mov	ax,	ds
	mov	es,	ax
	pop	ax
	mov	bp,	GetSVGAVBEInfoOKMessage
	int	10h

;=======	Get SVGA Mode Info

	mov	ax,	1301h
	mov	bx,	000Fh
	mov	dx,	0C00h		;row 12
	mov	cx,	24
	push	ax
	mov	ax,	ds
	mov	es,	ax
	pop	ax
	mov	bp,	StartGetSVGAModeInfoMessage
	int	10h


	mov	ax,	0x00
	mov	es,	ax
	mov	si,	0x800e

	mov	esi,	dword	[es:si]
	mov	edi,	0x8200

Label_SVGA_Mode_Info_Get:

	mov	cx,	word	[es:esi]

;=======	display SVGA mode information

	push	ax
	
	mov	ax,	00h
	mov	al,	ch
	call	Label_DispAL

	mov	ax,	00h
	mov	al,	cl	
	call	Label_DispAL
	
	pop	ax

;=======
	
	cmp	cx,	0FFFFh
	jz	Label_SVGA_Mode_Info_Finish

	mov	ax,	4F01h
	int	10h

	cmp	ax,	004Fh

	jnz	Label_SVGA_Mode_Info_FAIL	

	add	esi,	2
	add	edi,	0x100

	jmp	Label_SVGA_Mode_Info_Get
		
Label_SVGA_Mode_Info_FAIL:

	mov	ax,	1301h
	mov	bx,	008Ch
	mov	dx,	0D00h		;row 13
	mov	cx,	24
	push	ax
	mov	ax,	ds
	mov	es,	ax
	pop	ax
	mov	bp,	GetSVGAModeInfoErrMessage
	int	10h

Label_SET_SVGA_Mode_VESA_VBE_FAIL:

	jmp	$

Label_SVGA_Mode_Info_Finish:

	mov	ax,	1301h
	mov	bx,	000Fh
	mov	dx,	0E00h		;row 14
	mov	cx,	30
	push	ax
	mov	ax,	ds
	mov	es,	ax
	pop	ax
	mov	bp,	GetSVGAModeInfoOKMessage
	int	10h

;=======	set the SVGA mode(VESA VBE)

	mov	ax,	4F02h
	mov	bx,	4180h	;========================mode : 0x180 or 0x143
	int 	10h

	cmp	ax,	004Fh
	jnz	Label_SET_SVGA_Mode_VESA_VBE_FAIL

;=======	init IDT GDT goto protect mode 

	cli			;======close interrupt

	db	0x66
	lgdt	[GdtPtr]

;	db	0x66
;	lidt	[IDT_POINTER]

	mov	eax,	cr0
	or	eax,	1
	mov	cr0,	eax	

	jmp	dword SelectorCode32:GO_TO_TMP_Protect


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




