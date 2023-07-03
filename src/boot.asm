
.text
.code16
#======= Define Symbols
.global main
main:
    jmp Label_Start
    nop
    BS_OEMName: .ascii "MINEboot"
    BPB_BytesPerSec: .2byte 512
    BPB_SecPerClus: .byte 1
    BPB_RsvdSecCnt:.2byte 1
    BPB_NumFATs: .byte 2
    BPB_RootEntCnt: .2byte 224
    BPB_TotSec16: .2byte 2880
    BPB_Media: .byte 0xf0
    BPB_FATSz16: .2byte 9
    BPB_SecPerTrk: .2byte 18
    BPB_NumHeads: .2byte 2
    BPB_HiddSec: .4byte 0
    BPB_TotSec32: .4byte 0
    BS_DrvNum: .byte 0
    BS_Reserved1: .byte 0
    BS_BootSig: .byte 0x29
    BS_VolID: .4byte 0
    BS_VolLab: .ascii "boot loader"
    BS_FileSysType: .ascii "FAT12   "

Label_Start:
    movw  %cs, %ax
    movw  %ax, %ds
    movw  %ax, %es
    movw  %ax, %ss
    movw  $BaseOfStack, %sp

#======= clear screen
    movw  $0x0600, %ax
    movw  $0x0700, %bx
    movw  $0, %cx
    movw  $0x0184, %dx
    int   $0x10
     
#======= set focus
    movw $0x200, %ax
    movw $0, %bx
    movw $0, %dx
    int  $0x10

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
    int  $0x10

#======= reset floppy
    xorb %ah, %ah
    xorb %dl, %dl
    int $0x13
    
    
#======= search loader.bin
    movw $SectorNumOfRootDirStart, (SectorNo)
    
Lable_Search_In_Root_Dir_Begin:
    cmpw $0, (RootDirSizeForLoop)
    jz Label_No_LoaderBin
    decw (RootDirSizeForLoop)
    movw $0x00, %ax
    movw %ax, %es
    movw $0x8000, %bx
    movw (SectorNo), %ax
    movb $1, %cl
    call Func_ReadOneSector
    movw $LoaderFileName, %si
    movw $0x8000, %di
    cld # 无操作数，会将flag寄存器的方向位置，使方向标志位DF=0
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
    andw $0x0ffe0, %di
    addw $0x20, %di
    movw $LoaderFileName, %si
    jmp Label_Search_For_LoaderBin

Label_Goto_Next_Sector_In_Root_Dir:
    addw $1, (SectorNo)
    jmp Lable_Search_In_Root_Dir_Begin

#=======    display on screen : ERROR:No LOADER Found
Label_No_LoaderBin:
    movw $0x1301, %ax
    movw $0x008c, %bx
    movw $0x0100, %dx
    movw $21, %cx
    pushw %ax
    movw %ds, %ax
    movw %ax, %es
    popw %ax
    movw $NoLoaderMessage, %bp
    int $0x10
    jmp .


#=======    found loader.bin name in root director struct

Label_FileName_Found:
    movw $RootDirSectors, %ax
    andw $0x0ffe0, %di
    addw $0x01a, %di
    movw %es:(%di), %cx
    pushw %cx
    addw %ax, %cx
    addw $SectorBalance, %cx
    movw $BaseOfLoader, %ax
    movw %ax, %es
    movw $OffsetOfLoader, %bx
    movw %cx, %ax

Label_Go_On_Loading_File:
    pushw %ax
    pushw %bx
    movb $0x0e, %ah
    movb $'.', %al
    movb $0x0f, %bl
    int $0x10
    popw %bx
    popw %ax

    movb $1, %cl
    call Func_ReadOneSector
    popw %ax
    call Func_GetFATEntry
    cmpw $0x0fff, %ax
    jz Label_File_Loaded
    pushw %ax
    movw $RootDirSectors, %dx
    addw %dx, %ax
    addw $SectorBalance, %ax
    addw (BPB_BytesPerSec), %bx
    jmp Label_Go_On_Loading_File

Label_File_Loaded:
    
    jmp $BaseOfLoader, $OffsetOfLoader

#======= read one sector from floppy

Func_ReadOneSector:
    pushw %bp
    movw %sp, %bp
    subl $2, %esp
    movb %cl, -2(%bp)
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
    int  $0x13
    jc   Label_Go_On_Reading
    addl $2, %esp
    popw %bp
    ret

#======= get FAT Entry

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
    movw $8000, %bx
    addw $SectorNumOfFAT1Start, %ax
    movb $2, %cl
    call Func_ReadOneSector
    
    popw %dx
    addw %dx, %bx
    movw %es:(%bx), %ax
    cmpb $1, (Odd)
    jnz Label_Even_2
    shrw $4, %ax

Label_Even_2:
    andw $0x0fff, %ax
    popw %bx
    popw %es
    ret
    
RootDirSizeForLoop: .2byte RootDirSectors
SectorNo: .2byte 0
Odd: .byte 0

BaseOfStack = 0x7c00

BaseOfLoader =    0x1000
OffsetOfLoader = 0x00

RootDirSectors = 14
SectorNumOfRootDirStart = 19
SectorNumOfFAT1Start = 1
SectorBalance = 17    

#======= display messages
StartBootMessage: .ascii "Start Boot"
StartBootMessageLength = . - StartBootMessage
NoLoaderMessage: .ascii "ERROR:No LOADER Found"
LoaderFileName: .ascii "LOADER  BIN"
.byte 0


#======= fill zero until whole sector
.org 510,0
.2byte 0xaa55
