[BITS 16]
ORG 0

global	_main
_main:
	; relocate from 0x07C0:0000 to 0x0060:0000
	mov	ax, 0x07C0
	mov	ds, ax
	mov	ax, 0x0060
	mov	es, ax
	mov	cx, 256
	xor	si, si
	xor	di, di
	cld
	rep
	movsw
	jmp	0x0060:go

go:
	; install the interrupt
	cli
	push	es	; Save es segment register
	xor	ax, ax	; Zero ax
	mov	es, ax	; Move 0000 into es. So loading the vector table segment
	mov	WORD [es:0x20*4], ReadFile	; offset of INT 20
	mov	WORD [es:0x20*4+2], cs		; segment of INT 20
	mov	WORD [es:0x21*4], SaveFileTable	; offset of INT 21
	mov	WORD [es:0x21*4+2], cs		; segment of INT 21
	pop	es
	sti

	; set VGA mode
	mov	ax, 0x0002	; ah: set display mode; al: 16color,80x25,8page
	int	0x10		; invoke BIOS

	; post message
	mov	si, msgBootldr
	call	DisplayMessage
	mov	si, msgLoadFS
	call	DisplayMessage

	; read file system table LBA 1 (0, 0, 2) to 0x0080:0000
	mov	ax, 0x0080
	mov	es, ax
	xor	bx, bx
	mov	cx, 0x01	; 1 sector
	mov	ax, 0x01	; LBA 1
	call	ReadSectors

	; post message
	mov	si, msgDone
	call	DisplayMessage
	mov	si, msgLoadKernel
	call	DisplayMessage

	; read kernel (file0) to 0x0100:0000
	mov	cx, 0x0000
	mov	ax, 0x0100
	mov	es, ax
	xor	bx, bx
	int	0x20

	; post message
	mov	si, msgDone
	call	DisplayMessage

	; jmp to kernel
	mov	ax, 0x0100
	mov	ds, ax
	mov	es, ax
	mov	fs, ax
	mov	gs, ax
	jmp	0x0100:0x0000

end:
	jmp	end

;;
;; read file cl to es:bx
;;
ReadFile:
	pusha
	push	ds
	mov	ax, 0x0080
	mov	ds, ax		; ds = 0x0080
.START
	push	cx
	; read first cluster from LBA cl+2 (0, 0, cl+3) to es:bx first
	mov	ax, 0x02
	add	ax, cx		; LBA cl+2
	mov	cx, 0x05	; 5 sector
	call	ReadSectors
	pop	cx

	; file = [0x0080:0x80+0x10*n]
	mov	ax, 0x0010
	mul	cl		; ax = 0x10 * cl
	add	ax, 0x0080	; ax = 0x10 * cl + 0x80
	mov	bx, ax
	mov	al, [bx+0x02]	; next of the file

.NEXT
	xor	ah, ah

	; now al point to next cluster
	cmp	al, 0xFF
	je	.DONE

	push	ax
	; read cluster from LBA al+0x51 (0, 0, al+0x52) to es:bx first
	mov	ax, es
	add	ax, 0x100
	mov	es, ax		; es += 0x100;
	add	ax, 0x51	; LBA al+0x51
	mov	cx, 0x05	; 5 sector
	call	ReadSectors
	pop	ax

	; next = [0x0080:0x180+n-1]
	add	ax, 0x17F
	mov	bx, ax		; bx = 0x180 + ax - 1
	mov	al, [bx]	; next of the file

	jmp	.NEXT

.DONE
	pop	ds
	popa
	iret

;;
;; Save File System Table
;;
SaveFileTable:
	pusha
	push	es

	; es:bx = 0x0080:0000
	mov	ax, 0x0080
	mov	es, ax
	xor	bx, bx

	mov	ah, 0x03	; BIOS write sector
	mov	al, 0x01	; write one sector
	mov	ch, 0x00	; cylinder 0
	mov	cl, 0x02	; sector 2
	mov	dh, 0x00	; head 0
	mov	dl, 0x00	; drive A:

	int	0x13

	pop	es
	popa
	iret

; Print the word contained in the dx register to the screen.
PrintHex:
	pusha
	mov   cx, 4      	; 4 hex digits
.PrintDigit:
	rol   dx, 4      	; rotate so that lowest 4 bits are used
   	mov   ax, 0E0Fh		; ah = request, al = mask for nybble
   	and   al, dl
   	add   al, 90h		; convert al to ascii hex (four instructions)
   	daa			;
   	adc   al, 40h
   	daa
   	int   10h
   	loop  .PrintDigit
	popa
   	ret

; Print a newline.
PrintNL:			; print CR and NL
	push	ax
	mov	ax, 0E0Dh	; CR
       	int	10h
       	mov	al, 0Ah		; LF
       	int	10h
	pop	ax
       	ret

DisplayMessage:
	lodsb			; load next character
	or	al, al		; test for NUL character
	jz	.DONE
	mov	ah, 0x0E	; BIOS teletype
	mov	bh, 0x00	; display page 0
	mov	bl, 0x07	; text attribute
	int	0x10		; invoke BIOS
	jmp	DisplayMessage
.DONE:
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PROCEDURE ReadSectors
; reads "cx" sectors from disk starting at "ax" into memory location "es:bx"
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ReadSectors:
	push	ds
	push	ax
	mov	ax, 0x0060
	mov	ds, ax
	pop	ax
.MAIN
	mov	di, 0x0005	; five retries for error
.SECTORLOOP
	push	ax
	push	bx
	push	cx
	call	LBA2CHS
	mov	ah, 0x02	; BIOS read sector
	mov	al, 0x01	; read one sector
	mov	ch, BYTE [absCylinder]	; cylinder
	mov	cl, BYTE [absSector]	; sector
	mov	dh, BYTE [absHead]	; head
	mov	dl, BYTE [DriveNumber]	; drive
	int	0x13		; invoke BIOS
	jnc	.SUCCESS	; test for read error
	xor	ax, ax		; BIOS reset disk
	int	0x13		; invoke BIOS
	dec	di		; decrement error counter
	pop	cx
	pop	bx
	pop	ax
	jnz	.SECTORLOOP	; attempt to read again
	int	0x18
.SUCCESS
	pop	cx
	pop	bx
	pop	ax
	add	bx, WORD [BytesPerSector]	; queue next buffer
	inc	ax		; queue next sector
	loop	.MAIN		; read next sector
	pop	ds
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PROCEDURE LBA2CHS
; convert "ax" LBA addressing scheme to CHS addressing scheme
; absCylinder  = logical sector / (sectors per track * number of heads)
; absHead      = (logical sector / sectors per track) MOD number of heads
; absSector    = (logical sector / sectors per track) + 1
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LBA2CHS:
	xor	dx, dx			; prepare dx:ax for operation
	div	WORD [SectorsPerTrack]	; calculate
	inc	dl			; adjust for sector 0
	mov	BYTE [absSector], dl
	xor	dx, dx			; prepare dx:ax for operation
	div	WORD [NumHeads]		; calculate
	mov	BYTE [absHead], dl
	mov	BYTE [absCylinder], al
	ret

BytesPerSector		dw 0x0200
SectorsPerTrack		dw 0x0012
NumHeads		dw 0x0002
DriveNumber		db 0x00

absSector		db 0x00
absHead			db 0x00
absCylinder		db 0x00

msgBootldr	db "=====[Jasonmel bootloader]=====", 0x0D, 0x0A, 0x00
msgLoadFS	db "Loading file system table... ", 0x00
msgLoadKernel	db "Loading kernel... ", 0x00
msgDone		db "Done!", 0x0D, 0x0A, 0x00

TIMES 510-($-$$) DB 0
DW 0xAA55

