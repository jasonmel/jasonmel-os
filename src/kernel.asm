[BITS 16]

;;;;; segment .text ;;;;;
segment .text

_start:
	; set stack from 0x9000:0000 to 0x9000:FFFF
	mov	ax, 0x9000
	mov	ss, ax
	mov	sp, 0xFFFF

	; show welcome
	push	welcome
	call	print_str
	pop	cx

	push	ds
	; copy table here from 0x0080:0000 to 0x0100:label0
	mov	ax, 0x0080
	mov	ds, ax
	mov	ax, 0x0100
	mov	es, ax
	mov	cx, 384
	xor	si, si
	mov	di, label0
	cld
	rep
	movsw
	pop	ds

.SHELL
	; Show prompt
	push	prompt
	call	print_str
	pop	cx

	; Get command
	push	cmd
	call	scan_str
	pop	cx

	; Parse for ls
	push	cmd
	push	cmd_ls
	call	strcmp
	pop	cx
	pop	cx
	or	ax, ax
	jnz	.LS_HANDLER

;	; Parse for add
;	push	cmd
;	push	cmd_add
;	call	strcmp
;	pop	cx
;	pop	cx
;	or	ax, ax
;	jnz	.ADD_HANDLER

	; Parse for rm
	push	cmd
	push	cmd_rm
	call	strcmp
	pop	cx
	pop	cx
	or	ax, ax
	jnz	.RM_HANDLER

	; Parse for lls
	push	cmd
	push	cmd_lls
	call	strcmp
	pop	cx
	pop	cx
	or	ax, ax
	jnz	.LLS_HANDLER

;	; Parse for ladd
;	push	cmd
;	push	cmd_ladd
;	call	strcmp
;	pop	cx
;	pop	cx
;	or	ax, ax
;	jnz	.LADD_HANDLER

;	; Parse for lrm
;	push	cmd
;	push	cmd_lrm
;	call	strcmp
;	pop	cx
;	pop	cx
;	or	ax, ax
;	jnz	.LRM_HANDLER

;	; Parse for label
;	push	cmd
;	push	cmd_label
;	call	strcmp
;	pop	cx
;	pop	cx
;	or	ax, ax
;	jnz	.LABEL_HANDLER

	jmp	.DEFAULT_HANDLER

.LS_HANDLER
	call	ls_handler
	jmp	.NEXT

;.ADD_HANDLER
;	call	add_handler
;	jmp	.NEXT

.RM_HANDLER
	call	rm_handler
	int	0x21
	jmp	.NEXT

.LLS_HANDLER
	call	lls_handler
	jmp	.NEXT

;.LADD_HANDLER
;	call	ladd_handler
;	jmp	.NEXT

;.LRM_HANDLER
;	call	lrm_handler
;	jmp	.NEXT

;.LABEL_HANDLER
;	call	label_handler
;	jmp	.NEXT

.DEFAULT_HANDLER
	push	default
	call	print_str
	pop	cx
	call	print_nl
	jmp	.NEXT

.NEXT
	jmp	.SHELL
	; end of SHELL

	ret

;;
;; void ls_handler(void)
;;
	nop
	nop
ls_handler:
	push	bp
	mov	bp, sp

	push	ls_header
	call	print_str
	pop	cx

	push	ds
	mov	ax, 0x0080
	mov	ds, ax		; ds = 0x0080

	xor	cx, cx
.LOOP
	; file_n = [0x0080:0x80+0x10*n]
	mov	ax, 0x0010
	mul	cl		; ax = 0x10 * cl
	add	ax, 0x0080	; ax = 0x10 * cl + 0x80

	; check if valid file
	push	ax
	add	ax, 0x02
	mov	bx, ax
	pop	ax
	cmp	BYTE [bx], 0x00
	je	.NEXT

	push	ax
	call	print_file
	pop	ax

.NEXT
	inc	cl
	cmp	cl, 0x10
	jb	.LOOP

.DONE
	call	print_nl
	pop	ds
	pop	bp
	;leave
	ret

;;
;; void add_handler(void)
;;
;	nop
;	nop
;add_handler:
;	push	bp
;	mov	bp, sp

;.DONE
;	call	print_nl
;	pop	bp
;	;leave
;	ret

;;
;; void rm_handler(void)
;;
	nop
	nop
rm_handler:
	push	bp
	mov	bp, sp

	; Parse for files
	xor	dx, dx
.LOOP
	mov	ax, dx
	mov	cl, 0x10
	mul	cl		; ax = dx * 0x10
	add	ax, file0	; ax = file0 + dx * 0x10
	add	ax, 0x04

	mov	bx, cmd
	add	bx, 0x03	; bx = cmd + 0x03

	push	ax
	push	bx
	call	strcmp
	pop	cx
	pop	cx
	or	ax, ax
	jnz	.FOUND

	inc	dx
	cmp	dx, 0x10
	je	.NOT_FOUND
	jmp	.LOOP

.FOUND
	; now dx is the matched file no.

	; check if valid file
	mov	ax, dx
	mov	cl, 0x10
	mul	cl		; ax = dx * 0x10
	add	ax, file0	; ax = file0 + dx * 0x10
	mov	bx, ax
	cmp	BYTE [bx+0x02], 0x00
	je	.LOOP

	; save to kernel table
	mov	BYTE [bx+0x02], 0x00
	; save to system table
	mov	ax, dx
	mov	cl, 0x10
	mul	cl
	add	ax, 0x80
	mov	bx, ax
	push	es
	mov	ax, 0x0080
	mov	es, ax
	mov	BYTE [es:bx+0x02], 0x00
	pop	es

	push	rm_found
	call	print_str
	pop	cx
	jmp	.DONE

.NOT_FOUND
	push	rm_notfound
	call	print_str
	pop	cx

.DONE
	call	print_nl
	pop	bp
	;leave
	ret

;;
;; void lls_handler(void)
;;
	nop
	nop
lls_handler:
	push	bp
	mov	bp, sp

	; Parse for labels
	xor	dx, dx
.LOOP
	mov	ax, dx
	mov	cl, 0x08
	mul	cl		; ax = dx * 0x08
	add	ax, label0	; ax = label0 + dx * 0x08

	mov	bx, cmd
	add	bx, 0x04	; bx = cmd + 0x04

	push	ax
	push	bx
	call	strcmp
	pop	cx
	pop	cx
	or	ax, ax
	jnz	.FOUND

	inc	dx
	cmp	dx, 0x10
	je	.NOT_FOUND
	jmp	.LOOP

.FOUND
	mov	ax, 0x01
	mov	cl, dl
	shl	ax, cl		; now ax is the bit of the label

	push	ls_header
	call	print_str
	pop	cx

	push	ds
	push	ax
	mov	ax, 0x0080
	mov	ds, ax		; ds = 0x0080
	pop	ax
	;mov	[tmp], ax
	mov	[508], ax

	xor	cx, cx
.LLOOP
	; file_n = [0x0080:0x80+0x10*n]
	mov	ax, 0x0010
	mul	cl		; ax = 0x10 * cl
	add	ax, 0x0080	; ax = 0x10 * cl + 0x80

	; check if valid file
	push	ax
	add	ax, 0x02
	mov	bx, ax
	pop	ax
	cmp	BYTE [bx], 0x00
	je	.NEXT

	; check if right label
	push	ax
	mov	bx, ax
	;mov	ax, [tmp]
	mov	ax, [508]
	and	ax, [bx]
	mov	bx, ax
	pop	ax
	cmp	bx, 0x00
	je	.NEXT

	push	ax
	call	print_file
	pop	ax

.NEXT
	inc	cl
	cmp	cl, 0x10
	jb	.LLOOP

	pop	ds
	jmp	.DONE

.NOT_FOUND
	push	lls_notfound
	call	print_str
	pop	cx

.DONE
	call	print_nl
	pop	bp
	;leave
	ret

;;
;; void ladd_handler(void)
;;
;	nop
;	nop
;ladd_handler:
;	push	bp
;	mov	bp, sp
;
;.DONE
;	call	print_nl
;	pop	bp
;	;leave
;	ret

;;
;; void lrm_handler(void)
;;
;	nop
;	nop
;lrm_handler:
;	push	bp
;	mov	bp, sp
;
;.DONE
;	call	print_nl
;	pop	bp
;	;leave
;	ret

;;
;; void label_handler(void)
;;
;	nop
;	nop
;label_handler:
;	push	bp
;	mov	bp, sp
;
;.DONE
;	call	print_nl
;	pop	bp
;	;leave
;	ret

;;
;; int strcmp(char* str1, char* str2);
;; @param str1 String to be compared.
;; @param str2 String to be compared.
;; @return 0 if false, current address if true.
;;
	nop
	nop
strcmp:
	push	bp
	mov	bp, sp
	mov	si, WORD [bp+4]	; str1 (defined)
	mov	di, WORD [bp+6]	; str2 (user input)
.LOOP
	lodsb
	cmp	al, 0x00	; check for end of string
	je	.TRUE
	cmp	al, BYTE [di]	; check for str2
	jne	.FALSE

	inc	di
	jmp	.LOOP
.FALSE
	mov	ax, 0x00
	jmp	.DONE
.TRUE
	inc	di
	mov	ax, di
	jmp	.DONE
.DONE
	pop	bp
	;leave
	ret

;;
;; void print_file(file* f);
;; @param f File to print out.
;;
	nop
	nop
print_file:
	push	bp
	mov	bp, sp
	mov	ax, WORD [bp+4]

	; print file name
	push	ax
	add	ax, 0x04
	push	ax
	call	print_str
	pop	ax
	pop	ax

	call	print_sp
	; print size
	push	ax
	mov	bx, ax
	mov	al, [bx+0x03]
	mov	dx, ax
	call	print_hex
	pop	ax

	call	print_sp
	call	print_sp
	call	print_sp
	; print label
	push	ax
	mov	bx, ax
	mov	ax, [bx]	; ax = label_map
	xor	dx, dx
.LABEL_LOOP
	mov	bx, ax
	and	bx, 0x01
	cmp	bx, 0x00
	je	.LABEL_NEXT
	push	ax
	mov	ax, dx
	mov	bl, 0x08
	mul	bl
	push	ax
	call	print_str
	pop	ax
	pop	ax
	call	print_sp
.LABEL_NEXT
	shr	ax, 0x01
	inc	dx
	cmp	dx, 0x10
	je	.LABEL_OUT
	jmp	.LABEL_LOOP
.LABEL_OUT
	pop	ax

	call	print_nl
.DONE
	pop	bp
	;leave
	ret

;;
;; void print_str(char* str);
;; @param str String to print out.
;;
	nop
	nop
print_str:
	push	bp
	mov	bp, sp
	mov	si, WORD [bp+4]
	pusha
.LOOP
	lodsb			; load next character
	or	al, al		; test for NUL character
	jz	.DONE
	mov	ah, 0x0E	; BIOS teletype
	mov	bh, 0x00	; display page 0
	mov	bl, 0x07	; text attribute
	int	0x10		; invoke BIOS
	jmp	.LOOP
.DONE
	popa
	pop	bp
	;leave
	ret

;;
;; void scan_str(char* str);
;; @param str String to scan from the user.
;;
	nop
	nop
scan_str:
	push	bp
	mov	bp, sp
	mov	di, WORD [bp+4]
.LOOP
	mov	ah, 0x00	; get keyboard char
	int	0x16
	cmp	al, 0x0D	; if "return"
	je	.DONE		; yes? goto end;
	stosb

	; print out
	mov	ah, 0x0E	; BIOS teletype
	mov	bh, 0x00	; display page 0
	mov	bl, 0x07	; text attribute
	int	0x10		; invoke BIOS

	jmp	.LOOP
.DONE
	mov	al, 0x00	; put 0x0 at the end of string
	stosb

	call	print_nl

	pop	bp
	;leave
	ret

;;
;; print the word contained in the dx register to the screen.
;;
	nop
	nop
print_hex:
	pusha
	mov   cx, 4      	; 4 hex digits
.LOOP
	rol   dx, 4      	; rotate so that lowest 4 bits are used
   	mov   ax, 0E0Fh		; ah = request, al = mask for nybble
   	and   al, dl
   	add   al, 90h		; convert al to ascii hex (four instructions)
   	daa			;
   	adc   al, 40h
   	daa
   	int   10h
   	loop  .LOOP
	popa
   	ret

;;
;; print a new line
;;
	nop
	nop
print_nl:
	push	ax
	mov	ax, 0x0E0D	; CR
	int	10h
	mov	al, 0x0A	; LF
	int	10h
	pop	ax
	ret

;;
;; print a space
;;
	nop
	nop
print_sp:
	push	ax
	mov	ax, 0x0E20	; Space
	int	10h
	pop	ax
	ret

;;;;; segment .data ;;;;;
segment .data

welcome		db "=====[Jasonmel kernel]=====", 0x0D, 0x0A, 0x00
prompt		db "jasonmel> ", 0x00
cmd		db "                                                  ", 0x00

cmd_ls		db "ls", 0x00
cmd_add		db "add", 0x00
cmd_rm		db "rm", 0x00
cmd_lls		db "lls", 0x00
cmd_ladd	db "ladd", 0x00
cmd_lrm		db "lrm", 0x00
cmd_label	db "label", 0x00
default		db "Command not found!", 0x0D, 0x0A, 0x00

ls_header	db "[Name]     [Size] [Label]", 0x0D, 0x0A, 0x00

rm_found	db "File deleted!", 0x0D, 0x0A, 0x00
rm_notfound	db "File not found!", 0x0D, 0x0A, 0x00
lls_notfound	db "Label not found!", 0x0D, 0x0A, 0x00

; copied label_name
label0	db "debug", 0x00, "  "
label1	db "ok?", 0x00, "    "
label2	db "", 0x00, "       "
label3	db "", 0x00, "       "
label4	db "", 0x00, "       "
label5	db "", 0x00, "       "
label6	db "", 0x00, "       "
label7	db "", 0x00, "       "
label8	db "", 0x00, "       "
label9	db "", 0x00, "       "
labelA	db "", 0x00, "       "
labelB	db "", 0x00, "       "
labelC	db "", 0x00, "       "
labelD	db "", 0x00, "       "
labelE	db "", 0x00, "       "
labelF	db "", 0x00, "       "

; copied file table
;          label_num   next  size  file_name (11 char)
file0	db 0x01, 0x00, 0x01, 0x04, "kernel     ", 0x00
file1	db 0x03, 0x00, 0xFF, 0x01, "shell      ", 0x00
file2	db 0x02, 0x00, 0xFF, 0x01, "user1      ", 0x00
file3	db 0x02, 0x00, 0xFF, 0x01, "user2      ", 0x00
file4	db 0x00, 0x00, 0x00, 0x00, "           ", 0x00
file5	db 0x00, 0x00, 0x00, 0x00, "           ", 0x00
file6	db 0x00, 0x00, 0x00, 0x00, "           ", 0x00
file7	db 0x00, 0x00, 0x00, 0x00, "           ", 0x00
file8	db 0x00, 0x00, 0x00, 0x00, "           ", 0x00
file9	db 0x00, 0x00, 0x00, 0x00, "           ", 0x00
fileA	db 0x00, 0x00, 0x00, 0x00, "           ", 0x00
fileB	db 0x00, 0x00, 0x00, 0x00, "           ", 0x00
fileC	db 0x00, 0x00, 0x00, 0x00, "           ", 0x00
fileD	db 0x00, 0x00, 0x00, 0x00, "           ", 0x00
fileE	db 0x00, 0x00, 0x00, 0x00, "           ", 0x00
fileF	db 0x00, 0x00, 0x00, 0x00, "           ", 0x00

;;;;; segment .bss ;;;;;
segment .bss

