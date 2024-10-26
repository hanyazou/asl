	cpu	h80
	page	0

	;; 
	;; jump to label
	;; 
L8000:	equ	8000h
L9000:	equ	9000h
L76543210:	EQU	76543210h

	org	0
	jp	L8000
	jr	l8000
	jp	l9000
	expect  1370
	jr	l9000		; too far
	endexpect
	jp	l76543210

	;; 
	;; jump to address near 16-bit the boundary
	;; 
	org	0fff4h

	jp	l10000
	jp	l10000
L10000:
	nop

	;; 
	;; jump to relative address
	;; 
	org	1000h

	jp	$
	jr	$
	jp	nz,$
	jr	nz,$

	jp	$+100h
	jp	$-100h
	jr	$+100h
	jr	$-100h
	jp	nz,$+100h
	jp	nz,$-100h
	jr	nz,$+100h
	jr	nz,$-100h

	phase	80000000h

	jr	$+129		; OK
	jr	$-126		; OK
	jr	$+130		; OK (16 bit displacement)
	jr	$-127		; OK (16 bit displacement)
	jr	$+8003h		; OK (just 16 bit displacement)
	jr	$-7ffch		; OK (just 16 bit displacement)
	expect  1370,1370,1370,1370
	jr	$+8004h		; too far
	jr	$-7ffdh		; too far
	jr	$+800005h	; too far
	jr	$-7ffffch	; too far
	endexpect
