	cpu	h80
	page	0

        nop
        halt
        ret
;  0000_0000_0000_0011 to 0111_1110 reserved
	invl
        ret	nz
        ret	nc
        ret	nv
        ret	po
        ret	ns
        ret	z
        ret	c
        ret	v
        ret	pe
        ret	s
;  0000_0000_1000_1000 to 1110_1111 reserved
        ld	r0, 76543210h
        ld	r5, 0fedcba98h
        ld	r10, 54761032h
        ld	r15, 0dcfe98bah
        push	r0
        push	r5
        push	r10
        push	r15
        pop	r0
        pop	r5
        pop	r10
        pop	r15
        extn.w	r0
        extn.w	r5
        extn.w	r10
        extn.w	r15
        extn.b	r0
        extn.b	r5
        extn.b	r10
        extn.b	r15
	cpl	r0
	cpl	r5
	cpl	r10
	cpl	r15
	neg	r0
	neg	r5
	neg	r10
	neg	r15
	ld.w	r0, 3210h
	ld.w	r5, 7654h
	ld.w	r10, 1032h
	ld.w	r15, 5476h
	ld.sw	r0, 3210h
	ld.sw	r5, 7654h
	ld.sw	r10, 1032h
	ld.sw	r15, 5476h
	invf	0
	invf	5
	invf	10
	invf	15
	setf	0
	setf	5
	setf	10
	setf	15
	clrf	0
	clrf	5
	clrf	10
	clrf	15
	tstf	0
	tstf	5
	tstf	10
	tstf	15
	call	(r0)
	call	(r5)
	call	(r10)
	call	(r15)
	rst	00h
	rst	08h
	rst	20h
	rst	38h
	jp	(r0)
	jp	(r5)
	jp	(r10)
	jp	(r15)
	jr	(r0)
	jr	(r5)
	jr	(r10)
	jr	(r15)
	call	nz,(r0)
	call	nc,(r5)
	call	nv,(r10)
	call	po,(r15)
	call	ns,(r0)
	call	z,(r0)
	call	c,(r5)
	call	v,(r10)
	call	pe,(r15)
	call	s,(r0)
;  0 0010_10ff_rrrr reserved
;  0 0010_11ff_rrrr reserved
	jp	nz,(r0)
	jp	nc,(r5)
	jp	nv,(r10)
	jp	po,(r15)
	jp	ns,(r0)

	jp	z,(r0)
	jp	c,(r5)
	jp	v,(r10)
	jp	pe,(r15)
	jp	s,(r0)

	jr	nz,(r0)
	jr	nc,(r5)
	jr	nv,(r10)
	jr	po,(r15)
	jr	ns,(r0)

	jr	z,(r0)
	jr	c,(r5)
	jr	v,(r10)
	jr	pe,(r15)
	jr	s,(r0)

	sra	r0, 15
	sra	r5, 10
	sra	r10, 5
	sra	r15, 0

	srl	r0, 15
	srl	r5, 10
	srl	r10, 5
	srl	r15, 0

	sl	r0, 15
	sl	r5, 10
	sl	r10, 5
	sl	r15, 0

	rlc	r0, 15
	rlc	r5, 10
	rlc	r10, 5
	rlc	r15, 0

	add	r0, 15
	add	r5, 10
	add	r10, 5
	add	r15, 0

	sub	r0, 15
	sub	r5, 10
	sub	r10, 5
	sub	r15, 0

	djnz	r0, (r15)
	djnz	r5, (r10)
	djnz	r10, (r5)
	djnz	r15, (r0)

	ex	r0,  r31
	ex	r5,  r30
	ex	r10, r25
	ex	r15, r20
	ex	r20, r15
	ex	r25, r10
	ex	r30, r5
	ex	r31, r0

;  0 1110_aaaa_bbbb reserved
;  0 1111_aaaa_bbbb reserved

	ld.b	r0, 0ffh
	ld.b	r5, 0a5h
	ld.b	r10, 5ah
	ld.b	r15, 00h
	ld.sb	r0, 0ffh
	ld.sb	r5, 0a5h
	ld.sb	r10, 5ah
	ld.sb	r15, 00h
	ld	(r0), r15
	ld	(r5), r10
	ld	(r10), r5
	ld	(r15), r0
	ld	r0, (r15)
	ld	r5, (r10)
	ld	r10, (r5)
	ld	r15, (r0)

	ld.w	(r0), r15
	ld.w	(r5), r10
	ld.w	(r10), r5
	ld.w	(r15), r0
	ld.w	r0, (r15)
	ld.w	r5, (r10)
	ld.w	r10, (r5)
	ld.w	r15, (r0)

	ld.b	(r0), r15
	ld.b	(r5), r10
	ld.b	(r10), r5
	ld.b	(r15), r0
	ld.b	r0, (r15)
	ld.b	r5, (r10)
	ld.b	r10, (r5)
	ld.b	r15, (r0)

	out	(r0), r15
	out	(r5), r10
	out	(r10), r5
	out	(r15), r0
	in	r0, (r15)
	in	r5, (r10)
	in	r10, (r5)
	in	r15, (r0)

	out.w	(r0), r15
	out.w	(r5), r10
	out.w	(r10), r5
	out.w	(r15), r0
	in.w	r0, (r15)
	in.w	r5, (r10)
	in.w	r10, (r5)
	in.w	r15, (r0)

	out.b	(r0), r15
	out.b	(r5), r10
	out.b	(r10), r5
	out.b	(r15), r0
	in.b	r0, (r15)
	in.b	r5, (r10)
	in.b	r10, (r5)
	in.b	r15, (r0)

	ld	r0,  r31
	ld	r5,  r30
	ld	r10, r25
	ld	r15, r20
	ld	r20, r15
	ld	r25, r10
	ld	r30, r5
	ld	r31, r0

	add	r0,  r1,  r2
	add	r12, r13, r14
	sub	r0,  r1,  r2
	sub	r12, r13, r14
	mul	r0,  r1,  r2
	mul	r12, r13, r14
	div	r0,  r1,  r2
	div	r12, r13, r14
	and	r0,  r1,  r2
	and	r12, r13, r14
	or	r0,  r1,  r2
	or	r12, r13, r14
	xor	r0,  r1,  r2
	xor	r12, r13, r14
	cp	r0,  r1,  r2
	cp	r12, r13, r14
