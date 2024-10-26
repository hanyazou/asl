	page 0
	cpu H80

CONDAT	EQU	0

	LD.W	r1,MSG
        LD.W	r2,CONDAT
        LD.W	r3,6
        LD.SW	r4,-10
PRTMSG:	LD.B	r0,(r1)
	ADD	r1,1
	ADD	r8,r0,r0
	JR	Z,(r3)
	OUT.B	(r2),r0
	JR	(r4)
STOP:	HALT

	ORG	1000H
MSG:	DEFM	'Hello, world!'
        DEFB	13,10,0
