#	.code
#	.globl setjmp
#
#setjmp:
#	ld	[i0,0],sp
#	ld	[i0,4],rt
#	ld	[i0,8],fr
#	ld	[i0,12],d0
#	ld	[i0,16],d1
#	ld	[i0,20],d2
#	ld	[i0,24],d3
#	ld	[i0,28],d4
#	ld	[i0,32],d5
#	ld	[i0,36],d6
#	ld	[i0,40],d7
#	ld	zr,0
#	ret
#
#	.globl longjmp
#
#longjmp:
#	ld	sp,[i0,0]
#	ld	rt,[i0,4]
#	ld	fr,[i0,8]
#	ld	d0,[i0,12]
#	ld	d1,[i0,16]
#	ld	d2,[i0,20]
#	ld	d3,[i0,24]
#	ld	d4,[i0,28]
#	ld	d5,[i0,32]
#	ld	d6,[i0,36]
#	ld	d7,[i0,40]
#	ld	r14,i1
#	ret
#
#	.globl	ViewPoint
#	.data
#	.align 4
#
#InStack:
#	.word	0
