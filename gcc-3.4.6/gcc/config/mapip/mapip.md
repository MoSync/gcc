;; mapip.md	Machine description for the mapip3 virtual processor

;; Copyright (C) 2009 Mobile Sorcery AB

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License, version 2, as published by
;; the Free Software Foundation.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the Free
;; Software Foundation, 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;
;;	Attributes   ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Classification of each insn.
;; branch	conditional branch
;; jump		unconditional jump, indirect
;; call		unconditional call, indirect
;; call_i	unconditional call, direct
;; load		load instruction(s)
;; store	store instruction(s)
;; block	block memory operation
;; move		data movement within same register set
;; arith	integer arithmetic instruction
;; arith_i	integer arithmetic instruction with immediate
;; unary	unary integer operation
;; nop		no operation

(define_attr "type"
  "unknown,branch,cond8,jump,call,call_i,native,quick,load,store,block,move,arith,arith_i,float,float_i,unary,nop"
  (const_string "unknown"))

;; Data type used by an instruction
(define_attr "mode" "unknown,none,QI,HI,SI,DI,SF,DF" (const_string "unknown"))

;; Byte-length of instructions
(define_attr "length" ""
  (cond [(eq_attr "type" "branch,call_i,native,move,load,store,arith_i,float_i")
	 (const_int 8)]
	(const_int 4)))

;; Constants
(define_constants
  [(SP_REGNUM		1)
   (RA_REGNUM		2)
   (FP_REGNUM		3)
   (S0_REGNUM		4)
   (S1_REGNUM		5)
   (S2_REGNUM		6)
   (S3_REGNUM		7)
   (S4_REGNUM		8)
   (S5_REGNUM		9)
   (S6_REGNUM		10)
   (S7_REGNUM		11)
   (LAST_SAVED_REGNUM	11)
   (P0_REGNUM		12)
   (P1_REGNUM		13)
   (P2_REGNUM		14)
   (P3_REGNUM		15)
   (G0_REGNUM		16)
   (G1_REGNUM		17)
   (G2_REGNUM		18)
   (G3_REGNUM		19)
   (G4_REGNUM		20)
   (G5_REGNUM		21)
   (G6_REGNUM		22)
   (G7_REGNUM		23)
   (G8_REGNUM		24)
   (G9_REGNUM		25)
   (G10_REGNUM		26)
   (G11_REGNUM		27)
   (G12_REGNUM		28)
   (G13_REGNUM		29)
   (R0_REGNUM		30)
   (R1_REGNUM		31)
   (LAST_HARD_REGNUM	31)
   (RAP_REGNUM		32)
   (ARG_REGNUM		33)
   (CC_REGNUM		34)])
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic arithmetic operations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "general_operand" "%0,0")
		 (match_operand:SI 2 "general_operand" "r,i")))]
  ""
  "@
   add  %0,%2
   add  %0,%2")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "register_operand" "0,0")
		  (match_operand:SI 2 "general_operand" "r,i")))]
  ""
  "@
   sub  %0,%2
   sub  %0,%2")

;; multiply
(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(mult:SI (match_operand:SI 1 "register_operand" "0,0")
		 (match_operand:SI 2 "general_operand" "r,i")))]
  ""
  "@
   mul  %0,%2
   mul  %0,%2")

;; divide (signed)
(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(div:SI (match_operand:SI 1 "register_operand" "0,0")
		(match_operand:SI 2 "general_operand" "r,i")))]
  ""
  "@
  div  %0,%2
  div  %0,%2"
)

;; divide (unsigned)
(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(udiv:SI (match_operand:SI 1 "register_operand" "0,0")
		 (match_operand:SI 2 "general_operand" "r,i")))]
  ""
  "@
  divu %0,%2
  divu %0,%2")

;; and
(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(and:SI (match_operand:SI 1 "register_operand" "%0,0")
		(match_operand:SI 2 "general_operand" "r,i")))]
  ""
  "@
  and  %0,%2
  and  %0,%2")

;; or
(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ior:SI (match_operand:SI 1 "register_operand" "0,0")
		(match_operand:SI 2 "nonmemory_operand" "r,i")))]
  ""
  "@
  or   %0,%2
  or   %0,%2")

;; xor
(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(xor:SI (match_operand:SI 1 "register_operand" "%0,0")
		(match_operand:SI 2 "general_operand" "r,i")))]
  ""
  "@
  xor  %0,%2
  xor  %0,%2")

;; Shift left
(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,0")
		   (match_operand:SI 2 "general_operand" "r,i")))]
  ""
  "@
  sll  %0,%2
  sll  %0,%2")

;; Arithmetic shift right
(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "0,0")
		     (match_operand:SI 2 "general_operand" "r,i")))]
  ""
  "@
  sra  %0,%2
  sra  %0,%2")

;; Logical shift right
(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "0,0")
		     (match_operand:SI 2 "general_operand" "r,i")))]
  ""
  "@
  srl  %0,%2
  srl  %0,%2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extended arithmetic operations on 8 bit operands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unary arithmatic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Negate

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "neg %0,%1")

;; complement

(define_insn "one_cmplsi2"
  [(set (match_operand:SI         0 "register_operand" "=r")
 	(not:SI (match_operand:SI 1 "register_operand"  "r")))]
  ""
  "not %0,%1")

;; sign_extend(HImode) -> SImode

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "r")))]
  ""
  "xh %0,%1")

;; sign_extend(QImode) -> SImode

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "general_operand" "r")))]
  ""
  "xb   %0,%1")

;; zero_extend(QImode) -> SImode

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:QI 1 "general_operand" "0")))]
  ""
  "and  %0,#0xff  ; zero extend")

;; zero_extend(HImode) -> SImode
(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "general_operand" "0")))]
  ""
  "and  %0,#0xffff ; zero extend")

;;;;;;;;;;;;;;;;;;;
;; Data movement ;;
;;;;;;;;;;;;;;;;;;;

(define_insn "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand"	"=r,r,r,m")
	(match_operand:SI 1 "general_operand"		" r,i,m,r"))]
  ""
  "@
   ld   %0,%1
   ld   %0,%1
   ld   %0,[%1]
   ld   [%0],%1")

(define_insn "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand"	"=r,r,r,m")
	(match_operand:HI 1 "general_operand"		"r,i,m,r"))]
  ""
  "@
   ld   %0,%1
   ld   %0,%1
   ld.h %0,[%1]
   ld.h [%0],%1")

(define_insn "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand"	"=r,r,r,m")
	(match_operand:QI 1 "general_operand" 		"r,i,m,r"))]
  ""
  "@
   ld   %0,%1
   ld   %0,%1
   ld.b %0,[%1]
   ld.b [%0],%1")

;;;;;;;;;;;;;;;;;;;;;
;; Jumps and calls ;;
;;;;;;;;;;;;;;;;;;;;;

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jp   %0"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jp   %0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

(define_insn "tablejump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jp   %0 ;%1"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;;;;;;;;;;;;;;;;;;;;;
;;    calls
;;;;;;;;;;;;;;;;;;;;;

(define_expand "call"
  [(call (match_operand 0 "memory_operand" "m")
	 (match_operand 1 "" "i"))]
  ""
  "
{
  if (mapip_expand_call (operands))
    DONE;
  else
    FAIL;
}")

(define_expand "call_value"
  [(set (match_operand 0 "register_operand" "=r")
	(call (match_operand 1 "memory_operand" "m")
	      (match_operand 2 "" "i")))]
  ""
  "
{
  if (mapip_expand_call (&operands[1]))
    DONE;
  else
    FAIL;
}")

;; Indirect call through register

(define_insn "call_indirect"
  [(call (match_operand:SI 0 "register_operand" "")
	 (match_operand 1 "" ""))
   (clobber (reg:SI RA_REGNUM))]
  "!TARGET_NATIVE_CALLS"
  "call %0")

;; Direct call to non-native function
(define_insn "call_direct"
  [(call (match_operand:SI 0 "memory_operand" "m")
	 (match_operand 1 "" ""))
   (clobber (reg:SI RA_REGNUM))]
  "!TARGET_NATIVE_CALLS"
  "call %0")

;; Indirect call through register. Clobber all non-saved regs
(define_insn "call_indirect_clobber"
  [(call (match_operand:SI 0 "register_operand" "")
	 (match_operand 1 "" ""))
   (clobber (reg:SI RA_REGNUM))
   (clobber (reg:SI P0_REGNUM))
   (clobber (reg:SI P1_REGNUM))
   (clobber (reg:SI P2_REGNUM))
   (clobber (reg:SI P3_REGNUM))
   (clobber (reg:SI G0_REGNUM))
   (clobber (reg:SI G1_REGNUM))
   (clobber (reg:SI G2_REGNUM))
   (clobber (reg:SI G3_REGNUM))
   (clobber (reg:SI G4_REGNUM))
   (clobber (reg:SI G5_REGNUM))
   (clobber (reg:SI G6_REGNUM))
   (clobber (reg:SI G7_REGNUM))
   (clobber (reg:SI G8_REGNUM))
   (clobber (reg:SI G9_REGNUM))
   (clobber (reg:SI G10_REGNUM))
   (clobber (reg:SI G11_REGNUM))
   (clobber (reg:SI G12_REGNUM))
   (clobber (reg:SI G13_REGNUM))
   (clobber (reg:SI R0_REGNUM))
   (clobber (reg:SI R1_REGNUM))]
  "TARGET_NATIVE_CALLS"
  "call %0 ; call non-native with reg")

;; Direct call to non-native function. Clobber all non-saved regs

(define_insn "call_direct_clobber"
  [(call (match_operand:SI 0 "memory_operand" "m")
	 (match_operand 1 "" ""))
   (clobber (reg:SI RA_REGNUM))
   (clobber (reg:SI P0_REGNUM))
   (clobber (reg:SI P1_REGNUM))
   (clobber (reg:SI P2_REGNUM))
   (clobber (reg:SI P3_REGNUM))
   (clobber (reg:SI G0_REGNUM))
   (clobber (reg:SI G1_REGNUM))
   (clobber (reg:SI G2_REGNUM))
   (clobber (reg:SI G3_REGNUM))
   (clobber (reg:SI G4_REGNUM))
   (clobber (reg:SI G5_REGNUM))
   (clobber (reg:SI G6_REGNUM))
   (clobber (reg:SI G7_REGNUM))
   (clobber (reg:SI G8_REGNUM))
   (clobber (reg:SI G9_REGNUM))
   (clobber (reg:SI G10_REGNUM))
   (clobber (reg:SI G11_REGNUM))
   (clobber (reg:SI G12_REGNUM))
   (clobber (reg:SI G13_REGNUM))
   (clobber (reg:SI R0_REGNUM))
   (clobber (reg:SI R1_REGNUM))]
  "TARGET_NATIVE_CALLS"
  "call %0 ; call non-native")

;; Call to native function

(define_insn "call_native"
  [(parallel [(call (match_operand:SI 0 "native_operand" "")
		    (match_operand 1 "" ""))
	      (clobber (reg:SI R0_REGNUM))])]
  ""
  "call %0 ; call native")

(define_insn "return"
  [(return)
  (clobber (reg:SI RA_REGNUM))]
  "reload_completed && !profile_flag && simple_return ()"
  "ret")

(define_insn "return_internal"
  [(use (match_operand:SI 0 "register_operand" ""))
   (return)]
  ""
  "ret")

(define_expand "epilogue"
  [(const_int 2)]
  ""
  "
{
  mapip_expand_epilogue ();
  DONE;
}")

(define_expand "prologue"
  [(const_int 1)]
  ""
  "
{
  mapip_expand_prologue ();
  DONE;
}")

(define_insn "store_regs"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (const_int 1)]
  ""
  "push %0,%1")

(define_insn "restore_regs"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (const_int 2)]
  ""
  "pop  %0,%1")

;(define_insn "ret_internal"
;  [(match_operand:SI 0 "register_operand" "")
;   (match_operand:SI 1 "register_operand" "")
;   (return)]
;  ""
;  "ret  %0,%1")

;*******************************************
;				Switch (TARGET_MA_STATIC)
;*******************************************

(define_insn "casesi"
  [(set (pc)
	(if_then_else
;	 (leu (minus:SI (match_operand:SI 0 "general_operand" "g")
	 (leu (minus:SI (match_operand:SI 0 "register_operand" "r")
			(match_operand:SI 1 "immediate_operand" "i"))
	      (match_operand:SI 2 "immediate_operand" "i"))
	 (plus:SI (sign_extend:SI
		   (mem:SI
		    (plus:SI (pc)
			     (mult:SI (minus:SI (match_dup 0)
						(match_dup 1))
				      (const_int 4)))))
		  (label_ref (match_operand 3 "" "")))
	 (label_ref (match_operand 4 "" ""))))]
  ""
  "case %0,%1,%2,%3,%4")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditional branches ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; 32-bit compares
;;
(define_expand "tstsi"
  [(set (cc0)
 	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "
{
  if (operands[0])
    {
      cmp_ops[0] = operands[0];
      cmp_ops[1] = const0_rtx;
      DONE;
    }
}")

;; Prepare for conditional branch

(define_expand "cmpsi"
  [(set (cc0)
	(compare:CC (match_operand:SI 0 "register_operand" "r,r")
		    (match_operand:SI 1 "nonmemory_operand" "r,r")))]
  ""
  "
{
  if (operands[0])
    {
      cmp_ops[0] = operands[0];
      cmp_ops[1] = operands[1];
      DONE;
    }
}")

;;
;; 8-bit compares
;(define_expand "tstqi"
;  [(set (cc0)
; 	(match_operand:QI 0 "register_operand" "r"))]
;  ""
;  "
;{
;  if (operands[0])
;    {
;      cmp_ops[0] = operands[0];
;      cmp_ops[1] = const0_rtx;
;      DONE;
;    }
;}")

;(define_expand "cmpqi"
;  [(set (cc0)
;	(compare:CC (match_operand:QI 0 "register_operand" "r")
;		    (match_operand:QI 1 "imm8_compare_operand" "K")))]
;  ""
;  "
;{
;  if (operands[0])
;    {
;      cmp_ops[0] = operands[0];
;      cmp_ops[1] = operands[1];
;      DONE;
;    }
;}")

;; Compare 32 bits with sign-extended 8-bit immediate with 8 bit displacement

(define_insn "branch_si"
  [(set (pc)
	(if_then_else (match_operator:SI 0 "comparison_operator"
					 [(match_operand:SI 1 "register_operand" "r,r")
					  (match_operand:SI 2 "general_operand" "r,r")])
		      (match_operand:SI 3 "branch_dest_operand" "")
		      (pc)))]
  ""
;  "jc %C0 %1,%2,%3") 
  "* return output_branch (insn, operands, which_alternative == 1);")

;; Constant 8-bit conditional branch with 8-bit displacement

;(define_insn "branch_qi"
;  [(set (pc)
;	(if_then_else (match_operator:QI 0 "comparison_operator"
;					 [(match_operand:QI 1 "register_operand" "r")
;					  (match_operand:QI 2 "imm8_compare_operand" "K")])
;		      (match_operand:SI 3 "branch_dest_operand" "")
;		      (pc)))]
;  ""
;  "* return output_branch (insn, operands, TRUE);")

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "mapip_gen_branch (operands, EQ); DONE;")

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "mapip_gen_branch (operands, NE); DONE;")

(define_expand "blt"
  [(set (pc)
	(if_then_else (lt:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "mapip_gen_branch (operands, LT); DONE;")


(define_expand "ble"
  [(set (pc)
	(if_then_else (le:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "mapip_gen_branch (operands, LE); DONE;")

(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "mapip_gen_branch (operands, GT); DONE;")

(define_expand "bge"
  [(set (pc)
	(if_then_else (ge:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "mapip_gen_branch (operands, GE); DONE;")

(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "mapip_gen_branch (operands, LTU); DONE;")

(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "mapip_gen_branch (operands, LEU); DONE;")

(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "mapip_gen_branch (operands, GTU); DONE;")

(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "mapip_gen_branch (operands, GEU); DONE;")

;;
;; Misc.
;;

(define_insn "nop"
  [(const_int 0)]
  ""
  "")

;; Block move insn

;(define_expand "movstrsi"
;  [(use (match_operand:BLK 0 "memory_operand" ""))
;   (use (match_operand:BLK 1 "memory_operand" ""))
;   (use (match_operand:SI 2  "nonmemory_operand" ""))
;   (use (match_operand:SI 3  "immediate_operand" ""))]
;  ""
;  "
;{
;  rtx dst = XEXP (operands[0], 0);
;  rtx src = XEXP (operands[1], 0);
;  rtx count;
;
;  /* Everything must be in registers */
;  if (GET_CODE (dst) != REG)
;    dst = copy_to_mode_reg (Pmode, dst);
;  
;  if (GET_CODE (src) != REG)
;    src = copy_to_mode_reg (Pmode, src);
;
;  count = copy_to_mode_reg (SImode, operands[2]);
;
;  emit_insn (gen_movstrsi_internal (dst, src, count));
;  DONE;
;}")

;(define_insn "movstrsi_internal"
;  [(set (mem:BLK (match_operand:SI 0 "general_operand" "r"))
; 	(mem:BLK (match_operand:SI 1 "general_operand" "r")))
;   (use (match_operand:SI 2 "register_operand" "r"))]
;  ""
;  "ext movmem %0,%1,%2")

;; Block clear/set insn

;(define_expand "clrstrsi"
;  [(use (match_operand:BLK 0 "memory_operand" ""))
;   (use (match_operand:SI 1  "nonmemory_operand" ""))
;   (use (match_operand:SI 2  "immediate_operand" ""))]
;  ""
;  "
;{
;  rtx count;
;  rtx dst = XEXP (operands[0], 0);
;
;  if (GET_CODE (dst) != REG)
;    dst = copy_to_mode_reg (Pmode, dst);
;
;  count = copy_to_mode_reg (SImode, operands[1]);
;
;  emit_insn (gen_clrstrsi_internal (dst, count));
;  DONE;
;}")

;(define_insn "clrstrsi_internal"
;  [(set (mem:BLK (match_operand:SI 0 "general_operand" "r"))
; 	(const_int 0))
;   (use (match_operand:SI 1 "register_operand" "r"))]
;  ""
;  "ext setmem0 %0,%1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     FLOATING POINT OPERATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define_insn "movsf"
;  [(set (match_operand:SF 0 "nonimmediate_operand"	"=r,r,r,r,m,o")
;	(match_operand:SF 1 "general_operand"		    " r,F,m,o,r,r"))]
;  ""
;  "@
;   ld   %0,%1
;   ld   %0,%1
;   ld   %0,[%1]
;   ld   %0,[%1]
;   ld   [%0],%1
;   ld   [%0],%1")


(define_insn "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,r,r,m,o")
	(match_operand:SF 1 "general_operand"           " r,F,m,o,r,r"))]
  ""
  "* return mapip_move_word (operands, TRUE);")

(define_insn "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r,r,r,r,m,o")
	(match_operand:DF 1 "general_operand"           " r,F,m,o,r,r"))]
  ""
  "* return mapip_move_2words (operands, TRUE);")

