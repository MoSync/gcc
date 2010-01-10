/* Copyright (C) 2009 Mobile Sorcery AB

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License, version 2, as published by
the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
*/

#ifndef _MAPIP_H_
#define _MAPIP_H_

#include "mapip-mod.h"
#include "hwint.h"

/* Compilation flags */
extern int target_flags;

/* Use by conditional branches */
extern struct rtx_def *cmp_ops[2];

/* ******** Target specifications ******** */
#define TARGET_VERSION fprintf (stderr, " (MAPIP GCC Beta)");
#define ALTERNATIVE_SUBMIT_BUG_MSG \
 "Send bug reports to Antony Hartley <tony@mobilesorcery.com>."
 
 /* Names to predefine in the preprocessor for this target machine.  */
#define TARGET_CPU_CPP_BUILTINS() \
  do						\
    {						\
	builtin_define ("MAPIP");		\
	builtin_assert ("cpu=mapip");		\
	builtin_assert ("machine=mapip");		\
    } while (0) 


#define MASK_NATIVE_CALLS	0x00000001
#define TARGET_NATIVE_CALLS	(target_flags & MASK_NATIVE_CALLS)

#define MASK_OPTIM_BSS		0x00000002
#define TARGET_OPTIMIZE_BSS	(target_flags & MASK_OPTIM_BSS)

#define MASK_MA_STATIC		0x00000004
#define TARGET_MA_STATIC	(target_flags & MASK_MA_STATIC)

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (0)
#endif

#define TARGET_SWITCHES 						\
{									\
  { "ma-static", MASK_MA_STATIC,				\
    "switch to mobile author static mode" },		\
  { "",			TARGET_DEFAULT,	NULL},				\
}

extern const char *stack_size_opt;
extern const char *data_heap_size;
extern const char *code_heap_size;
extern const char *mapip_archscale_string;

extern const char *fixed_float_format;

/*
#ifndef SUBTARGET_OPTIONS
#define SUBTARGET_OPTIONS
#endif
*/

#define CC1_SPEC "-fno-rerun-cse-after-loop -fno-loop-optimize"
/* A C string constant that tells the GCC driver program options to
   pass to `cc1'.  It can also specify how to translate options you
   give to GCC into options for GCC to pass to the `cc1'.

   Do not define this macro if it does not need to do anything.  */

/*
#define TARGET_OPTIONS 							\
{ 									\
  { "stack=", &stack_size_opt, "Set default stack size", 0 },		\
  { "data=", &data_heap_size,  "Additional data heap space required", 0 }, \
  { "code=", &code_heap_size, "Additional code heap space required", 0 },	\
}
*/

#define TARGET_OPTIONS	\
{ {"archscale=", &mapip_archscale_string, N_("Specify arch scaling option"), 0}}


#define CONDITIONAL_REGISTER_USAGE mapip_prepare_call_regs ()
#ifdef GCC_303
#define CPP_PREDEFINES	"-Dmapip -Dmapip -DMAVM -Acpu(mapip) -Amachine(mapip)"
#endif
#define LIB_SPEC	"-lc"
#define LIBGCC_SPEC	"-lgcc -lmobileauthor"

#ifndef SUBTARGET_LINK_SPEC
#define SUBTARGET_LINK_SPEC
#endif

#define LINK_SPEC	\
"%{mstack=*:--stack=%*}\
 %{mdata=*:--data=%*}\
 %{mcode=*:--code=%*}"

/*
 SUBTARGET_LINK_SPEC
*/

#define MATH_LIBRARY ""

#define OPTIMIZATION_OPTIONS optimization_options

#define DONT_USE_BUILTIN_SETJMP
/* $sp + $ra + $fp + $s0...$s7 == 11
   11 * 4 == 44
*/
#define JMP_BUF_SIZE 44

/* Turn off C++ RTTI and exceptions by default */
#define CC1PLUS_SPEC "%{!frtti:-fno-rtti} %{!fexceptions:-fno-exceptions}"

/************************************
 * Storage layout
 ************************************/

/* Basic characteristics */
#define BITS_BIG_ENDIAN		0
#define BYTES_BIG_ENDIAN	0
#define WORDS_BIG_ENDIAN	0

#ifdef GCC_303
#define BITS_PER_UNIT		8
#define BITS_PER_WORD		32
#endif

#define UNITS_PER_WORD		4
#define MIN_UNITS_PER_WORD	4

#ifdef GCC_303
#define POINTER_SIZE		32
#endif

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
do						\
{						\
  enum machine_mode mode = TYPE_MODE (TYPE);	\
  if (GET_MODE_CLASS (mode) == MODE_INT		\
      && GET_MODE_SIZE (mode) < UNITS_PER_WORD	\
      && (TREE_CODE (TYPE) == INTEGER_TYPE	\
	  || TREE_CODE (TYPE) == ENUMERAL_TYPE	\
	  || TREE_CODE (TYPE) == BOOLEAN_TYPE	\
	  || TREE_CODE (TYPE) == CHAR_TYPE	\
	  || TREE_CODE (TYPE) == OFFSET_TYPE))	\
    (MODE) = mode;				\
} while (0)

/* Perform PROMOTE_MODE for function parameters and return values */
#define PROMOTE_FUNCTION_ARGS
#define PROMOTE_FUNCTION_RETURN
/*  #define PROMOTE_FOR_CALL_ONLY */

#define PROMOTE_PROTOTYPES 0

/* The lowest bit is used since code uses byte aligned functions, so the
   vbit must go into the delta field of pointers to member
   functions.  */

#define TARGET_PTRMEMFUNC_VBIT_LOCATION ptrmemfunc_vbit_in_delta

/*vbit in delta = ptrmemfunc_vbit_in_delta, in prefix = ptrmemfunc_vbit_in_pfn*/

/* Everything is basically word-aligned */
#define PARM_BOUNDARY		32
#define STACK_BOUNDARY		32
#define PREFERRED_STACK_BOUNDARY STACK_BOUNDARY

/*#define FUNCTION_BOUNDARY	32*/
#define FUNCTION_BOUNDARY	8		/* ARH 8bit alignment of functions/code */

#define BIGGEST_ALIGNMENT	32
#define MAX_OFILE_ALIGNMENT	32

/* Alignment for fields following a `int : 0' */
#define EMPTY_FIELD_BOUNDARY	32

/* Keep all structures at least four bytes */
/*  #define STRUCTURE_SIZE_BOUNDARY 32 */

/* We can't know exactly what requirements the _real_
   hardware impose on this, therefore we require all
   accesses to be strictly aligned */
#define STRICT_ALIGNMENT	1

/* The type of the bit-field affects the structure alignment */
#define PCC_BITFIELD_TYPE_MATTERS 1

#define DEFAULT_VTABLE_THUNKS 1

/**************************************
 * Layout of Source Language Data Types
 **************************************/
#define CHAR_TYPE_SIZE		8
#define SHORT_TYPE_SIZE		16

#define INT_TYPE_SIZE		32
#ifdef GCC_303
#define MAX_INT_TYPE_SIZE	32
#endif

#define LONG_TYPE_SIZE		32
#define MAX_LONG_TYPE_SIZE	32

#define LONG_LONG_TYPE_SIZE	64

#ifdef GCC_303
#define WCHAR_UNSIGNED		1
#endif
#define WCHAR_TYPE_SIZE 	16
#define WCHAR_TYPE "short unsigned int"

#define FLOAT_TYPE_SIZE		32
#define DOUBLE_TYPE_SIZE	64
#define LONG_DOUBLE_TYPE_SIZE	64

#define WIDEST_HARDWARE_FP_SIZE	32

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

#define DEFAULT_SIGNED_CHAR	1
#define DEFAULT_SHORT_ENUMS	0

/* Pointer type */
#define Pmode SImode

#ifdef GCC_303
#define TARGET_BELL	007
#define TARGET_BS	010
#define TARGET_TAB	011
#define TARGET_NEWLINE	012
#define TARGET_VT	013
#define TARGET_FF	014
#define TARGET_CR	015
#endif


/******************************
 * Real operations
 ******************************/

/* #define REAL_VALUE_TO_TARGET_SINGLE(X, L) (L) = mapip_real_value_to_target_single (X) */
/* #define REAL_VALUE_TO_TARGET_DOUBLE(X, L) mapip_real_value_to_target_double (X, L) */
/* #define REAL_VALUE_TO_TARGET_LONG_DOUBLE REAL_VALUE_TO_TARGET_DOUBLE */

/******************************
 * Register Usage
 ******************************/

/*
  $r0, $sp, $ra, $fp,	(0-3)
  $s0 - $s7		(4-11)
  $p0 - $p3		(12-15)
  $g0 - $g15		(16-31)
  $rap			(fake return pointer)
  $arg			(fake arg pointer)
 */

#define FIXED_REGISTERS		\
{ 				\
  1, 1, 1, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1			\
}

/* Register that are fixed or clobber over function calls */

#define CALL_USED_REGISTERS	\
{				\
  1, 1, 1, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1			\
}

/* The order in which registers are preferred to be allocated */
/*

#define REG_ALLOC_ORDER			\
{					\
  30, 12, 13, 14, 15,			\
  16, 17, 18, 19, 20, 21, 22, 23, 	\
  24, 25, 26, 27, 28, 29, 31, 3,	\
  0,  4,  5,  6,  7,  8,  9,  10, 11,	\
  2,  1,  RAP_REGNUM, ARG_REGNUM, CC_REGNUM	\
}
*/

#define REG_ALLOC_ORDER			\
{					\
  30, 			\
  16, 17, 18, 19, 20, 21, 22, 23, 	\
  24, 25, 26, 27, 28, 29, 31, 3,	\
  0,  4,  5,  6,  7,  8,  9,  10, 11,	\
  12, 13, 14, 15, \
  2,  1,  RAP_REGNUM, ARG_REGNUM, CC_REGNUM	\
}

/* !!! TEMP !!!
#define ORDER_REGS_FOR_LOCAL_ALLOC mapip_order_regs_for_local_alloc*/

#ifdef GCC_303
#define ENCODE_SECTION_INFO(DECL) mapip_encode_section_info (DECL)
#endif

/* How many registers are required to store a value of
   mode */
#define HARD_REGNO_NREGS(REGNO, MODE)		\
   ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)	\
    / UNITS_PER_WORD)

/* Allow anything to be stored in any of the hard registers */
/*#define HARD_REGNO_MODE_OK(REGNO, MODE) 1*/

#define HARD_REGNO_MODE_OK(REGNO, MODE) mapip_regno_mode_ok(REGNO,MODE)

#define MODES_TIEABLE_P(MODE1, MODE2)	\
  (GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2) \
   || GET_MODE_SIZE (MODE1) == GET_MODE_SIZE (MODE2))

#define AVOID_CCMODE_COPIES

#define FIRST_PSEUDO_REGISTER 35

/* Register classes */
enum reg_class
{
  NO_REGS,
  CC_REG,
  ALL_REGS,
  LIM_REG_CLASSES
};

/* All registers are general registers */
#define GENERAL_REGS ALL_REGS
#define N_REG_CLASSES (int) LIM_REG_CLASSES
#define REG_CLASS_NAMES	\
{			\
  "NO_REGS",		\
  "CC_REG",		\
  "ALL_REGS"		\
}

#define REG_CLASS_CONTENTS	\
{				\
  {0x00000000, 0x00000000},	\
  {0x00000000, 0x00000004},	\
  {0xFFFFFFFF, 0x00000003}	\
}

#define REGNO_REG_CLASS(REGNO)	(((REGNO) < FIRST_PSEUDO_REGISTER) ? GENERAL_REGS : NO_REGS)
#define BASE_REG_CLASS		GENERAL_REGS
#define INDEX_REG_CLASS		NO_REGS /* No index registers */

/* No special mapip register classes */
#define REG_CLASS_FROM_LETTER(CHAR) ((CHAR) == 'C' ? CC_REG : NO_REGS)

#define REGNO_OK_FOR_BASE_P(REGNO) \
  (REGNO_REG_CLASS (REGNO) == BASE_REG_CLASS)

#define REGNO_OK_FOR_INDEX_P(REGNO) 0 /* No index regs */

#define PREFERRED_RELOAD_CLASS(X, CLASS) (CLASS)

#define CLASS_MAX_NREGS(CLASS, MODE)		\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/*    ((CLASS == GENERAL_REGS)			\ */
/*      ? HARD_REGNO_NREGS (0, MODE)		\ */
/*      : 0) */

/*
  'I'	immediate values from -128 to 127
  'J'	immediate values from -32768 to 32767
  'K'	immediate values that fit in a byte.
  'L'   immediate values from 0 to 255.
 */
#define CONST_OK_FOR_LETTER_P(VAL,C) \
  ((C) == 'I' ? (VAL) >= -128 && (VAL) < 128 : \
   (C) == 'J' ? (VAL) >= -32768 && (VAL) < 32768 : \
   (C) == 'K' ? (VAL) >= -128 && (VAL) < 256 : \
   (C) == 'L' ? (VAL) >= 0 && (VAL) < 256 : 0)

#define CONST_DOUBLE_OK_FOR_LETTER_P(VAL,C) 0

/**************************************
 * Stack layout and Calling Conventions
 **************************************/
struct mapip_frame_info {
  int valid;		/* 0 values are not valid */
  int rmask;		/* mask of registers saved */
  int rblocks;		/* n of register blocks to store/restore */
  int first_reg; /* first saved register */ 
  int last_reg;  /* last saved register */
  int n_regs;		/* number of saved regs */
  int regs;		/* size of saved registers area */
  long total_size;	/* total size in bytes */
  long outgoing;	/* size of outgoing args area */
  long locals;		/* size locals area */
};

extern struct mapip_frame_info frame_info;

#define MAPIP_STACK_ALIGN(SIZE) \
  ((SIZE + (STACK_BOUNDARY/8 - 1)) & ~(STACK_BOUNDARY/8 - 1))

/* The $sp register is decreased to make room on the stack */
#define STACK_GROWS_DOWNWARD 1

#define FRAME_GROWS_DOWNWARD 1

/* Offset from frame pointer to first local variable */
#define STARTING_FRAME_OFFSET 0

#define STACK_POINTER_OFFSET 0
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Getting the return address of a frame other than the current one
   is extremely tricky, so just support the current one */
#define RETURN_ADDR_RTX(COUNT, FRAME) \
  ((COUNT == 0) \
   ? gen_rtx (MEM, Pmode, gen_rtx (REG, Pmode, RETURN_ADDRESS_POINTER_REGNUM))\
   : NULL_RTX)

/* Used by DWARF2 */
/*#define INCOMING_RETURN_ADDR_RTX gen_rtx (REG, Pmode, RA_REGNUM)*/

/* Hard stack registers */
#define STACK_POINTER_REGNUM		SP_REGNUM
#define HARD_FRAME_POINTER_REGNUM	FP_REGNUM

/* $g15 is the least likely to be used temporary register, so we
   use it for the static chain */
#define STATIC_CHAIN_REGNUM (G0_REGNUM+15)

/* The following registers are eliminated */
#define ARG_POINTER_REGNUM		ARG_REGNUM
#define RETURN_ADDRESS_POINTER_REGNUM	RAP_REGNUM
#define FRAME_POINTER_REGNUM		0

/* Frame and arg pointer elimination */
#define FRAME_POINTER_REQUIRED (current_function_calls_alloca)

#define ELIMINABLE_REGS							\
{{ ARG_POINTER_REGNUM,			STACK_POINTER_REGNUM },		\
 { ARG_POINTER_REGNUM,			HARD_FRAME_POINTER_REGNUM },	\
 { FRAME_POINTER_REGNUM,		STACK_POINTER_REGNUM },		\
 { FRAME_POINTER_REGNUM,		HARD_FRAME_POINTER_REGNUM },	\
 { RETURN_ADDRESS_POINTER_REGNUM,	RA_REGNUM },			\
 { RETURN_ADDRESS_POINTER_REGNUM,	STACK_POINTER_REGNUM },		\
 { RETURN_ADDRESS_POINTER_REGNUM,	HARD_FRAME_POINTER_REGNUM }}	\

#define CAN_ELIMINATE mapip_can_eliminate

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFVAR) \
  initial_elimination_offset ((FROM), (TO), &(OFFVAR))

/* Make space on the stack for all outgoing function parameters */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Caller always pops arguments */
#define RETURN_POPS_ARGS(FNDECL, FUNTYPE, STACKSIZE) 0

/* Return 0 if on stack, otherwise a REG rtx */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg (&CUM, MODE, TYPE, NAMED)

/*#define MUST_PASS_IN_STACK(mode,type) 0 */

/* Count function parameters */
typedef struct mapip_args {
  int argc;
  int words;
} CUMULATIVE_ARGS;

#ifdef GCC_303
#undef FUNCTION_OK_FOR_SIBCALL
#define FUNCTION_OK_FOR_SIBCALL(DECL) 0
#else
#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL mapip_ok_for_sibcall
#endif

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  (CUM).argc = (CUM).words = 0

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED) \
 	function_arg_advance (&CUM, MODE, TYPE, NAMED)

#define FUNCTION_ARG_REGNO_P(REGNO) ((REGNO) >= P0_REGNUM && (REGNO) <= (P0_REGNUM+3))

/* #define RETURN_REGNUM R0_REGNUM */
#define RETURN_REGNUM mapip_get_return_regs()

/* How scalars are returned.
 * Values are always returned in RETURN_REGNUM */
#define LIBCALL_VALUE(MODE) gen_rtx (REG, MODE, RETURN_REGNUM)

/* Generates suboptimal code for returning non-scalars, since
   PROMOTE_MODE is not applied */
#define FUNCTION_VALUE(TYPE, FUNC) mapip_function_value (TYPE, FUNC)

#define FUNCTION_VALUE_REGNO_P(REGNO) ((REGNO) == RETURN_REGNUM)

#ifdef GCC_303
#define TRADITIONAL_RETURN_FLOAT
#endif

/* Large return values */
#define RETURN_IN_MEMORY(type) (TYPE_MODE (type) == BLKmode)

/* Don't necessarily return structs in memory */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Pass the structure value address hidden */
#define STRUCT_VALUE 0

/* #define MAX_ARGS_IN_REGS 4 */
#define MAX_ARGS_IN_REGS mapip_max_args_in_regs()

#ifdef GCC_303
/*
#define ASM_OUTPUT_MI_THUNK(FILE,THUNKDECL,DELTA,FUNC)	\
{							\
  fprintf (FILE, "add i0,#0x%x\n", DELTA);		\
  fprintf (FILE, "jp &");				\
  assemble_name (FILE, XSTR(XEXP(DECL_RTL(FUNC),0),0));	\
  fputs ("\n", FILE);					\
}
*/
#else

#undef  TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK mapip_output_mi_thunk

#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK default_can_output_mi_thunk_no_vcall

#endif

#ifdef GCC_303
#define FUNCTION_EPILOGUE(FILE,SIZE) frame_info.valid = 0
#else
#undef  TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE mapip_output_function_epilogue
#endif

#define EPILOGUE_USES(REGNO) (reload_completed && (REGNO) == RA_REGNUM)

/* Profiling */

/*#define FUNCTION_PROFILER(FILE, NUM)		\
{						\
  char *p0 = reg_names[P0_REGNUM];		\
  fprintf (FILE, "ld $g0,%s\n", p0);		\
  fprintf (FILE, "move\t$g1,%s\n", p0);		\
  fprintf (FILE, "move\t$g2,%s\n", p0);		\
  fprintf (FILE, "move\t$g3,%s\n", p0);		\
  fprintf (FILE, "ldi\t%s,LD%d\n", p0, NUM);	\
  fprintf (FILE, "call\t_mcount\n");		\
  fprintf (FILE, "move\t%s,$g0\n", p0);		\
  fprintf (FILE, "move\t%s,$g1\n", p0);		\
  fprintf (FILE, "move\t%s,$g2\n", p0);		\
  fprintf (FILE, "move\t%s,$g3\n", p0);		\
}
*/

#define FUNCTION_PROFILER(FILE, NUM)
#define PROFILE_BEFORE_PROLOGUE

#if 0
#define FUNCTION_BLOCK_PROFILER_EXIT(FILE)
#endif

/* Variable argument lists */
#if 0
#define SETUP_INCOMING_VARARGS
#endif
#define STRICT_ARGUMENT_NAMING 1

/****************
 * Trampolines
 ****************/

#define TRAMPOLINE_SIZE 4

#undef INITIALIZE_TRAMPOLINE
#define INITIALIZE_TRAMPOLINE(ADDR, FUNC, CTX) mapip_initialize_trampoline(ADDR, FUNC, CTX)

/************************
 * Addressing modes
 ************************/
#define HAVE_POST_INCREMENT 0
#define HAVE_POST_DECREMENT 0
#define HAVE_PRE_DECREMENT 0
#define HAVE_PRE_INCREMENT 0

#define CONSTANT_ADDRESS_P(X) CONSTANT_P(X)
/* #define CONSTANT_ADDRESS_P(X) mapip_constant_address_p (X) */

/* One base register */
#define MAX_REGS_PER_ADDRESS 1

#ifdef REG_OK_STRICT
#define REG_OK_STRICT_P 1
#else
#define REG_OK_STRICT_P 0
#endif

#define REG_OK_FOR_BASE_P(X) mapip_reg_ok_for_base_p(X,REG_OK_STRICT_P)

#define REG_OK_FOR_INDEX_P(x) 0 
#define BASE_REGISTER_RTX_P(X)  \
  (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)		\
do {								\
  if (mapip_legitimate_address (MODE, X, REG_OK_STRICT_P))	\
    goto LABEL;							\
} while (0)

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN) do {} while (0)

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL) ((void)0)

#define LEGITIMATE_CONSTANT_P(X) 1

/************************
 * Operation Costs
 ***********************/

#define SLOW_BYTE_ACCESS 1				/* Was 0*/
#ifdef GCC_303
#define SLOW_ZERO_EXTEND 1				/* Was off */
#endif

#define MOVE_RATIO 8

/* Max bytes to move in one instruction */
#define MOVE_MAX 4

#define MOVE_BY_PIECES_P(SIZE,ALIGNMENT) ((SIZE) < MOVE_RATIO)

/* Dont' try to be smart and load function adresses into
   registers */
   
#define NO_FUNCTION_CSE
#define NO_RECURSIVE_FUNCTION_CSE

#ifdef GCC_303
#define CONST_COSTS(X,CODE,OUTER_CODE)					\
  case CONST_INT:							\
    if ((OUTER_CODE) == SET)						\
      {									\
          return COSTS_N_INSNS (2);					\
      }									\
									\
    if ((OUTER_CODE) == LT || (OUTER_CODE) == LE			\
	 || (OUTER_CODE) == EQ || (OUTER_CODE) == NE			\
	 || (OUTER_CODE) == GE || (OUTER_CODE) == GT			\
	 || (OUTER_CODE) == LTU || (OUTER_CODE) == LEU			\
	 || (OUTER_CODE) == GEU || (OUTER_CODE) == GTU)			\
      return COSTS_N_INSNS (2);				\
													\
   return COSTS_N_INSNS (2);						\
									\
  case CONST:								\
    {									\
      rtx offset = const0_rtx;						\
      rtx symref = eliminate_constant_term (XEXP (X, 0), &offset);	\
									\
      if (GET_CODE (symref) == LABEL_REF)				\
        return COSTS_N_INSNS (1);					\
      return COSTS_N_INSNS (2);						\
    }									\
									\
  case LABEL_REF:							\
    return COSTS_N_INSNS (1);						\
  case SYMBOL_REF:							\
    return COSTS_N_INSNS (2);						\
									\
  case CONST_DOUBLE:							\
    {									\
      rtx high, low;							\
      split_double (X, &high, &low);					\
      return COSTS_N_INSNS ((high == CONST0_RTX (GET_MODE (high))	\
			     || low == CONST0_RTX (GET_MODE (low)))	\
			    ? 2 : 4);					\
    }

#define RTX_COSTS(X,CODE,OUTER_CODE)				\
  case NOT:							\
  case NEG:							\
    return COSTS_N_INSNS (2);	\
								\
  case XOR:							\
  case IOR:							\
  case AND:							\
  case PLUS:							\
  case MINUS:							\
  case ASHIFT:							\
  case ASHIFTRT:						\
  case LSHIFTRT:						\
  case MULT:							\
    {								\
      rtx op = XEXP (X, 1);					\
      if (GET_CODE (op) == CONST_INT)			\
        return COSTS_N_INSNS (2);				\
      return COSTS_N_INSNS (1);				\
    }								\
    break;
#endif	/*GCC_303*/

#define REGISTER_MOVE_COST(MODE, FROM, TO)			\
  (2)

/* #define BRANCH_COST 1 */

#define COND8_MIN (-128*4)
#define COND8_MAX (127*4)

/* ARH - No longer needed */
/* #define MACHINE_DEPENDENT_REORG(INSN) machine_dependent_reorg (INSN) */

/****************************
 * Assembler/Output interface
 ****************************/
 
#define TEXT_SECTION_ASM_OP	".code"
#define DATA_SECTION_ASM_OP	".data"
#define BSS_SECTION_ASM_OP	".bss"

#define READONLY_DATA_SECTION data_section

#ifdef GCC_303
#define ASM_FILE_START(FILE)	asm_file_start (FILE)
#define ASM_FILE_END(FILE)		asm_file_end (FILE)
#endif
#define ASM_COMMENT_START	";"
#define ASM_APP_ON		".asm_on\n"
#define ASM_APP_OFF		".asm_off\n"
#ifdef GCC_303
#define ASM_OPEN_PAREN		"("
#define ASM_CLOSE_PAREN		")"
#endif
#define SET_ASM_OP		".set"
#define ASM_LONG		".word"
#define ASM_BYTE_OP		".byte"

#ifdef GCC_303
/* Float output */
#define ASM_OUTPUT_LONG_DOUBLE ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE, REAL) \
  mapip_output_double (FILE, REAL)
#define ASM_OUTPUT_FLOAT(FILE, REAL) \
  mapip_output_float (FILE, REAL)

/* Integer output */
#define ASM_DOUBLE_INT(FILE,EXP) mapip_output_double_int(FILE,EXP)

#define ASM_OUTPUT_INT(FILE,EXP)	\
{					\
  fprintf (FILE, ".word\t");		\
  output_addr_const (FILE, (EXP));	\
  fprintf (FILE, "\n");			\
}

#define ASM_OUTPUT_SHORT(FILE,EXP)	\
{					\
  fprintf (FILE, "\t.half\t");		\
  output_addr_const (FILE, (EXP));	\
  fprintf (FILE, "\n");			\
}

#define ASM_OUTPUT_CHAR(FILE,EXP)	\
{					\
  fprintf (FILE, "\t.byte\t");		\
  output_addr_const (FILE, (EXP));	\
  fprintf (FILE, "\n");			\
}

#define ASM_OUTPUT_BYTE(FILE, VAL) \
  fprintf (FILE, "\t.byte\t%d\n", (int)(VAL))

#define ASM_OUTPUT_ASCII asm_output_ascii
#endif	/*GCC_303*/

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)		\
do {								\
  fprintf (FILE, "\t.comm\t");					\
  assemble_name (FILE, NAME);					\
  fprintf (FILE, ", %d ; size=%d\n", ROUNDED, SIZE);		\
} while (0)

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
do {							\
  fprintf (FILE, "\t.lcomm\t");				\
  assemble_name (FILE, NAME);				\
  fprintf (FILE, ", %d ; size=%d\n", ROUNDED, SIZE);	\
} while (0)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"
 
#ifdef GCC_303
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGNMENT)	\
  asm_output_common (FILE, NAME, SIZE, ALIGNMENT, 0)

#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGNMENT)	\
  asm_output_common (FILE, NAME, SIZE, ALIGNMENT, 1)

#define ASM_OUTPUT_LABEL(FILE, NAME) \
  (assemble_name (FILE, NAME), fputs (":\n", FILE))

/*
#define ASM_WEAKEN_LABEL(FILE, NAME)			\
{							\
  fputs ("\t.weak\t", FILE);				\
  assemble_name (FILE, NAME);				\
  fputc ('\n', FILE);					\
}
*/
#endif	/*GCC_303*/

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)	\
{													\
  mapip_write_func_name(FILE, NAME);						\
}

/*
ASM_OUTPUT_LABEL (FILE, NAME);	\
*/


#define ASM_OUTPUT_LABELREF(FILE, NAME) \
  mapip_output_labref(NAME, FILE)
/* fputs (NAME, FILE) */
/*  mapip_output_labref(NAME, FILE) */
/*  fputs (NAME, FILE) */

#ifdef GCC_303
#define ASM_GLOBALIZE_LABEL(FILE, NAME)	\
 (fputs ("\t.globl\t", FILE),		\
  assemble_name (FILE, NAME), fputs ("\n", FILE))
#endif

#define ASM_GENERATE_INTERNAL_LABEL(BUF, PFX, NUM) \
mapip_gen_internal_label(BUF, PFX, NUM)

/*  sprintf (BUF, "*%s%d", PFX, NUM) */
/*  sprintf (BUF, "*.%s%d", PFX, NUM) */

#ifdef GCC_303
#define ASM_OUTPUT_INTERNAL_LABEL(FILE, PFX, NUM) \
	mapip_write_interal_label(FILE, PFX, NUM)

/*  fprintf (FILE, "(outintlab)%s%d:\n", PFX, NUM) */
/*  fprintf (FILE, "\n.local %s%d\n", PFX, NUM);	*/
/*  fprintf (FILE, ".%s%d:\n", PFX, NUM) */
#else
#undef TARGET_ASM_INTERNAL_LABEL
#define TARGET_ASM_INTERNAL_LABEL \
	mapip_write_interal_label
#endif

#define ASM_FORMAT_PRIVATE_NAME(BUF, NAME, NUM)		\
  ((BUF) = (char*) alloca (strlen ((NAME)) + 13),	\
   sprintf ((BUF), "%s.%d", (NAME), (NUM)))

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, NUM) \
  fprintf (FILE, "\t.word L%d\n", NUM)
/*  fprintf (FILE, "\t.word .L%d\n", NUM) */

#define ASM_OUTPUT_DEF(FILE, NAME, VAL)	\
{					\
  fprintf (FILE, "\t.set\t");		\
  assemble_name (FILE, NAME);		\
  fputs ("=", FILE);			\
  assemble_name (FILE, VAL);		\
  fputs ("\n", FILE);			\
}

/* Output of instructions */

/*
#define REGISTER_NAMES						\
{								\
    "$0", "$sp",  "$ra",  "$fp", 				\
   "$s0", "$s1",  "$s2",  "$s3", "$s4", "$s5", "$s6", "$s7",	\
   "$p0", "$p1",  "$p2",  "$p3", "$g0", "$g1", "$g2", "$g3",	\
   "$g4", "$g5",  "$g6",  "$g7", "$g8", "$g9", "$g10","$g11",	\
  "$g12","$g13", "$r0", "$r1", "$rap", "$arg", "cc"		\
}
*/

#define REGISTER_NAMES						\
{								\
   "zr", "sp",  "rt",  "fr", 				\
   "d0", "d1",  "d2",  "d3", "d4", "d5", "d6", "d7",	\
   "i0", "i1",  "i2",  "i3", "r0", "r1", "r2", "r3",	\
   "r4", "r5",  "r6",  "r7", "r8", "r9", "r10","r11",	\
  "r12","r13", "r14", "r15", "rap", "arg", "cc"		\
}


#ifdef GCC_303
/* Additional register names for the convenience of the inline
   assembler programmer */
#define ADDITIONAL_REGISTER_NAMES					\
{													\
  {"zr", 0},										\
  {"sp", 1}, {"rt", 2}, {"fr", 3}, {"d0", 4},		\
  {"d1", 5}, {"d2", 6}, {"d3", 7}, {"d4", 8},		\
  {"d5", 9}, {"d6", 10}, {"d7", 11}, {"i0", 12},	\
  {"i1", 13}, {"i2", 14}, {"i3", 15}, {"r0", 16},	\
  {"r1", 17}, {"r2", 18}, {"r3", 19}, {"r4", 20},	\
  {"r5", 21}, {"r6", 22}, {"r7", 23}, {"r8", 24},	\
  {"r9", 25}, {"r10", 26}, {"r11", 27}, {"r12", 28},\
  {"r13", 29}, {"r14", 30}, {"r15", 31},			\
}
#endif

#define PRINT_OPERAND print_operand
#define PRINT_OPERAND_ADDRESS print_operand_address
#define PRINT_OPERAND_PUNCT_VALID_P(c) ((c) == 'C' || (c) == 'N')


/* Alignment Commands */
#define ASM_OUTPUT_SKIP(FILE, NBYTES) \
  fprintf (FILE, "\t.space\t%u\t;(ASM_OUTPUT_SKIP)\n", (NBYTES))

#define ASM_OUTPUT_ALIGN(FILE, PWR) 				\
switch(PWR)											\
{													\
	case 1:											\
  	fprintf (FILE, "\t.align 2\n");					\
  	break;											\
													\
	case 2:											\
  	fprintf (FILE, "\t.align 4\n");					\
  	break;											\
}

/* Debug Output */
#define DBX_REGISTER_NUMBER(n) (n)

/*************************
 * Miscellaneous
 *************************/
#define PREDICATE_CODES							\
{"branch_dest_operand",		{ LABEL_REF }},				\
{"call_operand",		{ CONST_INT, CONST, SYMBOL_REF, REG}},	\
{"native_operand",		{ SYMBOL_REF}},
/* {"imm8_compare_operand",	{ CONST_INT }}, */

/* Type used for case table indexes */
#define CASE_VECTOR_MODE Pmode

/*#define STORE_FLAG_VALUE 1*/

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
/*  #define WORD_REGISTER_OPERATIONS (!TARGET_EXTENDED) */

/*
  Define this macro to be a C expression indicating when insns that
  read memory in MODE, an integral mode narrower than a word, set the
  bits outside of MODE to be either the sign-extension or the
  zero-extension of the data read.  Return `SIGN_EXTEND' for values
  of MODE for which the insn sign-extends, `ZERO_EXTEND' for which
  it zero-extends, and `NIL' for other modes.
  
  This macro is not called with MODE non-integral or with a width
  greater than or equal to `BITS_PER_WORD', so you may return any
  value in this case.  Do not define this macro if it would always
  return `NIL'.  On machines where this macro is defined, you will
  normally define it as the constant `SIGN_EXTEND' or `ZERO_EXTEND'.
*/

/*  #define LOAD_EXTEND_OP(MODE) */


/* Define this macro if loading short immediate values into registers
   sign extends. */
#define SHORT_IMMEDIATES_SIGN_EXTEND

/*
  A C expression that is nonzero if on this machine the number of
  bits actually used for the count of a shift operation is equal to
  the number of bits needed to represent the size of the object
  being shifted.  When this macro is non-zero, the compiler will
  assume that it is safe to omit a sign-extend, zero-extend, and
  certain bitwise `and' instructions that truncates the count of a
  shift operation.  On machines that have instructions that act on
  bitfields at variable positions, which may include `bit test'
  instructions, a nonzero `SHIFT_COUNT_TRUNCATED' also enables
  deletion of truncations of the values that serve as arguments to
  bitfield instructions.
  
  If both types of instructions truncate the count (for shifts) and
  position (for bitfield operations), or if no variable-position
  bitfield instructions exist, you should define this macro.
  
  However, on some machines, such as the 80386 and the 680x0,
  truncation only applies to shift operations and not the (real or
  pretended) bitfield operations.  Define `SHIFT_COUNT_TRUNCATED' to
  be zero on such machines.  Instead, add patterns to the `md' file
  that include the implied truncation of the shift instructions.
  
  You need not define this macro if it would always have the value
  of zero. */

#define SHIFT_COUNT_TRUNCATED 1

#ifdef GCC_303
#define EASY_DIV_EXPR TRUNC_DIV_EXPR
#endif
#define TRULY_NOOP_TRUNCATION(oprec, inprec) 1
#define FUNCTION_MODE Pmode

#define DOLLARS_IN_IDENTIFIERS 1

#define TARGET_MEM_FUNCTIONS

#ifdef GCC_303
#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTR, IDENT, ARGS) \
	mapip_valid_machine_decl_attribute (DECL, ATTR, IDENT, ARGS)

#define VALID_MACHINE_TYPE_ATTRIBUTE(TYPE, ATTR, IDENT, ARGS) \
	mapip_valid_machine_type_attribute (TYPE, ATTR, IDENT, ARGS)
#endif

#endif	/*_MAPIP_H_*/
