/* Subroutines used for code generation on the MoSync PIP-E Architecture
   Contributed Mobile Sorcery AB
*/

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

#include "config.h"
#include "system.h"
#include <signal.h>
#include "coretypes.h"
#include "mapip.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "insn-constants.h"
#include "conditions.h"
#include "insn-attr.h"
#include "recog.h"
#include "toplev.h"
#include "output.h"

#include "basic-block.h"

#include "tree.h"
#include "function.h"
#include "expr.h"
#include "flags.h"
#include "reload.h"
#include "output.h"
#include "tm_p.h"
#include "ggc.h"
#include "target.h"
#include "target-def.h"

#ifdef GCC_303
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free
#endif

static void print_mem_expr_old PARAMS ((FILE *, rtx));
#ifdef GCC_303
static int branch_dest PARAMS ((rtx));
static int fix_branches PARAMS ((rtx));
#endif
static void
 mapip_asm_out_constructor (rtx symbol, int priority ATTRIBUTE_UNUSED);
static void
 mapip_asm_out_destructor (rtx symbol, int priority ATTRIBUTE_UNUSED);
void
abort_with_error (const char *reason);
static bool
mapip_rtx_costs (rtx x, int code, int outer_code, int *total);

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS mapip_rtx_costs

#undef  TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE mapip_output_function_epilogue

void mapip_asm_file_start();


#undef  TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START mapip_asm_file_start

struct gcc_target targetm = TARGET_INITIALIZER;

int target_flags = TARGET_DEFAULT;

struct small_common
{
  const char *name;
  int size;
  int align;
  int local;
  struct small_common *next;
};

struct small_common *first_common;

const char *stack_size_opt;
const char *data_heap_size;
const char *code_heap_size;
const char *stack_debug_opt;
const char *mapip_archscale_string;

static long largest_frame_size;
static int current_opt_level;

struct mapip_frame_info frame_info;


/*#define SHOULD_COMBINE_STORE_RESTORE 0*/

#define SHOULD_COMBINE_STORE_RESTORE \
  (optimize_size || frame_info.rblocks == 1 || (optimize >= 2 && frame_info.rblocks <= 2 && frame_info.rblocks > 0))

rtx cmp_ops[2];

int mapip_call_used[FIRST_PSEUDO_REGISTER] = CALL_USED_REGISTERS;
int mapip_fixed_regs[FIRST_PSEUDO_REGISTER] = FIXED_REGISTERS;

/****************************************
 
****************************************/

#if 1//def GCC_303
void
abort_with_error (const char *reason)
{
  error (reason);
  abort ();
}
#endif


/****************************************
 
****************************************/

/*
  if (optimize >= 1)
    {
      flag_defer_pop = 1;
      flag_thread_jumps = 1;
#ifdef DELAY_SLOTS
      flag_delayed_branch = 1;
#endif
#ifdef CAN_DEBUG_WITHOUT_FP
      flag_omit_frame_pointer = 1;
#endif
      flag_guess_branch_prob = 1;
      flag_cprop_registers = 1;
!      flag_loop_optimize = 1;
!      flag_if_conversion = 1;
!      flag_if_conversion2 = 1;
    }

  if (optimize >= 2)
    {
!      flag_crossjumping = 1;
      flag_optimize_sibling_calls = 1;
      flag_cse_follow_jumps = 1;
      flag_cse_skip_blocks = 1;
      flag_gcse = 1;
      flag_expensive_optimizations = 1;
      flag_strength_reduce = 1;
      flag_rerun_cse_after_loop = 1;
      flag_rerun_loop_opt = 1;
      flag_caller_saves = 1;
      flag_force_mem = 1;
      flag_peephole2 = 1;
#ifdef INSN_SCHEDULING
      flag_schedule_insns = 1;
      flag_schedule_insns_after_reload = 1;
#endif
      flag_regmove = 1;
      flag_strict_aliasing = 1;
      flag_delete_null_pointer_checks = 1;
      flag_reorder_blocks = 1;
!      flag_reorder_functions = 1;
!      flag_unit_at_a_time = 1;
    }

  if (optimize >= 3)
    {
      flag_inline_functions = 1;
      flag_rename_registers = 1;
!      flag_unswitch_loops = 1;
!      flag_web = 1;
    }

  if (optimize < 2 || optimize_size)
    {
      align_loops = 1;
      align_jumps = 1;
      align_labels = 1;
      align_functions = 1;
*/


void
optimization_options (optimize, size)
     int optimize;
     int size ATTRIBUTE_UNUSED;
{

	current_opt_level = optimize;

/*	flag_omit_frame_pointer = 1;
	flag_optimize_sibling_calls = 0;
*/
  if (optimize >= 2)
    {
/*#ifdef GCC_303*/
/*	flag_omit_frame_pointer = 1; */
	flag_omit_frame_pointer = 0;

/*#endif*/
      target_flags |= MASK_NATIVE_CALLS;
    }

    printf ("--> GCC PIPIL Compiler v2:" __TIME__":"__DATE__" (O%d)", current_opt_level);

/*	if (mapip_archscale_string)
	    printf (":(Scaled)");
*/
    printf ("\n");

}

/***********************************
*
***********************************/

void mapip_prepare_call_regs ()
{
  int regno;

  if (TARGET_NATIVE_CALLS)
    {
      for (regno = P0_REGNUM; regno < R0_REGNUM; regno++)
        call_used_regs[regno] = 0;
    }
}

/***********************************
*
***********************************/

void mapip_write_interal_label(FILE *theFile, char *Prefix, int Num)
{
	if (strcmp(Prefix, "LBB") == 0)
	{
		fprintf (theFile, "\t.dlab %s%d\n", Prefix, Num);
		return;
	}

	if (strcmp(Prefix, "LBE") == 0)
	{
		fprintf (theFile, "\t.dlab %s%d\n", Prefix, Num);
		return;
	}

	if (strcmp(Prefix, "L") == 0)			/* line labels */
	{
		fprintf (theFile, "L%d:\n", Num);
		return;
	}

	fprintf (theFile, "%s%d:\n", Prefix, Num);
}

/***********************************
*
***********************************/

void mapip_gen_internal_label(char *buf, char *Prefix, int Num)
{
	if (strcmp(Prefix, "L") == 0)			/* instruction labels */
	{
		sprintf (buf, "*L%d", Num);
		return;
	}

	if (strcmp(Prefix, "LTHUNK") == 0)			/* Trap thunk labels */
	{
		sprintf (buf, "*%%%d", Num);
		return;
	}

  sprintf (buf, "*%s%d", Prefix, Num);
}

/***********************************
*
***********************************/

void mapip_output_labref(char *Name, FILE *theFile)
{
  fputs("_", theFile);
  fputs(Name, theFile);
}

/***********************************
*
***********************************/

/*
void asm_file_start (file)
     FILE *file;
{

  fprintf (file, "; START\n");

  first_common = NULL;
}
*/

void mapip_asm_file_start()
{
/*	fprintf (asm_out_file, "; START\n"); */

	fprintf (asm_out_file, ".model ");
  
	if (!mapip_archscale_string)
	{
		fprintf (asm_out_file, "full\n");
		return;
	}

	fprintf (asm_out_file, "%s\n", mapip_archscale_string);
}

/***********************************
*
***********************************/

/* void mapip_output_mi_thunk(FILE *file, tree thunkdecl, HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset, tree funcdecl) */

void mapip_output_mi_thunk(	FILE *file,
							tree thunkdecl ATTRIBUTE_UNUSED,
			 				HOST_WIDE_INT delta,
			 				HOST_WIDE_INT vcall_offset ATTRIBUTE_UNUSED,
			 				tree funcdecl)
{
	fprintf (file, ";** MI THUNK BEGIN **\n");

	fprintf (file, "add i0,#0x%lx\n", delta);

	fprintf (file, "call &");
	assemble_name (file, XSTR(XEXP(DECL_RTL(funcdecl),0),0));
	fputs ("\n", file);

	fprintf (file, ";** MI THUNK END **\n");
	return;
}

/***********************************
*
***********************************/

#ifdef GCC_303

void asm_file_end (file)
     FILE *file;
{
  fprintf (file, "; (asm_file_end)\n" );

  if (first_common != NULL)
    {
      struct small_common *com = first_common;

      fprintf (file, "; Small uninitialized data:\n%s\n", BSS_SECTION_ASM_OP);
      ASM_OUTPUT_ALIGN(file, com->align);
      
      while (com != NULL)
        {
          if (!com->local)
            ASM_GLOBALIZE_LABEL(file, com->name);
          
          ASM_OUTPUT_LABEL (file, com->name);
          ASM_OUTPUT_SKIP(file, com->size);
          com = com->next;
        }

      first_common = NULL;
    }

  fprintf (file, ".end\n");
}

/****************************************
 
****************************************/

void
asm_output_common (file, name, size, alignment, local)
     FILE *file;
     char *name;
     int size;
     int alignment;
     int local;
{
  fprintf (file, "; (asm_output_common)\n" );

  if (TARGET_OPTIMIZE_BSS && alignment < BIGGEST_ALIGNMENT)
    {
      static int align_pow2[] = { 0, 0, 1, 2 };
      int align = align_pow2[alignment / BITS_PER_UNIT];

      struct small_common *com;

      com = ggc_alloc (sizeof (struct small_common));

      com->name = ggc_strdup (name);
      com->size = size;
      com->align = align;
      com->local = local;

      if (first_common == NULL)
        {
          first_common = com;
          com->next = NULL;
        }
      else
        {
          struct small_common *tmp = first_common;
          while (tmp->next != NULL && align < tmp->next->align)
            tmp = tmp->next;
          com->next = tmp->next;
          tmp->next = com;
        }
    }
  else
    {
      int rounded;
      
      if (size == 0)
        rounded = 1;
      else
        rounded = size;
      
      rounded += (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1;
      rounded = (rounded / (BIGGEST_ALIGNMENT / BITS_PER_UNIT)
                 * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));
      
      if (local)
        ASM_OUTPUT_LOCAL (file, name, size, rounded);
      else
        ASM_OUTPUT_COMMON (file, name, size, rounded);
    }
}

/****************************************
 
****************************************/

void
asm_output_ascii (file, ptr, len)
     FILE *file;
     char *ptr;
     int len;
{
  int i;
  int col;
  char *str = (char*) (ptr);

  if (len <= 0)
    return;
  
  fputs ("\t.string\t\"", file);
  for (i = col = 0; i < len; i++)
    {
      switch (str[i])
        {
        case TARGET_NEWLINE:
          fputs ("\\n", file);
          col += 2;
          break;
        case 0:
          fputs ("\\0", file);
          col += 2;
          break;
        case '"':
          fputs ("\\\"", file);
          col += 2;
          break;
        default:
          if (str[i] >= ' ' && str[i] < 0177)
            {
              putc (str[i], file);
              col++;
            }
          else
            {
              fprintf (file, "\\x%02x", (unsigned char) str[i]);
              col += 4;
            }
        }
      
      if (col >= 60)
        {
          fputs ("\"\n\t.string\t\"", file);
          col = 0;
        }
    }
  fputs ("\"\n", file);
}

/****************************************
 
****************************************/

void
mapip_output_float (file, real)
     FILE *file;
     REAL_VALUE_TYPE real;
{
  HOST_WIDE_INT val;

  REAL_VALUE_TO_TARGET_SINGLE (real, val);

/*abort_with_error("Fatal: floating point not supported");*/

  fputs ("\t.word\t", file);
  fprintf (file, HOST_WIDE_INT_PRINT_HEX, val);
  fprintf (file, "\t\t; %.12g\n", real);
}

/****************************************
 
****************************************/

void
mapip_output_double (file, real)
     FILE *file;
     REAL_VALUE_TYPE real;
{
  HOST_WIDE_INT val[2];

  REAL_VALUE_TO_TARGET_DOUBLE (real, val);

/*abort_with_error("Fatal: floating point not supported");*/

  fputs ("\t.word\t", file);
  fprintf (file, HOST_WIDE_INT_PRINT_HEX, val[0]);
  fputc (',', file);
  fprintf (file, HOST_WIDE_INT_PRINT_HEX, val[1]);
  fprintf (file, "\t\t; %.20g\n", real);
}

/****************************************
 
****************************************/

void
mapip_output_double_int (file, x)
     FILE *file ATTRIBUTE_UNUSED;
     rtx x;
{

abort_with_error("Fatal: Floating point not supported");

  assemble_integer(operand_subword(x,0,0,DImode),UNITS_PER_WORD,1);
  assemble_integer(operand_subword(x,1,0,DImode),UNITS_PER_WORD,1);
}
#endif	/*GCC_303*/

/***********************************
*
***********************************/
 
void print_operand (FILE *file, rtx x, int letter)
{
	register enum rtx_code code;

	if (!x)
	{
		error ("PRINT_OPERAND null pointer");
		return;
	}

	code = GET_CODE (x);

	if (code == SIGN_EXTEND)
		x = XEXP (x, 0), code = GET_CODE (x);

	if (letter == 'C' || letter == 'N')
	{
		switch (code)
		{
		case EQ: fputs ("eq", file); return;
		case NE: fputs ("ne", file); return;
		case GT: fputs ("gt", file); return;
		case GE: fputs ("ge", file); return;
		case LT: fputs ("lt", file); return;
		case LE: fputs ("le", file); return;
		case GTU: fputs ("gtu", file); return;
		case GEU: fputs ("geu", file); return;
		case LTU: fputs ("ltu", file); return;
		case LEU: fputs ("leu", file); return;
		default: abort_with_insn (x, "PRINT_OPERAND, invalid insn for %%C");
		}

		return;
	}    

	if (code == REG)
	{
		int regno = REGNO(x);

		if (letter == 'D')					/* High part of double */
			regno++;

		fprintf (file, "%s", reg_names[regno]);
		return;
	}

	if (code == MEM)
	{
		output_address (XEXP (x, 0));
		return;
	}

	if (code == CONST_INT || code == CONST_DOUBLE)
	{
/* !! Added ARH 20-04-08 Fixed problem with immediate floats */

		if (GET_MODE(x) == SFmode)
		{
			REAL_VALUE_TYPE d;
			long l;

			fprintf (file, "#");
			REAL_VALUE_FROM_CONST_DOUBLE (d, x);
			REAL_VALUE_TO_TARGET_SINGLE (d, l);
			fprintf (file, HOST_WIDE_INT_PRINT_HEX, l);
			fprintf(file, "\t\t; %.12g", d);
			return;
		}

/* !! End */

		fprintf (file, "#");
		fprintf (file, HOST_WIDE_INT_PRINT_HEX, INTVAL (x));
		return;
	}

	fprintf (file, "#");
	output_addr_const (file, x);
}

/***********************************
*
***********************************/

void print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  if (!addr)
    {
      error ("PRINT_OPERAND_ADDRESS, null pointer");
      return;
    }

  switch (GET_CODE (addr))
    {
    case REG:
      fputs (reg_names[REGNO (addr)], file);
      break;

    case PLUS:
      {
        register rtx reg    = (rtx)0;
        register rtx offset = (rtx)0;
        register rtx arg0   = XEXP (addr, 0);
        register rtx arg1   = XEXP (addr, 1);

/*  	debug_rtx (addr); */
        
        if (GET_CODE (arg0) == REG)
          {
            reg = arg0;
            offset = arg1;
            if (GET_CODE (offset) == REG)
              abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, 2 regs");
          }
        else if (GET_CODE (arg1) == REG)
          {
            reg = arg1;
            offset = arg0;
          }
        else if (CONSTANT_P (arg0) && CONSTANT_P (arg1))
          {
            output_addr_const (file, addr);
            break;
          }
        else if (GET_CODE (arg0) == MEM)
          {
            print_mem_expr_old (file, arg0);
            offset = arg1;
          }
        else
          abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, no regs");

        if (reg)
          fprintf (file, "%s,", reg_names[REGNO (reg)]);

        if (offset)
          {
            if (!CONSTANT_P (offset))
              abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, invalid insn #2");
            output_addr_const (file, offset);
          }
      }
      break;

    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST:
/*ARH*/
      fprintf (file, "&");

      output_addr_const (file, addr);
      break;

    default:
      abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, invalid insn #1");
    }
}

/***********************************
*
***********************************/

static void
print_mem_expr_old (file, op)
     FILE *file;
     rtx op;
{
  rtx arg0;
  arg0 = XEXP (op, 0);
  switch (GET_CODE (arg0))
    {
    case PLUS:
      print_operand_address (file, arg0);
      fprintf (file, "+");
      break;
    case REG:
      print_operand_address (file, arg0);
      fputc (',', file);
      break;
    case MEM:
      print_mem_expr_old (file, arg0);
      break;
    default:
      print_operand (file, op, 0);
    }
}

/****************************************
 
****************************************/

void
abort_with_insn (insn, reason)
     rtx insn;
     const char *reason;
{
  error (reason);
  debug_rtx (insn);
  abort ();
}

/****************************************
 
****************************************/

long
mapip_branch_cost (code, cmp)
     enum rtx_code code;
     rtx cmp;
{
/*  if (GET_CODE (cmp) == CONST_INT)
     return COSTS_N_INSNS (2);
*/
/*    {
      if (INTVAL (cmp) >= -128 && INTVAL (cmp) < 256
          && (GET_MODE (cmp) == SImode || GET_MODE (cmp) == QImode))
        return 0;
      else if (INTVAL (cmp) >= -32768 && INTVAL (cmp) < 32768)
        return COSTS_N_INSNS (2);
      else
        return COSTS_N_INSNS (4);
    }

  else
*/
    return COSTS_N_INSNS (2);
}

/****************************************
 
****************************************/

void
mapip_order_regs_for_local_alloc ()
{
#ifdef GCC_303
  int i;
  rtx insn;
#endif
  int regno;
  
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    reg_alloc_order[regno] = regno;
}

/****************************************
 
****************************************/

int
mapip_valid_machine_decl_attribute (decl, attributes, identifier, args)
     tree decl ATTRIBUTE_UNUSED;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier;
     tree args;
{
  if (is_attribute_p ("native", identifier))
    return (args == NULL_TREE);

  return 0;
}

/****************************************
 
****************************************/

int
mapip_valid_machine_type_attribute (type, attributes, identifier, args)
     tree type;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier;
     tree args;
{
  if (TREE_CODE (type) != FUNCTION_TYPE && TREE_CODE (type) != TYPE_DECL)
    return 0;

  if (is_attribute_p ("native", identifier))
    return (args == NULL_TREE);

  return 0;
}

/****************************************
 
****************************************/

void
mapip_encode_section_info (decl)
	tree decl;
{
  tree attr;
  rtx x;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    return;

  x = DECL_RTL (decl);
  if (GET_CODE (x) == MEM)
    x = XEXP (x, 0);

  attr = DECL_ATTRIBUTES (decl);
  if (!lookup_attribute ("native", attr))
    {
      if (GET_CODE (x) == SYMBOL_REF)
        SYMBOL_REF_FLAG (x) = 0;
      return;
    }

  /*
  if (!DECL_EXTERNAL (decl))
    {
      error_with_decl (decl, "`%s' redeclared as non-native");
      return;
    }
  */
  
  if (GET_CODE (x) == SYMBOL_REF)
    SYMBOL_REF_FLAG (x) = 1;
}


/*
int SaveReg(int reg)
{
	if (!regs_ever_live[reg])
		return 0;

	if (!call_used_regs[reg])
		return 0;

	if (reg == HARD_FRAME_POINTER_REGNUM)
		if (!frame_pointer_needed)
			return 0;

    if (reg == RA_REGNUM)
   		if (current_function_is_leaf)
   			return 0;

	return 1;
}
*/

/****************************************
 
****************************************/

#define SAVE_REGISTER_P(REGNO) \
  ((regs_ever_live[REGNO] && !call_used_regs[REGNO])			\
   || (REGNO == HARD_FRAME_POINTER_REGNUM && frame_pointer_needed) \
   || (REGNO == RA_REGNUM && regs_ever_live[REGNO] && !current_function_is_leaf) \
   || (REGNO == RA_REGNUM && current_opt_level==0) )		/* New added for Freddy */

int
compute_frame_size (locals)
     int locals;
{
  int total_size;
  int regs;
  int rmask;
  int i;
  int rblocks;
  int first;
  int last;
  
  /* Calculate saved registers */

  regs = rmask = rblocks = 0;
  first = last = 0;
  
  for (i = RA_REGNUM; i <= LAST_SAVED_REGNUM; i++)
    {
      if (SAVE_REGISTER_P (i))
        {
          if (!first)
            first = i;

          last = i;

          regs += UNITS_PER_WORD;

          if ((rmask & (1 << (i-1))) == 0)
            rblocks++;
          
          rmask |= 1 << i;
        }
    }


  if (SHOULD_COMBINE_STORE_RESTORE)
    regs = UNITS_PER_WORD * (1 + last - first);

  /* Compute total size of stack frame */

  total_size = regs;
  total_size += locals;
  total_size += current_function_outgoing_args_size;
  total_size += current_function_pretend_args_size;
  frame_info.total_size = total_size;

  /* Store the size of the various parts of the stack frame */

  frame_info.rmask = rmask;
  frame_info.rblocks = rblocks;
  frame_info.regs = regs;
  frame_info.n_regs = regs / UNITS_PER_WORD;
  frame_info.first_reg = first;
  frame_info.last_reg = last;
  frame_info.outgoing = current_function_outgoing_args_size;

  /* FIXME: ??? */

  frame_info.outgoing += current_function_pretend_args_size;
  frame_info.locals = locals;
  frame_info.valid = reload_completed;

  if (frame_info.total_size > largest_frame_size)
    {
      largest_frame_size = frame_info.total_size;
    }

  return frame_info.total_size;
}

/****************************************
 
****************************************/

int
simple_return ()
{
  int i;

  if (frame_pointer_needed)
    return 0;
  else
    {
      i = (frame_info.valid
           ? frame_info.total_size
           : compute_frame_size (get_frame_size ()));

      return (i == 0);
    }
}

/****************************************
 
****************************************/

/* Newer Version */

void mapip_expand_epilogue ()
{
	int i, j;
	long framesize;
	long adjust;

	framesize = (frame_info.valid ? frame_info.total_size: compute_frame_size (get_frame_size ()));

	if (simple_return ())
	{
		emit_jump_insn (gen_return_internal (gen_rtx_REG (SImode, RA_REGNUM)));      
		frame_info.valid = 0;
		return;
	}

	adjust = framesize - frame_info.regs;

	if (adjust != 0)
	{
		rtx sp = gen_rtx_REG (SImode, SP_REGNUM);
		emit_insn (gen_addsi3 (sp, sp, GEN_INT (adjust)));      
	}

	/* Restore pushed registers in reverse order */

	if (frame_info.regs == 0)
	{
		emit_jump_insn (gen_return_internal (gen_rtx_REG (SImode, RA_REGNUM)));
		frame_info.valid = 0;
		return;
	}

	//* Must be a frame */

	int rblocks = frame_info.rblocks;

	if (SHOULD_COMBINE_STORE_RESTORE)
	{
		emit_insn (gen_restore_regs (gen_rtx_REG (SImode, frame_info.first_reg),
									 gen_rtx_REG (SImode, frame_info.last_reg)));

		emit_jump_insn (gen_return_internal (gen_rtx_REG (SImode, RA_REGNUM)));

		frame_info.valid = 0;
		return;
	}

	int rmask = frame_info.rmask;

	for (i = LAST_SAVED_REGNUM; i >= 0; )
	{
		j = i;

		while (rmask & (1 << j))
			j--;

		if (j < i)
		{                  
			if (--rblocks == 0)
			{
//				emit_jump_insn (gen_ret_internal (gen_rtx_REG (SImode, j+1),gen_rtx_REG (SImode, i)));

				emit_insn (gen_restore_regs (gen_rtx_REG (SImode, frame_info.first_reg),
											 gen_rtx_REG (SImode, frame_info.last_reg)));

				emit_jump_insn (gen_return_internal (gen_rtx_REG (SImode, RA_REGNUM)));

				frame_info.valid = 0;
				return;
			}
			else
			{
				emit_insn (gen_restore_regs (gen_rtx_REG (SImode, j+1),
				gen_rtx_REG (SImode, i)));
			}

			i = j;
		}
		else
			i--;
	}

	frame_info.valid = 0;
}

/***********************************
	reset function epilogue
***********************************/

void mapip_output_function_epilogue(FILE *file, int size)
{
/*	fprintf(file, ";*epilogue %d*\n", size); */
	frame_info.valid = 0;
}

/***********************************
Output function name and information
***********************************/

/*struct dump_info di;*/
/* char info[256];*/

void mapip_write_func_name(FILE *file, char *name)
{
	int reg_count = current_function_args_info.words;
	rtx x = current_function_return_rtx;

	rtx func_rtx = current_function_decl;

	register enum rtx_code code;
	char *mtype = "void";

/*	c_dump_tree ( (void *)info, func_rtx); */
/*	dump_node (func_rtx, 1, file); */
		
	// Void functions return void

	if (x)
	{
		code = GET_CODE (x);

		if (code == SIGN_EXTEND)
			x = XEXP (x, 0), code = GET_CODE (x);
	
		switch(GET_MODE(x))
		{

			case SFmode:	mtype = "float";	break;
			case DFmode:	mtype = "double";	break;
			case SImode:	mtype = "int";		break;
			default:		mtype = "?";		break;
		}
	}
	
	/* emit the function directive with the number of input params*/
	
	fprintf (file, "\n.func ");
	assemble_name (file, name);
	fprintf (file, ", %d", reg_count);
	fprintf (file, ", %s", mtype);
	fprintf (file, "\n");
}

/****************************************
 
****************************************/

/* Output assembler code for a function prologue */
void
mapip_expand_prologue ()
{
  long framesize;
  long adjust;
  rtx sp = NULL_RTX;
  rtx fp = NULL_RTX;

  framesize = (frame_info.valid
	       ? frame_info.total_size
	       : compute_frame_size (get_frame_size ()));
  
  if (framesize > 0 || frame_pointer_needed)
    sp = gen_rtx_REG (SImode, SP_REGNUM);
  
  if (frame_pointer_needed)
    fp = gen_rtx_REG (SImode, FP_REGNUM);
  
  /* Save the necessary registers */
  if (SHOULD_COMBINE_STORE_RESTORE)
    {
      emit_insn (gen_store_regs (gen_rtx_REG (SImode, frame_info.first_reg),
                                 gen_rtx_REG (SImode, frame_info.last_reg)));
    }
  else
    {
      int rmask = frame_info.rmask;
      int i,j;
      
      for (i = 0; rmask != 0; )
        {
          if (rmask & 1)
            {
              for (j = i; rmask & 1; j++)
                rmask >>= 1;
                            
              emit_insn (gen_store_regs (gen_rtx_REG (SImode, i),
                                         gen_rtx_REG (SImode, j-1)));
              i = j;
            }
          else
            {
              rmask >>= 1;
              i++;
            }
        }
    }
  
  adjust = framesize - frame_info.regs;
  if (adjust != 0)
    {
      /* Adjust $sp to make room for locals */
      emit_insn (gen_subsi3 (sp, sp, GEN_INT (adjust)));
    }
  
  if (frame_pointer_needed)
    {
      /* Adjust frame pointer to point to arguments */
      if (framesize > 0)
        {
          emit_move_insn (fp, sp);
          emit_insn (gen_addsi3 (fp, fp, GEN_INT (framesize)));
 
        }
      else
        {
          emit_move_insn (fp, sp);
        }
    }
}

/****************************************
 
****************************************/

rtx
mapip_function_value (TYPE, FUNC)
     tree TYPE;
     tree FUNC ATTRIBUTE_UNUSED;
{
  enum machine_mode mode = TYPE_MODE (TYPE);
  PROMOTE_MODE (mode, 0, TYPE);
  return gen_rtx_REG (mode, RETURN_REGNUM);
}

/****************************************
 
****************************************/

void
function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  cum->argc++;
  switch (mode)
    {
    default:
    case BLKmode:
      cum->words += ((int_size_in_bytes (type) + UNITS_PER_WORD - 1)
                     / UNITS_PER_WORD);
      break;

    case DFmode:
    case DImode:
      cum->words += 2;
      break;

    case VOIDmode:
    case QImode:
    case HImode:
    case SImode:
    case SFmode:
      /* All integers below or equal to word size are promoted to word size */
      cum->words++;
      break;
    }
}

/****************************************
 
****************************************/

rtx
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  rtx x;
  int words;

  words = cum->words;

  if (words > MAX_ARGS_IN_REGS || named == 0)
    return 0;

  if (type)
    PROMOTE_MODE (mode, 0, type);

  switch (mode)
    {
    default:
    case BLKmode:
      words += ((int_size_in_bytes (type) + UNITS_PER_WORD - 1)
                / UNITS_PER_WORD);
      break;

    case DFmode:
    case DImode:
      words += 2;
      break;

    case VOIDmode:
    case SFmode:
    case QImode:
    case HImode:
    case SImode:
      words++;
      break;
    }

  if (words > MAX_ARGS_IN_REGS)
    return 0;

  x = gen_rtx_REG (mode, P0_REGNUM + cum->words);

  if (x == 0)
    abort ();

  return x;
}

/****************************************
 
****************************************/

int
branch_dest_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == LABEL_REF);
}

/****************************************
 
****************************************/

int
call_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (CONSTANT_ADDRESS_P (op)
	  || (GET_CODE (op) == REG && op != arg_pointer_rtx
	      && !(REGNO (op) >= FIRST_PSEUDO_REGISTER
		   && REGNO (op) <= LAST_VIRTUAL_REGISTER)));
}

/****************************************
 
****************************************/

int native_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == MEM)
    op = XEXP (op, 0);

  return (GET_CODE (op) == SYMBOL_REF && SYMBOL_REF_FLAG (op));
}

/****************************************
 
****************************************/

int
double_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx addr;

  if (GET_CODE (op) != MEM
      || ! memory_operand (op, mode))
    {
      /* During reload, we accept a pseudo register if it has an
         appropriate memory address.  If we don't do this, we will
         wind up reloading into a register, and then reloading that
         register from memory, when we could just reload directly from
         memory.  */
      if (reload_in_progress
          && GET_CODE (op) == REG
          && REGNO (op) >= FIRST_PSEUDO_REGISTER
          && reg_renumber[REGNO (op)] < 0
          && reg_equiv_mem[REGNO (op)] != 0
          && double_memory_operand (reg_equiv_mem[REGNO (op)], mode))
        return 1;
    }
  
  /* Make sure that 4 added to the address is a valid memory address.
     This essentially just checks for overflow in an added constant.  */
  
  addr = XEXP (op, 0);

  if (CONSTANT_ADDRESS_P (addr))
    return 1;
  
  return memory_address_p ((GET_MODE_CLASS (mode) == MODE_INT
                            ? SImode
                            : SFmode),
                           plus_constant (addr, 4));
}

/***********************************
*
***********************************/

int mapip_expand_call (rtx operands[])
{
  rtx addr = XEXP (operands[0], 0);

  if (GET_CODE(addr) != REG
      && (!CONSTANT_ADDRESS_P (addr)
          || !call_operand (addr, VOIDmode)))
    {
      XEXP (operands[0], 0) = addr = force_not_mem (addr);
    }

  if (GET_CODE(addr) == SYMBOL_REF && SYMBOL_REF_FLAG(addr) == 1)
    {
      emit_call_insn (gen_call_native (operands[0],
                                       operands[1]));
      return 1;
    }
  
  if (TARGET_NATIVE_CALLS)
    {
      if (GET_CODE (addr) == REG)
        emit_call_insn (gen_call_indirect_clobber (operands[0], operands[1]));
      else if (CONSTANT_ADDRESS_P (addr))
        emit_call_insn (gen_call_direct_clobber (operands[0], operands[1]));
      else
        return 0;
    }
  else
    {
      if (GET_CODE (addr) == REG)
        emit_call_insn (gen_call_indirect (operands[0], operands[1]));
      else if (CONSTANT_ADDRESS_P (addr))
        emit_call_insn (gen_call_direct (operands[0], operands[1]));
      else
        return 0;
    }

  return 1;
}

/***********************************
*
***********************************/

void mapip_gen_branch (operands, test)
     rtx operands[];
     enum rtx_code test;
{
  enum machine_mode mode;
  rtx label;
  rtx cmp0 = cmp_ops[0];
  rtx cmp1 = cmp_ops[1];
  rtx reg = 0;
  int addr;

  mode = GET_MODE (cmp0);
  if (mode == VOIDmode)
    mode = GET_MODE (cmp1);

  if (mode == VOIDmode)
    mode = SImode;

  /* Generate branch destinations */
  label = gen_rtx (LABEL_REF, VOIDmode, operands[0]);

  if (GET_CODE (cmp1) == CONST_INT)
    {
      /* Check if immediate compare operand fits the
         constraints, otherwise force it to a register */
		
/*      if (((mode == SImode || mode == HImode)
           && (INTVAL (cmp1) < -128
               || INTVAL (cmp1) > 127))
          || (mode != SImode && mode != QImode && mode != HImode))
*/

		/* Always force to register */

/*GOT HERE*/

        cmp1 = force_reg (mode, cmp1);
    }
  
  if (GET_MODE_SIZE(mode) < UNITS_PER_WORD
      && GET_CODE (cmp1) != CONST_INT)
    {
      int unsignedp = (test == GTU || test == GEU
		       || test == LTU || test == LEU);

      /* Extend first operand */
      rtx target = gen_reg_rtx (SImode);

      if (unsignedp)
        emit_insn (gen_rtx_SET (SImode, target,
                                gen_rtx_ZERO_EXTEND(SImode, cmp0)));
      else
        emit_insn (gen_rtx_SET (SImode, target,
                                gen_rtx_SIGN_EXTEND(SImode, cmp0)));
      
      emit_move_insn (cmp0, cmp0);
      cmp0 = target;
      
      /* Extend second operand */
      target = gen_reg_rtx (SImode);
      
      if (unsignedp)
        emit_insn (gen_rtx_SET (SImode, target,
                                gen_rtx_ZERO_EXTEND(SImode, cmp1)));
      else
        emit_insn (gen_rtx_SET (SImode, target,
                                gen_rtx_SIGN_EXTEND(SImode, cmp1)));
      
      emit_move_insn (cmp1, cmp1);
      cmp1 = target;
      mode = SImode;
    }

  /* Do the jump */
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
                               gen_rtx_IF_THEN_ELSE (VOIDmode,
                                                     gen_rtx (test, mode,
                                                              cmp0, cmp1),
                                                     label, pc_rtx)));
}

/*---------------------------------
- 		No longer needed
----------------------------------*/

/*

static int branch_dest (branch)
     rtx branch;
{
  rtx dest = SET_SRC (PATTERN (branch));
  int dest_uid;

  if (GET_CODE (dest) == IF_THEN_ELSE)
    dest = XEXP (dest, 1);
  dest = XEXP (dest, 0);
  dest_uid = INSN_UID (dest);
  return INSN_ADDRESSES (dest_uid);
}
*/


/*---------------------------------
-	Output assembly to perform a
-		conditional branch.
- 		No longer needed
----------------------------------*/


const char * output_branch (insn, operands, rel8)
     rtx insn;
     rtx operands[];
     int rel8;
{

  if (INTVAL(operands[2]) == 0)
    return "jc   %C0,%1,zr,%3";
  
  return "jc   %C0,%1,%2,%3";
}

/*---------------------------------
- 		New Float/Double Mover
----------------------------------*/

const char * mapip_move_word (rtx operands[], int unsignedp)
{
	rtx dst = operands[0];
	rtx src = operands[1];

	enum rtx_code c0 = GET_CODE (dst);
	enum rtx_code c1 = GET_CODE (src);

	int subreg_word0 = 0;
	int subreg_word1 = 0;

	//  const char *ret = 0;

	while (c0 == SUBREG)
	{
		subreg_word0 += SUBREG_BYTE (dst);
		dst = SUBREG_REG (dst);
		c0 = GET_CODE (dst);
	}

	while (c1 == SUBREG)
	{
		subreg_word1 += SUBREG_BYTE (src);
		src = SUBREG_REG (src);
		c1 = GET_CODE (src);
	}

	if (c0 == REG && c1 == REG)
	{
		int r0 = REGNO (dst) + subreg_word0;
		int r1 = REGNO (src) + subreg_word1;

		if (r0 == r1)
			return "";

		return "ld %0,%1";
	}

	if (c0 == REG)
	{
		if (c1 == MEM)
			return "ld %0,[%1]";

		if (optimize_size && c1 == CONST_INT && INTVAL (src) == 0)
			return "ld %0,#0";

		if (CONSTANT_P (src))
			return "ld %0,%1";
	}

	if (c0 == MEM)
	{
		if (c1 == REG)
		return "ld  [%0],%1";
	}

	// Bad stuff has happened

	abort_with_error("Fatal: error in floating point system");
	return "";
}

/***********************************
*  Test if a reg can be used as a
*		  base register
***********************************/

int mapip_reg_ok_for_base_p (x, strict)
     rtx x;
     int strict;
{
  if (GET_MODE (x) == QImode || GET_MODE (x) == HImode)
    return 0;

  return (strict
	  ? REGNO_OK_FOR_BASE_P (REGNO (x))
	  : (REGNO (x) < FIRST_PSEUDO_REGISTER || REGNO (x) >= FIRST_PSEUDO_REGISTER));
}

/****************************************
 
****************************************/

int
mapip_legitimate_address (mode, x, strict)
     enum machine_mode mode;
     rtx x;
     int strict;
{
  int valid;

  if (CONSTANT_ADDRESS_P (x))
    return 1;

  while (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  if (GET_CODE (x) == REG && mapip_reg_ok_for_base_p (x, strict))
    return 1;

  valid = 0;
  if (GET_CODE (x) == PLUS)
    {
      register rtx x0 = XEXP (x, 0);
      register rtx x1 = XEXP (x, 1);

      register enum rtx_code c0;
      register enum rtx_code c1;

      while (GET_CODE (x0) == SUBREG)
	x0 = SUBREG_REG (x0);

      c0 = GET_CODE (x0);

      while (GET_CODE (x1) == SUBREG)
	x1 = SUBREG_REG (x1);

      c1 = GET_CODE (x1);

      if (c0 == REG
	  && mapip_reg_ok_for_base_p (x0, strict))
        {
	  if (c1 == CONST_INT || CONSTANT_ADDRESS_P (x1))
	    valid = 1;
	}

      if (!valid)
	{
/*  	  fprintf (stderr, "INVALID(%d): ", reload_completed); */
/*  	  debug_rtx (x); */
	}
    }

  return valid;
}

/****************************************
 
****************************************/

int mapip_can_eliminate (from, to)
     int from;
     int to;
{
  int leaf = current_function_is_leaf;
  if (from == RETURN_ADDRESS_POINTER_REGNUM
      && (!leaf || (to == RA_REGNUM && leaf)))
    return 1;
  
  if (from != RETURN_ADDRESS_POINTER_REGNUM
      && ((to == HARD_FRAME_POINTER_REGNUM)
          || (to == STACK_POINTER_REGNUM && !frame_pointer_needed)))
    {
      return 1;
    }
  return 0;
}

/****************************************
 
****************************************/

void
initial_elimination_offset (from, to, poffset)
     int from;
     int to;
     int *poffset;
{
  int framesize;
  int printit = 0;

  framesize = (frame_info.valid
               ? frame_info.total_size
               : compute_frame_size (get_frame_size ()));

  if (from == ARG_POINTER_REGNUM)
    {
      if (to == HARD_FRAME_POINTER_REGNUM)
	*poffset = 0;
      else
	*poffset = framesize;
      printit = 1;
    }
  else if (from == FRAME_POINTER_REGNUM)
    {
      if (to == STACK_POINTER_REGNUM)
	*poffset = frame_info.outgoing + frame_info.locals;
      else
	*poffset = -frame_info.regs;
      printit = 1;
    }
  else /* if (from == RETURN_ADDRESS_POINTER_REGNUM) */
    {
      if (to == FRAME_POINTER_REGNUM)
	*poffset = -UNITS_PER_WORD;
      else if (to == STACK_POINTER_REGNUM)
	*poffset = framesize - UNITS_PER_WORD;
      else
	*poffset = 0;
      printit = 1;
    }

}

/****************************************
 
****************************************/

const char * mapip_move_2words (rtx operands[], int unsignedp)
{
	const char *ret = 0;
	rtx op0 = operands[0];
	rtx op1 = operands[1];
	enum rtx_code code0 = GET_CODE (operands[0]);
	enum rtx_code code1 = GET_CODE (operands[1]);
	int subreg_word0 = 0;
	int subreg_word1 = 0;

	while (code0 == SUBREG)
	{
		subreg_word0 += SUBREG_BYTE (op0);
		op0 = SUBREG_REG (op0);
		code0 = GET_CODE (op0);
	}

	if (code1 == SIGN_EXTEND)
	{
		op1 = XEXP (op1, 0);
		code1 = GET_CODE (op1);
	}

	while (code1 == SUBREG)
	{
		subreg_word1 += SUBREG_BYTE (op1);
		op1 = SUBREG_REG (op1);
		code1 = GET_CODE (op1);
	}
      
	/* Sanity check.  */
	if (GET_CODE (operands[1]) == SIGN_EXTEND
		&& code1 != REG
		&& code1 != CONST_INT
		/* The following three can happen as the result of a questionable
		cast.  */
		&& code1 != LABEL_REF
		&& code1 != SYMBOL_REF
		&& code1 != CONST)
		abort ();

	if (code0 == REG)
	{
		int regno0 = REGNO (op0) + subreg_word0;

		if (code1 == REG)
		{
			int regno1 = REGNO (op1) + subreg_word1;

			/* Just in case, don't do anything for assigning a register
			to itself.  */
	
			if (regno0 == regno1)
				return "";

			if (regno0 != (regno1+1))
				return "ld %0,%1\n\tld %D0,%D1 ;1";

			return "ld %D0,%D1\n\tld %0,%1 ;2";
		}

		if (code1 == CONST_DOUBLE)
		{
			if (op1 != CONST0_RTX (GET_MODE (op1)))
			{
				split_double (op1, operands + 2, operands + 3);
				return "ld %0,%2\n\tld %D0,%3 ;3";
			}
				return "ld %0,zr\n\tld %D0,zr ;4";
		}

		if (code1 == CONST_INT && INTVAL (op1) == 0)
		{
			return "ld %0,zr\n\tld %D0,zr ;5";
		}

		if (code1 == CONST_INT && GET_MODE (op0) == DImode)
		{
			if (HOST_BITS_PER_WIDE_INT < 64)
			{
				operands[2] = GEN_INT (INTVAL (operands[1]) >= 0 ? 0 : -1);
				return "ld %D0,%2\n\tld %0,%1 ;6";
			}

			/* We use multiple shifts here, to avoid warnings about out
			of range shifts on 32 bit hosts.  */

			operands[2] = GEN_INT (INTVAL (operands[1]) >> 16 >> 16);
			operands[1]	= GEN_INT (INTVAL (operands[1]) << 16 << 16 >> 16 >> 16);
			return "ld %D0,%2\n\tld %0,%1 ;7";
		}
		
		if (code1 == MEM)
		{
			if (double_memory_operand (op1, GET_MODE (op1)))
			{
#ifdef GCC_303
				operands[2] = adj_offsettable_operand (op1, 4);
#else
				operands[2] = adjust_address(op1, DImode, 4);
#endif
				if (reg_mentioned_p (op0, op1))
					return "ld %D0,[%2]\n\tld %0,[%1] ;7";
	
				return "ld %0,[%1]\n\tld %D0,[%2] ;8";
			}
		}

		if (code1 == LABEL_REF)
			return "ld %0,%1 ;9";

		if (code1 == SYMBOL_REF || code1 == CONST)
			return "ld %0,%1 ;10";
	}

	if (code0 == MEM)
	{
		if (code1 == REG)
		{
			int regno1 = REGNO (op1) + subreg_word1;

			if (double_memory_operand (op0, GET_MODE (op0)))
			{
#ifdef GCC_303
				operands[2] = adj_offsettable_operand (op0, 4);
#else
				operands[2] = adjust_address(op0, DImode, 4);
#endif
				return "ld [%0],%1\n\tld [%2],%D1 ;11";
			}
		}

		if (((code1 == CONST_INT && INTVAL (op1) == 0)
			|| (code1 == CONST_DOUBLE
			&& op1 == CONST0_RTX (GET_MODE (op1))))
			&& (double_memory_operand (op0, GET_MODE (op0))))
			{
#ifdef GCC_303
				operands[2] = adj_offsettable_operand (op0, 4);
#else
				operands[2] = adjust_address(op0, DImode, 4);
#endif
				return "ld [%0],%1\n\tld [%2],%1 ;12";
			}
	}

	abort_with_error("Bad Double Move");
	return 0;
}

/****************************************
 
****************************************/

static void mapip_asm_out_constructor (rtx symbol, int priority ATTRIBUTE_UNUSED)
{
  fprintf (asm_out_file, "\t.ctor ");
  assemble_name (asm_out_file, XSTR (symbol, 0));
  fprintf (asm_out_file, "\n");
}

/****************************************
 
****************************************/

static void mapip_asm_out_destructor (rtx symbol, int priority ATTRIBUTE_UNUSED)
{
  fprintf (asm_out_file, "\t.dtor ");
  assemble_name (asm_out_file, XSTR (symbol, 0));
  fprintf (asm_out_file, "\n");
}

/****************************************
 
****************************************/

bool mapip_ok_for_sibcall (tree decl, tree exp)
{
	return 0;
}

/****************************************
 
****************************************/

static bool mapip_rtx_costs (rtx x, int code, int outer_code, int *total)
{
  switch (code) {
  case NOT:
  case NEG:
    *total = COSTS_N_INSNS (2);
	return true;
  case XOR:
  case IOR:
  case AND:
  case PLUS:
  case MINUS:
  case ASHIFT:
  case ASHIFTRT:
  case LSHIFTRT:
  case MULT:
    {
      rtx op = XEXP (x, 1);
      if (GET_CODE (op) == CONST_INT)
      {
		*total = COSTS_N_INSNS (2);
		return true;
	  }
      *total = COSTS_N_INSNS (1);
	  return true;
    }
  }
  return false;
}

/****************************************
 			Register enums
****************************************/

enum
{
	REG_zero = 0,
	REG_sp,
	REG_rt,
	REG_fr,
	REG_d0,
	REG_d1,
	REG_d2,
	REG_d3,
	REG_d4,
	REG_d5,
	REG_d6,
	REG_d7,
	REG_i0,
	REG_i1,
	REG_i2,
	REG_i3,

	REG_r0,
	REG_r1,
	REG_r2,
	REG_r3,
	REG_r4,
	REG_r5,
	REG_r6,
	REG_r7,
	REG_r8,
	REG_r9,
	REG_r10,
	REG_r11,
	REG_r12,
	REG_r13,
	REG_r14,
	REG_r15
};

/****************************************
This allows us to scale the register set
****************************************/

int args_in_regs = 4;

int mapip_regno_mode_ok(unsigned int regno, enum machine_mode mode)
{
	args_in_regs = 4;

	if (!mapip_archscale_string)
	{
		/* 32 Register mode */
		return 1;
	}

	if (strcmp(mapip_archscale_string,"full") == 0)
	{
		/* 32 Register mode */
		return 1;
	}
	
	if (strcmp(mapip_archscale_string,"tiny") == 0)
	{
		/* 5 Register mode - sp, rt, fr, r14, r15 */

		args_in_regs = 0;

		if ((regno >= REG_i0) && (regno <= REG_i3))
			return 0;
						
		if ((regno >= REG_r0) && (regno <= REG_r13))
			return 0;

		if ((regno >= REG_d0) && (regno <= REG_d7))
			return 0;

		return 1;
	}

	if (strcmp(mapip_archscale_string,"mini") == 0)
	{
		/* 7 Register mode - sp, rt, i0, i1, fr, r14, r15 */

		args_in_regs = 2;

		if ((regno >= REG_i2) && (regno <= REG_i3))
			return 0;
						
		if ((regno >= REG_r0) && (regno <= REG_r13))
			return 0;

		if ((regno >= REG_d0) && (regno <= REG_d7))
			return 0;

		return 1;
	}

	if (strcmp(mapip_archscale_string,"compact") == 0)
	{
		/* 9 Register mode - sp, rt, fr, i0, i1, i2, i3, r14, r15 */

		args_in_regs = 4;
						
		if ((regno >= REG_r0) && (regno <= REG_r13))
			return 0;

		if ((regno >= REG_d0) && (regno <= REG_d7))
			return 0;

		return 1;
	}

	if (strcmp(mapip_archscale_string,"half") == 0)
	{
		/* 16 Register mode - sp, rt, fr, d0, d1, d2, d3, d4, d5, i0, i1, i2, i3, r14, r15 */

		args_in_regs = 4;
				
		if ((regno >= REG_r0) && (regno <= REG_r13))
			return 0;

		if ((regno >= REG_d6) && (regno <= REG_d7))
			return 0;

		return 1;
	}

	/* Bad mode */

	error("unknown scaling mode, not supported !!\n");
	return 0;
}

/****************************************
   This allows us to scale parameters
****************************************/

int mapip_max_args_in_regs()
{
	return args_in_regs;
}

/****************************************
   This allows us to scale parameters
****************************************/

int mapip_get_return_regs()
{
	return R0_REGNUM;
}

/****************************************
		Deal with trampolines
****************************************/

void mapip_initialize_trampoline(rtx tramp, rtx fnaddr, rtx cxt)
{
	error("Nested functions/trampolines are not supported !!\n");
}
