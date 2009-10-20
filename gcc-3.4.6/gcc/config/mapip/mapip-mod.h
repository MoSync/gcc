/* Definitions to use the mapip module object forma */
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

#define OBJECT_FORMAT_MAPIPMOD

/*
extern const char *mapipmod_version;

#define SUBTARGET_OPTIONS \
  { "version=", &mapipmod_version, "Specify minimum API version required." }

#define SUBTARGET_LINK_SPEC \
 "%{mversion=*:--api-version=%*}"
*/

/* Defined to avoid having __main called from main */
#define HAS_INIT_SECTION

#define CTORS_SECTION_ASM_OP	".section\t.ctors,a\n"
#define DTORS_SECTION_ASM_OP	".section\t.dtors,a\n"

#ifdef GCC_303
#define EXTRA_SECTIONS in_ctors, in_dtors

#define EXTRA_SECTION_FUNCTIONS \
  CTORS_SECTION_FUNCTION	\
  DTORS_SECTION_FUNCTION

#define CTORS_SECTION_FUNCTION						\
void									\
ctors_section ()							\
{									\
  if (in_section != in_ctors)						\
    {									\
      fputs (CTORS_SECTION_ASM_OP, asm_out_file);			\
      in_section = in_ctors;						\
    }									\
}

#define DTORS_SECTION_FUNCTION						\
void									\
dtors_section ()							\
{									\
  if (in_section != in_dtors)						\
    {									\
      fputs (DTORS_SECTION_ASM_OP, asm_out_file);			\
      in_section = in_dtors;						\
    }									\
}

#define ASM_OUTPUT_CONSTRUCTOR(FILE, NAME)				\
{									\
  ctors_section ();							\
  fprintf (FILE, "\t.word\t");						\
  assemble_name (FILE, NAME);						\
  fprintf (FILE, "\n");							\
}

#define ASM_OUTPUT_DESTRUCTOR(FILE, NAME)				\
{									\
  dtors_section ();                   					\
  fprintf (FILE, "\t.word\t");						\
  assemble_name (FILE, NAME);              				\
  fprintf (FILE, "\n");							\
}
#endif

#define TARGET_ASM_CONSTRUCTOR mapip_asm_out_constructor
#define TARGET_ASM_DESTRUCTOR mapip_asm_out_destructor

#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME, RELOC)		\
{									\
  const char *mode;							\
  if (DECL && TREE_CODE (DECL) == FUNCTION_DECL)			\
    mode = "ax";							\
  else if (DECL && DECL_READONLY_SECTION (DECL, RELOC))			\
    mode = "ar";							\
  else									\
    mode = "a";								\
  fprintf (FILE, ".section\t%s,%s\n", NAME, mode);			\
}

/* We're not ELF, but we do use the same stabs definitions */

/* Output DBX (stabs) debugging information if doing -gstabs.  */

#undef  DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

/* Make LBRAC and RBRAC addresses relative to the start of the
   function.  The native Solaris stabs debugging format works this
   way, gdb expects it, and it reduces the number of relocation
   entries...  */

#undef  DBX_BLOCKS_FUNCTION_RELATIVE
#define DBX_BLOCKS_FUNCTION_RELATIVE 1

/* ... but, to make this work, functions must appear prior to line info.  */

#undef  DBX_FUNCTION_FIRST
#define DBX_FUNCTION_FIRST

/* When generating stabs debugging, use N_BINCL entries.  */

#undef  DBX_USE_BINCL
#define DBX_USE_BINCL

/* There is no limit to the length of stabs strings.  */

#ifndef DBX_CONTIN_LENGTH
#define DBX_CONTIN_LENGTH 0
#endif

/* Like block addresses, stabs line numbers are relative to the
   current function.  */

/*
#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(FILE, LINE)												\
do																						\
  {																						\
    static int sym_lineno = 1;															\
    char temp[256];																		\
    ASM_GENERATE_INTERNAL_LABEL (temp, "LM", sym_lineno);								\
    fprintf (FILE, "\t.stabn 68,0,%d,", LINE);											\
    assemble_name (FILE, temp);															\
    putc ('-', FILE);																	\
    assemble_name (FILE, XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0));			\
    putc ('\n', FILE);																	\
    ASM_OUTPUT_INTERNAL_LABEL (FILE, "LM", sym_lineno);									\
    sym_lineno += 1;																	\
  }																						\
while (0)
*/

#undef  ASM_OUTPUT_SOURCE_LINE	
#define ASM_OUTPUT_SOURCE_LINE(FILE, LINE, COUNTER)												\
do																						\
  {																						\
    fprintf (FILE, "\t.line %d\n", LINE);													\
  }																						\
while (0)


/* Generate a blank trailing N_SO to mark the end of the .o file, since
   we can't depend upon the linker to mark .o file boundaries with
   embedded stabs.  */

#undef  DBX_OUTPUT_MAIN_SOURCE_FILE_END
#define DBX_OUTPUT_MAIN_SOURCE_FILE_END(FILE, FILENAME)			\
  asm_fprintf (FILE,"\t.text\n\t.stabs \"\",%d,0,0,%LLetext\n%LLetext:\n", N_SO)


/* NEW out source dir */

#undef DBX_OUTPUT_MAIN_SOURCE_DIRECTORY
#define DBX_OUTPUT_MAIN_SOURCE_DIRECTORY(file,name)		\
	fprintf (file, "\t.sourcedir '%s'\n", name)

#undef DBX_OUTPUT_MAIN_SOURCE_FILENAME
#define DBX_OUTPUT_MAIN_SOURCE_FILENAME(file,name)		\
	fprintf (file, "\t.sourcefile '%s'\n", name)

/* Define this macro to say how to output to STREAM the debugging information
   for the start of a scope level for variable names.  The argument NAME is the
   name of an assembler symbol (for use with `assemble_name') whose value is
   the address where the scope begins.  */

/*
#undef DBX_OUTPUT_LBRAC
#define DBX_OUTPUT_LBRAC(file,name)	\
  if (depth > 1) {			\
    fprintf (file, ".lbrac \"\",");	\
    assemble_name (file, name);		\
    fprintf (file, ",0x%x,0,%d\n", N_LBRAC, depth); }
*/

/*
#undef DBX_OUTPUT_RBRAC
#define DBX_OUTPUT_RBRAC(file,name)	\
  if (depth > 1) {			\
    fprintf (file, ".rbrac \"\",");	\
    assemble_name (file, name);		\
    fprintf (file, ",0x%x,0,%d\n", N_RBRAC, depth); }
*/

/*
#undef DBX_OUTPUT_CATCH
#define DBX_OUTPUT_CATCH (asmfile, decl, buf) do {} while(0)
*/

/*---------------------------------------------------------------------*/

/* Set Identity */

#ifdef GCC_303
#undef ASM_IDENTIFY_GCC
#define ASM_IDENTIFY_GCC(FILE) fputs ("# gcc2_compiled.:\n", FILE)
#endif
