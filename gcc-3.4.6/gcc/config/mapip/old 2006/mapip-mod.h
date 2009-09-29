/* Definitions to use the mapip module object format.
   Copyright (C) 2001 Free Software Foundation, Inc.
   Contributed by Anders Norlander <anorland@synergenix.se>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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
#include "../dbxelf.h"

#ifdef GCC_303
#undef ASM_IDENTIFY_GCC
#define ASM_IDENTIFY_GCC(FILE) fputs ("# gcc2_compiled.:\n", FILE)
#endif
