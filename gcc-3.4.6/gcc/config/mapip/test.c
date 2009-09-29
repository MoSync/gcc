//****************************************************************************
//							  Symbol handler
//****************************************************************************

//****************************************************************************************
//								Header file for Symbols
//****************************************************************************************

#define SYMMAX 5000				// Maximum symbols in table
#define LABMAX 32				// Maximum symbol length
#define NAMESPACE 64000			// Maximum memory for name storage

//****************************************
//	  Variable cast types and flags
//****************************************

enum
{
	CAST_unsign_flag	= 0x8000,		// Set if type is signed
	CAST_unsign_mask	= 0x7fff,		

	CAST_ptr_flag		= 0x4000,		// Set if type is a ptr
	CAST_ptr_mask		= 0xbfff,

	CAST_const_flag		= 0x2000,
	CAST_const_mask		= 0xdfff,

	CAST_static_flag	= 0x1000,
	CAST_static_mask	= 0xefff ,

	CAST_volatile_flag	= 0x0800,
	CAST_volatile_mask	= 0xf7ff,

	CAST_enum_flag		= 0x0400,
	CAST_enum_mask		= 0xfbff,

	TYPE_mask			= 0x00ff,		// Mask for types

	TYPE_empty			= 0,			// CAST_types (bottom 8 bits of type) 
	TYPE_void			= 1,
	TYPE_char			= 2,
	TYPE_int			= 3,
	TYPE_long			= 4,
	TYPE_struct 		= 5,
	TYPE_function		= 6,
	TYPE_multi			= 7
};

//****************************************
//			Cast token codes
//****************************************

enum
{
	TOKEN_signed		= 1,
	TOKEN_unsigned,
	TOKEN_static,
	TOKEN_const,
	TOKEN_volatile,
	TOKEN_enum,

	TOKEN_void,
	TOKEN_char,
	TOKEN_int,
	TOKEN_short,
	TOKEN_long,
	TOKEN_struct
};

//****************************************
//    Section types in symbol table
//****************************************

enum
{
	section_Empty		= 0,				// Empty section
	section_Keyword,
	section_Cast,
	section_Types,
	section_Function,
	section_Macro,
	section_Storage,

	section_Global = 1000,				// All global sections come after this
	section_Global_Start= 1000,				// All global sections come after this
	section_Global_End = 1999,

	section_Local = 3000,				// All local sections come after this
	section_Local_Start	= 3000,				// All local sections come after this
	section_Local_End	= 3999

};

//****************************************
//		  Symbol table structure
//****************************************

typedef struct
{
			char *name;			// Symbol name in Symbol buffer
			char len;			// Length of actual name

			int	section;		// Symbol table section
			int type;			// Symbol type (see CAST_... bitfield settings above)
			int init;			// Initialised ?
			int used;			// used ?

			void *data;
} SYMBOL;


//****************************************
//	   Struct for a parameter entry
//****************************************

typedef struct _PARSE_INFO
{
	// Type declaration info
	
	SYMBOL *typesym;				// Pointer to Type in symbol table
	int type;						// the type number
	int typeflag;					// flags for type (Const etc)
	int indlevel;					// Level of indirections

	//	name 
	
	char *identifier;
	
	// Array info

	int arraywidth;
	int *arrayinfo;

	// Functions
	
	int function;
	int paramcount;
	int prototype;
	struct _PARSE_INFO *paraminfo;

	// Initialization

	int init;						// Is anything initialized
	int value;						// The value (if initdata == 0)
	char *initdata;					// init data
	int init_entries;				// Amount of string entries
	int init_memsize;				// Memory used by initdata

} PARSE_INFO;

//****************************************************************************************
//								Header file for Evaluator
//****************************************************************************************

//****************************************
//          Evaluator Structure
//****************************************

typedef struct
{
			SYMBOL *sym;			// Pointer to original symbol
			long	value;			// Constant value or address variable string
			char	num;			// Numeric  = 1:  Not Numeric = 0
			int		type;			// copy of SYMBOL.flags (used in type conversion)
			//int	rtype;			// right type
			//char	ind;			// Indirection level
} EVAL;

//****************************************
//	   Struct for a parameter entry
//****************************************
/*
typedef struct
{
	SYMBOL *typesym;				// Pointer to Type in symbol table
	int typemix;					// Addisional flags for type
	int typeflag;					// Addisional flags for type
	int indlevel;					// Level of indirections

} PARAM;
*/
//****************************************
//		  Some useful defines
//****************************************

//#define bypass()			tptr += strlen((char *)tptr) + 1

#define iswhite(c)			(c == ' ' || c == '\t' || c == 0x0d || c == 0x0a )
#define iscsymf(c)			(isalpha(c) || c == '_')
#define iscsym(c)			(isalnum(c) || c == '_')

//#define TwoChar(c1,c2)		*ptr == c1 && *(ptr+1) == c2

#define NULL 0

//****************************************
//				Prototypes
//****************************************

/* ProtoBuilder(tm) AutoPrototype Private file */
/* Copyright A.R.Hartley 1996 v2.1W*/

/* Prototypes for 'PClib.c' */

char * NewPtr(long len);
char * NewPtrClear(long len);
char * NewPtrOrFail(long len);
char * ReallocOrFail(char *mem, long len, long extralen);
void DisposePtr(char *mem);

/* Prototypes for 'main.c' */

void main();
char * OpenFile(char *FileName);
void FreeFile(char *fptr);

/* Prototypes for 'parse.c' */

void CompileFunction();
void Compile();
void Statements();
int CheckLabel();
void MultiStatement();
void Statement();

/* Prototypes for 'Eval.c' */

void GetOper();
int Indexer(EVAL *Part1);
void InitExpMain();
long EvalNumeric(void);
void Expression(EVAL *Part1);
void assign(EVAL *Part1);
void logor(EVAL *Part1);
void logand(EVAL *Part1);
void xor(EVAL *Part1);
void or(EVAL *Part1);
void and(EVAL *Part1);
void GreaterLess(EVAL *Part1);
void shifts(EVAL *Part1);
void plus(EVAL *Part1);
void mult(EVAL *Part1);
void unary(EVAL *Part1);
void variable(EVAL *Part1);
void GetIdentifier(EVAL *Part1);
void primary(EVAL *Part1);

/* Prototypes for 'Symbols.c' */

int InitSymbolTable(void);
void CloseSymbolTable(void);
SYMBOL * FreeSymbol(void);
SYMBOL * FindSymbols(char *string,int sectionStart,int sectionEnd);
SYMBOL * StoreSymbol(SYMBOL *NewSym,char *string);
int DelSymbol(SYMBOL *ThisSym);
void PushLocalPosition();
void PopLocalPosition();
SYMBOL * Declare(char *ThisName,int Init,int Type, void *data);
SYMBOL * DeclareFunction(char *ThisName,void *data, int *found);
SYMBOL * DeclareType(char *ThisName,int Type, void *data);

/* Prototypes for 'Tokens.c' */

int	 Token(char *token);
void SkipToken(char *token);
void NeedToken(char *token);
int NextToken(char *token);
void SkipLine();
void SkipComment();
void SkipWhiteSpace();
void ParseExp();
void GetExpString();
long NumericExp();
void PtrNumericExp(char *expptr);
void GetName();
void GetFileName();
long GetEscString();
void SkipQuote(char QToken);
void SkipPair(char LToken,char RToken);
int HexDig(char digit);
char * GetSym();
char * GetStr();
long GetHexNum(register int digits);
long GetDecNum(int digits);
long GetEscCode();
long GetNum();
char * MakeTemp();
char * UseTemp(int num);
char * LastTemp();
void CopyLastTemp0();
void ExpandSym(int size);

/* Prototypes for 'OutCode.c' */

void EnterFrame(long size);
void ExitFrame(long size);
long TempVar();
void WriteTempVar(long templocal);
void Goto(long templocal);
void GotoName(char *name);
void GotoOnFalse(long templocal);
void GotoOnTrue(long templocal);
void SetBreak(long point);
void ReleaseBreak();
void GotoBreak();
void SetContinue(long point);
void ReleaseContinue();
void GotoContinue();

/* Prototypes for 'Output.c' */

void Error(char *Template, ...);
void Warning(char *Template, ...);
void Report(char *Template, ...);
void OutEval(char *Template, ...);

/* Prototypes for 'Declare.c' */

int ParseDecl(PARSE_INFO *ThisInfo);
void PartialParseDecl(PARSE_INFO *OldInfo,PARSE_INFO *ThisInfo);
int ParseStructDecl(PARSE_INFO *ThisInfo);
void PartialParseStructDecl(PARSE_INFO *OldInfo,PARSE_INFO *ThisInfo);
void CheckProtoType(PARSE_INFO *ThisInfo, PARSE_INFO *SymInfo);
int GetGlobalDeclarations();
void DeclareStruct();

/* Prototypes for 'DeclLib.c' */

void InitParseInfo(PARSE_INFO *ThisInfo);
void DisposeParseInfo(PARSE_INFO *ThisInfo);
void AddInitData(PARSE_INFO *ThisInfo, char *str);
long GetTypeInfo(PARSE_INFO *ThisInfo);
void GetPtrInfo(PARSE_INFO *ThisInfo);
void GetIdentInfo(PARSE_INFO *ThisInfo);
void GetArrayInfo(PARSE_INFO *ThisInfo);
void GetFuntionInfo(PARSE_INFO *ThisInfo);
void GetInitInfo(PARSE_INFO *ThisInfo);
void GetStructData(PARSE_INFO *ThisInfo);
void ShowParseInfo(PARSE_INFO *ThisInfo);

//****************************************
//				Globals
//****************************************

extern long Global;
extern char	Name[];

SYMBOL			*SymTab;			// Ptr to Symbol data
long			SymbolCount;		// Amount of Symbols
long			LocalPos;			// Position of locals

//****************************************
//	Initialises Symbol table to null.
//****************************************

int InitSymbolTable(void)
{
	register SYMBOL *Sym;
	register long n;

	SymTab = (SYMBOL *) NewPtr( (long) (sizeof(SYMBOL) * SYMMAX));

	if (SymTab == NULL)
	{
		printf("Error : Could not allocate symbol space\n");
		return NULL;
	}
	
	Sym = SymTab;
	n = SYMMAX;

	do
	{
		Sym->type = 0;	
		Sym->name = NULL;	
		Sym->data = NULL;	
		Sym++;
	}
	while(--n);

	SymbolCount = 0;
	LocalPos = section_Local_Start;
	
	return 1;
}

//****************************************
//	Initialises Symbol table to null.
//****************************************

void CloseSymbolTable(void)
{
	DisposePtr((char *) SymTab);
}

//****************************************
//		 Find a free Symbol slot
//****************************************

SYMBOL * FreeSymbol(void)
{
	long	n;
	SYMBOL *Sym = SymTab;

	n = SYMMAX;
	
	do
	{
		if (Sym->type == 0 )
		{
			return Sym;
		}

		Sym++;
	}
	while(--n);

	return NULL;
}

//****************************************
//  *Symbol	FindSymbols (Ptr string)
//
//	Trys to find the Symbol at *string 
// (ASCZ terminated) returns a Ptr to a
//  Symbol containing the Symbol data.
// if NULL returned then the Symbol
// was not found.
//****************************************

SYMBOL * FindSymbols(char *string,int sectionStart,int sectionEnd)
{
	long	n,len;
	SYMBOL *Sym = SymTab;

	len = strlen(string);

	n=SYMMAX;
	do
	{
		if ( (Sym->type != 0)  &&
			 (Sym->len == len) &&
			 (Sym->section >= sectionStart) &&
			 (Sym->section <= sectionEnd) &&
			  (strcmp(string,Sym->name) == 0)
			)
		{
			return Sym;
		}

		Sym++;
	}
	while(--n);

	return NULL;
}

//****************************************
//*Symbol	StoreSymbol (Symbol NewSymbol)
//
//	Stores a new symbol in the Symbol table,
//	returns NULL if unsuccessful.
//
//     This storage system is Dynamic
//****************************************

SYMBOL * StoreSymbol(SYMBOL *NewSym,char *string)
{
	SYMBOL *Sym = FreeSymbol();
	char *ThisName;					// Location of PTR to name
	int StringLen;
	
	StringLen = strlen(string) + 1;	// Get length of string
	
	// if there was not Symbol space quit

	if (Sym == NULL)
			return NULL;

	// if there was no name space quit

	ThisName = (char *) NewPtr((long) StringLen);
	
	if (!ThisName)
			return NULL;

	// Copy the name to the NamesList

	memcpy(ThisName,string,StringLen);
	memcpy(Sym,NewSym,sizeof(SYMBOL));

	// Set the Symbol data ptr

	Sym->name = ThisName;
	Sym->len = StringLen-1;

	// Carry forward the names pointer

	return Sym;
}

//****************************************
//		   Delete a Symbol entry
//    int DelSymbol(SYMBOL *ThisSym)
//****************************************

int DelSymbol(SYMBOL *ThisSym)
{
	ThisSym->type = 0;								// Make entry empty

	//OutEval("------ Undeclare '%s'\n",(char *) ThisSym->name);

	if (ThisSym->name)
		DisposePtr((char *) ThisSym->name);			// Give back memory

//	if (ThisSym->data)
//		DisposePtr((char *) ThisSym->data);			// Give back memory

	return 1;										// Say o.k
}

//****************************************
//
//****************************************

void PushLocalPosition()
{
	LocalPos++;
}

//****************************************
//
//****************************************

void PopLocalPosition()
{
	register SYMBOL *Sym;
	register long n;

	Sym = SymTab;
	n = SYMMAX;

	do
	{
		if (Sym->type && Sym->section == LocalPos)
			DelSymbol(Sym);

		Sym++;
	}
	while(--n);

	LocalPos--;
}

//****************************************
//		  Declare an identifier
//****************************************

SYMBOL * Declare(char *ThisName,int Init,int Type, void *data)
{
	SYMBOL	sym;
	
	if (FindSymbols(ThisName,section_Local_Start,section_Local_End))
	{
		Error("Identifier '%s' redeclared",Name);
		exit(0);
	}

	if (Global)
		sym.section		= section_Global;		// Global decl
	else
		sym.section		= LocalPos;				// Local vars

	sym.type		= Type;
	sym.init		= Init;
	sym.used		= 0;
	sym.data		= data;
	
	OutEval("------ Declared var '%s'\n",ThisName);
	
	return	StoreSymbol(&sym,ThisName);
}

//****************************************
//		  Declare an function
//****************************************

SYMBOL * DeclareFunction(char *ThisName,void *data, int *found)
{
	SYMBOL	sym;
	SYMBOL	*ThisSym;


	ThisSym = FindSymbols(ThisName,section_Function,section_Function);
	
	if (ThisSym)
	{
		*found = 1;
		return ThisSym;
	}

	OutEval("------ Function Declared '%s'\n",ThisName);

	sym.section		= section_Function;
	sym.type		= TYPE_function;
	sym.data		= data;
	
	return	StoreSymbol(&sym,ThisName);
}

//****************************************
//			Declare a type
//****************************************

SYMBOL * DeclareType(char *ThisName,int Type, void *data)
{
	SYMBOL	sym;
	
	if (FindSymbols(ThisName,section_Types,section_Types))
	{
		Error("Type '%s' already exists",ThisName);
		exit(0);
	}

	OutEval("------ Type declare '%s'\n",ThisName);

	sym.section		= section_Types;
	sym.type		= Type;
	sym.data		= data;

	return	StoreSymbol(&sym,ThisName);
}


