/* File name: platy.c
 * Purpose:This is the main program for Assignment#3 - Platypus Parser
 *  CST8152 - Compilers
 *  Version: 1.17.2
 *  Author: Svillen Ranev
 *  Date: 25 November 2017
 */ 

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in other compilers  projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <stdlib.h> /* Constants for calls to exit()*/

#include <string.h>
#include <stdarg.h>

#include "buffer.h"
#include "token.h"

/* Input buffer parameters */
#define INIT_CAPACITY 200 /* initial buffer capacity */
#define INC_FACTOR 15       /* increment factor */
/* String Literal Table parameters */
#define STR_INIT_CAPACITY 100 /* initial string literal table capacity */
#define STR_CAPACITY_INC  50   /* initial string literal table capacity inc */

/*check for ANSI C compliancy */
#define ANSI_C 0
#if defined(__STDC__)
#undef ANSI_C
#define ANSI_C 1
#endif

/* Global objects - variables */
static Buffer *sc_buf; /* pointer to input (source) buffer */
Buffer * str_LTBL; /* this buffer implements String Literal Table */
                  /* it is used as a repository for string literals */
int scerrnum;     /* run-time error number = 0 by default (ANSI) */
/*external objects */
extern int synerrno /* number of syntax errors reported by the parser */;
extern int line; /* source code line number - defined in scanner.c */
/* function declarations (prototypes) */
extern void parser(Buffer * sc_buf);
/* For testing purposes */
 extern void scanner_init(Buffer * sc_buf);
/* extern Token malpar_next_token(Buffer * sc_buf);*/

void err_printf(char *fmt, ...);
void display (Buffer *ptrBuffer); 
long get_filesize(char *fname);
void garbage_collect(void);


/*  main function takes a PLATYPUS source file as
 *  an argument at the command line.
 *  usage: parser source_file_name
 */    
int main(int argc, char ** argv){

	FILE *fi;       /* input file handle */	
        int loadsize = 0; /*the size of the file loaded in the buffer */
        int ansi_c = !ANSI_C; /* ANSI C flag */
/* Check if the compiler option is set to compile ANSI C */
/* __DATE__, __TIME__, __LINE__, __FILE__, __STDC__ are predefined preprocessor macros*/
  if(ansi_c){
    err_printf("Date: %s  Time: %s",__DATE__, __TIME__);
    err_printf("ERROR: Compiler is not ANSI C compliant!\n");
    exit(1);
  }

/*check for correct arrguments - source file name */
      if (argc <= 1){
/* __DATE__, __TIME__, __LINE__, __FILE__ are predefined preprocessor macros*/
       err_printf("Date: %s  Time: %s",__DATE__, __TIME__);
       err_printf("Runtime error at line %d in file %s", __LINE__, __FILE__);
       err_printf("%s%s%s",argv[0],": ","Missing source file name.");
       err_printf("%s%s%s","Usage: ", "parser", "  source_file_name");
        exit(EXIT_FAILURE);
	}	

/* create a source code input buffer - multiplicative mode */	
	sc_buf = b_allocate(INIT_CAPACITY,INC_FACTOR,'m');
	if (sc_buf == NULL){
	  err_printf("%s%s%s",argv[0],": ","Could not create source buffer");
	  exit(EXIT_FAILURE);
	}


/*open source file */
	if ((fi = fopen(argv[1],"r")) == NULL){
		err_printf("%s%s%s%s",argv[0],": ", "Cannot open file: ",argv[1]);
		exit (1);
	}
/* load source file into input buffer  */
     printf("Reading file %s ....Please wait\n",argv[1]);
     loadsize = b_load (fi,sc_buf);
     if(loadsize == RT_FAIL1)
       err_printf("%s%s%s",argv[0],": ","Error in loading buffer.");

/* close source file */	
 	fclose(fi);
/*find the size of the file  */
    if (loadsize == LOAD_FAIL){
     printf("The input file %s %s\n", argv[1],"is not completely loaded.");
     printf("Input file size: %ld\n", get_filesize(argv[1]));
    }
/* Add SEOF (EOF) to input buffer and display the source buffer */
      if(b_compact(sc_buf,EOF)){
         display(sc_buf);
      }

/* create string Literal Table */	
  str_LTBL = b_allocate(INIT_CAPACITY,INC_FACTOR,'a');
	if (str_LTBL == NULL){
		err_printf("%s%s%s",argv[0],": ","Could not create string buffer");
		exit(EXIT_FAILURE);
	}

/*registrer exit function */	
 atexit(garbage_collect);
	
/*Testbed for buffer, scanner,symbol table and parser*/

/* Initialize scanner  */
	scanner_init(sc_buf);

/* Start parsing */
	printf("\nParsing the source file...\n\n");
	
        parser(sc_buf);
             
	return (EXIT_SUCCESS); /* same effect as exit(0) */
}/*end of main */

/* Error printing function with variable number of arguments
 */
void err_printf( char *fmt, ... ){

  va_list ap;
  va_start(ap, fmt);

  (void)vfprintf(stderr, fmt, ap);
	va_end(ap);

  /* Move to new line */
  if( strchr(fmt,'\n') == NULL )
	  fprintf(stderr,"\n");
}

/* The function return the size of an open file 
 */
long get_filesize(char  *fname){
   FILE *input;
   long flength;
   input = fopen(fname, "r");
   if(input == NULL){
      err_printf("%s%s","Cannot open file: ",fname);
      return 0;
   }  
   fseek(input, 0L, SEEK_END);
   flength = ftell(input);   
   fclose(input);
   return flength;
}

/* The function display buffer contents 
 */
void display (Buffer *ptrBuffer){
  printf("\nPrinting input buffer parameters:\n\n");
  printf("The capacity of the buffer is:  %d\n",b_capacity(ptrBuffer));
  printf("The current size of the buffer is:  %d\n",b_limit(ptrBuffer)); 
  printf("\nPrinting input buffer contents:\n\n");
  b_rewind(ptrBuffer);
  b_print(ptrBuffer);
}

/* The function frees all dynamically allocated memory. 
   This function is always called
   despite how the program terminates - normally or abnormally. 
*/
void garbage_collect(void){
  if(synerrno)
    printf("\nSyntax errors: %d\n",synerrno);
  printf("\nCollecting garbage...\n");
  b_free(sc_buf);
  b_free(str_LTBL);  
}


