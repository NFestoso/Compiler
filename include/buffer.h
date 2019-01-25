/* REPLACE the file header below with your file header (see CST8152_ASSAMG.pdf for details).
* File Name: buffer.h
* Version: 1.17.2
* Author: S^R
* Date: 4 September 2017
* Preprocessor directives, type declarations and prototypes necessary for buffer implementation
* as required for CST8152-Assignment #1.
* The file is not completed.
* You must add your function declarations (prototypes).
* You must also add your constant definitions and macros,if any.
*/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

							/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */
														   /* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

														   /* constant definitions */
														   /* You may add your own constant definitions here */
#define FIX_SIZE_MODE 0       /*Fix mode*/
#define ADDITIVE_MODE 1      /*Additive mode*/
#define MULTIPLICATIVE_MODE -1 /*Multiplicative mode*/
#define RT_FAIL1 -1         /* fail return value */
#define RT_FAIL2 -2         /* fail return value */
#define LOAD_FAIL -2       /* load fail error */
#define SET_R_FLAG 1       /* realloc flag set value */
#define ZERO 0            /*0 return value*/
#define ONE 1             /*1 return value*/

#ifdef B_FULL /*If the user defined the macro than use the macro b_isfull instead of the function b_isfull()*/
#define b_isfull(pBD) ((pBD != NULL) ? ((pBD->addc_offset == pBD->capacity) ? 1 : 0) : -1) /* Used a conditional operator in order to evaluate if the buffer is full*/
#endif 


														   /* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  r_flag;     /* reallocation flag */
	char  mode;       /* operational mode indicator*/
	int   eob;       /* end-of-buffer flag */
} Buffer, *pBuffer;


/* function declarations */
Buffer *b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer * const pBD);
void b_free(Buffer * const pBD);

#ifndef B_FULL /*if the macro B_FUll is not defined by the user, then undefine it and declare a function prototype instead*/
#undef b_isfull
int b_isfull(Buffer * const pBD);
#endif
short b_limit(Buffer* const pBD);
short b_capacity(Buffer* const pBD);
short b_mark(Buffer * const pBD, short mark);
int b_mode(Buffer * const pBD);
size_t b_incfactor(Buffer* const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
int b_eob(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_print(Buffer * const pBD);
Buffer * b_compact(Buffer * const pBD, char symbol);
char b_rflag(Buffer * const pBD);
short b_reset(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_getcoffset(Buffer * const pBD);
int b_rewind(Buffer * const pBD);
char *b_location(Buffer * const pBD, short loc_offset);

/*
Place your function declarations here.
Do not include the function header comments here.
Place them in the buffer.c file
*/

#endif 
