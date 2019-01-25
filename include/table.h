/*
File name:	table.h
Compiler:	MS Visual Studio 2015
Author:		Nathan Festoso - 040825359
Author:		Billy Saint-Fort - 040831048
Course:		CST 8152 - Compilers, Lab Section: 12
Assignment:	2
Date:		Oct 30, 2017
Professor:	Sv. Ranev
Purpose:	Contains the transition table for the scanner
Function list: -
*/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*
*    Source end-of-file (SEOF) sentinel symbol
*    '\0' or only one of the folowing constants: 255, 0xFF , EOF
*/

/*  Single-lexeme tokens processed separately one by one
*  in the token-driven part of the scanner
*  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
*		space
*  !!comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', '#' ,
*  .AND., .OR. , SEOF, 'wrong symbol',
*/

#define ES 12 /* Error state */
#define IS -1 /* Inavalid state */

#define NOT_KW -1 /* Not a keyword */

/* State transition table definition */
#define TABLE_COLUMNS 8

/*
Transition Table - type of states defined in separate table
Columns: [a-w,y-z,G-Z] , [A-F] , [x] , [1-9] , [0] , [.] , [$] , other
*/
int  st_table[][TABLE_COLUMNS] = {
	{ 1, 1, 1, 4, 6, ES,ES,ES },
	{ 1, 1, 1, 1, 1, 2, 3, 2 },
	{ IS, IS, IS, IS, IS, IS, IS, IS },
	{ IS, IS, IS, IS, IS, IS, IS, IS },
	{ ES, ES, ES, 4, 4, 7, 5, 5 },
	{ IS, IS, IS, IS, IS, IS, IS, IS },
	{ ES, ES, 9, ES, 5, 7, ES, 5 },
	{ 8,8,8,7,7,8,8,8 },
	{ IS, IS, IS, IS, IS, IS, IS, IS },
	{ ES, 10, ES, 10, 10, 13, ES, 13 },
	{ ES, 10, ES, 10, 10, 11, 11, 11 },
	{ IS, IS, IS, IS, IS, IS, IS, IS },
	{ IS, IS, IS, IS, IS, IS, IS, IS },
	{ IS, IS, IS, IS, IS, IS, IS, IS }
};

/* Accepting state table definition */
#define ASWR     1  /* accepting state with retract */
#define ASNR     2  /* accepting state with no retract */
#define NOAS     3  /* not accepting state */

int as_table[] = {
	NOAS,
	NOAS,
	ASWR,
	ASNR,
	NOAS,
	ASWR,
	NOAS,
	NOAS,
	ASWR,
	NOAS,
	NOAS,
	ASWR,
	ASNR,
	ASWR
};

/* Accepting action function declarations */
Token aa_func02(char *lexeme); /*VID*/
Token aa_func03(char *lexeme); /*SVID*/
Token aa_func05(char *lexeme); /*Floating-point literal*/
Token aa_func08(char *lexeme); /*Integer decimal literal*/
Token aa_func11(char *lexeme); /*Integer Hex literal*/
Token aa_func12(char *lexeme); /*Error*/
Token aa_func13(char *lexeme); /*Error with retract */

							   /*
							   Defining a new type: pointer to function (of one char * argument)
							   returning Token
							   */
typedef Token(*PTR_AAF)(char *lexeme);

/* Accepting function (action) callback table (array) definition */
PTR_AAF aa_table[] = {
	NULL,
	NULL,
	aa_func02,
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	NULL,
	aa_func11,
	aa_func12,
	aa_func13
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  10

char * kw_table[] =
{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
};

#endif
