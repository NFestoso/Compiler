/*********************************************************
File name:	parser.h
Compiler:	MS Visual Studio 2015
Author:		Nathan Festoso - 040825359
Billy Saint-Fort - 040831048
Course:		CST 8152 - Compilers, Lab Section: 10
Assignment:	3
Date:		Jan 2, 2018
Professor:	Sv. Ranev
Purpose:	Semantic analysis for PLATYPUS language
*********************************************************/

#include "buffer.h"
#include "token.h"

#define NO_ATTR -1
#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

Token lookahead; /*Current input token*/
Buffer * sc_buf; /*Temporary lexeme buffer*/

int synerrno; /*Error counter*/

/*Parser Prototypes*/
void parser(Buffer * in_buf); /*Begin parsing input*/
void match(int pr_token_code, int pr_token_attribute); /*Matches current token with expected token*/
void syn_eh(int sync_token_code); /*Error handler*/
void syn_printe(); /*Error printer*/
void gen_incode(char str[]); /*Prints message when production is recognized*/

/*Grammar Prototypes*/
void program(void);				/*Program*/
void statement(void);			/*Single statement*/
void statements(void);			/*Multiple statements*/
void statements_p(void);		/*Primary statements*/
void opt_statements(void);		/*Optional statements*/
void assign_statement(void);	/*Assignment statement*/
void select_statement(void);	/*Selection statement (IF)*/
void it_statement(void);		/*Iteration statement (WHILE)*/
void in_statement(void);		/*Input statement (READ)*/
void out_statement(void);		/*Output statement (WRITE)*/
void full_list(void);			/*Full VID list*/
void vid(void);					/*Variable Identifier*/
void var_list(void);			/*Variable list*/
void var_list_p(void);			/*primary variable list*/
void assign_exp(void);			/*Assignment expression*/
void arith_exp(void);			/*Arithmetic expression*/
void primary_arith_exp(void);	/*Primary arithmetic expression*/
void unary_exp(void);			/*Unary expression*/
void add_arith_exp(void);		/*Additive arithmetic expression*/
void add_arith_exp_p(void);
void multi_arith_exp(void);		/*Multiplicative arithmetic expression*/
void multi_arith_exp_p(void);
void string_exp(void);			/*String expression*/
void string_exp_p(void);
void primary_string_exp(void);	/*Primary string expression*/
void conditional_exp(void);		/*Conditional expression*/
void logical_OR_exp(void);		/* .OR. */
void logical_OR_exp_p(void);
void logical_AND_exp(void);		/* .AND. */
void logical_AND_exp_p(void);
void relation_exp(void);		/*Relational expression*/
void primary_arith_relation_exp(void);	/*Primary arithmetic relational expression*/
void primary_string_relation_exp(void);	/*Primary string relational expression*/
void add_arith_op(void);		/*Additive operators*/
void multi_arith_op(void);		/*Multiplicative operators*/
void relation_op(void);			/*Relational operator*/
void pre_condition(void);		/*Pre-condition*/
