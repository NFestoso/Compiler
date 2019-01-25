/*
File name:	scanner.c
Compiler:	MS Visual Studio 2015
Author:		Nathan Festoso - 040825359
Author:		Billy Saint-Fort - 040831048
Course:		CST 8152 - Compilers, Lab Section: 12
Assignment:	2
Date:		Oct 30, 2017
Professor:	Sv. Ranev
Purpose:	Reads a source program file and process a stream of tokens
Function list:	scanner_init(), Token malar_next_token(), get_next_state(), int char_class(),
Token aa_func02(), Token aa_func03(), Token aa_func05(), Token aa_func08(),
Token aa_func11(), Token aa_func12(), Token aa_func13(), long atolh(),
int iskeyword(), Token set_runtime_error_token()
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL;	/*String literal table */
int line;					/* current line number of the source code */
extern int scerrnum;		/* defined in platy_st.c - run-time error number */

							/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

					   /* scanner.c static(local) function  prototypes */
static int char_class(char c);					/* character class function */
static int get_next_state(int, char, int *);	/* state machine function */
static long atolh(char * lexeme);				/* converts hexadecimal string to decimal value */
static int iskeyword(char * kw_lexeme);			/* keywords lookup function */
static Token set_runtime_error_token();			/* Creates runtime error token */


												/*Initializes scanner */
int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
											   /* in case the buffer has been read previously  */
	b_rewind(sc_buf);
	b_clear(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
						/*   scerrnum = 0;  *//*no need - global ANSI C */
}


/************************************************************************************
Purpose:           Match a lexemes and returns a token, reads the lexeme from the input buffer one character at a time and, and returns a token struture if it finds a token pattern.
It set's the token code and the token attribute accordinly.
Author:            Billy Saint-Fort
History/Versions:  1.0
Called functions:  b_addc(), bgetc(), set_runtime_token_error(), b_retract(), b_getcoffset(), b_mark(), b_limit(), char_class(), get_next_state()
Parameters:        Buffer structure (Buffer * sc_buf)
Return value:      Token
Algorithm:		   1. In a loop, grab a character from the input buffer
2. Check if the character is part of a comment if it is skip over the whole comment, likewise if it's a space skip over the space.
3. If it's a new line or a carriage return, increment the line counter.
4. if it's none of the above, check to see if the character is a special case, if it is assign the proper token code and attribute.
5. If the character is not a special case, check to see if the the character is part of a string litteral.
6. If its part of a string litteral, read the whole string and then add it to a temporary lexeme buffer.
7. if it's not any of the above, check to see if character is a letter or digit, if its not a letter or a digit, return a error token.
8. Otherwise go in the final state machine algorithm, keep grabbing a new chracter until an accepting state is reached, than go to the matching accepting function with the lexeme and set a token code and an attribute accordingly.
9. Once completed return the token from the accepting function and assign it to the token in malar(Token t) and return the token.
10. Continue looping until character is equal SEOF.

************************************************************************************/
Token malar_next_token(Buffer * sc_buf)
{
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */

	short charcounter = 0; /*counts the amount of characters in the lexeme*/
	char tempsymbol; /*Holds temporary symbol in order to check for an error*/
	short temp_cap = 0;/*Temporary capacity for temporary buffer*/
	short offset = 0;/*Holds str_LBTL offset*/
	int i;/*for loop counter*/

	while (1) {
		/*Get char*/
		c = b_getc(sc_buf);
		/*If char is valid*/
		if (c != RT_FAIL2) {
			/*Check for special cases*/
			switch (c)
			{
			case ' ':
				continue;
			case '\n':
				line++;
				continue;
			case '\r':
				line++;
				continue;
			case '\t':
				continue;
			case '!':
				if ((c = b_getc(sc_buf)) == RT_FAIL2) {
					return set_runtime_error_token();
				}
				//Check for SEOF
				if (c == 255 || c == '\0') {
					t.code = SEOF_T;
					return t;
				}
				/*If there's a comment skip over it*/
				if (c == '!') {
					while (c != '\n') {
						if ((c = b_getc(sc_buf)) == RT_FAIL2) {
							return set_runtime_error_token();
						}
						/*If there's a new line, increment the line counter*/
						if (c == '\n' || c == '\r') {
							line++;
						}
					}
					continue;
				}
				/*Assign an error token and skip over the whole line*/
				else {
					t.code = ERR_T;
					t.attribute.err_lex[0] = '!';
					t.attribute.err_lex[1] = c;
					t.attribute.err_lex[2] = '\0';
					while (c != '\n') {
						if ((c = b_getc(sc_buf)) == RT_FAIL2) {
							return set_runtime_error_token();
						}
					}
					return t;
				}
				break;
			case '=':
				/* Grab the next character to check for the == sign*/
				if ((c = b_getc(sc_buf)) == RT_FAIL2) {
					return set_runtime_error_token();
				}
				/*If == is found assign and return the appropriate token*/
				if (c == '=') {
					t.code = REL_OP_T;
					t.attribute.rel_op = EQ;
					return t;
				}
				/*If the == is not found*/
				/*Retract the buffer to where it previously was and return token*/
				b_retract(sc_buf);
				t.code = ASS_OP_T;
				return t;

			case '(':
				t.code = LPR_T;
				return t;
			case ')':
				t.code = RPR_T;
				return t;
			case '{':
				t.code = LBR_T;
				return t;
			case '}':
				t.code = RBR_T;
				return t;
			case '==':
				t.code = REL_OP_T;
				t.attribute.log_op = EQ;
				return t;
			case '<':
				/*Grab the next character to check for <> sign*/
				if ((c = b_getc(sc_buf)) == RT_FAIL2) {
					return set_runtime_error_token();
				}
				/*If > is found assign and return the appropriate token*/
				if (c == '>') {
					t.code = REL_OP_T;
					t.attribute.rel_op = NE;
					return t;
				}
				/*Retract the buffer to where it previously was*/
				b_retract(sc_buf);
				/*Assign the token and return*/
				t.code = REL_OP_T;
				t.attribute.log_op = LT;
				return t;

			case '>':
				t.code = REL_OP_T;
				t.attribute.log_op = GT;
				return t;
			case ',':
				t.code = COM_T;
				return t;
			case '#':
				t.code = SCC_OP_T;
				return t;
			case '"':
				/*Process String Litteral*/
				while (1) {
					if ((c = b_getc(sc_buf)) == RT_FAIL2) {
						return set_runtime_error_token();
					}
					/*If there's a new line, increment the line counter*/
					if (c == '\n' || c == '\r') {
						line++;
					}
					charcounter++; /*increment char counter*/
								   /*If the string is illegal*/
					if (c == '\0' || c == 255)
					{
						/*Set counter to beginning of lexeme*/
						lexstart = (b_getcoffset(sc_buf) - charcounter);
						/*Return error token*/
						t.code = ERR_T;
						/*Set length*/
						/*If lexeme length is bigger than what err_lex can hold*/
						/*Error length can only hold 20 chars so length becomes err_length - 4 */
						if (charcounter >= ERR_LEN - 4) {
							charcounter = ERR_LEN - 4;	/*Set charcounter to max length of err_lex*/
						}
						/*Copy lexeme into err_lex*/
						t.attribute.err_lex[0] = '"'; /*Add " in front of error lexeme*/
						for (i = 1; i <= charcounter; i++)
						{
							t.attribute.err_lex[i] = sc_buf->cb_head[lexstart];
							lexstart++;
						}
						/*Add 3 ... and a string terminator*/
						t.attribute.err_lex[i] = '.';
						t.attribute.err_lex[i + 1] = '.';
						t.attribute.err_lex[i + 2] = '.';
						t.attribute.err_lex[i + 3] = '\0';
						/*Return token*/
						return t;
					}
					/*If the end of the string litteral is found*/
					if (c == '"')
					{	/*Initializing lexstart*/
						lexstart = 0;
						/*Set mark to beginning of String*/
						if (b_mark(sc_buf, (b_getcoffset(sc_buf) - charcounter)) == RT_FAIL1) {
							return set_runtime_error_token();
						}
						/*Set start counter to start of string*/
						if (sc_buf->markc_offset != 0)
						{
							lexstart = (b_getcoffset(sc_buf) - charcounter);
						}
						/*Set end counter to end of string */
						lexend = b_getcoffset(sc_buf) - 1;
						/*Set the offset for the token attribute to the offset the first character is is being added to*/
						offset = b_limit(str_LTBL);
						/*Start adding from the beginning of the string to the end*/
						while (lexstart < lexend)
						{
							/*Add symbol to str_LBTL buffer*/
							if (b_addc(str_LTBL, sc_buf->cb_head[lexstart]) == NULL) {
								return set_runtime_error_token();
							}
							/*Increment start counter*/
							lexstart++;
						}
						/*Add \0 to indicate end of string*/
						b_addc(str_LTBL, '\0');
						/*Set String token once it reaches the end of the string*/
						t.code = STR_T;
						/*Attribute of the string token is the offset from the beginning of str-LBTL char buffer to the beginning of the string */
						t.attribute.str_offset = offset;
						return t;
					}
				}
			case ';':
				t.code = EOS_T;
				return t;
			case '-':
				t.code = ART_OP_T;
				t.attribute.arr_op = MINUS;
				return t;
			case '+':
				t.code = ART_OP_T;
				t.attribute.arr_op = PLUS;
				return t;
			case '*':
				t.code = ART_OP_T;
				t.attribute.arr_op = MULT;
				return t;
			case '/':
				t.code = ART_OP_T;
				t.attribute.arr_op = DIV;
				return t;
			case '.':
				/* continue to grab the next character to check for .AND. if found set token code appropriately*/
				/*else if not found retract the buffer for the amount of steps we moved*/
				if ((c = b_getc(sc_buf)) == RT_FAIL2) {
					return set_runtime_error_token();
				}
				if (c == 'A') {
					if ((c = b_getc(sc_buf)) == RT_FAIL2) {
						return set_runtime_error_token();
					}
					/*If there's a new line, increment the line counter*/
					if (c == '\n' || c == '\r') {
						line++;
					}
					if (c == 'N')
					{
						if ((c = b_getc(sc_buf)) == RT_FAIL2) {
							return set_runtime_error_token();
						}
						/*If there's a new line, increment the line counter*/
						if (c == '\n' || c == '\r') {
							line++;
						}
						if ((c == 'D'))
						{
							if ((c = b_getc(sc_buf)) == RT_FAIL2) {
								return set_runtime_error_token();
							}
							/*If there's a new line, increment the line counter*/
							if (c == '\n' || c == '\r') {
								line++;
							}
							if (c == '.')
							{
								t.code = LOG_OP_T;
								t.attribute.log_op = AND;
								return t;
							}
							/*Retract 4 times to get back to where we were and assign error token*/
							b_retract(sc_buf);
							b_retract(sc_buf);
							b_retract(sc_buf);
							b_retract(sc_buf);
							t.code = ERR_T;
							t.attribute.err_lex[0] = '.';
							t.attribute.err_lex[1] = '\0';
							return t;

						}
						/*Retract 3 times to get back to where we were and assign error token*/
						b_retract(sc_buf);
						b_retract(sc_buf);
						b_retract(sc_buf);
						t.code = ERR_T;
						t.attribute.err_lex[0] = '.';
						t.attribute.err_lex[1] = '\0';
						return t;
					}
					/*Retract 2 times to get back to where we were and assign error token*/
					b_retract(sc_buf);
					b_retract(sc_buf);
					t.code = ERR_T;
					t.attribute.err_lex[0] = '.';
					t.attribute.err_lex[1] = '\0';
					return t;
				}
				/*Check for .OR. if the first character after . is not A*/
				/*Check for O*/
				else if (c == 'O') {
					if ((c = b_getc(sc_buf)) == RT_FAIL2) {
						return set_runtime_error_token();
					}
					/*If there's a new line, increment the line counter*/
					if (c == '\n' || c == '\r') {
						line++;
					}
					/*Check for R*/
					if (c == 'R')
					{
						if ((c = b_getc(sc_buf)) == RT_FAIL2) {
							return set_runtime_error_token();
						}
						/*If there's a new line, increment the line counter*/
						if (c == '\n' || c == '\r') {
							line++;
						}
						/*Check .*/
						if (c == '.')
						{
							t.code = LOG_OP_T;
							t.attribute.log_op = OR;
							return t;
						}
						/*If there's an error when checking for . retract 3 times to get back to where we were and assign error token*/
						b_retract(sc_buf);
						b_retract(sc_buf);
						b_retract(sc_buf);
						t.code = ERR_T;
						t.attribute.err_lex[0] = '.';
						t.attribute.err_lex[1] = '\0';
						return t;
					}
					/*If there's an error when checking for R Retract 2 times to get back to where we were and assign error token*/
					b_retract(sc_buf);
					b_retract(sc_buf);
					t.code = ERR_T;
					t.attribute.err_lex[0] = '.';
					t.attribute.err_lex[1] = '\0';
					return t;
				}
				/* If there's an error when checking for A or O Retract once and return an error token*/
				b_retract(sc_buf);
				t.code = ERR_T;
				t.attribute.err_lex[0] = '.';
				t.attribute.err_lex[1] = '\0';
				return t;

				/*If SEOF is reached */
			case '\0':
				t.code = SEOF_T;
				return t;
			case 255:
				t.code = SEOF_T;
				return t;
			default:
				/*If c is not a special character a letter or a number, set error token */
				if ((char_class(c)) > 4)
				{
					t.code = ERR_T;
					t.attribute.err_lex[0] = c;
					t.attribute.err_lex[1] = '\0';
					return t;
				}
				/* Otherwise process Transition table*/
				/*Determine if c is a letter or a digit*/
				if (char_class(c) >= 0 && char_class(c) <= 4)
				{
					/*Set the mark at the beginning of the lexeme and save it in lexstart*/
					lexstart = b_mark(sc_buf, (b_getcoffset(sc_buf) - 1));
					if (lexstart == RT_FAIL1) {
						return set_runtime_error_token();
					}
					/*Final state machine*/
					/*Begin with state 0*/
					state = 0;
					while (1) {
						/*Get next state*/
						state = get_next_state(state, c, &accept);
						/*If state is accepting, leave machine and call accepting function*/
						if (accept == ASWR || accept == ASNR) {
							break;
						}
						/*Get next character*/
						if ((c = b_getc(sc_buf)) == RT_FAIL2) {
							return set_runtime_error_token();
						}
					}
					/*If the accepting state is an accepting state with retract, retract the buffer*/
					if (accept == ASWR) {
						b_retract(sc_buf);
					}
					/*Set lexend to end of lexeme*/
					lexend = b_getcoffset(sc_buf);
					/*the capacity for the temp buffer is the amount of characters in the lexeme*/
					temp_cap = lexend - lexstart;
					/*Temporary lexeme buffer*/
					if ((lex_buf = b_allocate(temp_cap, 1, 'a')) == NULL)
					{
						return set_runtime_error_token();
					}
					/*Retract getcoffset till the beginning of the lexeme*/
					while (b_getcoffset(sc_buf) != lexstart) {
						b_retract(sc_buf);
					}
					/*Go through each character of the lexeme and add it to the temp lexeme buffer*/
					while (lexstart < lexend) {
						/*Add the character to the symbol to check for errors before adding it to the buffer*/
						tempsymbol = b_getc(sc_buf);
						if (tempsymbol == RT_FAIL2) {
							return set_runtime_error_token();
						}
						/*Add character to temporary buffer*/
						if ((b_addc(lex_buf, tempsymbol)) == NULL) {
							return set_runtime_error_token();
						}
						/*Increment lexstart*/
						lexstart++;
					}
					/*add string terminator*/
					b_addc(lex_buf, '\0');
					/*Dont need to check which state it is since we already check for accepting state in algorithm */
					/*Call accepting function*/
					t = (*aa_table[state])(lex_buf->cb_head);
					/*Free temp buffer when completed*/
					b_free(lex_buf);
					return t;
				}
				else {
					/*Check for other characters if necessary */
					if (char_class(c) >= 5 && char_class(c) <= 7) {
						/*Set an error token for illegal character*/
						t.code = ERR_T;
						/*Set character to attribute*/
						t.attribute.err_lex[0] = c;
						t.attribute.err_lex[1] = '\0';
					}
				}
			}
		}
	}
	/*Run time error*/
	return set_runtime_error_token();
}


/*Svillen*/
int get_next_state(int state, char c, int *accept) {
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:

	Assertion failed: test, file filename, line linenum

	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}


/************************************************************************************
Purpose:			Identifiy the table column associated with the input character
Author:				Nathan Festoso
Versions:			1.0
Called functions:	-
Parameters:			char c
Return value:		int - column number
Algorithm:			Compare input character to ASCII value ranges
************************************************************************************/
int char_class(char c)
{
	int column;
	/* Identify character */
	if ((c >= 'a' && c <= 'w') || (c == 'y' || c == 'z') || (c >= 'G' && c <= 'Z'))
		column = 0;
	else if (c >= 'A' && c <= 'F')
		column = 1;
	else if (c == 'x')
		column = 2;
	else if (c >= '1' && c <= '9')
		column = 3;
	else if (c == '0')
		column = 4;
	else if (c == '.')
		column = 5;
	else if (c == '$')
		column = 6;
	/* other */
	else
		column = 7;

	return column;
}


/************************************************************************************
Purpose:			Identify input lexeme as a keyword or AVID
Author:				Nathan Festoso
Versions:			1.0
Called functions:	iskeyword(), strlen()
Parameters:			char lexeme[]
Return value:		Token - Keyword or AVID
Algorithm:			Check if lexeme is a keyword and create token, if not, create an AVID token
************************************************************************************/
Token aa_func02(char lexeme[]) {
	Token t;
	int kw_index;
	int length = strlen(lexeme);
	int i;

	/* check if lexeme is a keyword */
	kw_index = iskeyword(lexeme);
	if (kw_index != NOT_KW) {
		/*lexeme is keyword*/
		t.attribute.kwt_idx = kw_index;
		t.code = KW_T;
		return t;
	}

	/* lexeme is AVID */
	/* add lexeme to token truncated to specified length */
	for (i = 0; i < VID_LEN && i < length; i++)
		t.attribute.vid_lex[i] = lexeme[i];
	/* add string terminator */
	t.attribute.vid_lex[i] = '\0';

	/* set token code */
	t.code = AVID_T;

	return t;
}


/************************************************************************************
Purpose:           Accepting function for String variable identifier.
It set's the token code and the token attribute accordinly for a string variable identifier.
Author:            Billy Saint-Fort
History/Versions:  1.0
Called functions:  str_len()
Parameters:        lexeme (char lexeme [])
Return value:      Token
Algorithm:		   1. Set token code to SVID
2. Set token attribute vid_lex according to length.
3. Return a token when completed.

************************************************************************************/
Token aa_func03(char lexeme[]) {

	Token t; /*Creating a token*/
	int i = 0; /*Counter for for loop*/
	int length = strlen(lexeme); /*Holds length of lexeme*/

								 /*Setting a variable token*/
	t.code = SVID_T;
	/*If the lexeme is longer than vid_len*/
	if (length > VID_LEN) {
		length = VID_LEN - 1;
	}
	else {
		/*Store the lexeme length into the variable length*/
		length = strlen(lexeme);
	}
	/*Only take the first eight characters and store it in vid_lex*/
	for (i = 0; i < length; i++)
	{
		t.attribute.vid_lex[i] = lexeme[i];
	}
	/*If the lexeme length is bigger than 8 and it's lenght is set to 7 add '$' and '\0' at the end of the string*/
	if (strlen(lexeme) > VID_LEN && length == VID_LEN - 1)
	{
		/*Add $ to the name*/
		t.attribute.vid_lex[i] = '$';
		/*Add \0 to the end to make a c string*/
		t.attribute.vid_lex[i + 1] = '\0';
		return t;
	}
	/* Otherwise Ada the string terminator at the end of the svid*/
	t.attribute.vid_lex[i] = '\0';
	/*Return token*/
	return t;
}


/************************************************************************************
Purpose:           Accepting function for Decimal Integer Litteral.
It set's the token code and the token attribute accordingly for a Decimal Integer litteral.
Author:            Billy Saint-Fort
History/Versions:  1.0
Called functions:  str_len()
Parameters:        lexeme (char lexeme [])
Return value:      Token
Algorithm:		   1. Loop through lexeme and convert string representation of number to a decimal integer litteral value.
2. Check decimal integer litteral value for overflow and check to see if it's out of range, if it's is set error token and set token attribute err.properly according to length.
3. If no error is found, set the token to an INT_L token and set the token attribute int value to the decimal integer litteral value calculated and return token.

************************************************************************************/
Token aa_func05(char lexeme[]) {
	Token t; /*Creating a token */
	int length = strlen(lexeme); /*Holds length of lexeme*/
	long decimal_integer_value = 0; /*Holds decimal integer value*/
	long base10 = 1; /*Holds base10 (multiply value)*/
	int i = 0; /*Counter for loop*/
	char num; /*digit representation of number*/

			  /*Loop through the length of the string decimal integer starting from the last digit */
	for (i = length - 1; i >= 0; i--)
	{
		/*If overflow occurs set code to error*/
		if (decimal_integer_value < 0) {
			i = -2; /*i set to -2 if overflow happened*/
			t.code = ERR_T;
		}
		num = lexeme[i]; /*Assign the string digit to the num variable*/
						 /*Associate the value in num with the actual integer number and mutiply it by the correct base10 number and add it to the decimal_integer_value*/
		switch (num) {
		case '0':
			decimal_integer_value += 0;
			base10 *= 10;
			break;
		case '1':
			decimal_integer_value += 1 * base10;
			base10 *= 10;
			break;
		case '2':
			decimal_integer_value += 2 * base10;
			base10 *= 10;
			break;
		case '3':
			decimal_integer_value += 3 * base10;
			base10 *= 10;
			break;
		case '4':
			decimal_integer_value += 4 * base10;
			base10 *= 10;
			break;
		case '5':
			decimal_integer_value += 5 * base10;
			base10 *= 10;
			break;
		case '6':
			decimal_integer_value += 6 * base10;
			base10 *= 10;
			break;
		case '7':
			decimal_integer_value += 7 * base10;
			base10 *= 10;
			break;
		case '8':
			decimal_integer_value += 8 * base10;
			base10 *= 10;
			break;
		case '9':
			decimal_integer_value += 9 * base10;
			base10 *= 10;
			break;
		default:
			break;
		}
	}
	/*If the decimal integer value is out of range or there's an overflow*/
	if (decimal_integer_value > SHRT_MAX || decimal_integer_value < 0 || i == -2)
	{
		/*Return an error token*/
		t = aa_table[ES](lexeme);
		return t;
	}
	else {
		/*Set the token to an integer litteral*/
		t.code = INL_T;
		/*Set the decimal_integer_value as the attribute for the token*/
		t.attribute.int_value = (short)decimal_integer_value;
		/*Return the token*/
		return t;
	}
}


/************************************************************************************
Purpose:           Accepting function for Floating Point Litteral.
It set's the token code and the token attribute accordinly for a Floating point litteral.
Author:            Billy Saint-Fort
History/Versions:  1.0
Called functions:  str_len()
Parameters:        lexeme (char lexeme [])
Return value:      Token
Algorithm:		   1. Loop through lexeme and find point, store the index of point in a variable
2. Loop through each digit before point, add them up accordingly considering their position in the number and store the number into a variable.
3. Loop through each digit after the point, add them up accordingly considering their position after the point and store the number into a vairable.
4. Add the two numbers stored in the variable together to create the floating point litteral.
5. Check floating point number to know if it's out of range, if it is create an error token and assign token attribute err_lex accordingly according to length.
6. Otherwise set the token code to Floating point litteral, and set the attribute to a flt_value and assign it the floating point litteral.
7. Return token when completed.
************************************************************************************/
Token aa_func08(char lexeme[]) {
	Token t; /*Creating a token */
	int length = strlen(lexeme); /*Holds length of lexeme*/
	double base10 = 1; /*Holds base10 (multiply value)*/
	int i = 0; /*counter for forloop*/
	int pointindex = 0; /*Holds the offset where the point is in the lexeme*/
	double numbeforepoint = 0; /*Holds the actual number result of the digits before the point */
	double numafterpoint = 0; /*Holds the actual number result of the digits after the point*/
	double floatingpointnum = 0.0; /*Holds the final floating point number after adding numbeforepoint and numafterpoint*/

								   /*Loop through the length of the string float starting from the last character */
	for (i = length - 1; i >= 0; i--)
	{
		/*Assign the index of the point to pointindex*/
		if (lexeme[i] == '.') {
			pointindex = i;
		}
	}
	/*Initialize base10 to 1*/
	base10 = 1;
	/*Read everything before point and add up all the numbers and store it in numberbeforepoint*/
	for (i = pointindex; i >= 0; i--) {
		/*Recognize each string digit for it's actual representation and add it to the numberbeforepoint*/
		switch (lexeme[i - 1]) {
		case '0':numbeforepoint += (0 * base10); break;
		case '1':numbeforepoint += (1 * base10); break;
		case '2':numbeforepoint += (2 * base10); break;
		case '3':numbeforepoint += (3 * base10); break;
		case '4':numbeforepoint += (4 * base10); break;
		case '5':numbeforepoint += (5 * base10); break;
		case '6':numbeforepoint += (6 * base10); break;
		case '7':numbeforepoint += (7 * base10); break;
		case '8':numbeforepoint += (8 * base10); break;
		case '9':numbeforepoint += (9 * base10); break;
		default:break;
		}
		base10 *= 10;/*Increment base10 times 10 to get actual representation of the number value and position in the lexeme*/
	}
	base10 = 10;
	/*Read everything after point and add up all the numbers and store it in numberafterpoint*/
	/*Because we're calculating numbers after the point we have to divide the digits by base10 because they get smaller the farther we go away from the point*/
	for (i = pointindex; i <= length; i++) {
		switch (lexeme[i + 1]) {
		case '0':numafterpoint += 0; break;
		case '1':numafterpoint += (1 / base10); break;
		case '2':numafterpoint += (2 / base10); break;
		case '3':numafterpoint += (3 / base10); break;
		case '4':numafterpoint += (4 / base10); break;
		case '5':numafterpoint += (5 / base10); break;
		case '6':numafterpoint += (6 / base10); break;
		case '7':numafterpoint += (7 / base10); break;
		case '8':numafterpoint += (8 / base10); break;
		case '9':numafterpoint += (9 / base10); break;
		default:break;
		}
		base10 *= 10; /*Base 10 is multiplied by 10 every single time*/
	}
	/*Add numbeforepoint and numafterpoint together*/
	floatingpointnum = numbeforepoint + numafterpoint;
	/*If there's more then 7 digits after the point and the value is 0.0000000 =  out of range-> error*/
	if ((float)floatingpointnum == 0.000000 && ((strlen(lexeme) - pointindex) > 6) || floatingpointnum > FLT_MAX)
	{ /*If float is so small that it can't be represented it will appear as zero so check lenght too, if its bigger than 7 we have an error*/
	  /*Return an error token*/
		t = aa_table[ES](lexeme);
		return t;
	}
	/*Set code to FPL_T and FLT value and return Token*/
	t.code = FPL_T;
	t.attribute.flt_value = (float)floatingpointnum;
	return t;
}


/************************************************************************************
Purpose:			Convert a string representation of a hex value to a decimal integer value
Author:				Nathan Festoso
Versions:			1.0
Called functions:	atolh(), strlen()
Parameters:			char lexeme[]
Return value:		Token - Integer literal or error
Algorithm:			Convert String to int, validate range, set and return token
************************************************************************************/
Token aa_func11(char lexeme[]) {
	Token t;
	short dec = 0;
	long hex = atolh(lexeme);

	/* check for overflow on 2 byte integer */
	if (hex > SHRT_MAX) {
		t = aa_table[ES](lexeme);
		return t;
	}

	/* Set HIL value */
	dec = (short)hex;
	t.attribute.int_value = dec;

	/* set token code */
	t.code = INL_T;

	return t;
}


/************************************************************************************
Purpose:			Creates and returns error token using input lexeme
Author:				Nathan Festoso
Versions:			1.0
Called functions:	strlen()
Parameters:			char lexeme[]
Return value:		Error token
Algorithm:			Set error token using lexeme and return
************************************************************************************/
Token aa_func12(char lexeme[]) {
	Token t;
	int length = strlen(lexeme);
	int i;

	/* check error length and set */
	if (length > ERR_LEN) {
		/* lexeme exeeds error length*/
		for (i = 0; i < ERR_LEN - 3; i++)
			t.attribute.err_lex[i] = lexeme[i];
		/* append ... */
		for (; i < ERR_LEN; i++)
			t.attribute.err_lex[i] = '.';
	}
	else {
		for (i = 0; i < length; i++)
			t.attribute.err_lex[i] = lexeme[i];
	}
	t.attribute.err_lex[i] = '\0';

	/* set token code */
	t.code = ERR_T;
	return t;

}


/************************************************************************************
Purpose:           Accepting function for state 13.
Sets error token.
Author:            Billy Saint-Fort
History/Versions:  1.0
Called functions:  str_len()
Parameters:        lexeme (char lexeme [])
Return value:      Token
Algorithm:		   1. Set error token code to ERR_T
2. Set token attribute err.lex to lexeme according to length.
3. Return token

************************************************************************************/
Token aa_func13(char lexeme[]) {
	/*Go to function 12 and set error token approprietly*/
	Token t;
	t = aa_table[ES](lexeme);
	return t;
}


/************************************************************************************
Purpose:			Convert hex string representation to integer
Author:				Nathan Festoso
Versions:			1.0
Called functions:	strlen()
Parameters:			char * lexeme
Return value:		Result of hex conversion or ES if overflow occurs
Algorithm:			Starting from the end of the hex value, sum together each characters
corresponding hex value with its base 16 representation
************************************************************************************/
long atolh(char * lexeme) {
	long result = 0;
	long multiplier = 1;
	int length = strlen(lexeme);
	int i;
	char c;
	/* Calculate HEX starting from end */
	for (i = length - 1; i > 1; i--) {
		/* Identify Hex character */
		c = lexeme[i];
		switch (c) {
		case '1': result += 1 * multiplier; break;
		case '2': result += 2 * multiplier; break;
		case '3': result += 3 * multiplier; break;
		case '4': result += 4 * multiplier; break;
		case '5': result += 5 * multiplier; break;
		case '6': result += 6 * multiplier; break;
		case '7': result += 7 * multiplier; break;
		case '8': result += 8 * multiplier; break;
		case '9': result += 9 * multiplier; break;
		case 'A': result += 10 * multiplier; break;
		case 'B': result += 11 * multiplier; break;
		case 'C': result += 12 * multiplier; break;
		case 'D': result += 13 * multiplier; break;
		case 'E': result += 14 * multiplier; break;
		case 'F': result += 15 * multiplier; break;
		default: /* character was 0 */;
		}
		/* increase multiplier to next hex value */
		multiplier *= 16;
	}
	return result;
}


/************************************************************************************
Purpose:			Identify if input is a keyword
Author:				Nathan Festoso
Versions:			1.0
Called functions:	strcmp()
Parameters:			char * kw_lexeme
Return value:		index of keyword in keyword table or NOT_KW if no match
Algorithm:			Compare input lexeme to keyword table and return result
************************************************************************************/
int iskeyword(char * kw_lexeme) {
	int i;

	/* check keyword table */
	for (i = 0; i < KWT_SIZE; i++)
		if (strcmp(kw_table[i], kw_lexeme) == 0)
			return i;

	/* not a keyword */
	return NOT_KW;
}

/************************************************************************************
Purpose:           Sets the runtime error token.
Sets error token.
Author:            Billy Saint-Fort
History/Versions:  1.0
Called functions:  None
Parameters:        None
Return value:      Token
Algorithm:		   1. Sets token code to err_t.
2. Set scerrnum to a number
3. Assign to err_lex "RUN TIME ERROR"
4. Return token
************************************************************************************/
Token set_runtime_error_token()
{
	Token t; /*Create token*/
	scerrnum = 2; /*set global variable number to 2 for error*/
	t.code = ERR_T; /*Assign err_t code to token*/
					/*Assign to err.lex "RUN TIME ERROR"*/
	t.attribute.err_lex[0] = 'R';
	t.attribute.err_lex[1] = 'U';
	t.attribute.err_lex[2] = 'N';
	t.attribute.err_lex[3] = ' ';
	t.attribute.err_lex[4] = 'T';
	t.attribute.err_lex[5] = 'I';
	t.attribute.err_lex[6] = 'M';
	t.attribute.err_lex[7] = 'E';
	t.attribute.err_lex[8] = ' ';
	t.attribute.err_lex[9] = 'E';
	t.attribute.err_lex[10] = 'R';
	t.attribute.err_lex[11] = 'R';
	t.attribute.err_lex[12] = 'O';
	t.attribute.err_lex[13] = 'R';
	return t;/*Return token*/
}