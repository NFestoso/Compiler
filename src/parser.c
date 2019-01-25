/*********************************************************
File name:	parser.c
Compiler:	MS Visual Studio 2015
Author:		Nathan Festoso - 040825359
Billy Saint-Fort - 040831048
Course:		CST 8152 - Compilers, Lab Section: 10
Assignment:	3
Date:		Jan 2, 2018
Professor:	Sv. Ranev
Purpose:	Semantic analysis for PLATYPUS language
Function list:	parser(), match(), syn_eh(), syn_printe(), gen_incode(), program(),
statement(), statements(), statements_p(), opt_statements(), assign_statement(),
select_statement(), it_statement(), in_statement(), out_statement(), full_list(),
vid(), var_list_p(), var_list(), assign_exp(), arith_exp(), primary_arith_exp(),
unary_exp(), add_arith_exp(), add_arith_exp_p(), multi_arith_exp(),
multi_arith_exp_p(), string_exp(), string_exp_p(), primary_string_exp(),
conditional_exp(), logical_OR_exp(), logical_OR_exp_p(), logical_AND_exp(),
logical_AND_exp_p(), relation_exp(), primary_arith_relation_exp(), primary_string_relation_exp(),
add_arith_op(), multi_arith_op(), relation_op(), pre_condition()
*********************************************************/

#include <string.h>
#include <stdlib.h>
#include "parser.h"

/*External declarations*/
extern Token malar_next_token(Buffer * sc_buf);
extern int line;
extern  Buffer * str_LTBL;
extern char * kw_table[];

/*Svillen*/
void parser(Buffer * in_buf) {
	sc_buf = in_buf;
	lookahead = malar_next_token(sc_buf);
	program();
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*********************************************************
Purpose:	Match the current input token with the token expected by compiler
Author:		Nathan Festoso
Versions:	2
Called functions: malar_next_token(), syn_eh(), syn_printe()
Parameters:	int pr_token_code - Expected token code
int pr_token_attribute - Expected token attribute
Algorithm:	1 - Check if token codes match
2 - Check if attributes match depending on code
3 - Advance to next token
4 - If new token is error type, report it
*********************************************************/
void match(int pr_token_code, int pr_token_attribute) {
	/*Match FAIL*/
	if (lookahead.code != pr_token_code) { syn_eh(pr_token_code); return; }
	/*Source end of file*/
	if (lookahead.code == SEOF_T) return;

	/* Attribute used only when code is { KW_T, LOG_OP_T, ART_OP_T, REL_OP_T } */
	if (pr_token_code == KW_T || pr_token_code == LOG_OP_T || pr_token_code == ART_OP_T || pr_token_code == REL_OP_T)
		/*Match FAIL*/
		if (lookahead.attribute.get_int != pr_token_attribute) { syn_eh(pr_token_code); return; }

	/*Match SUCCESS*/
	if ((lookahead = malar_next_token(sc_buf)).code == ERR_T) {
		/*New token is error token*/
		syn_printe();
		lookahead = malar_next_token(sc_buf);
		++synerrno;
	}
}

/*Billy*/
void syn_eh(int sync_token_code)
{
	/*Function calls syn_printe()*/
	syn_printe();
	/*Increment error counter*/
	synerrno++;
	/*Implement panic mode error recovery*/

	/*Endless while loop will generate warning*/
	while (1) {
		/*Can reach end of source file without finding matching token*/
		/*To prevent from overunnning buffer, check if end of file is reached*/
		if (lookahead.code != SEOF_T)
		{
			lookahead = malar_next_token(sc_buf);
			/*If the function looks for a sync_token code different then SEOFT and reaches the end of the source file*/
			if (sync_token_code != SEOF_T && lookahead.code == SEOF_T) {
				/*Call exit with synerrno*/
				exit(synerrno);
			}
			/*IF a matching token is found and it's not SEOF_T, function advances the input token one more time and returns*/
			else if (lookahead.code == sync_token_code && lookahead.code != SEOF_T)
			{
				lookahead = malar_next_token(sc_buf);
				return;
			}
			/*If a macthing token is found and the token is SEOF_T, the function returns*/
			else if (lookahead.code == sync_token_code && lookahead.code == SEOF_T) {
				return;
			}
		}
	}
}

/* error printing function for Assignment 3 (Parser), F17 */
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

 /*Billy*/
void gen_incode(char str[]) {
	unsigned int i = 0; /*Counter*/
						/*Prints message when production is recognized*/
	for (i = 0; i < strlen(str); i++)
	{
		printf("%c", str[i]);
	}
	printf("\n");
}

/*Svillen*/
void program(void) {
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*Billy*/
void statement(void) {
	/*First set: {AVID,SVID,KW_T(IF),KW_T(WHILE),KW_T(READ),KW_T(WRITE)*/
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		assign_statement();
		break;
	case KW_T:
		/*Check for IF, WHILE, READ and WRITE*/
		if (lookahead.attribute.get_int == IF) {
			select_statement();
		}
		if (lookahead.attribute.get_int == WHILE) {
			it_statement();
		}
		if (lookahead.attribute.get_int == READ) {
			in_statement();
		}
		if (lookahead.attribute.get_int == WRITE) {
			out_statement();
		}
		break;
	default:
		syn_printe();
		break;
	}
}

/*Billy*/
void statements(void) {
	statement();
	statements_p();
}

/*Billy*/
void statements_p(void)
{
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statement();
		statements_p();
		break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT here and in
		statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT) {
			statement();
			statements_p();
			break;
		}
	default: /*empty string - optional statements*/;
		return;
	}
}

/*Svillen*/
void opt_statements(void) {
	/* FIRST set: {AVID_T,SVID_T,KW_T,e (but not PLATYPUS, ELSE, THEN, REPEAT)} */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT here and in
		statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT) {
			statements();
			break;
		}
	default: /*empty string - optional statements*/;
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*Billy*/
void assign_statement(void) {
	assign_exp();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*Billy*/
void select_statement(void) {
	match(KW_T, IF);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_exp();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}

/*Billy*/
void it_statement(void)
{
	match(KW_T, WHILE);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_exp();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}

/*Svillen*/
void in_statement(void) {
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	var_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/*Billy*/
void out_statement(void) {
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	full_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/*Billy*/
void full_list(void)
{
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		var_list();
		break;
	case STR_T: match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		return;
	}
}

/*Billy*/
void vid(void) {
	switch (lookahead.code)
	{
	case AVID_T: match(AVID_T, NO_ATTR);
		break;
	case SVID_T: match(SVID_T, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
}

/*Billy*/
void var_list_p(void) {
	switch (lookahead.code) {
	case COM_T:
		match(COM_T, NO_ATTR);
		vid();
		var_list_p();
		break;
	default:
		/*Empty*/
		return;
	}
}

/*Billy*/
void var_list(void)
{
	vid();
	var_list_p();
	gen_incode("PLATY: Variable list parsed");

}

/*Billy*/
void assign_exp(void) {
	if (lookahead.code == AVID_T) {
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arith_exp();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
	}
	else if (lookahead.code == SVID_T) {
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		string_exp();
		gen_incode("PLATY: Assignment expression (string) parsed");
	}
	else
	{
		syn_printe();
	}
}

/*Billy*/
void arith_exp(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == MINUS || lookahead.attribute.arr_op == PLUS) {
			unary_exp();
		}
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	case LPR_T:
	case AVID_T:
	case FPL_T:
	case INL_T:
		add_arith_exp();
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	default:
		syn_printe();
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	}
}

/*Billy*/
void primary_arith_exp(void) {
	switch (lookahead.code) {
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arith_exp();
		match(RPR_T, NO_ATTR);
		break;
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*Billy*/
void unary_exp(void) {
	add_arith_op();
	primary_arith_exp();
	gen_incode("PLATY: Unary arithmetic expression parsed");
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<additive arithmetic expression> ->
			<multiplicative arithmetic expression> <additive arithmetic expression'>
First Set:	FIRST(<additive arithmetic expression>) = {LPR_T, AVID, FPL_T, INL_T}
*********************************************************/
void add_arith_exp(void) {
	multi_arith_exp();
	add_arith_exp_p();
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<additive arithmetic expression'> ->
			<additive operator> <multiplicative arithmetic expression> <additive arithmetic expression'>
			| E
First Set:	FIRST(<additive arithmetic expression'>) = {ART_OP_T(+), ART_OP_T(-), E}
*********************************************************/
void add_arith_exp_p(void) {
	/* + or - */
	if (lookahead.code == ART_OP_T &&
		(lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS)) {
		add_arith_op();
		multi_arith_exp();
		add_arith_exp_p();
		gen_incode("PLATY: Additive arithmetic expression parsed");
	}
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<multiplicative arithmetic expression> ->
			<primary arithmetic expression> <multiplicative arithmetic expression'>
First Set:	FIRST(<multiplicative arithmetic expression>) = {LPR_T, AVID, FPL_T, INL_T}
*********************************************************/
void multi_arith_exp(void) {
	primary_arith_exp();
	multi_arith_exp_p();
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<multiplicative arithmetic expression'>->
<multiplicative operator> <primary arithmetic expression> <multiplicative arithmetic expression'>
| E
First Set:	FIRST(<multiplicative arithmetic expression'>) = {ART_OP_T(*), ART_OP_T(/), E}
*********************************************************/
void multi_arith_exp_p(void) {
	/* * or / */
	if (lookahead.code == ART_OP_T &&
		(lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV)) {
		multi_arith_op();
		primary_arith_exp();
		multi_arith_exp_p();
		gen_incode("PLATY: Multiplicative arithmetic expression parsed");
	}
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<string expression> ->
<primary string expression> <string expression'>
First Set:	FIRST(<string expression>) = {SVID, STR_T}
*********************************************************/
void string_exp(void) {
	primary_string_exp();
	string_exp_p();
	gen_incode("PLATY: String expression parsed");
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<string expression'> ->
#<primary string expression> <string expression'>
| E
First Set:	FIRST(<string expression'>) = {SCC_OP_T, E}
*********************************************************/
void string_exp_p() {
	if (lookahead.code == SCC_OP_T) {
		match(SCC_OP_T, NO_ATTR);
		primary_string_exp();
		string_exp_p();
	}
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<primary string expression> -> SVID_T | STR_T
First Set:	FIRST(<primary string expression>) = {SVID_T, STR_T}
*********************************************************/
void primary_string_exp(void) {
	/* String vid */
	if (lookahead.code == SVID_T)
		match(SVID_T, NO_ATTR);
	/* String literal */
	else if (lookahead.code == STR_T)
		match(STR_T, NO_ATTR);
	/* Error */
	else { syn_printe(); }
	gen_incode("PLATY: Primary string expression parsed");
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<conditional expression> -> <logical OR expression>
First Set:	FIRST(<conditional expression>) = {AVID_T, FPL_T, INL_T}
*********************************************************/
void conditional_exp(void) {
	logical_OR_exp();
	gen_incode("PLATY: Conditional expression parsed");
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<logical OR expression> ->
<logical AND expression> <logical OR expression'>
First Set:	FIRST(<logical OR expression>) = {AVID_T, FPL_T, INL_T}
*********************************************************/
void logical_OR_exp(void) {
	logical_AND_exp();
	logical_OR_exp_p();

}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<logical OR expression'> ->
.OR. <logical AND expression> <logical OR expression'>
| E
First Set:	FIRST(<logical OR expression'>) = {LOG_OP_T(OR), E}
*********************************************************/
void logical_OR_exp_p(void) {
	/* .OR. */
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == OR)
	{
		match(LOG_OP_T, OR);
		logical_AND_exp();
		logical_OR_exp_p();
		gen_incode("PLATY: Logical OR expression parsed");
	}
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<logical AND expression> ->
<relational expression> <logical AND expression'>
First Set:	FIRST(<logical AND expression>) = {AVID_T, FPL_T, INL_T}
*********************************************************/
void logical_AND_exp(void) {
	relation_exp();
	logical_AND_exp_p();
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<logical AND expression'> ->
.AND. <relational expression> <logical AND expression'>
| E
First Set:	FIRST(<logical AND expression'>) = {LOG_OP_T (AND), E}
*********************************************************/
void logical_AND_exp_p(void) {
	/* .AND. */
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == AND)
	{
		match(LOG_OP_T, AND);
		relation_exp();
		logical_AND_exp_p();
		gen_incode("PLATY: Logical AND expression parsed");
	}
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<relational expression>->
<primary a_relational expression> <relational operator> <primary a_relational expression>
| <primary s_relational expression> <relational operator> <primary s_relational expression>
First Set:	FIRST(<relational expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*********************************************************/
void relation_exp(void) {
	/* Arithmetic */
	if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T) {
		primary_arith_relation_exp();
		relation_op();
		primary_arith_relation_exp();
	}
	/* String */
	else if (lookahead.code == SVID_T || lookahead.code == STR_T) {
		primary_string_relation_exp();
		relation_op();
		primary_string_relation_exp();
	}
	/* Error */
	else { syn_printe(); }

	gen_incode("PLATY: Relational expression parsed");
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<primary a_relational expression> -> AVID_T | FPL_T | INL_T
First Set:	FIRAT(<primary a_relational expression>) = {AVID_T, FPL_T, INL_T}
*********************************************************/
void primary_arith_relation_exp(void) {
	switch (lookahead.code) {
	case AVID_T: match(AVID_T, NO_ATTR);
		break;
	case FPL_T: match(FPL_T, NO_ATTR);
		break;
	case INL_T: match(INL_T, NO_ATTR);
		break;
	default:
		/* Error */
		syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<primary s_relational expression> -> <primary string expression>
First Set:	FIRST(<primary s_relational expression>) = {SVID_T, STR_T}
*********************************************************/
void primary_string_relation_exp(void) {
	primary_string_exp();
	gen_incode("PLATY: Primary s_relational expression parsed");
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<additive arithmetic operator> -> + | -
First Set:	FIRST(<additive arithmetic operator>) = {ART_OP_T(+), ART_OP_T(-)}
*********************************************************/
void add_arith_op(void) {
	/* + */
	if (lookahead.attribute.arr_op == PLUS) match(ART_OP_T, PLUS);
	/* - */
	else if (lookahead.attribute.arr_op == MINUS) match(ART_OP_T, MINUS);
	/* Error */
	else syn_printe();
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<multiplicative arithmetic operator> -> * | /
First Set:	FIRST(<multiplicative arithmetic operator>) = {ART_OP_T(*), ART_OP_T(/)}
*********************************************************/
void multi_arith_op(void) {
	/* (*) multiplication */
	if (lookahead.attribute.arr_op == MULT) match(ART_OP_T, MULT);
	/* (/) division */
	else if (lookahead.attribute.arr_op == DIV) match(ART_OP_T, DIV);
	/* Error */
	else syn_printe();
}

/*********************************************************
Author:		Nathan Festoso
Grammar:	<relational operator> -> == | <> | > | <
First Set:	FIRST(<relational operator>) = {REL_OP_T(==), REL_OP_T(<>), REL_OP_T(>), REL_OP_T(<)}
*********************************************************/
void relation_op(void) {
	/* Equal to */
	if (lookahead.attribute.rel_op == EQ) match(REL_OP_T, EQ);
	/* Not equal*/
	else if (lookahead.attribute.rel_op == NE) match(REL_OP_T, NE);
	/* Greater than */
	else if (lookahead.attribute.rel_op == GT) match(REL_OP_T, GT);
	/* Less than */
	else if (lookahead.attribute.rel_op == LT) match(REL_OP_T, LT);
	/* Error */
	else syn_printe();
}

/*Billy*/
void pre_condition(void) {
	if (lookahead.code == KW_T && lookahead.attribute.get_int == TRUE) {
		match(KW_T, TRUE);
	}
	else if (lookahead.code == KW_T && lookahead.attribute.get_int == FALSE) {
		match(KW_T, FALSE);
	}
	else
	{
		syn_printe();
	}
}