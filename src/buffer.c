/***********************************************************************************************************************************
File name:	        buffer.c
Compiler:           Ansi C
Author:             Billy Saint- Fort, 040-831-048
Course:             CST 8152 - Compilers, Lab Section: 12
Assignement:        1
Date:               2017-09-28
Professor:          Svillen Ranev
Purpose:            Programming and Using Dynamic Structures (buffers) with C
Function list:
b_allocate(), b_addc(), b_clear(),
b_free(), b_isfull(), b_limit(),
b_capacity(), b_mark(), b_mode(),
b_incfactor(), b_load(), b_isempty(),
b_eob(), b_getc(), b_print(), b_compact(),
b_rflag(), b_retract(), b_reset(), b_getcoffset(),
b_rewind(), b_location()
***********************************************************************************************************************************/

#include "buffer.h"
#define _CRTDBG_MAP_ALLOC	// need this to get the line identification
//_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF|_CRTDBG_LEAK_CHECK_DF); // in main, after local declarations
//NB must be in debug build
/**********************************************************************************************************************************
Purpose:            This function creates a new buffer structure in memory (heap) and a character buffer using the values set in the parameters if valid
Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	calloc(), free(), malloc()
Parameters:			short init_capacity(0 to SHRT_MAX -1) char inc_factor (1 to 255), char o_mode (Additive, multiplicative or fixed)
Return value:		Buffer Structure
Algorithm:
-Define pointer to buffer structure using calloc()
-Validate all parameters coming in,
-Allocate memory for one dynamic character buffer using malloc() with param init_capacity, validate charbuffer pointer, if not valid free all dynamically allocated memory and return Null
-According to the mode (additive, multiplicative or fixed)  and the value of infactor set the the buffer structure member variables(mode and infactor).
-Set buffer capacity  according to the param init capacity and return the pointer to buffer structure

**********************************************************************************************************************************/

Buffer *b_allocate(short init_capacity, char inc_factor, char o_mode) {
	/* Variable used as a pointer to buffer structure*/
	/* Allocating dynamic memory to one pointer pointing to a buffer structure */
	/* Calloc will also initilize all the member variables to zero*/
	pBuffer pBuffer = calloc(ONE, sizeof(Buffer));

	/*Check function parameters, if anyone of them are wrong free the pointer to buffer structure and return NULL */
	if (init_capacity < 0 || init_capacity >(SHRT_MAX - 1) || pBuffer == NULL || init_capacity == 0 && o_mode == 'f')
	{
		free(pBuffer);
		return NULL;
	}
	/*Check inc_factor for 0 and changed to fix mode if it is*/

	/*If init capacity is between range, allocate dynamic mem for a pointer to character buffer using function param init_capacity if no fault is found assign the pointer to cb_head */
	if (init_capacity > 0 && init_capacity <= SHRT_MAX - 1) {

		char * charbuffer = (char*)malloc(init_capacity * sizeof(char));
		if (charbuffer != NULL) {

			pBuffer->cb_head = charbuffer;
		}
		/*if fault is found in the dynamic memory allocation free the memory assigned to the character buffer and free the memory assigned to pBuffer pointing to the buffer structure*/
		else
		{

			free(charbuffer);
			free(pBuffer);
			return NULL;
		}
	}
	/*If the operative mode is fixed or the increment factor is 0, set the mode and the buffer inc_factor to zero*/
	if (o_mode == 'f' || (unsigned char)inc_factor == 0) {
		pBuffer->mode = ZERO;
		inc_factor = ZERO;
	}
	/*If the operative mode is fixed and the increment factor is not zero, set the mode and the buffer inc_factor to zero*/
	else if (o_mode == 'f' && (unsigned char)inc_factor != 0)
	{
		pBuffer->mode = FIX_SIZE_MODE;
		inc_factor = ZERO;
	}/* If the operative mode is additive and increment factor is in the range of 1 to 255 inclusive , set the mode to additive and the buffer increment factor to the value of param inc_factor*/
	else if (o_mode == 'a' && (unsigned char)inc_factor >= ONE && (unsigned char)inc_factor <= 255)
	{
		pBuffer->mode = ADDITIVE_MODE;
		pBuffer->inc_factor = inc_factor;
	}
	/*If the operative mode is multiplicative and increment factor is in the range of 1 to 100 inclusive, set the mode to multiplicative and the buffer inc_factor to the param increment factor value */
	else if (o_mode == 'm' && (unsigned char)inc_factor >= ONE && (unsigned char)inc_factor <= 100)
	{
		pBuffer->mode = MULTIPLICATIVE_MODE;
		pBuffer->inc_factor = inc_factor;
	}/*If the parameters do not meet any of these conditions, return NULL */
	else
	{
		return NULL;
	}
	/*Copies the param init_capacity value into the buffer capacity member variable and returns the pointer to buffer structure */
	pBuffer->capacity = init_capacity;
	return pBuffer;
}

/**************************************************************************************************************************************************
Purpose:            This function is used to add a symbol into the character buffer array.

Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	sizeof(), realloc
Parameters:			pBuffer const pBD, char symbol
Return value:		Pointer to Buffer Structure
Algorithm:
-Validate all parameters coming in,
-Use the mode of the buffer to assign it's capacity, validate capacity
- if not full add symbol to character buffer, increase offset if possible
-if the buffer is full calculate new capacity according to mode (additive, multiplicative) allocate dynamic memory for new capacity needed, validate it, assign it to buffer,
-add symbol to buffer if possible and increment if possible
-return a pointer to buffer structure

**************************************************************************************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol) {

	/*check parameters*/
	if (pBD == NULL) {
		return NULL;
	}
	short available_space = 0; /*Hold available space for buffer*/
	int new_increment = 0; /*New increment based on available space and increment factor*/
	char * temp_pointer = 0; /*Temporary pointer to assign new memory based on new capacity needed for buffer*/
	int addc_bytes = 0; /* addc_offset in bytes*/
	int new_capacity = 0; /*New capacity that will go be used to allocate dynamic mermory for buffer according to new capacity based on mode and calculations*/
	pBD->r_flag = 0;

							 /* if buffer mode is in fixed mode and is full return NULL*/

	if (pBD->mode == FIX_SIZE_MODE && b_isfull(pBD) == ONE) {
		return NULL;
	}
	/*If the capacity is equal to the total capacity, assign total capacity -1 to it according to datatype*/
	/*if (pBD->capacity == SHRT_MAX)
	{
		pBD->capacity = SHRT_MAX - 1;
	}
	/*If capacity is invalid return null*/
	/*if (pBD->capacity < ZERO) {
		return NULL;
	}*/
	/*Add c offset calculated in bytes because size of char may change according to platform */
	addc_bytes = (unsigned short)(pBD->addc_offset) *(sizeof(char));

	/*if the addc offset is smaller than the current capacity, go ahead and add the symbol to the character buffer, increment the offset and return the pointer to buffer structure*/
	if (addc_bytes < pBD->capacity)
	{
		/*Check for overflow*/
		if (pBD->cb_head != NULL) {

			pBD->cb_head[pBD->addc_offset] = symbol;
			pBD->addc_offset = pBD->addc_offset + 1;
			return pBD;
		}

	}
	/*If the addc_offset in bytes is bigger or equal to the current capacity and the buffer is in fixed size mode return NULL*/
	if (addc_bytes >= pBD->capacity && pBD->mode == FIX_SIZE_MODE) {
		return NULL;
	}

	/*If the buffer is already full and the buffer mode is in additive or multiplicative calculate the new capacity as required and validate it*/
	if (pBD->mode == ADDITIVE_MODE || pBD->mode == MULTIPLICATIVE_MODE)
	{
		/*If the buffer is in additive mode*/
		if (pBD->mode == ADDITIVE_MODE)
		{
			/*The new capacity equal the old capacity + incfactor converted in bytes */
			new_capacity = (pBD->capacity + (((unsigned char)pBD->inc_factor) * sizeof(char)));

			/*If the new capacity is equal to the total capacity, then set the new capacity to total capacity - 1 */
			if (new_capacity == SHRT_MAX) {
				new_capacity = SHRT_MAX - 1;

			}/*If the new capacity is invalid return NULL*/
			if (new_capacity == pBD->capacity || new_capacity < ZERO || new_capacity >= (SHRT_MAX - 1)) {
				return NULL;
			}
		}
		/*If the operational mode is multiplicative calculate the new capacity accordingly*/
		else if (pBD->mode == MULTIPLICATIVE_MODE)
		{
			/*the function tries to increase the current capacity using the following formula*/
			available_space = ((SHRT_MAX - 1) - pBD->capacity);
			new_increment = (available_space * (unsigned char)pBD->inc_factor);
			new_increment = (new_increment / 100);
			new_capacity = pBD->capacity + new_increment;

			/*If the new increment is zero and the capacity is not max out, set it to (SHRT_MAX - 1)*/
			if (new_increment == ZERO && pBD->capacity != SHRT_MAX)
			{
				new_capacity = (SHRT_MAX - 1);
			}
		}
		/*If the new capacity is smaller than zero or bigger than total capacity allowed set it to total capacity - 1 according to datatype */
		if (new_capacity > (SHRT_MAX - 1))
		{
			new_capacity = (SHRT_MAX - 1);
		}
		/*Re-Allocated dynamic memory for character buffer using new capacity and make temp_pointer point to it */
		temp_pointer = realloc(pBD->cb_head, new_capacity);
		if (temp_pointer == NULL) {
			return NULL;
		}

		/*If there was a change in address compared to the old character buffer, set flag accordingly*/
		if (pBD->cb_head != temp_pointer)
		{
			pBD->r_flag = 1;
		}
		/*If the memory reallocation was successful make the character buffer point to the new memory address, assign the new capacity to the buffer member variable capacity*/
		if (temp_pointer != NULL)
		{
			pBD->cb_head = temp_pointer;
			pBD->capacity = (short)new_capacity;
			/*if there is still splace in the character buffer, add the symbol*/
			if (addc_bytes < new_capacity) {
				pBD->cb_head[pBD->addc_offset] = symbol;
				/*if it's possible to increment the offset(smaller or equal current capacity do it*/
				if (addc_bytes <= new_capacity) {
					pBD->addc_offset++;
					/*Returns a pointer to the Buffer structure*/
					return pBD;
				}
			}
		}
		/*Return NUll if anything failed*/
		return NULL;
	}
	/*Return null if anything failed*/
	return NULL;
}
/*************************************************************************************************************************************************************************
Purpose:            This function is used to clear the buffer. It re-initializes all the needed members of the buffer desciptor, so that it appear empty.
The next b_addc() should put the character at the beginning of character buffer. the function does not clear the existing content in the buffer.
If there's an error, the function returns -1 for failure.


Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	None
Parameters:			pBuffer const pBD,
Return value:		Integer value (0 or 1)
Algorithm:
-Validate pointer to Buffer Structure coming in.
-Re-initilialize data members so that next call puts addc to the beginning of the character buffer.
-Re-initialize all the appropriate variable according not to existing content from the buffer.

***************************************************************************************************************************************************************************/
int b_clear(Buffer * const pBD) {
	/*re-initializes all appropriate data members of the given Buffer structure (buffer descriptor*/
	if (pBD != NULL) {
		pBD->addc_offset = ZERO;
		pBD->getc_offset = ZERO;
		pBD->eob = ZERO;
		pBD->markc_offset = ZERO;
		pBD->r_flag = ZERO;
		return ZERO;
	}
	return RT_FAIL1;
}
/*************************************************************************************************************************************************************************************
Purpose:            This function is used to free all dynamic memory allocated for the buffer. Hence, the function frees the character buffer and the buffer descriptor if possible.


Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	free()
Parameters:			pBuffer const pBD,
Return value:		none(void)
Algorithm:
-Validate pointer to Buffer Structure coming in.
-Re-initilialize data members so that next call puts addc to the beginning of the character buffer.
-Re-initialize all the appropriate variable according not to existing content from the buffer.

********************************************************************************************************************************************************/
void b_free(Buffer * const pBD) {
	/*de-allocates (frees) the memory occupied by the character buffer and the Buffer structure (buffer descriptor) if the buffer is not null*/
	if (pBD != NULL) {

		if (pBD->cb_head != NULL) {
			free(pBD->cb_head);
			free(pBD);
		}
	}
}
#ifndef B_FULL /*If the user does not use the macro make sure to undefine the macro and use the function below*/
#undef b_isfull
/**************************************************************************************************************************************************8
Purpose:            This function is used to know ifthe character buffer is full if possible.


Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	none()
Parameters:			pBuffer const pBD,
Return value:		Integer
Algorithm:
-Validate pointer to Buffer Structure coming in.
-Check if addc_offset and capacity are the same value, return the appropriate value based on the result.
***********************************************************************/
int b_isfull(Buffer * const pBD) {
	if (pBD != NULL) {

		short addc_offsetinbytes = pBD->addc_offset * sizeof(char);
		/*function returns 1 if the character buffer is full*/
		if (addc_offsetinbytes == pBD->capacity)
		{
			//printf("In function b_isfull");
			return ONE;
		}
		/*Return 0 otherwise*/
		//printf("In function b_isfull");
		return ZERO;
	}
	/*return -1 if a run time error happens*/
	return RT_FAIL1;
}
#endif 
/***********************************************************************
Purpose:            This function returns the current limit of the character buffer if possible.


Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	none()
Parameters:			pBuffer const pBD,
Return value:		Short
Algorithm:
-Validate pointer to Buffer Structure coming in.
-Compare addc_offset with capacity and return the limit if appropriate.
***********************************************************************/
short b_limit(Buffer* const pBD) {
	/*Validate pointer to buffer structure and make sure it's not equal to NULL*/
	if (pBD != NULL) {

		/*Check to make sure addc_offset is never larger than current limit,
		which is the amount of space measured in chars begin used by all the characters stored in the character buffer.*/
		/*if (pBD->addc_offset <= pBD->capacity) {
			/*Return addc_offset offset*/
			return(pBD->addc_offset);
		/*}*/
		/*Otherwise return Failure*/
	

	}
	/*Return Failure if buffer is null*/
	return RT_FAIL1;
}
/******************************************************************************************************
Purpose:            This function is used to return the current capacity of the buffer if possible.


Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	none()
Parameters:			pBuffer const pBD,
Return value:		Short capacity from character buffer or failure
Algorithm:
-Validate pointer to Buffer Structure coming in.
-Return capacity
************************************************************************************************************/
short b_capacity(Buffer* const pBD) {
	/*If the buffer is valid, return the capacity*/
	if (pBD != NULL) {

		return pBD->capacity;
	}
	/*Error return -1*/
	return RT_FAIL1;
}
/***********************************************************************
Purpose:            This function is used to sets markc_offset to mark. Mark must be witing within the current limit of the buffer.
Returns markc_offset when complete.


Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	none()
Parameters:			pBuffer const pBD, buffer short mark or failure
Return value:		short marc_offset from character buffer, or failure
Algorithm:
-Validate pointer to Buffer Structure coming in.
-If mark is within the current limit of the buffer, set the buffer markc_offset to mark and return the markc_offset from the buffer.
***********************************************************************/
short b_mark(Buffer * const pBD, short mark) {
	/*Validate pointer to buffer strucure*/
	if (pBD != NULL) {
		/*The function sets markc_offset to mark. Must be within the current limit of the buffer (0 to addc_offset inclusive). The function returns the currently set markc_offset. If a run-time error is possible, the function should return NULL*/
		if (mark >= ZERO && mark <= (pBD->addc_offset)) {
			pBD->markc_offset = mark;
			return pBD->markc_offset;
		}
		return RT_FAIL1;
	}
	return RT_FAIL1;
}
/*************************************************************************************************************************************************
Purpose:            This function is used to return the value of mode. If a failure happends, notify the calling function by returning failure.



Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	none()
Parameters:			pBuffer const pBD
Return value:		short marc_offset from character buffer, or failure
Algorithm:
-Validate pointer to Buffer Structure coming in.
-Return mode from buffer structure
***************************************************************************************************************************************************/
int b_mode(Buffer * const pBD) {
	/*returns the value of mode to the calling function. If a run-time error is possible, the function should notify the calling function about the failure*/
	if (pBD != NULL) {
		return pBD->mode;
	}
	return RT_FAIL2;
}
/*************************************************************************************************************************************************
Purpose:            This function is used to return the non negative value of inc_factor, if there's an error return 256



Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	none()
Parameters:			pBuffer const pBD,
Return value:		unsigned char for inc_factor and integer value of 256 for failure
Algorithm:
-Validate pointer to Buffer Structure coming in.
-Return inc_factor from pointer to buffer structure, if a failure happends return appropriate value
***************************************************************************************************************************************************/
size_t b_incfactor(Buffer* const pBD) {
	/*Validate pointer to buffer structure*/
	if (pBD != NULL) {
		/*cast incfactor to unsigned char in order to return non negative numbers*/
		return (unsigned char)pBD->inc_factor;
	}
	/*Return 256 if the pointer to buffer structure is not valid*/
	return 256;
}
/*************************************************************************************************************************************************
Purpose:            This function reads from an input file specified by param fi into the buffer structure. This function uses fgetc(fi) characters and uses b_add()c to add the characters in the buffer.
If the character can't fit into the buffer, the function returns -2 for a loading failure. This keeps on going until feof(fi) is hit telling the function it's at the end of the file.
The end of file character is not added to the character buffer. If an error occurs return failure.



Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	fgetc(), feof(), b_addc()
Parameters:			file *const fi and Buffer* const pBD,
Return value:		Integer count of chars added, or failure
Algorithm:
-Validate pointer to Buffer Structure coming in.
-Validate file coming in
-Check for end of file
-Try to add character from file to buffer, if successful grab the next char and do the same thing,
if not return failure, when completed successfully return the number of chars successfully added.
***************************************************************************************************************************************************/
int b_load(FILE * const fi, Buffer*const pBD) {

	int symbol = ZERO; /*Integer variable to store symbol*/
	int added_symbol_count = ZERO;/*Integer variable to count amount of chars added to character buffer*/

								  /*If the pointer points to a valid buffer structure*/
	if (pBD != NULL)
	{
		/*If the file was loaded successful*/
		if (fi != NULL)
		{
			/*Keep adding characters until we reach the end of file*/
			while (1)
			{
				symbol = fgetc(fi); /*put that char in the symbol variable*/
									/*Check for end of file, break if it is*/
				if (feof(fi)) {
					break;
				}
				/*Try to add the symbol casted to char to the character buffer if it fails return LOAD_FAIL*/
				if ((b_addc(pBD, (char)symbol)) == NULL) {
					return LOAD_FAIL;
				}
				/*Increment the symbol counter*/
				added_symbol_count++;
			}
			/*Return symbol*/
			return added_symbol_count;
		}
		/*Return -1 for Failure*/
		return RT_FAIL1;
	}
	/*Return -1 for Failure*/
	return RT_FAIL1;
}
/*************************************************************************************************************************************************
Purpose:            This function is used to see if the buffer is empty, returns a value accordingly



Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	None
Parameters:		    Buffer* constpBD,
Return value:		Integer value 1, 0 or -1 for failure
Algorithm:
-Validate pointer to Buffer Structure coming in.
-Check value of addc_offset and return value accordingly.

***************************************************************************************************************************************************/
int b_isempty(Buffer * const pBD) {
	/*Validate pointer to buffer structure coming in*/
	if (pBD != NULL)
	{
		/*If the addc offset is Zero return 1, otherwise 0*/
		if (pBD->addc_offset == ZERO) {
			return ONE;
		}
		return ZERO;
	}
	/*Returning -1 for failure*/
	return RT_FAIL1;
}
/*************************************************************************************************************************************************
Purpose:            This function is used to return eob



Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	None
Parameters:		    Buffer * const pBD,
Return value:		integer end of buffer value or -1 for failure
Algorithm:
-Validate pointer to Buffer Structure coming in.
-Returns eob value from Buffer strucure, return -1 for failure

***************************************************************************************************************************************************/
int b_eob(Buffer* const pBD) {
	/*Validate pointer to buffer structure coming in*/
	if (pBD != NULL) {
		/*Return eob value from buffer structure*/
		return pBD->eob;
	}
	/*Return -1 for failure*/
	return RT_FAIL1;
}
/*************************************************************************************************************************************************
Purpose:            This function gets the character located at get_coffset after validation, increments offset before returning. If any errors are found it returns a value accordingly.



Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	None
Parameters:		    Buffer * const pBD,
Return value:		character from character buffer or value from from failure(-1 if at end, -2 for failure)
Algorithm:
-Validate pointer to Buffer Structure coming in.
-compare getc_offset and addc_offset and set eob for buffer and return -1
-Otherwise assign 0 to eob
-retrieve character from buffer, increase offset and return.
-return -2 for failure

***************************************************************************************************************************************************/
char b_getc(Buffer *const pBD) {
	char c; /*Variable that will hold character from character buffer*/

			/*Validate pointer to buffer structure*/
	if (pBD != NULL) {
		/*Compare getc_offset and addc_offset if thery are the same set eob to 1 and return -1*/
		if (pBD->getc_offset == pBD->addc_offset) {
			pBD->eob = ONE;
			return RT_FAIL1;
		}
		/*Otherwise set eob to zero*/
		pBD->eob = ZERO;
		/*assign the char at getcoffset in the character buffer and assign it to c*/
		c = pBD->cb_head[pBD->getc_offset];
		/*Increment get c_offset*/
		pBD->getc_offset++;
		/*Return character from character buffer*/
		return c;
	}
	return RT_FAIL2;
}
/*************************************************************************************************************************************************
Purpose:            This function is used to print character by character from the contents of buffer.



Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions: 	b_eob(), b_getc(), printf(), b_limit()
Parameters:			Buffer* const pBD,
Return value:		Integer count of chars printed, or failure -1 for empty buffer
Algorithm:
-Get character using b_getc
-check if buffer is empty, if not's print character.
-Get the next character using b_getc
-Increment counter and loop again until we reach the end of the buffer
***************************************************************************************************************************************************/
int b_print(Buffer * const pBD) {
	char c = ZERO; /*Variable that will hold character from character buffer*/
	int counter = 0; /*Counter to count amount of chars printed*/

					 /*if the buffer is valid and the limit is not reached get print */
	if (pBD != NULL && b_limit(pBD) != ZERO)
	{
		c = b_getc(pBD);/*get a char from char buffer and assign it to variable*/
						/* If the of the buffer is not reached print*/
		while (b_eob(pBD) == ZERO)
		{
			/*Check if the end of the buffer is reached*/
			if (b_eob(pBD) == ZERO)
			{
				printf("%c", c); /*Print char if it's not*/
				c = b_getc(pBD); /*Get new char after that and increment counter*/
				counter++;
			}
		}
		printf("\n"); /*Print space after */
		return counter;
	}
	printf("Empty buffer\n");
	return RT_FAIL1;
}
/*************************************************************************************************************************************************
Purpose:            This function is used to print character by character from the contents of buffer.



Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions:   sizeof()
Parameters:			Buffer* const pBD, char symbol
Return value:		Pointer to buffer structure

Algorithm:         -Validate pointer to buffer structure
-Calculate new capacity and assign it to a variable
-Create a temporary pointer and reallocate memory for cb_head using new capacity
-If the temp pointer memory reallocation is successful, check it's address and if it's different from the original character buffer memory address
set the flag accordingly.
-set pointer to buffer
-assign the new capacity to the buffer's capacity
-add the symbol to the character buffer
-Increment the offset
-return pointer to buffer structure

***************************************************************************************************************************************************/
Buffer* b_compact(Buffer * const pBD, char symbol) {

	short new_capacity; /* Holds the new calculated capacity for the character buffer*/
	char * temp_pointer; /*temp pointer for dynamic memory re-allocation using new capacity for character buffer. */

	if (pBD != NULL)
	{
		/*The new capacity is the current limit plus a space for one more character.*/
		new_capacity = ((pBD->addc_offset + 1) * sizeof(char));
		/*if there's a type overflow*/
		if (new_capacity < 0) {
			return NULL;
		}
		/*The function uses realloc() to adjust the new capacity.*/
		temp_pointer = (char*)realloc(pBD->cb_head, new_capacity);

		/*If the dynamic memory reallocation was succesful, check for a change in memory address, if there's one, set flag accordingly*/
		if (temp_pointer != NULL)
		{
			if (pBD->cb_head != temp_pointer) {
				pBD->r_flag = SET_R_FLAG;
			}
			pBD->r_flag = 0;
			/*Assign head to point to memory address temp points too*/
			pBD->cb_head = temp_pointer;
			/*Assign the new capacity to the buffer capacity*/
			pBD->capacity = new_capacity;
			/*add the symbol at the end of the buffer and increment offset*/
			pBD->cb_head[pBD->addc_offset] = symbol;
			pBD->addc_offset++;
			return pBD;
		}
		/*Return Null if there was any errors when reallocating the memory for the character buffer*/
		return NULL;
	}
	return NULL;
}
/*************************************************************************************************************************************************
Purpose:            This function returns r_flag.



Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions:   None
Parameters:			Buffer* const pBD
Return value:		char(r_flag) or -1 for failure

Algorithm:         -Validate pointer to buffer structure
-return r_flag from the buffer structure

***************************************************************************************************************************************************/
char b_rflag(Buffer * const pBD) {
	if (pBD != NULL) {
		return pBD->r_flag;
	}
	return RT_FAIL1;
}
/*************************************************************************************************************************************************
Purpose:            This function decrements getc_offset by 1 and returns getc_offset if there's no error



Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions:   None
Parameters:			Buffer* const pBD
Return value:		short(retract) or -1 for failure

Algorithm:         -Validate pointer to buffer structure
-decrement getc_offset from buffer member variable
-return getc_offset from buffer member variable if there's no errors

***************************************************************************************************************************************************/
short b_retract(Buffer * const pBD) {
	if (pBD != NULL) {
		if (pBD->getc_offset >= 1) {
			pBD->getc_offset = pBD->getc_offset - 1;
		}
		return pBD->getc_offset;
	}
	return RT_FAIL1;
}
/*************************************************************************************************************************************************
Purpose:            This function sets getc_offset to the value of the current markc_offset and then proceeds to return getc_offset if no errors were found



Author:		        Billy Saint-Fort
History/Versions:   Version 1
Called functions:   None
Parameters:			Buffer* const pBD
Return value:		short(getc_offset)

Algorithm:         -Validate pointer to buffer structure
-set buffer member variable getc_offset tp buffer member variable markc_offset
-return getc_offset from the buffer structure
-return -1 if there was an error

***************************************************************************************************************************************************/
short b_reset(Buffer * const pBD) {
	if (pBD != NULL) {
		pBD->getc_offset = pBD->markc_offset;
		return pBD->getc_offset;
	}
	return RT_FAIL1;
}
/*************************************************************************************************************
Purpose:            This function returns getc_offset if there's no errors



Author :             Billy Saint-Fort
History / Versions:  Version 1
Called functions:    None
Parameters:          Buffer* const pBD
Return value:        short(getc_offset) or -1 for failure

Algorithm :        -Validate pointer to buffer structure
-return getc_offset from buffer member variable if there's no errors
***************************************************************************************************************************************************/
short b_getcoffset(Buffer * const pBD) {
	if (pBD != NULL) {
		return pBD->getc_offset;
	}
	return RT_FAIL1;
}
/*************************************************************************************************************
Purpose:            This function sets the getc_offset and the markc_offset so that the buffer can be read again if there's no error.



Author :             Billy Saint-Fort
History / Versions:  Version 1
Called functions:    None
Parameters:          Buffer* const pBD
Return value:        int for success or -1 for failure

Algorithm :         -Validate pointer to buffer structure
-set getc_offset and markc_offset to 0 and return.
***************************************************************************************************************************************************/
int b_rewind(Buffer * const pBD) {
	if (pBD != NULL) {
		pBD->getc_offset = ZERO;
		pBD->markc_offset = ZERO;
		return ZERO;
	}
	return RT_FAIL1;
}
/*************************************************************************************************************
Purpose:            This function returns a pointer to a location of the character buffer indicated by ioc_offset.(beginning of



Author :             Billy Saint-Fort
History / Versions:  Version 1
Called functions:    None
Parameters:          Buffer* const pBD, short ioc_offset
Return value:        Pointer to location of cb buffer if possible.

Algorithm :         -Validate pointer to buffer structure
-return pointer to location of character buffer

***************************************************************************************************************************************************/
char *b_location(Buffer * const pBD, short loc_offset) {
	if (pBD != NULL) {
		if (loc_offset >= 0 && loc_offset <= pBD->addc_offset) {
			return (pBD->cb_head + loc_offset);
		}
		
	}
	return NULL;
}