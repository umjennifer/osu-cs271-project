 TITLE String Primitives & Macros Program     (Proj6_umj.asm)

; Author: Jennifer Um
; Last Modified: 2022-03-04
; OSU email address: umj@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6                Due Date: 2022-03-13
; Description:	Prompt and receive ten valid SDWORDs from a user, then display the numbers, the sum, and the average.
;				Program will use macros to read and write strings.
;				ReadInt, ReadDec, WriteInt, and WriteDec are not allowed in this program.

mGetString MACRO m_prompt, m_input_str, m_input_str_len, m_input_str_maxlen
	push EDX
	push ECX
	push EAX

	mov EDX, m_prompt
	call WriteString

	mov EDX, m_input_str
	mov ECX, m_input_str_maxlen
	call ReadString
	mov m_input_str_len, EAX
	;call CRLF

	pop EAX
	pop ECX
	pop EDX
ENDM

mDisplayString MACRO m_str_memory
	PUSH  EDX
	MOV   EDX, m_str_memory
	CALL  WriteString
	POP   EDX
ENDM

INCLUDE Irvine32.inc

.data
LO						EQU -2147483648
MAXLEN					EQU 50
VALID					EQU 0
INVALID					EQU 1
DESIREDVALIDCOUNTTOTAL	EQU 10
ASCIIDECIMALOFMINUSSIGN EQU 45

special_case_int_array	SDWORD	2,1,4,7,4,8,3,6,4,8 ; int array representation of absolute value of -2147483648 (most negative valid SD num)
intro_title				BYTE	"PROGRAMMING ASSIGNMENT 6: Designing low-level I/O procedures",13,10,"Written by: Jennifer Um",13,10,13,10,0
intro_info				BYTE	"Please provide 10 signed decimal integers.",13,10,"Each number needs to be small enough to fit inside a 32 bit register.",13,10,"After you have finished inputting the raw numbers I will display a list of the integers, their sum, and their average value.",13,10,13,10,0
goodbye					BYTE	13,10,13,10,"Thanks for playing!",13,10,0
prompt					BYTE	"Please enter an signed number: ",0
input_error				BYTE	"ERROR: You did not enter a signed number or your number was too big.",13,10,0
array_title				BYTE	13,10,"You entered the following numbers:",13,10,0
sum_title				BYTE	13,10,"The sum of these numbers is: ",0
average_title			BYTE	13,10,"The truncated average is: ",0
comma_and_space			BYTE	", ",0
period_and_space		BYTE	". ",0
extra_credit_info		BYTE	"** EC: Number each line of user input and display a running subtotal of the user’s valid numbers. These displays must use WriteVal.",13,10,13,10,0
valid_numbers_array		SDWORD	DESIREDVALIDCOUNTTOTAL DUP(0)
reverse_input_int_array	SDWORD	MAXLEN DUP(0)
multiplicand_int_array	SDWORD	MAXLEN DUP(0)
input_int_array			SDWORD	MAXLEN DUP(0)
reverse_int_array		DWORD	MAXLEN DUP(0)
reverse_ascii_str		BYTE	MAXLEN DUP(0)
input_str				BYTE	MAXLEN DUP(0)
reverse_input_str		BYTE	MAXLEN DUP(0)
ascii_str				BYTE	MAXLEN DUP(0)
input_int				SDWORD	0
num_of_digits			DWORD	0
index_tracker			DWORD	0
input_state				DWORD	INVALID
multiplier				SDWORD	1
valid_number_count		DWORD	0
valid_numbers_sum		SDWORD	0
valid_numbers_average	SDWORD	0
input_char_as_int		SDWORD	?
quotient				SDWORD	?
remainder				SDWORD	?
input_str_len			DWORD	?

.code
main PROC
	; print introduction
	push OFFSET extra_credit_info
	push OFFSET intro_info
	push OFFSET intro_title
	call Introduction

	_getNumbersLoop:
		mov EAX, valid_number_count
		inc EAX

		; EC: print each line of user input and display a running subtotal of the user’s valid numbers
		push OFFSET index_tracker
		push OFFSET ascii_str
		push OFFSET reverse_ascii_str
		push OFFSET reverse_int_array
		push EAX
		call WriteVal
		mDisplayString OFFSET period_and_space

		; get the input
		push OFFSET special_case_int_array
		push OFFSET input_int_array
		push OFFSET multiplicand_int_array
		push OFFSET input_int
		push OFFSET index_tracker
		push OFFSET input_error
		push OFFSET reverse_input_int_array
		push OFFSET multiplier
		push OFFSET num_of_digits
		push MAXLEN
		push OFFSET input_state
		push OFFSET input_char_as_int
		push OFFSET reverse_input_str
		push OFFSET input_str_len
		push input_str_len
		push OFFSET input_str
		push OFFSET prompt
		call ReadVal

		cmp input_state, INVALID
		je _getNumbersLoop	; input_state = INVALID
		; at this point, input_state is valid

		; store input_int into the array
		mov EAX, valid_number_count		; this is also the index to add the element
		mov EBX, 4						; type of SDWORD
		mul EBX							; EAX = index * type
		mov EBX, OFFSET valid_numbers_array
		add EBX, EAX					; EBX = OFFSET valid_numbers_array + (index * type)
		mov EDI, EBX					; EDI = OFFSET valid_numbers_array + (index * type)
		mov EAX, input_int
		mov [EDI], EAX

		; increment valid number count
		mov EAX, valid_number_count
		inc EAX
		mov valid_number_count, EAX

		; check if we have enough valid numbers
		cmp EAX, 10
		jne _getNumbersLoop			; valid_number_count < 10
		jmp _calculateValidNumbers	; valid_number_count > 10

	_calculateValidNumbers:
		; calculate the sum
		push OFFSET index_tracker
		push OFFSET valid_numbers_sum
		push OFFSET valid_numbers_array
		call SumValidNumbers

		; calculate the average
		push OFFSET valid_numbers_sum
		push OFFSET valid_numbers_average
		call AverageValidNumbers

		; reset index_tracker to 0
		mov EAX, 0
		mov index_tracker, EAX

		; print the results of the calculation
		push OFFSET comma_and_space
		push OFFSET index_tracker
		push OFFSET ascii_str
		push OFFSET reverse_ascii_str
		push OFFSET reverse_int_array
		push valid_numbers_sum
		push valid_numbers_average
		push OFFSET valid_numbers_array
		push OFFSET array_title
		push OFFSET sum_title
		push OFFSET average_title
		call PrintResults
		
		; print farewell
		push OFFSET goodbye
		call Farewell

	Invoke ExitProcess,0	; exit to operating system
main ENDP

; -- WriteVal --
; Turn the SDWORD int into a string and print it
; preconditions: the following should already be set
;	- [EBP+8] int
; postconditions
;	- none
; receives:
;	- [EBP+24] OFFSET index_tracker
;	- [EBP+20] OFFSET ascii_str
;	- [EBP+16] OFFSET reverse_ascii_array
;	- [EBP+12] OFFSET reverse_int_array
;	- [EBP+8] int
; return
;	- [EBP+24] OFFSET index_tracker
;	- [EBP+20] OFFSET ascii_str
;	- [EBP+16] OFFSET reverse_ascii_array
;	- [EBP+12] OFFSET reverse_int_array
WriteVal PROC
	push EBP
	mov EBP, ESP
	push EAX
	push EBX
	push ECX
	push EDX
	push ESI
	push EDI

	; reset index_tracker
	mov EDI, [EBP+24]	;OFFSET index_tracker
	mov EAX, 0
	mov [EDI], EAX
	
	mov EAX, [EBP+8]	; int to convert to string

	cmp EAX, 0
	jl _negativeNumber	; EAX < 0
	jmp _positiveNumber ; EAX >= 0

	_positiveNumber:
		mov ECX, 0
		mov EDI, [EBP+12]	;OFFSET reverse_int_array
		_positiveNumDividebyTenLoop:
			CDQ
			mov EBX, 10		; divisor
			idiv EBX		; EAX=quotient, EDX= negative remainder

			add EDX, 48		; EDX = postive remainder int + 48 = ASCII decimal representation
			mov [EDI], EDX

			add EDI, 4
			inc ECX

			cmp EAX, 0
			jne _positiveNumDividebyTenLoop ; EAX != 0
			jmp _fillReverseASCIIStr		; EAX = 0 at this point
	
	_negativeNumber:
		mov ECX, 0
		mov EDI, [EBP+12]		;OFFSET reverse_int_array
		_negativeNumDividebyTenLoop:
			CDQ
			mov EBX, 10			; divisor
			idiv EBX			; EAX=quotient, EDX= negative remainder
			push EAX	; quotient
			mov EAX, -1 
			mov EBX, EDX		; EBX = negative remainder
			imul EBX			; EAX = negative remainder * -1 = positive remainder
			add EAX, 48			; EAX = postive remainder int + 48 = ASCII decimal representation
			mov [EDI], EAX

			add EDI, 4
			inc ECX

			pop EAX		; quotient

			cmp EAX, 0
			jne _negativeNumDividebyTenLoop ; EAX != 0
			jmp _appendMinusSignASCII; EAX = 0 at this point

		_appendMinusSignASCII:
			mov EAX, ECX		; ECX currently holds the number of digits in the number and the index to add the int rep. of '-'
			mov EBX, 4			; Type of WORD
			mul EBX				; EAX = size of type * index
			mov EBX, [EBP+12]	;OFFSET reverse_int_array
			add EBX, EAX		; EBX = OFFSET reverse_int_array + (size of type * index)
			mov EDI, EBX		; EDX = OFFSET reverse_int_array + (size of type * index)
			mov EAX, ASCIIDECIMALOFMINUSSIGN
			mov [EDI], EAX

			; increment ECX
			inc ECX

			jmp _fillReverseASCIIStr
			
	; move BYTE values of reverse_int_array into reverse_ascii_array
	; ECX = length of the reverse_int_array at this point
	_fillReverseASCIIStr:
		; populate index_tracker
		mov EDI, [EBP+24] ; OFFSET index_tracker
		mov [EDI], ECX
	
		mov EDX, 0
		_fillReverseASCIIStrLoop:

			; get OFFSET of index in reverse_int_array being read
			mov EAX, EDX		; EAX = index
			mov EBX, 4			; type of DWORD
			push EDX
			mul EBX				; EAX = type of DWORD * index
			pop EDX
			mov EBX, [EBP+12]	;OFFSET reverse_int_array
			add EBX, EAX		; EBX = OFFSET reverse_int_array + (type of DWORD * index)
			mov ESI, EBX		; ESI = OFFSET reverse_int_array + (type of DWORD * index)

			; get OFFSET of index in reverse_ascii_array being updated
			mov EAX, EDX		; EAX = index
			mov EBX, 1			; type of BYTE
			push EDX
			mul EBX				; EAX = type of BYTE * index
			pop EDX
			mov EBX, [EBP+16]	;OFFSET reverse_ascii_str
			add EBX, EAX		; EBX = OFFSET reverse_ascii_str + (type of BYTE * index)
			mov EDI, EBX		; EBX = OFFSET reverse_ascii_str + (type of BYTE * index)

			mov EAX, [ESI] ; OFFSET reverse_int_str + (type of DWORD * index)
			LODSB
			STOSB

			; increase the index
			inc EDX
			loop _fillReverseASCIIStrLoop

	_fillASCIIStr:
		mov ESI, [EBP+24]	; OFFSET index_tracker
		mov ECX, [ESI]

		mov ESI,[EBP+16]	;OFFSET reverse_ascii_str
		add ESI, ECX
		dec ESI
		mov EDI, [EBP+20]	; OFFSET ascii_str
		_fillASCIIStrLoop:
			STD
			LODSB
			CLD
			STOSB
			loop _fillASCIIStrLoop

	_appendNullChar:
		mov ESI, [EBP+24] ; OFFSET index_tracker
		mov ECX, [ESI]
		mov EAX, 0
		mov EDI, [EBP+20] ; OFFSET ascii_str
		add EDI, ECX
		STOSB

	mDisplayString [EBP+20] ; OFFSET ascii string

		pop EDI
		pop ESI
		pop EDX
		pop ECX
		pop EBX
		pop EAX
		pop EBP
		RET 20
WriteVal ENDP

; -- PrintResults --
; Print the results (numbers in array, average, and sum).
; preconditions: the following should already be set
;	- [EBP+48] OFFSET comma_and_space
;	- [EBP+28] valid_numbers_sum
;	- [EBP+24] valid_numbers_average
;	- [EBP+20] OFFSET valid_numbers_array
;	- [EBP+16] OFFSET array_title
;	- [EBP+12] OFFSET sum_title
;	- [EBP+8] OFFSET average_title
; postconditions
;	- none
; receives:
;	- [EBP+48] OFFSET comma_and_space
;	- [EBP+44] OFFSET index_tracker
;	- [EBP+40] OFFSET ascii_str
;	- [EBP+36] OFFSET reverse_ascii_str
;	- [EBP+32] OFFSET reverse_int_array
;	- [EBP+28] valid_numbers_sum
;	- [EBP+24] valid_numbers_average
;	- [EBP+20] OFFSET valid_numbers_array
;	- [EBP+16] OFFSET array_title
;	- [EBP+12] OFFSET sum_title
;	- [EBP+8] OFFSET average_title
; returns:
;	- [EBP+48] OFFSET comma_and_space
;	- [EBP+44] OFFSET index_tracker
;	- [EBP+40] OFFSET ascii_str
;	- [EBP+36] OFFSET reverse_ascii_str
;	- [EBP+32] OFFSET reverse_int_array
PrintResults PROC
	push EBP
	mov EBP, ESP
	push EAX
	push EBX
	push ECX
	push EDX
	push ESI
	push EDI

	; display array
	mDisplayString [EBP+16] ; display array_title

	mov ECX, DESIREDVALIDCOUNTTOTAL
	;mov ESI, [EBP+20] ;OFFSET valid_numbers_array
	mov EDX, 0 ; used for indexing
	_printArrayValueLoop:
		mov EAX, EDX ; index of number to print
		mov EBX, 4 ; type of DWORD
		push EDX
		mul EBX ; EAX = type * index
		pop EDX
		mov EBX, [EBP+20] ;OFFSET valid_numbers_array
		add EBX, EAX ; EBX = OFFSET valid_numbers_array + (type * index)
		mov ESI, EBX ; ESI = OFFSET valid_numbers_array + (type * index)
		mov EAX, [ESI]

		; get the string of the number 
		push [EBP+44] ;OFFSET index_tracker
		push [EBP+40] ;OFFSET ascii_str
		push [EBP+36] ;OFFSET reverse_ascii_array
		push [EBP+32] ;OFFSET reverse_int_array
		push EAX
		Call WriteVal
		;mDisplayString [EBP+40] ; OFFSET ascii string
		
		; determine if we need to print comma and space
		cmp EDX, DESIREDVALIDCOUNTTOTAL-1
		je _continuePrintArrayValueLoop ; EDX = 10
		mDisplayString [EBP+48] ; EDX != 10. OFFSET comma_and_space

		_continuePrintArrayValueLoop:
			inc EDX ; update to next index
			loop _printArrayValueLoop

	; display sum
	mDisplayString [EBP+12] ; display sum_title
	push [EBP+44] ;OFFSET index_tracker
	push [EBP+40] ;OFFSET ascii_str
	push [EBP+36] ;OFFSET reverse_ascii_array
	push [EBP+32] ;OFFSET reverse_int_array
	mov EAX, [EBP+28] ; valid_numbers_sum
	push EAX
	Call WriteVal
	;mDisplayString [EBP+40]


	; display average
	mDisplayString [EBP+8] ; display array_title
	push [EBP+44] ;OFFSET index_tracker
	push [EBP+40] ;OFFSET ascii_str
	push [EBP+36] ;OFFSET reverse_ascii_array
	push [EBP+32] ;OFFSET reverse_int_array
	mov EAX, [EBP+24] ; valid_numbers_average
	push EAX
	Call WriteVal
	;mDisplayString [EBP+40]

	pop EDI
	pop ESI
	pop EDX
	pop ECX
	pop EBX
	pop EAX
	pop EBP
	RET 44
PrintResults ENDP

; -- AverageValidNumbers --
; Average the numbers in the array.
; preconditions: the following should already be set
;	- [EBP+12] OFFSET valid_numbers_sum
;	- constant DESIREDVALIDCOUNTTOTAL should be set
; postconditions
;	- none
; receives:
;	- [EBP+12] OFFSET valid_numbers_sum
;	- [EBP+8] OFFSET valid_numbers_array
; returns
;	- [EBP+8] OFFSET valid_numbers_average
AverageValidNumbers PROC
	push EBP
	mov EBP, ESP
	push EAX
	push EBX
	push ECX
	push EDX
	push ESI
	push EDI

	mov ESI, [EBP+12]	;OFFSET valid_numbers_sum
	mov EAX, [ESI]
	CDQ
	mov EBX, DESIREDVALIDCOUNTTOTAL
	IDIV EBX
	mov EDI, [EBP+8]	; OFFSET valid_numbers_average
	mov [EDI], EAX

	pop EDI
	pop ESI
	pop EDX
	pop ECX
	pop EBX
	pop EAX
	pop EBP
	RET 8
AverageValidNumbers ENDP

; -- SumValidNumbers --
; Sum the numbers in the array.
; preconditions: the following should already be set
;	- [EBP+16] OFFSET index_tracker
;	- [EBP+12] OFFSET valid_numbers_sum
;	- [EBP+8] OFFSET valid_numbers_array
;	- constant DESIREDVALIDCOUNTTOTAL should be set
; postconditions
;	- none
; receives:
;	- [EBP+16] OFFSET index_tracker
;	- [EBP+12] OFFSET valid_numbers_sum
;	- [EBP+8] OFFSET valid_numbers_array
; returns
;	- [EBP+16] OFFSET index_tracker
;	- [EBP+12] OFFSET valid_numbers_sum
SumValidNumbers PROC
	push EBP
	mov EBP, ESP
	push EAX
	push EBX
	push ECX
	push EDX
	push ESI
	push EDI

	; reset index_tracker to 0
	mov EDI, [EBP+16] ; OFFSET index_tracker
	mov EAX, 0
	mov [EDI], EAX


	mov ECX, DESIREDVALIDCOUNTTOTAL
	_updateSumLoop:
		; get value of index_tracker
		mov ESI, [EBP+16]	;OFFSET index_tracker
		mov EAX, [ESI]

		; get value stored at index in valid_numbers_array
		mov EBX, 4			; type of SDWORD
		mul EBX				; EAX = type * index
		mov EBX, [EBP+8]	;OFFSET valid_numbers_array
		add EBX, EAX		; EBX = OFFSET valid_numbers_array + (type * index)
		mov ESI, EBX		; ESI = OFFSET valid_numbers_array + (type * index)
		mov EAX, [ESI]		;EAX =value stored at valid_numbers_array address

		; get valid_numbers_sum
		mov EDI, [EBP+12]	;OFFSET valid_numbers_sum
		mov EBX, [EDI]		; EBX = valid_numbers_sum
		add EBX, EAX		; EBX = value stored at valid_numbers_array address + valid_numbers_sum
		mov [EDI], EBX
		
		; inc index_tracker
		mov EDI, [EBP+16]	;OFFSET index_tracker
		mov EAX, [EDI]
		inc EAX
		mov [EDI], EAX

		loop _updateSumLoop

	pop EDI
	pop ESI
	pop EDX
	pop ECX
	pop EBX
	pop EAX
	pop EBP
	RET 12
SumValidNumbers ENDP

; -- ReadVal --
; Prompt user to enter a number, read the input string from the user and verify it is a valid SDWORD.
;	- if valid, convert it to an int if its a valid SDWORD number.
;	- if invalid, print error message
; preconditions: the following should already be set
;	- [EBP+72] OFFSET special_case_int_array
;	- [EBP+36] MAXLEN 
;	- [EBP+8] OFFSET prompt (str)
; postconditions
;	- none
; receives:
;	- [EBP+72] OFFSET special_case_int_array
;	- [EBP+68] OFFSET input_int_array
;	- [EBP+64] OFFSET multiplicand_int_array
;	- [EBP+60] OFFSET input_int
;	- [EBP+56] OFFSET index_tracker
;	- [EBP+52] OFFSET input_error
;	- [EBP+48] OFFSET reverse_input_int_array
;	- [EBP+44] OFFSET multiplier
;	- [EBP+40] OFFSET num_of_digits
;	- [EBP+36] MAXLEN
;	- [EBP+32] OFFSET input_state
;	- [EBP+28] OFFSET input_char_as_int
;	- [EBP+24] OFFSET reverse_input_str
;	- [EBP+20] OFFSET input_str_len
;	- [EBP+16] OFFSET input_str_len
;	- [EBP+12] OFFSET input_str
;	- [EBP+8] OFFSET prompt (str)
; returns:
;	- [EBP+68] OFFSET input_int_array
;	- [EBP+64] OFFSET multiplicand_int_array
;	- [EBP+60] OFFSET input_int
;	- [EBP+56] OFFSET index_tracker
;	- [EBP+52] OFFSET input_error
;	- [EBP+48] OFFSET reverse_input_int_array
;	- [EBP+44] OFFSET multiplier
;	- [EBP+40] OFFSET num_of_digits
;	- [EBP+32] OFFSET input_state
;	- [EBP+28] OFFSET input_char_as_int
;	- [EBP+24] OFFSET reverse_input_str
;	- [EBP+20] OFFSET input_str_len
;	- [EBP+12] OFFSET input_str
ReadVal PROC
	push EBP
	mov EBP, ESP
	push EAX
	push EBX
	push ECX
	push EDX
	push ESI
	push EDI

	;reset the following variables
	;	- [EBP+60] OFFSET input_int
	;	- [EBP+56] OFFSET index_tracker
	;	- [EBP+40] OFFSET num_of_digits
	mov EAX, 0
	mov EDI, [EBP+60] ; OFFSET input_int
	mov [EDI], EAX
	mov EDI, [EBP+56] ;OFFSET index_tracker
	mov [EDI], EAX
	mov EDI, [EBP+40] ;OFFSET num_of_digits
	mov [EDI], EAX

	; set the following variables
	;	- [EBP+16] input_str_len
	;	- [EBP+12] OFFSET input_str
	;	- [EBP+8] OFFSET prompt (str)
	; get by value the following variables
	;	- [EBP+36] MAXLEN
	mGetString [EBP+8], [EBP+12], [EBP+16], [EBP+36]

	; update global variable of input_str_len
	mov EDI, [EBP+20]	; address of input_str_len
	mov EAX, [EBP+16]	; value of input_str_len
	mov [EDI], EAX

	_checkIfInputLenZero:
		mov EAX, [EBP+16]		; address of input_str_len
		cmp EAX, 0
		jg _reverseInputString	; input_str_len > 0
		jmp _setInvalidInput	; input_str_len <= 0
		; at this point, input_str_len >= 1

	; reverse input_str and place in reverse_input_str
	; set up loop counter and indices
	_reverseInputString:
		mov ECX, [EBP+16]	; input_str_len
		mov ESI, [EBP+12]	; address of input_str
		add ESI, ECX
		dec ESI
		mov EDI, [EBP+24]	; address of reverse_input_str
		; reverse string
		_revLoop:
			STD
			LODSB
			CLD
			STOSB
			LOOP   _revLoop
	
	push [EBP+44]		; OFFSET multiplier
	push [EBP+40]		; OFFSET num_of_digits
	mov EAX, [EBP+32]	; OFFSET input_state
	push EAX
	push [EBP+28]		; OFFSET input_char_as_int
	push [EBP+16]		; input_str_len
	push [EBP+24]		; OFFSET reverse_input_str
	call ValidateStringInputIsAnIntAndSetUpForConversion
	
	; check input_state
	mov ESI, [EBP+32]	; OFFSET input_state
	mov EAX, [ESI]
	cmp EAX, INVALID
	je _setInvalidInput	; EAX (input_state) = INVALID
	
	; at this point, input_str is a valid number 
	push [EBP+72]		; OFFSET special_case_int_array
	push [EBP+68]		; OFFSET input_int_array
	push [EBP+64]		; multiplicand_int_array
	push [EBP+60]		; OFFSET input_int
	push [EBP+56]		; OFFSET index tracker
	push [EBP+48]		; OFFSET reverse_input_int_array
	mov ESI, [EBP+44]	; OFFSET multiplier; pass by value
	mov EAX, [ESI]
	push EAX
	mov ESI, [EBP+40]	; OFFSET num_of_digits; pass by value
	mov EAX, [ESI]
	push EAX
	mov EAX, [EBP+32]	; OFFSET input_state
	push EAX
	push [EBP+28]		; OFFSET input_char_as_int
	push [EBP+16]		; input_str_len
	push [EBP+24]		; OFFSET reverse_input_str
	call ConvertValidatedInputStringToSDWORD

	; check input_state
	mov ESI, [EBP+32]	;OFFSET input_state
	mov EAX, [ESI]
	cmp EAX, INVALID
	je _setInvalidInput	; EAX (input_state) = INVALID
	jmp _setValidInput	; EAX (input_state) = VALID

	_setInvalidInput:
		mDisplayString [EBP+52] ; OFFSET input error
		jmp _endReadVal

	_setValidInput:
		jmp _endReadVal

	_endReadVal:
		pop EDI
		pop ESI
		pop EDX
		pop ECX
		pop EBX
		pop EAX
		pop EBP
		RET 68
ReadVal ENDP

; -- ConvertValidatedInputStringToSDWORD --
; Get an input that is within SDWPORD bounds from the reverse_input_string
; preconditions: the following should already be set
;	- [EBP+52] offset special_case_int_array
;	- [EBP+32] OFFSET reverse_input_int_array
;	- [EBP+28] multiplier
;	- [EBP+24] num_of_digits
;	- [EBP+12] input_str_len 
;	- [EBP+8] OFFSET reverse_input_str
; postconditions
;	- none
; receives:
;	- [EBP+52] OFFSET special_case_int_array
;	- [EBP+48] OFFSET input_int_array
;	- [EBP+44] OFFSET multiplicand_int_array
;	- [EBP+40] OFFSET input_int
;	- [EBP+36] OFFSET index_tracker
;	- [EBP+32] OFFSET reverse_input_int_array
;	- [EBP+28] multiplier
;	- [EBP+24] num_of_digits
;	- [EBP+20] OFFSET input_state
;	- [EBP+16] OFFSET input_char_as_int
;	- [EBP+12] input_str_len 
;	- [EBP+8] OFFSET reverse_input_str
; returns
;	- [EBP+52] OFFSET special_case_int_array
;	- [EBP+48] OFFSET input_int_array
;	- [EBP+44] OFFSET multiplicand_int_array
;	- [EBP+40] OFFSET input_int
;	- [EBP+36] OFFSET index_tracker
;	- [EBP+32] OFFSET reverse_input_int_array
;	- [EBP+20] OFFSET input_state
;	- [EBP+16] OFFSET input_char_as_int
;	- [EBP+8] OFFSET reverse_input_str
ConvertValidatedInputStringToSDWORD PROC
	push EBP
	mov EBP, ESP
	push EAX
	push EBX
	push ECX
	push EDX
	push ESI
	push EDI

	CLD

	; for the length of num_of_digits, upload input int equivalent into reverse_input_int_array
	mov ECX, [EBP+24]	; num_of_digits
	mov ESI, [EBP+8]	; address of reverse_input_str

	; update index_tracker to 0
	mov EAX, 0
	mov EDI, [EBP+36] ;OFFSET index_tracker
	mov [EDI], EAX
	_copyCharIntEquivalentIntoArrayLoop:
		LODSB
		sub AL, 48			; ASCII of char decimal - 48 = input character as int
		mov EDI, [EBP+16]	; OFFSET input_char_as_int
		mov [EDI], EAX		; mov EAX (32 bit int) into input_char_as_int

		; get index_tracker  and store in EAX
		mov ESI, [EBP+36]	;OFFSET index_tracker
		mov EAX, [ESI]
		
		mov EBX, 4			; Type of SDWORD
		mul EBX				; EAX = index_tracker * Type
		mov EBX, [EBP+32]	; OFFSET reverse_input_int_array
		add EBX, EAX		; EBX = OFFSET reverse_input_int_array + (index_tracker * Type)
		mov EDI, EBX		; EDI = OFFSET reverse_input_int_array + (index_tracker * Type)
		mov ESI, [EBP+16]	; OFFSET input_char_as_int
		mov EAX, [ESI]		; mov input_char_as_int into EAX

		mov [EDI], EAX		; mov input_char_as_int into correct index in reverse_input_int_array

		; inc index_tracker
		mov EDI, [EBP+36]	;OFFSET index_tracker
		mov EAX, [EDI]
		inc EAX
		mov [EDI], EAX

		; repoint ESI to next index in reverse_input_str
		; EAX currently holds correct index value
		mov EBX, 1
		mul EBX				; EAX = index value * Type
		mov EBX, [EBP+8]	; offset reverse_input_str
		add EBX, EAX		; EBX = OFFSET reverse_input_str + (index value * Type)
		mov ESI, EBX		; ESI = OFFSET reverse_input_str + (index value * Type)
		
		loop _copyCharIntEquivalentIntoArrayLoop
		
	; reverse reverse_input_int_array and store it in input_int_array
	mov ECX, [EBP+24]		; num_of_digits
	; update index_tracker to num_of_digits -1
	mov EAX, ECX
	dec EAX
	mov EDI, [EBP+36]		;OFFSET index_tracker
	mov [EDI], EAX

	; set up arrays
	mov ESI, [EBP+32] ;OFFSET reverse_input_int_array
	_updateInputIntArrayLoop:
		; store current value in ESI in index_tracker index of input_int_array
		push ESI
		; index to update in input_int_array
		mov ESI, [EBP+36]	;OFFSET index_tracker
		mov EAX, [ESI]		; EAX contains index to update in input_int_array

		mov EBX, 4			; type of SDWORD
		mul EBX				; EAX = type of SDWORD * index to update in input_int_array
		mov EBX, [EBP+48]	;OFFSET input_int_array
		add EBX, EAX		; EBX = OFFSET input_int_array+ (type of SDWORD * index to update in input_int_array)
		mov EDI, EBX		; EDI = OFFSET input_int_array+ (type of SDWORD * index to update in input_int_array) 

		pop ESI
		mov EAX, [ESI]		; get current value from reverse_input_int_array
		mov [EDI], EAX		; update input_int_array at specified index with current value from reverse_input_int_array

		; move forward in input_int_array
		add ESI, 4 

		; dec index tracker by one
		mov EDI, [EBP+36]	;OFFSET index_tracker
		mov EAX, [EDI]		; EAX contains current value in input_int_array
		dec EAX
		mov [EDI], EAX
		
		loop _updateInputIntArrayLoop

	; check to see if input array represents "-2147483648" (most negative valid SDWORD)
	_checkIfInputIntArrayEqualToSpecialValue:
		mov EAX, [EBP+28]	;  multiplier
		cmp EAX, -1
		jne _skipInputIntArrayEqualToSpecialValue ; EAX != -1, therefore number is positive
		mov EAX, [EBP+12]	; check if length of input matches lengthf of special number 
		cmp EAX, 11			; max str length of "-2147483648"
		jne _skipInputIntArrayEqualToSpecialValue ; EAX != 11
		; compare if values in special_case_int_array and input_int_array are the same
		mov ECX, [EBP+24]	; num_of_digits ; DEBUG
		mov ESI, [EBP+48]	; OFFSET input_int_array
		mov EDI, [EBP+52]	; OFFSET special_case_int_array
		_compareArrayLoop:
			mov EAX, [ESI]	; OFFSET input_int_array
			mov EBX, [EDI]	; OFFSET special_case_int_array
			cmp EAX, EBX
			je _continueCompareArrayLoop				; EAX = EBX
			jmp _skipInputIntArrayEqualToSpecialValue	; EAX != EBX

			_continueCompareArrayLoop:
				add ESI, 4
				add EDI, 4
				LOOP _compareArrayLoop

		; update input_int
		mov EDI, [EBP+40]
		mov EAX, LO
		mov [EDI], EAX

		jmp _validNumber

	_skipInputIntArrayEqualToSpecialValue:
		; update input_int 
		; update index_tracker to 0
		mov EAX, 0
		mov EDI, [EBP+36]	;OFFSET index_tracker
		mov [EDI], EAX
		; for the length of pow tracker, update values in multiplicand_int_array to 1
		mov ECX, [EBP+24]	; num_of_digits
		mov EDI, [EBP+44]	;OFFSET multiplicand_int_array
		_updateValInArrayToOneLoop:
			mov EAX, 1
			mov [EDI], EAX
			add EDI, 4
			loop _updateValInArrayToOneLoop

		; update mulitiplicand_int_array and input_int_array
		;	each the value in each mulitiplicand_int_array will be multiplied against the value in the
		;	same index of input_int_array. For example if mulitiplicand_int_array = [100, 10, 1], 
		;	and input_int_array = [4, 2, 3], input_int_array then be equal to [400, 20, 3]

		; update multiplicand_int_array
		; update index_tracker to 0
		mov EAX, 0
		mov EDI, [EBP+36]		;OFFSET index_tracker
		mov [EDI], EAX
		; update ECX
		mov ECX, [EBP+24]		; num_of_digits
		cmp ECX, 1
		jle _updateInputIntArray ; (ECX = num_of_digits) <= 1
		; at this point, (ECX = num_of_digits) > 1
		dec ECX					; start loop at num_of_digits - 1
		mov ESI, [EBP+44]		; OFFSET multiplicand_int_array
		mov EDI, [EBP+44]		; OFFSET multiplicand_int_array

		_multiplicandLoop:
			push ECX
			mov EBX, 1
			_updateEBXLoop:
				mov EAX, 10
				mul EBX
				mov EBX, EAX
			loop _updateEBXLoop
			pop ECX
		
			; update value in multiplicand_int_array
			mov [EDI], EBX

			; go to next index in multiplicand_int_array
			add ESI, 4
			add EDI, 4
			loop _multiplicandLoop

		_updateInputIntArray:
			; multiply the values in input_int_array * respective value in multiplicand_int_array
			mov ECX, [EBP+24]	; num_of_digits 
			mov EDI, [EBP+48]	; OFFSET input_int_array
			mov ESI, [EBP+44]	; OFFSET multiplicand_int_array ; DEBUG
			_multiplyValuesInInputIntArrayLoop:
				mov EAX, [ESI]	; EAX = current value in multiplicand_int_array
				mov EBX, [EDI]	; EBX = current value in reverse_input_int_array
				mul EBX ; EAX = current value in multiplicand_int_array * current value in reverse_input_int_array
				mov [EDI], EAX	; value in EDI = current value in multiplicand_int_array * current value in reverse_input_int_array
				; move forward in both arrays
				add ESI, 4
				add EDI, 4
				loop _multiplyValuesInInputIntArrayLoop

			; add values to input_int and check for overflow 
			mov ECX, [EBP+24]	; num_of_digits
			mov ESI, [EBP+48] ;OFFSET input_int_array
			mov EDI, [EBP+40] ; OFFSET input_int
			_addToInputInt:
			
				mov EBX, [ESI]		;OFFSET input_int_array
				mov EAX, [EDI]		; OFFSET input_int
				add EAX, EBX
				jo _invalidNumber	; we've overflowed
				mov [EDI], EAX

				mov EAX, [EBP+24]	; num_of_digits
				cmp ECX, EAX		; if ECX = EAX, then we've only added one digit. at this point, we can multiply by multiplier

				add ESI, 4
				loop _addToInputInt
				jmp _validAbsoluteNumber

			_validAbsoluteNumber:
				; multiply by multiplier and cjeck for overflow 
				mov ESI, [EBP+40]	; OFFSET input_int
				mov EAX, [ESI]		; input_int value
				mov EBX, [EBP+28]	; multiplier
				imul EBX			; EAX = multiplier * input_int value
				jo _invalidNumber	; we've overflowed
				mov EDI, [EBP+40]	; OFFSET input_int
				mov [EDI], EAX
				jmp _validNumber

		_validNumber:
			; update input_state
			mov EDI, [EBP+20]	; OFFSET input_state
			mov EAX, VALID
			mov [EDI], EAX
			jmp _endConvertValidatedInputStringToSDWORD

		_invalidNumber:
			; update input_state
			mov EDI, [EBP+20]	; OFFSET input_state
			mov EAX, INVALID
			mov [EDI], EAX
			jmp _endConvertValidatedInputStringToSDWORD
	
	_endConvertValidatedInputStringToSDWORD:
		pop EDI
		pop ESI
		pop EDX
		pop ECX
		pop EBX
		pop EAX
		pop EBP
		RET 48
ConvertValidatedInputStringToSDWORD ENDP

; -- ValidateStringInputIsAnIntAndSetUpForConversion --
; Validate that the input string is a valid int and set up prerequisites to have ReadVal convert this string to an int.
; preconditions
;	- none
; postconditions
;	- none
; receives:
;	- [EBP+28] OFFSET multiplier
;	- [EBP+24] OFFSET num_of_digits
;	- [EBP+20] OFFSET input_state
;	- [EBP+16] OFFSET input_char_as_int
;	- [EBP+12] input_str_len
;	- [EBP+8] OFFSET reverse_input_str
; returns:
;	- [EBP+28] OFFSET multiplier
;	- [EBP+24] OFFSET num_of_digits
;	- [EBP+20] OFFSET input_state
;	- [EBP+12] input_str_len
ValidateStringInputIsAnIntAndSetUpForConversion PROC
	push EBP
	mov EBP, ESP
	push EAX
	push EBX
	push ECX
	push EDX
	push ESI
	push EDI

	CLD

	; reset num_of_digits to 0
	mov EDI, [EBP+24] ; OFFSET num_of_digits
	mov EAX, 0
	mov [EDI], EAX

	; begin validation of char
	mov ECX, [EBP+12]	; input_str_len
	mov ESI, [EBP+8]	; address of reverse_input_str
	_validateCharLoop:
		LODSB ; put byte of char in AL

		;check if ECX is at first char
		cmp ECX, 1
		je _validateInputFirstChar		; ECX = 1
		jmp _validateInputNonFirstChar	; ECX != 1

		_validateInputNonFirstChar:
			cmp AL, 48					; 48d in ASCII = '0'
			jl _setInvalidInput			; AL < 48
			cmp AL, 57					; 57d in ASCII = '9'
			jg _setInvalidInput			; AL > 57
			; nonfirst input_str char is valid at this point

			;convert ASCII to int
			sub AL, 48					; current AL value (ASCII representation) - 48 = input character as int
			mov EDI, [EBP+16]			; OFFSET input_char_as_int
			mov [EDI], EAX				; mov EAX into input_char_as_int

			; inc num_of_digits
			mov EDI, [EBP+24]			; address of num_of_digits
			mov EBX, [EDI]
			inc EBX
			mov [EDI], EBX

			LOOP   _validateCharLoop

	_validateInputFirstChar:
		_checkIfPlusChar:
			cmp AL, 43						; 43d in ASCII = '+'
			je _setMultiplierOne			; AL = 43
			jmp _checkIfMinusChar			; AL != 43

		_checkIfMinusChar:
			cmp AL, 45						; 45d in ASCII = '-'
			je _setMultiplierNegativeOne	; AL = 45
			jmp _validateIfNumber			; AL != 45

		; at this point, the first char in the input is not a '+' or '-'
		_validateIfNumber:
			cmp AL, 48						; 48d in ASCII = '0'
			jl _setInvalidInput				; AL < 48
			cmp AL, 57						; 57d in ASCII = '9'
			jg _setInvalidInput				; AL > 57

			; inc num_of_digits
			mov EDI, [EBP+24]				; address of num_of_digits
			mov EBX, [EDI]
			inc EBX
			mov [EDI], EBX
			jmp  _setMultiplierOne			; first char is a valid number, and therefore a positive number

	_setMultiplierOne:
		mov EDI, [EBP+28]		; address of multiplier
		mov EBX, 1
		mov [EDI], EBX
		jmp _setValidInput

	_setMultiplierNegativeOne:
		mov EDI, [EBP+28]		; address of multiplier
		mov EBX, -1
		mov [EDI], EBX
		jmp _setValidInput
 
	_setInvalidInput:
		mov EDI, [EBP+20]		; address of input_state
		mov EAX, INVALID
		mov [EDI], EAX
		jmp _endGetIntFromReverseStringInput

	_setValidInput:
		mov EDI, [EBP+20]		; address of input_state
		mov EAX, VALID
		mov [EDI], EAX
		jmp _endGetIntFromReverseStringInput

	_endGetIntFromReverseStringInput:
		pop EDI
		pop ESI
		pop EDX
		pop ECX
		pop EBX
		pop EAX
		pop EBP
		RET 24
ValidateStringInputIsAnIntAndSetUpForConversion ENDP

; -- introduction --
; Procedure to introduce the program.
; preconditions: string inputs must exist already
; postconditions: none
; receives: global strings listed below
;		- [EBP+16] extra_credit_info
;		- [EBP+12] intro_info
;		- [EBP+8] intro_title
; returns: none
Introduction PROC
	push EBP
	mov EBP, ESP
	push EDX

	mDisplayString [EBP+8]	; address of intro_title
	mDisplayString [EBP+12]	; address of intro_info
	mDisplayString [EBP+16]	; address of extra_credit_info

	pop EDX
	pop EBP
	RET 12
Introduction ENDP

; -- farewell --
; Procedure to end the program with a farewell.
; preconditions: string input must exist already
; postconditions: none
; receives: global strings listed below
;		- [EBP+8] goodbye
; returns: none
Farewell PROC
	push EBP
	mov EBP, ESP
	push EDX

	mDisplayString [EBP+8]	; address of goodbye

	pop EDX
	pop EBP
	RET 4
Farewell ENDP



END main
