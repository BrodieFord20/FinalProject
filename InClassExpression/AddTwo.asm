TITLE  FinalProject

;Limit of characters to be entered
maxInput = 20

KEY = 239
;Limit of data to be entered to file
bufferSize = 20

INCLUDE Irvine32.inc
;--------------------------Data---------------------------------;
.data

;Data Variables for the welcome, user choice, user input and goodbye messages
	welcome BYTE "Encryption Prescription",0

	desMsg BYTE "Hello user, we are here to protect and secure your data.",0dh, 0ah
		   BYTE "Click OK to begin!",0

	encryptMsg BYTE "Encryption Complete",0

	choiceMsg BYTE "Thank you for entrusting us with your data.", 0dh,0ah
		      BYTE "Would you like to encrypt more data?", 0
			  
	guideMsg BYTE "Please enter in the data you would like us to protect: ",0

	encryptDataMsg BYTE "Your encrypted data: ",0
	
	decryptDataMsg BYTE "Your decrypted data: ",0

	firstsizeofData DWORD ?
	sizeofData DWORD ?

	userData BYTE maxInput DUP (?), 0
	newData BYTE maxInput DUP (?), 0

	filename BYTE "data.txt", 0
	encryptFile BYTE "SECRETFILE.txt",0
	
	;; {RSA VARIABLES}
	; parameters for miller-rabin primality test:
	primeCandidate DWORD (?)
	r WORD 0
	a WORD 0
	u WORD 0
	z WORD 0
	;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;
	
	; Primes:
	p WORD (?)	; prime 1 used for key generation
	q WORD (?)	; prime 2 used for key generation
	;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%; 
	
	;Key Exchange parameters:
	
	n DWORD 0
	phi_n DWORD 0
	e DWORD 0
	;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;
	
	; EEA PARAMETERS
	;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;
	r_0 DWORD 0	; Stores phi_n, then is overwritten with new values
	r_1 DWORD 0	; Stores e, then is overwritten with new values
	r_i DWORD 0
	s_0 DWORD 0
	t_0 DWORD 0
	s_1 DWORD 0
	t_1 DWORD 1
	i DWORD 1
	;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;
	loopstorage dword 0 ; <- can change the implementation and will likely be unecessary

	;Data variables for the file I/O
	fileHandle DWORD ?

;------------------------------------------------------------------;

;---------------------------Code-----------------------------------;
.code

;/////////////////////////////////////////////////;
main PROC

;Save registers, Generate greeting message, Restore registers
	pushad
	mov ebx, OFFSET welcome
	mov edx, OFFSET desMsg
	call MsgBox
	popad

;Call to allow user to enter data
	call enterData

;Call to make sure user has no more data to enter
	call promptUser

;Call to write uncrypted message to data file
	call userEnteredMessage
	
	mov edx, OFFSET decryptDataMsg

;Set text color
	mov eax, Green
	call SetTextColor

;Call to show decrypted message
	call showData

;Call to encrypt data
	call encryptData
	mov edx, OFFSET encryptDataMsg

;Call to write encrptyed message to secure file
	call encrypt

;Set text color
	mov eax, Red
	call SetTextColor

;Call to show encrypted message
	call showData

	
;Call to decrypt data
;call encryptData



	mov eax, White
	call SetTextColor
	exit
main ENDP

;/////////////////////////////////////////////////;

;Procedure that prompts the user to enter in data
;Allows the user to input data upto specified amount
;Saves and Restores registers
enterData PROC
	pushad ;Save 32-bit registers
	mov edx, OFFSET guideMsg ;Display prompt
	call WriteString
	call Crlf
	mov ecx, maxInput ;Max character count
	mov edx, OFFSET userData ;Point to this array
	call ReadString
	mov firstsizeofData, eax ;Save the length of string entered
	popad
	ret
enterData ENDP

;/////////////////////////////////////////////////;

;Procedure that takes a user-defined message
;Passes this message through an encryption function or decryption function depending on what is passed in.
;Then keeps track of this data
encryptData PROC
	pushad 
	;Use the size of data for both user entries to get correct loop counter
	mov ebx, sizeofData
	add firstsizeofData, ebx
		mov ecx, firstsizeofData;Loop counter
		mov esi, 0 ;Index 0 in array

	L1:
		xor userData[esi], KEY ;Translate a byte
		add esi, 1 ;Point to next byte
	loop L1

	popad

	ret
encryptData ENDP

;/////////////////////////////////////////////////;

;Procedure that creates a file using the defined text file above
;Reads in the users entered data using EDX and the limit defined in bufferSize which is stored in ECX
;And displays the decrpyted message in the file
;Uses EAX, ECX, EDX
userEnteredMessage PROC
pushad
	mov edx, OFFSET filename
	call CreateOutputFile
	mov fileHandle, eax
	mov ecx, bufferSize
	mov edx, OFFSET userData
	call WriteToFile
popad
ret
userEnteredMessage ENDP

;/////////////////////////////////////////////////;

;Procedure that creates a file using the defined text file above
;Takes the users data in through EDX and the limit defined in bufferSize which is stored in ECX
;Then displays the encrpyted message in the file
;Uses EAX, ECX, EDX
encrypt PROC
pushad
	mov edx, OFFSET encryptFile
	call CreateOutputFile
	mov fileHandle, eax
	mov ecx, bufferSize
	mov edx, OFFSET userData
	call WriteToFile
popad
ret
encrypt ENDP

;/////////////////////////////////////////////////;

;Procedure that takes in data from user and displays it (Encrypted or Decrypted)
;Data passed in through EDX
showData PROC 
	call WriteString
	pushad
	mov edx, OFFSET userData ;display buffer
	call WriteString
	call Crlf
	;mov edx, OFFSET newData
	;call WriteString 
	call Crlf
	popad
	ret
showData ENDP

;/////////////////////////////////////////////////;

promptUser PROC
 
 pushad
 	mov ebx, OFFSET encryptMsg
 	mov edx, OFFSET choiceMsg
 	call MsgBoxAsk
 ;Check value of eax to determine what procedure to call (use .IF to check, pass eax to variable if needed)
 	.IF eax == 6
 		mov ecx, maxInput ;Max character count
 		mov edx, OFFSET userData
 		add edx, firstsizeofData;Point to this array
 		call ReadString
 		mov sizeofData, eax ;Save the length of string entered
 	.ENDIF
 	call Crlf
 	call WriteToFile
 
 popad
 ret
 promptUser ENDP
 
 ;////////////////////////////////////////////////;
 
 findPrimes PROC
	pushad
	
	suitableRand:
		call Randomize					; note: not a super secure rng seed, but very little but the algorithm is secure anyways.
		mov AX, EFh
		call RandomRange				; prime candidate now in eax
		test eax, 1						; test if odd, if not, generate another number.
	jz suitableRand
	add eax, 10h						; shift eax up into desired range (so that it uses around 16 bits)
	mov primeCandidate, ax

	sub ax, 1h							; get the number r s.t. primeCandidate - 1 = 2^(u) * r
	getR:
		mov bx, 2h						; algorithm: divide eax by 2, check if there's a remainder. if not,
		div bx							; store eax in r, and jump back to division. Repeat until remainder. 
		test edx, 1						; r guaranteed to be the last complete division.
		jnz _clear
		inc u							; increment u (initial value = 0)
		mov r, eax
	jmp getR

	_clear:
	mov ecx, 10d						;// security parameter for miller-rabin primality test (s = 10)
	millerRabinLoop:
		call Randomize
		mov ax, primeCandidate
		sub eax, 4h						; set up selection of a (e) {2, 3, ... , primeCandidate - 2}
		call RandomRange				; generates number from {0, 1, 2, ... , primeCandidate - 4}
		add eax, 2h						; bumps up index to desired value
		mov a, ax
		mov loopstorage, ecx
		
		mov ecx, r						; set up the loop to raise value of a to the power of r
		
		powerLoop:
			mul a						; raises value of a to r power [NOTE: Could overflow into edx, depending on a || NEED TO ACCOUNT FOR] <--!! Still a problem.
		loop powerLoop					; probably solve by setting edx to zero by default and checking to see if a nonzero entry in EDX? Need to review how MUL works.
		
		div primeCandidate				; divide by prime candidate [NOTE, if the power of a needs to be stored in eax, might need to pad prime candidate to fit.]
			
		cmp edx, 1						; run a few checks on the result of the division (is mod = 1? primeCandidate - 1? If so, try again.)
		jz suitableRand
		mov ebx, primeCandidate
		dec ebx
		cmp edx, ebx
		jz suitableRand
		
		mov z, edx						; If value is good, store modulus in z
			
		mov ecx, u						; 2^u <- this u 
		dec ecx							; set up to loop u-1 times
			
		compositeLoop:
			mov ax, z 
			mul ax						; might also need to check for overflow here.
			div primeCandidate
			;POTENTIAL NEED FOR OVERFLOW MITIGATION STATEMENTS;
			mov z, edx
			cmp edx, 1
			jz suitableRand
		loop compositeLoop:
		cmp z, ebx						; NOTE: EBX still contains primeCandidate - 1
		jnz suitableRand
		
		mov ecx, loopstorage
		loop millerRabinLoop
		
	popad
	
	mov eax, primeCandidate
	
	ret

findPrimes ENDP

;////////////////////////////////////////////////////;

; Procedure handling the Extended Euclidean Algorithm :
; INPUT: positive integers r_0 and r_1 s.t. r_0 > r_1
; OUTPUT: gcd(R_0, r_1), as well as s and t s.t. gcd(r_0, r_1) = s*r_0 + t*r_1
; NOTE: For purposes of RSA, s doesn't need to be calculated.
EEA PROC
	generateE:
		call Randomize
		mov eax, phi_n
		sub eax, 2
		call RandomRange
		inc eax
		mov e, eax
		mov r_1, eax
		mov [r_i + 4], eax
		mov ebx, phi_n
		mov r_0, ebx
		mov [r_i + 8], ebx
		
		euclidLoop:
			;r_i = r_(i-2) mod r_(i-1)
			mov eax, r_0
			div r_1
			mov r_i, edx
			
			mov eax, r_0				; q_(i-1) = (r_(i-2) - r_i)/r_(i-1)
			sub eax, r_i
			div r_1
			
			mov eax, r_1				; set r_0 = r_1, r_1 = r_i
			mov r_0, eax
			mov eax, r_i
			mov r_1, eax
			
			mul t_1						; make sure no overflow?
			mov ebx, t_0				; t_i = t_(i-2) - q_(i-1) * t_(i-1)
			sub ebx, eax
			
			mov eax, t_1				; set t_0 = t_1, t_1 = t_i
			mov t_0, eax
			mov t_1, ebx
			
			cmp r_1, 0
		jnz euclidLoop 
		
		; BY END: has modular inverse of e stored in t. 
		
		ret
		
EEA ENDP

;////////////////////////////////////////////////////;

RSAenc PROC
	
	call findPrimes
	mov p, ax							; find a likely prime to stick into p
	call findPrimes
	mov q, ax							; find a likely prime to stick into q
	mul ax, p 							;[!!!!] NOTE: Not sure how to manage numbers larger than 32 bits
	
	mov n, dx
	mov [n+2], ax						; NOTE: Not sure if little/big endian is default. Went with little, I think.
	
	mov ax, p							; compute phi_n = (p-1)(q-1)
	dec ax
	mov bx, q
	dec bx
	mul bx
	
	mov phi_n, dx						; store phi_n (little endian?)
	mov [phi_n+2], ax
	
	
	; COMPUTE E S.T. E REL. PRIME TO PHI_N. USE EXTENDED EUCLIDEAN ALGORITHM. And then we're in business, I think. Just need to run an xor.
	call EEA
	
	
RSAenc ENDP

END main
