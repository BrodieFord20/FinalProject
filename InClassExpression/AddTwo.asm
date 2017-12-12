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
	primeCandidate WORD ?
	r DWORD 1
	a WORD 0
	z WORD 0
	;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;
	
	; Primes:
	p WORD ?	; prime 1 used for key generation
	q WORD ?	; prime 2 used for key generation
	;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%; 
	
	;Key Exchange parameters:
	
	n WORD 0
	phi_n WORD 0
	e WORD 0
	;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;
	
	; EEA PARAMETERS
	;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;
	r_0 WORD 0	; Stores phi_n, then is overwritten with new values
	r_1 WORD 0	; Stores e, then is overwritten with new values
	r_i WORD 0
	qu WORD 0
	t_0 SWORD 0
	t_1 SWORD 1
	i WORD 1
	;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;
	loopstorage dword 0 ; <- can change the implementation and will likely be unecessary

	;Encryption Variables;
	encryp LABEL DWORD 
	encrypA WORD ?
	encrypB WORD ?

	holder WORD 0

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

; Calls the RSA key generation
	call RSAkey

;Call to encrypt data
	;call encryptData
	call RSAencryp 
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
 
 findPrimes PROC						; note: SUPER FIXED!
	pushad
	
	mov eax, 0
	mov edx, 0

	suitableRand:
		mov ax, 0FCh
		call RandomRange				; prime candidate now in eax
		add ax, 3
		test eax, 1						; test if odd, if not, generate another number.
	jz suitableRand

	;mov ax, 103d						; FOR TESTING PURPOSES
	mov primeCandidate, ax	


	sub ax, 1h							; get the number r s.t. primeCandidate - 1 = 2^(u) * r
	getRandU:
		shr eax, 1
		mov r, eax
		test eax, 1						; r guaranteed to be the last complete division.
		jnz _clear
	jmp getRandU

	jz suitableRand

	_clear:
	mov ecx, 10d						;// security parameter for miller-rabin primality test (s = 10)
	millerRabinLoop:
		mov ax, primeCandidate
		sub eax, 4h						; set up selection of a (e) {2, 3, ... , primeCandidate - 2}
		call RandomRange					; generates number from {0, 1, 2, ... , primeCandidate - 4}
		add eax, 2h						; bumps up index to desired value
		mov a, ax
		mov loopstorage, ecx
		
		mov ecx, r						; set up the loop to raise value of a to the power of r
		dec ecx
		
		powerLoop:
			mul a						; raises value of a to r power [NOTE: Could overflow into edx, depending on a || NEED TO ACCOUNT FOR] Solved by taking mod every time.
			div primeCandidate			; works because (ab) mod n = (a mod n) * (b mod n)
			mov ax, dx
		loop powerLoop
	
		cmp edx, 1						; run a few checks on the result of the division (is mod = 1? primeCandidate - 1? If so, freaking excellent. Get out of there.)
		jz yay

		mov ebx, 0
		mov bx, primeCandidate
		dec ebx
		cmp edx, ebx
		jz yay
		
		mov z, dx						; If value hasn't yet been ejected, store modulus in z
		
		compositeLoop:
			cmp r, ebx						; NOTE: EBX still contains primeCandidate - 1
			jz suitableRand					; stop if r = primecandidate - 1 (multiplies r by 2 each loop, undoing the earlier factoring)

			mov ax, z						; z = z^2 mod primeCandidate
			mul ax
			div primeCandidate
			mov z, dx

			shl r, 1						; r = 2*r 

			cmp edx, 1
			jz suitableRand
			cmp edx, ebx
			jz yay
		loop compositeLoop
		
		yay:
		mov ecx, loopstorage
		dec ecx
	jnz millerRabinLoop
		
	popad

	
	mov ax, primeCandidate
	mov primeCandidate, 0
	mov r, 1
	mov a, 0
	mov z, 0
	ret

findPrimes ENDP

;////////////////////////////////////////////////////;

; Procedure handling the Extended Euclidean Algorithm :
; INPUT: positive integers r_0 and r_1 s.t. r_0 > r_1
; OUTPUT: gcd(R_0, r_1), as well as s and t s.t. gcd(r_0, r_1) = s*r_0 + t*r_1
; NOTE: For purposes of RSA, s doesn't need to be calculated.
EEA PROC
	generateE:
	pushad
		mov eax, 0
		mov edx, 0

	gcdLoop:						; repeats until gcd of random value e and phi_n = 1, saves value of e, and calls it bueno.
		mov ax, phi_n
		sub eax, 2						; find a random value in the range [0, phi_n - 2]
		call RandomRange
		inc eax							; bump up value into desired range of [1, phi_n - 1]
		mov e, 28213						; store this value as the public exponent e. Need to check whether e and phi_n are relatively prime. 
		mov r_1, 28213						; store e in r_1 as a starting value 
		mov ebx, 0
		mov bx, phi_n					; store phi_n in r_0 as a starting value. (NOTE: r_0 > r_1.)	
		mov r_0, bx
		
		euclidLoop:
			mov eax, 0
			mov edx, 0
			mov ax, r_0				; r_i = r_(i-2) mod r_(i-1) || NOTE: r_(i-2) is always stored in r_0, r_(i-1) in r_1.
			div r_1
			mov r_i, dx
			
			mov ax, r_0				; q_(i-1) = (r_(i-2) - r_i)/r_(i-1) || NOTE: q is the value r_0 = q * r_1 + r_i , i.e. the number of times r_1 can be multiplied
			sub ax, r_i				;									|| into the value of r_0.
			mov edx, 0				; needs to clear because residual values in edx messing up division
			div r_1					
			mov qu, ax				; stores value of q in qu
			
			mov ax, r_1				; set r_0 = r_1, r_1 = r_i
			mov r_0, ax
			mov ax, r_i
			mov r_1, ax
			
			mov ax, qu
			imul t_1						
			mov bx, t_0				; t_i = t_(i-2) - q_(i-1) * t_(i-1) NOTE: Since t_0 = 0 and t_1 = 1, this results in a negative number.
			sub bx, ax
			
			mov ax, t_1				; set t_0 = t_1, t_1 = t_i
			mov t_0, ax
			mov t_1, bx
			
			xor r_1, 0
		jnz euclidLoop 
		
		cmp r_0, 1					; if gcd != 1, get back in there and try again
	jnz gcdLoop

	;test t_0, 80h
	;mul t_0
	;mov ax, r_0
	;div phi_n
	;test edx, 1
	;jnz gcdLoop

	;gcd (r_0, r_1) = r_(i-1) [In terms of implementation, r_0.] NOTE: As of this build, gcd works but the inverse value is broken.
	; t = t_1 contains the inverse of r_0 modulo phi_n.
	; need to test t_1 * r_0 mod phi_n. If output isn't equal to 1, then go and generate another value until you get one that works.
	; BY END: has modular inverse of e stored in t_1 
		
	popad
	ret
		
EEA ENDP

;////////////////////////////////////////////////////;

RSAkey PROC
pushad

	call Randomize
	;call findPrimes
	mov p, 241							; find a likely prime to stick into p

	;call findPrimes

	mov q, 157							; find a likely prime to stick into q
	mov ax, q
	mul p 								;[!!!!] NOTE: Not sure how to manage numbers larger than 32 bits
	
	mov n, ax
	
	mov ax, p							; compute phi_n = (p-1)(q-1)
	dec ax
	mov bx, q
	dec bx
	mul bx
	
	mov phi_n, ax						; store phi_n
	
	
	; COMPUTE E S.T. E REL. PRIME TO PHI_N. USE EXTENDED EUCLIDEAN ALGORITHM. And then we're in business, I think. OH GOD THIS IS HARDER THAN I THOUGHT
	call EEA
	
	popad
	ret
	
RSAkey ENDP

;////////////////////////////////////////////////;

RSAencryp PROC					; Encrypts user data (up to two characters, so good) according to RSA scheme
	pushad

	mov eax, 0
	mov edx, 0
	mov ecx, 0

	mov ax, word PTR userData
	mov holder, ax

	mov cx, e

	Loop5:
		mul holder
		mov encrypA, ax
		mov encrypB, dx
		mov eax, encryp
		mov ebx, 0FFFFh
		div ebx
		movzx eax, dx
	Loop Loop5
	
	mov userdata, al
	mov userdata + 1, ah
	popad
	ret
RSAencryp ENDP

END main

;////////////////////////////////////////////////;

RSAdecryp PROC								; decrypts user data according to RSA scheme - NOTE: still needs to read in encrypted data from right place.
	pushad

	mov eax, 0
	mov edx, 0
	mov ecx, 0

	mov ax, word PTR userData
	mov holder, ax

	mov cx, t_0

	Loop5:
		mul holder
		mov encrypA, ax
		mov encrypB, dx
		mov eax, encryp
		mov ebx, 0FFFFh
		div ebx
		movzx eax, dx
	Loop Loop5
	
	mov userdata, al
	mov userdata + 1, ah
	popad
	ret
RSAencryp ENDP

END main
