TITLE  FinalProject

;Limit of characters to be entered
maxInput = 20

KEY = 239
;Limit of data to be entered to file
bufferSize = 20

INCLUDE Irvine32.inc
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

	sizeofData DWORD ?

	userData BYTE maxInput DUP (?), 0

	filename BYTE "data.txt", 0
	encryptFile BYTE "SECRETFILE.txt",0

	;Data variables for the file I/O
	fileHandle DWORD ?

.code
main PROC

;Save registers, Generate greeting message, Restore registers
	pushad
	mov ebx, OFFSET welcome
	mov edx, OFFSET desMsg
	call MsgBox
	popad

;Call to allow user to enter data
	call enterData

;Call to write uncrypted message to data file
	call userEnteredMessage

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
	call encryptData
	mov edx, OFFSET decryptDataMsg

;Set text color
	mov eax, Green
	call SetTextColor

;Call to show decrypted message
	call showData


	;PUT THIS IN THE PROCEDURE AFTER ENCRYPTION
	;Save registers, Generate choice message, Restore registers
	;pushad
	;mov ebx, OFFSET encryptMsg
	;mov edx, OFFSET choiceMsg
	;call MsgBoxAsk
	;Check value of eax to determine what procedure to call (use .IF to check, pass eax to variable if needed)
	;popad

	mov eax, White
	call SetTextColor
	exit
main ENDP


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
	mov sizeofData, eax ;Save the length of string entered
	popad
	ret
enterData ENDP


;Procedure that takes a user-defined message
;Passes this message through an encryption function or decryption function depending on what is passed in.
;Then keeps track of this data
encryptData PROC
	pushad 
		mov ecx, sizeofData;Loop counter
		mov esi, 0 ;Index 0 in array

	L1:
		xor userData[esi], KEY ;Translate a byte
		add esi, 1 ;Point to next byte
	loop L1

	popad

	ret
encryptData ENDP

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
;
;readData PROC
;pushad
;	mov edx, 0
;	mov ecx, bufferSize
;	mov eax, fileHandle
;	mov edx, OFFSET newData
;	call ReadFromFile
;popad
;ret
;readData ENDP

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
;Procedure that takes in data from user and displays it (Encrypted or Decrypted)
;Data passed in through EDX
showData PROC 
	pushad
	call WriteString
	mov edx, OFFSET userData ;display buffer
	call WriteString
	call Crlf
	popad
	ret
showData ENDP


END main
