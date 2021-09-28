            PAGE 0              ; suppress page headings in ASW listing file

;*******************************************************************************
;*                                                                             *
;*             4K, 8K and 16K  Altair BASIC for 8080 and 8085 SBCs             *
;*                   Portions Copyright © 2021 by Jim Loos                     *
;*                                                                             *
;* On the Altair, the "sense switches" are used by BASIC to select the type of *
;* serial card and the the number of stop bits; in this case, an 88-2SIO card  *
;* at addresses 0x10 and 0x11 (octal 20 and 21) with 2 stop bits. These BASIC  *
;* images have been hacked to eliminate the requirement for sense switches.    *
;*                                                                             *
;* Configure Teraterm for 7 data bits, odd parity, 2 stop bits.                *
;* For the 8080 SBC use 115200 bps. For the 8085 SBC, use 19200 bps.           *
;*                                                                             *
;* When using Teraterm's "Send File" function to send an Intel hex file or     *
;* BASIC source file to the Altair BASIC interpreter, set Teraterm's serial    *
;* port options for 1 msec/char and 50 msec/line transmit delay.               *
;*                                                                             *
;*******************************************************************************

                cpu     8085
; memory addresses
ram_start:      equ     0000H               ; ram (0000-7FFF) starts here
ram_end:        equ     7FFFH
cpu_type:       equ     ram_end             ; 0 for 8080, 5 for 8085
delay_value:    equ     ram_end-1           ; 117 for 8080, 199 for 8085 (for the millisecond delay subroutine)
esc_count:      equ     ram_end-2           ; number of times escape key pressed stored here
stack_top:      equ     ram_end-16          ; top of stack

; i/o port addresses
acia_status:    equ     10H                 ; MC6850 ACIA status port address (88-2SIO serial card at MITS standard address of 10H (octal 20)
acia_data:      equ     11H                 ; MC6850 ACIA data port address   (88-2SIO serial card at MITS standard address of 11H (octal 21)

;for the 8255 on the 8080 SBC... note: the odd addresses are because address lines A0 and A1 to the 8255 are inverted.
cwr_80:         equ     0F8H                    
portC_80:       equ     0F9H
portB_80:       equ     0FAH
portA_80:       equ     0FBH

;for the 8155 on the 8085 SBC...
cmd_85          equ     0F8H
portA_85        equ     0F9H
portB_85        equ     0FAH
portC_85        equ     0FBH

led_port:       equ     0FAH                ; the same on both 8080 and 8085 SBCs

cr              equ     0DH                 ; carriage return
lf              equ     0AH                 ; line feed

                org     8000H               ; EPROM located 8000H-FFFFH

start:          di
                jmp eprom
eprom:          lxi sp,stack_top            ; set Stack Pointer to top of ram
                mvi a,00000011b
                out acia_status             ; reset the ACIA and reset the startup flip-flop
                mvi a,00000101b             ; 7 data bits, odd parity, 2 stop bits, divide clock by 16, no interrupts
                out acia_status             ; configure the ACIA

                ; detect either 8080 or 8085 CPU
                mvi a,00011111b
                sim                         ; set 7.5, 6.5 and 5.5 interrupt masks (8085 only)
                xra a                       ; clear the accumulator
                rim                         ; read interrupt masks (8085 only)
                ana a                       ; set flags
                jnz i8085                   ; anything but all zeros means an 8085 CPU

;8080 CPU detected
i8080:          mvi a,117                   ; for the 8080 SBC using a 1.843 MHz clock...
                sta delay_value             ; for the millisecond delay subroutine
                mvi a,99H
                out cwr_80                  ; program 8255 port B as output, ports A and C as inputs
                xra a
                sta cpu_type                ; 0 for 8080 CPU
                jmp clear

;8085 CPU detected
i8085:          mvi a,199                   ; for the 8085 SBC using a 3.072 MHz clock...
                sta delay_value             ; for the millisecond delay subroutine
                mvi a,02H
                out cmd_85                  ; program 8155 port B as output, ports A and C as inputs
                mvi a,5                     
                sta cpu_type                ; 5 for 8085 CPU

clear:          xra a
                out led_port                ; turn off all LEDs                
                sta esc_count               ; clear escape key count
                lxi h,signon_txt1           ; address of the initial part of the sign on message
                call puts                   ; print "Mini-Altair 808..."
                lda cpu_type                ; "0" for 8080 CPU or "5" for 8085 CPU
                call outhexnibble           ; print "0" for 8080 CPU or "5" for 8085 CPU
                lxi h,signon_txt2
                call puts                   ; print the remainder of the sign on message

showmenu:       lxi h,menu_txt
                call puts                   ; print the menu
prompt:         lxi h,prompt_txt
                call puts                   ; print the prompt

getinput:       call cin                    ; get a character from input
                cpi ':'                     ; is it the start-of-record character for a hex download?
                jnz testfor1
                jmp startfound              ; start of record detected, jump into Intel hex file download routine

testfor1:       cpi '1'                     ; load 4K BASIC?
                jnz testfor2
                lxi h,basic4k               ; address of start of Altair 4K BASIC image
                lxi b,basic4kend-basic4k    ; 4096 bytes to copy
                jmp loadBASIC               ; load 4K BASIC image into RAM

testfor2:       cpi '2'                     ; load 8K BASIC?
                jnz testfor3
                lxi h,basic8k               ; address of start of Altair 8K BASIC image
                lxi b,basic8kend-basic8k    ; 8192 bytes to copy
                jmp loadBASIC               ; load 8K BASIC image into RAM

testfor3:       cpi '3'                     ; load ROM BASIC?
                jnz testfor4
                jmp basic16k                ; jump to 16K BASIC in ROM

testfor4:       cpi '4'                     ; BASIC warm start?
                jnz testfor5

                call newline
                lxi B,basic16kbytes
                lxi H,ram_start+0040H
                lxi D,8
                call block_compare          ; check if 16K BASIC has been previously loaded
                jc  0C0A1H                  ; yes, jump to 16K BASIC warm start address

                lxi B,basic4kbytes
                lxi H,ram_start
                lxi D,8
                call block_compare          ; check if 4K BASIC has been previously loaded
                jc  ram_start               ; yes, jump to 4K BASIC warm start address

                lxi B,basic8kbytes
                lxi H,ram_start
                lxi D,8
                call block_compare          ; check if 8K BASIC has been previously loaded
                jc  ram_start               ; yes, jump to 8K BASIC warm start address

noBASIC:        lxi h,notloaded_txt         ; no, display "BASIC not loaded"
                call puts
                jmp prompt

testfor5:       cpi '5'                     ; system monitor?
                jnz testfor6
                jmp monitor                 ; execute the monitor program

testfor6:       cpi '6'                     ; hex file download?
                jnz testforESC
                jmp download                ; Intel Hex file download

; pressing 'Escape' three times followed by '?' displays the copyright message
testforESC:     cpi 1BH                     ; is it the escape key?
                jnz testforQMark            ; no, next test

                lda esc_count
                inr a                       ; increment count each time the escape key is received
                sta esc_count
                jmp getinput                ; go back for another character

testforQMark:   cpi '?'                     ; is it '?'
                jnz resetcount              ; jump if not

                lda esc_count
                cpi 3                       ; were the previous three keys 'escape'?
                jnz resetcount              ; jump if not

                lxi h,signon_txt1           ; address of the initial part of the sign on message
                call puts                   ; print "Mini-Altair 808..."
                lda cpu_type
                call outhexnibble           ; print the CPU type "0" or "5" depending on CPU
                lxi h,signon_txt2           
                call puts                   ; print the remainder of the sign on message
                lxi h,copyright_txt
                call puts                   ; print the copyright message
                xra a
                sta esc_count               ; reset count back to start
                jmp prompt                  ; prompt for another character

resetcount:     xra a
                sta esc_count               ; reset count back to start
                jmp showmenu                ; display the menu and get another character

; -----------------------------------------------------------------------------------------
; compare two blocks of memory. HL contains the starting address of the first block.
; BC contains the starting address of the second block. DE contains the byte count.
; return with carry set if the blocks match.
; -----------------------------------------------------------------------------------------
block_compare:  ldax B                      ;fetch byte from the first block
                cmp M                       ;compare it to the byte from the second block
                jz block_comp1              ;jump if they match
                stc                         ;else, return with carry clear in no match
                cmc
                ret

block_comp1:    inx B                       ;next byte from the first block
                inx H                       ;next byte from the second block
                dcx D                       ;decrement the count
                mov A,D
                ora E                       ;has the count reached zero?
                jnz block_compare           ;loop back if more bytes to compare
                stc                         ;else, return with carry set if both blocks match
                ret

; bytes loaded into RAM by 4K, 8K and 16K BASIC respectively
; used to test if BASIC has been loaded.
basic4kbytes:   db 0F3H,0C3H,001H,002H,09EH,004H,007H,008H
basic8kbytes:   db 0F3H,0C3H,0F7H,002H,038H,007H,0E5H,00DH
basic16kbytes:  db 0C3H,0A1H,0C0H,0C3H,090H,000H,0C3H,098H

; -----------------------------------------------------------------------------------------
; Download a file in Intel hex format. Jump to entry address when the download is finished.
; A hard reset will be required if a checksum error is encountered durning the download.
; -----------------------------------------------------------------------------------------
download:       lxi h,waiting_txt           ; address of the download message
                call puts                   ; print "Waiting for hex download..."

; wait for the start of record character ':'
getstart:       call cin                    ; get the first character
                ani 01111111B               ; mask off MSB
                cpi 1BH                     ; escape?
                jz showmenu
                cpi 03H                     ; control C?
                jz showmenu
                call cout                   ; echo the character
                cpi ':'                     ; test for start of record
                jnz getstart                ; go back if not start of record

; the start of record character ":" has been received
startfound:     call getrecord              ; get the next Intel hex record
                jc getstart                 ; loop back for more records until finished

; the last record has been received. the entry address is in HL
                push h                      ; push the entry address on the stack
                push h                      ; push it again
                lxi h,signoff_txt           ; print sign off message
                call puts
                pop h                       ; pop the entry address
                call outhexword             ; print the entry address
                call newline                ; start on a new line
                lxi b,500
                call delay                  ; 500 mSec delay
                pop h                       ; pop the entry address
                pchl                        ; jump to entry address

; loads a record in Intel hex format. returns with carry cleared when the last record was received.
getrecord:      call getbyte                ; start of record received, get length of record (1 byte)
                ana a                       ; test for zero (zero length means last record)
                jnz continue                ; jump if this is not the last record

                ; receive the last record
                call getword                ; get address (2 bytes) of last record
                call getbyte                ; get record type (1 byte) of last record
                call getbyte                ; get checksum (1 byte) of last record
                call cin                    ; get the carriage return at the end of the line
                stc
                cmc                         ; clear carry to indicate last record received
                ret                         ; return when last record is received

continue:       mov c,a                     ; save the length in C as the checksum
                mov b,a                     ; B is the counter for the number of data bytes to receive
                call getword                ; get the address (2 bytes)
                mov a,c                     ; get the checksum from C
                add h                       ; add high byte of address to the checksum
                add l                       ; add low byte of address to the checksum
                mov c,a                     ; save the checksum in C

                call getbyte                ; get the next byte as type (1 byte)
                add c                       ; add the type byte to the checksum
                mov c,a                     ; save checksum in C

nextbyte:       call getbyte                ; type received, get the data (1 byte)
                mov m,a                     ; save the data byte in memory
                inx h                       ; point to next memory location
                add c                       ; add the data byte to the checksum
                mov c,a                     ; save the checksum in c
                dcr b                       ; reduce count of remaining data bytes
                jnz nextbyte                ; get the next byte in the record
                call getbyte                ; all data bytes received, get the checksum (1 byte)
                add c                       ; add to computed checksum
                jnz checksumerror           ; jump if not ok
                stc                         ; set carry to indicate more records to follow
                ret

checksumerror:  lxi h,chksumerr_txt
                call puts                   ; print "Checksum error! Press RESET."
                hlt                         ; stay here until hard reset

; gets four ascii characters - a 16 bit value into HL, return with carry set if everything is ok
getword:        call getbyte                ; get first byte
                rnc                         ; if bad checksum return immediately, don't wait for second byte
                mov h,a                     ; save the high byte of the address in H
                call getbyte                ; get second byte
                mov l,a                     ; save the low byte of the address in L
                ret

; gets two ascii characters (1 byte), from the terminal, returns with the byte in the accumulator. cy=0 if fails
getbyte:        push b                      ; save BC pair
                call getnibble              ; get first nibble
                jnc exitgb                  ; if bad checksum return immediately, don't wait for second nibble
                rlc                         ; shift into...
                rlc                         ; upper nibble...
                rlc                         ; of result.
                rlc                         ; so we can insert lower nibble
                mov b,a                     ; keep high digit in b
                call getnibble              ; get second digit
                jnc exitgb                  ; exit immediately if bad checksum
                ora b                       ; insert high digit
                stc                         ; set carry flag to indicate success
exitgb:         pop b                       ; restore BC pair
                ret

; gets an ACSII character from the terminal which represents a nibble of the data byte
getnibble:      call cin                    ; get a character
                ani 01111111B               ; mask off MSB
                cpi ' '                     ; test for blank (abort1)
                rz                          ; if so, return indicating bad (cy=0)
                cpi 0DH                     ; test for <cr> (abort2)
                rz                          ; if so, return indicating bad
                cpi '0'                     ; test for invalid (ASCII code less than '0')
                jc getnibble                ; if invalid, go back for another nibble
                cpi 'G'                     ; test for invalid (ASCII code greater than 'F')
                jnc getnibble               ; if invalid, go back for another nibble
                call cout                   ; valid digit, display the character
                cpi 3AH                     ; test for invalid
                jc number                   ; if ok, we are in
                cpi 'A'                     ; test for invalid
                jc getnibble                ; if bad, ignore
                sui 7                       ; convert to digit
number:         sui 30H                     ; convert to binary
                stc                         ; indicate success
                ret

; -----------------------------------------------------------------------------------------
; Copy the Altair BASIC image from ROM to RAM. Jump to the start of BASIC at 0000H
; after the image is loaded.
; -----------------------------------------------------------------------------------------
loadBASIC:      lxi d,ram_start             ; beginning of ram
                call blockcopy              ; copy the 8192 bytes of the Altair 8K BASIC image from ROM to RAM
                call newline
                lxi b,500
                call delay                  ; 500 mSec delay
                jmp ram_start               ; jump to start of BASIC interpreter code at address 0000

; ------------------------------------------------------------------------------------
; copy a block of BC bytes from address pointed to by HL to address pointed to by DE.
; HL contains the source address of the block to be copied
; DE contains the destination address of the block to be copied
; BC contains the number of bytes to be copied.
; ------------------------------------------------------------------------------------
blockcopy:      mov a,m                     ; retrieve the source byte pointed to by HL
                stax d                      ; store it at the destination pointed to by DE
                inx h                       ; next source address
                inx d                       ; next destination address
                dcx b                       ; decrement count
                mov a,c
                cpi 00H                     ; end of page?
                jnz blockcopy
                mov a,b
                cpi 00H                     ; last page
                jnz blockcopy
                ret

; ------------------------------------------------------------------------------------
; wait for a character from the serial port, return character in A
; ------------------------------------------------------------------------------------
cin:            in acia_status
                rrc
                jnc cin                    ; go back and try again if the receive register is empty
                in acia_data               ; input the character from the receive register
                ret

; ------------------------------------------------------------------------------------
; type a carriage return and line feed to start a new line on the terminal
; (falls through to the cout function below)
; ------------------------------------------------------------------------------------
newline:        mvi a,cr
                call cout
                mvi a,lf

; ------------------------------------------------------------------------------------
; type the character in A to the serial port
; ------------------------------------------------------------------------------------
cout:           push psw
cout1:          in acia_status
                rrc
                rrc
                jnc  cout1
                pop psw
                out acia_data               ; output the character to the transmit register
                ret

; ------------------------------------------------------------------------------------
; type a string pointed to by HL terminated by zero to the serial port
; ------------------------------------------------------------------------------------
puts:           mov a,m                     ; retrieve the character
                ora a
                rz                          ; return if end of string
                call cout                   ; send the character out to the console
                inx h                       ; next address
                jmp puts

; ------------------------------------------------------------------------------------
; type the word in HL as four hex digits
; ------------------------------------------------------------------------------------
outhexword:     push    psw                 ; save psw
                mov     a,h                 ; get high byte
                call    outhexbyte          ; and type it
                mov     a,l                 ; get low byte
                call    outhexbyte          ; and type it
                pop     psw                 ; restore psw
                ret                         ; and return

; ------------------------------------------------------------------------------------
; type the byte in A as two hex digits
; ------------------------------------------------------------------------------------
outhexbyte:     push psw                    ;save PSW
                rrc                         ; shift
                rrc                         ; to
                rrc                         ;  left
                rrc                         ;   nibble
                call outhexnibble           ; type hex nibble
                pop psw                     ; restore data
                call outhexnibble           ; type right nibble
                ret                         ; and exit

; ------------------------------------------------------------------------------------
; type the 4 bit nibble in A as a hex digit
; ------------------------------------------------------------------------------------
outhexnibble:   push psw                    ; save PSW
                ani 0FH                     ; isolate nibble b3>b0
                cpi 10                      ; see if >9
                jc  $+5                     ; nibble <=9
                adi 7                       ; adjust alpha char
                adi '0'                     ; add in ascii 0
                call cout                   ; type nibble
                pop psw
                ret

;*********************************************************
; delay 1 millisecond times the value in BC.
; "delay_value" has been set previously depending on the CPU type.
; (117 for 8080, 199 for 8085)
;*********************************************************
delay:          push psw
                push d
                push b
                lda delay_value
                mov e,a
d1:             mov d,e
d2:             dcr d
                jnz d2
                dcx b
                mov a,b
                ora c
                jnz d1
                pop b
                pop d
                pop psw
                ret

signon_txt1:    db      cr,lf,lf,"Mini-Altair 808",0
signon_txt2:    db      " Version assembled ",DATE," at ",TIME,cr,lf,0
copyright_txt:  db      "Portions Copyright 2021 by Jim Loos",cr,lf,lf,0
menu_txt:       db      cr,lf,lf
                db      "1 - Altair 4K BASIC cold start",cr,lf
                db      "2 - Altair 8K BASIC cold start",cr,lf
                db      "3 - Altair ROM BASIC cold start",cr,lf
                db      "4 - Altair BASIC warm start",cr,lf
                db      "5 - System Monitor",cr,lf
                db      "6 - Intel Hex file download",cr,lf,lf,0
prompt_txt:     db      ">>",0
signoff_txt:    db      cr,lf,"Jumping to entry address ",0
waiting_txt:    db      cr,lf,"Waiting for hex download...",cr,lf,lf,0
chksumerr_txt:  db      cr,lf,"Checksum error! Press RESET.",cr,lf,0
notloaded_txt:  db      cr,lf,"BASIC has not yet been loaded!",cr,lf,lf,0

            org 8470H

; in the BASIC images, all instances of 0DBH,0FFH (IN 0FFH) have been replaced with 03EH,000H (MVI Aa,00)
; to eliminate the requirement for the Altair's "sense switches".

            ; Altair 4K BASIC 4.0 image
basic4k:    db  0F3H,0C3H,0E6H,00DH,09EH,004H,007H,008H,07EH,0E3H,0BEH,023H,0E3H,0C2H,0D6H,001H,023H,07EH,0FEH,03AH,0D0H,0C3H,06CH,004H,0F5H,03AH,027H,000H,0C3H,07BH,003H,000H
            db  07CH,092H,0C0H,07DH,093H,0C9H,001H,00DH,03AH,07AH,001H,0B7H,0C2H,0E8H,009H,0C9H,0E3H,022H,041H,000H,0E1H,0C3H,03BH,000H,0C9H,054H,001H,04EH,023H,046H,023H,0C5H
            db  0C3H,040H,000H,0F2H,009H,0B0H,00AH,006H,00AH,0A6H,004H,02FH,00CH,06DH,00CH,0A0H,00CH,0C5H,04EH,044H,0C6H,04FH,052H,0CEH,045H,058H,054H,0C4H,041H,054H,041H,0C9H
            db  04EH,050H,055H,054H,0C4H,049H,04DH,0D2H,045H,041H,044H,0CCH,045H,054H,0C7H,04FH,054H,04FH,0D2H,055H,04EH,0C9H,046H,0D2H,045H,053H,054H,04FH,052H,045H,0C7H,04FH
            db  053H,055H,042H,0D2H,045H,054H,055H,052H,04EH,0D2H,045H,04DH,0D3H,054H,04FH,050H,0D0H,052H,049H,04EH,054H,0CCH,049H,053H,054H,0C3H,04CH,045H,041H,052H,0CEH,045H
            db  057H,0D4H,041H,042H,028H,0D4H,04FH,0D4H,048H,045H,04EH,0D3H,054H,045H,050H,0ABH,0ADH,0AAH,0AFH,0BEH,0BDH,0BCH,0D3H,047H,04EH,0C9H,04EH,054H,0C1H,042H,053H,0D5H
            db  053H,052H,0D3H,051H,052H,0D2H,04EH,044H,0D3H,049H,04EH,080H,0FDH,001H,0E3H,003H,057H,006H,003H,005H,0F2H,005H,024H,007H,004H,006H,010H,005H,0DDH,004H,0A9H,002H
            db  024H,005H,077H,004H,0CCH,004H,0EDH,004H,005H,005H,0FDH,001H,065H,005H,09FH,003H,0AEH,002H,09DH,002H,079H,01EH,008H,079H,018H,008H,07CH,0F1H,008H,07CH,03DH,009H
            db  04EH,046H,053H,04EH,052H,047H,04FH,044H,046H,043H,04FH,056H,04FH,04DH,055H,04CH,042H,053H,044H,044H,02FH,030H,049H,044H,040H,00FH,0FFH,0FFH,041H,00FH,02CH,000H
            db  000H,000H,000H,000H,000H,000H,000H,020H,042H,059H,054H,045H,053H,020H,046H,052H,045H,045H,00DH,034H,04BH,020H,042H,041H,053H,049H,043H,020H,034H,02EH,030H,00DH
            db  043H,04FH,050H,059H,052H,049H,047H,048H,054H,020H,04DH,049H,054H,053H,020H,031H,039H,037H,036H,000H,093H,003H,084H,003H,081H,004H,000H,000H,069H,00DH,092H,00DH
            db  000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H
            db  000H,000H,000H,000H,000H,000H,000H,000H,000H,020H,045H,052H,052H,04FH,052H,000H,020H,049H,04EH,020H,000H,00DH,04FH,04BH,00DH,000H,021H,004H,000H,039H,07EH,023H
            db  0FEH,081H,0C0H,0F7H,0E3H,0E7H,001H,00DH,000H,0E1H,0C8H,009H,0C3H,09EH,001H,0CDH,0C7H,001H,0C5H,0E3H,0C1H,0E7H,07EH,002H,0C8H,00BH,02BH,0C3H,0B5H,001H,0E5H,02AH
            db  073H,001H,006H,000H,009H,009H,03EH,0E5H,03EH,0DEH,095H,06FH,03EH,0FFH,09CH,067H,039H,0E1H,0D8H,01EH,00CH,001H,01EH,002H,001H,01EH,014H,0CDH,0BDH,002H,0CDH,098H
            db  005H,021H,000H,001H,057H,03EH,03FH,0DFH,019H,07EH,0DFH,0D7H,0DFH,021H,089H,001H,0CDH,0B1H,005H,02AH,01AH,001H,07CH,0A5H,03CH,0C4H,03DH,00BH,001H,0C0H,0C1H,03EH
            db  0C1H,021H,095H,001H,0CDH,0E6H,00DH,021H,0FFH,0FFH,022H,01AH,001H,0CDH,049H,003H,0D7H,03CH,03DH,0CAH,007H,002H,0F5H,0CDH,0ABH,004H,0D5H,0CDH,0D4H,002H,047H,0D1H
            db  0F1H,0D2H,04CH,004H,0D5H,0C5H,0D7H,0B7H,0F5H,0CDH,085H,002H,0C5H,0D2H,041H,002H,0EBH,02AH,06FH,001H,01AH,002H,003H,013H,0E7H,0C2H,034H,002H,060H,069H,022H,06FH
            db  001H,0D1H,0F1H,0CAH,068H,002H,02AH,06FH,001H,0E3H,0C1H,009H,0E5H,0CDH,0AFH,001H,0E1H,022H,06FH,001H,0EBH,074H,0D1H,023H,023H,073H,023H,072H,023H,011H,01FH,001H
            db  01AH,077H,023H,013H,0B7H,0C2H,060H,002H,0CDH,0AAH,002H,023H,0EBH,062H,06BH,07EH,023H,0B6H,0CAH,007H,002H,023H,023H,023H,0AFH,0BEH,023H,0C2H,079H,002H,0EBH,073H
            db  023H,072H,0C3H,06DH,002H,02AH,01CH,001H,044H,04DH,07EH,023H,0B6H,02BH,0C8H,0C5H,0F7H,0F7H,0E1H,0E7H,0E1H,0C1H,03FH,0C8H,03FH,0D0H,0C3H,088H,002H,0C0H,02AH,01CH
            db  001H,0AFH,077H,023H,077H,023H,022H,06FH,001H,0C0H,02AH,01CH,001H,02BH,022H,06BH,001H,0CDH,077H,004H,02AH,06FH,001H,022H,071H,001H,022H,073H,001H,0C1H,02AH,018H
            db  001H,0F9H,0AFH,06FH,0E5H,0C5H,02AH,06BH,001H,0C9H,03EH,03FH,0DFH,03EH,020H,0DFH,0CDH,049H,003H,023H,00EH,005H,011H,01FH,001H,07EH,0FEH,020H,0CAH,01CH,003H,047H
            db  0FEH,022H,0CAH,02FH,003H,0B7H,0CAH,036H,003H,0D5H,011H,050H,000H,0C5H,006H,07FH,04EH,04EH,0EBH,023H,0B6H,0F2H,0F3H,002H,004H,07EH,0E6H,07FH,0CAH,018H,003H,0B9H
            db  0C2H,0F3H,002H,0EBH,0E5H,013H,01AH,0B7H,0FAH,015H,003H,04FH,0D7H,0B9H,0CAH,005H,003H,0E1H,0C3H,0F0H,002H,048H,0F1H,0EBH,0EBH,079H,0C1H,0D1H,023H,012H,013H,00CH
            db  0D6H,08EH,0C2H,0D9H,002H,047H,07EH,0B7H,0CAH,036H,003H,0B8H,0CAH,01CH,003H,023H,012H,00CH,013H,0C3H,026H,003H,021H,01EH,001H,012H,013H,012H,013H,012H,0C9H,005H
            db  02BH,0DFH,0C2H,04EH,003H,0DFH,0CDH,098H,005H,021H,01FH,001H,006H,001H,0CDH,093H,003H,04FH,0FEH,00DH,0CAH,093H,005H,0FEH,040H,0CAH,045H,003H,0FEH,05FH,0CAH,03FH
            db  003H,0FEH,020H,0DAH,04EH,003H,0FEH,07DH,0D2H,04EH,003H,078H,0FEH,048H,03EH,007H,0D2H,077H,003H,079H,071H,023H,004H,0DFH,0C3H,04EH,003H,0FEH,048H,0CCH,098H,005H
            db  03CH,032H,027H,000H,0DBH,010H,0E6H,002H,0CAH,084H,003H,0F1H,0D3H,011H,0F5H,000H,000H,0F1H,0C9H,0DBH,010H,0E6H,001H,0CAH,093H,003H,0DBH,011H,0E6H,07FH,0C9H,0CDH
            db  0ABH,004H,0C0H,0C1H,0CDH,085H,002H,0C5H,0E1H,0F7H,0C1H,078H,0B1H,0CAH,001H,002H,0CDH,081H,004H,0C5H,0CDH,098H,005H,0F7H,0E3H,0CDH,045H,00BH,03EH,020H,0E1H,0DFH
            db  07EH,0B7H,023H,0CAH,0A8H,003H,0F2H,0BFH,003H,0D6H,07FH,04FH,011H,051H,000H,01AH,013H,0B7H,0F2H,0CFH,003H,00DH,0C2H,0CFH,003H,0DFH,01AH,013H,0B7H,0F2H,0D9H,003H
            db  0C3H,0C0H,003H,0CDH,010H,005H,0E3H,0CDH,09AH,001H,0D1H,0C2H,0F0H,003H,009H,0F9H,0EBH,00EH,008H,0CDH,0BEH,001H,0E5H,0CDH,003H,005H,0E3H,0E5H,02AH,01AH,001H,0E3H
            db  0CFH,095H,0CDH,09AH,006H,0E5H,0CDH,02BH,00AH,0E1H,0C5H,0D5H,001H,000H,081H,051H,05AH,07EH,0FEH,097H,03EH,001H,0C2H,022H,004H,0CDH,09BH,006H,0E5H,0CDH,02BH,00AH
            db  0EFH,0E1H,0C5H,0D5H,0F5H,033H,0E5H,02AH,06BH,001H,0E3H,006H,081H,0C5H,033H,0CDH,081H,004H,07EH,0FEH,03AH,0CAH,04CH,004H,0B7H,0C2H,0D6H,001H,023H,07EH,023H,0B6H
            db  0CAH,0FFH,001H,023H,05EH,023H,056H,0EBH,022H,01AH,001H,0EBH,0D7H,011H,02FH,004H,0D5H,0C8H,0D6H,080H,0DAH,010H,005H,0FEH,014H,0D2H,0D6H,001H,007H,04FH,006H,000H
            db  0EBH,021H,0CCH,000H,009H,04EH,023H,046H,0C5H,0EBH,0D7H,0C9H,0FEH,020H,0CAH,010H,000H,0FEH,030H,03FH,03CH,03DH,0C9H,0EBH,02AH,01CH,001H,02BH,022H,075H,001H,0EBH
            db  0C9H,0DBH,010H,0E6H,001H,0C8H,0CDH,093H,003H,0FEH,003H,0CAH,0FDH,001H,07EH,0FEH,041H,0D8H,0FEH,05BH,03FH,0C9H,0D7H,0CDH,09AH,006H,0EFH,0FAH,0A6H,004H,03AH,07AH
            db  001H,0FEH,090H,0DAH,085H,00AH,01EH,008H,0C3H,0DBH,001H,02BH,011H,000H,000H,0D7H,0D0H,0E5H,0F5H,021H,098H,019H,0E7H,0DAH,0D6H,001H,062H,06BH,019H,029H,019H,029H
            db  0F1H,0D6H,030H,05FH,016H,000H,019H,0EBH,0E1H,0C3H,0AFH,004H,00EH,003H,0CDH,0BEH,001H,0C1H,0E5H,0E5H,02AH,01AH,001H,0E3H,03EH,08CH,0F5H,033H,0C5H,0CDH,0ABH,004H
            db  0C0H,0CDH,085H,002H,060H,069H,02BH,0D8H,01EH,00EH,0C3H,0DBH,001H,0C0H,016H,0FFH,0CDH,09AH,001H,0F9H,0FEH,08CH,01EH,004H,0C2H,0DBH,001H,0E1H,022H,01AH,001H,021H
            db  02FH,004H,0E3H,001H,03AH,00EH,000H,07EH,0B7H,0C8H,0B9H,0C8H,023H,0C3H,007H,005H,0CDH,029H,007H,0CFH,09DH,0D5H,0CDH,09AH,006H,0E3H,022H,06BH,001H,0E5H,0CDH,037H
            db  00AH,0D1H,0E1H,0C9H,0CDH,09AH,006H,07EH,0CDH,010H,00AH,016H,000H,0D6H,09CH,0DAH,040H,005H,0FEH,003H,0D2H,040H,005H,0FEH,001H,017H,0B2H,057H,0D7H,0C3H,02DH,005H
            db  07AH,0B7H,0CAH,0D6H,001H,0F5H,0CDH,09AH,006H,0CFH,096H,02BH,0F1H,0C1H,0D1H,0E5H,0F5H,0CDH,05AH,00AH,03CH,017H,0C1H,0A0H,0E1H,0CAH,005H,005H,0D7H,0DAH,0DDH,004H
            db  0C3H,051H,004H,02BH,0D7H,0CAH,098H,005H,0C8H,0FEH,022H,0CCH,0B0H,005H,0CAH,063H,005H,0FEH,094H,0CAH,0D5H,005H,0E5H,0FEH,02CH,0CAH,0C1H,005H,0FEH,03BH,0CAH,0EDH
            db  005H,0C1H,0CDH,09AH,006H,0E5H,0CDH,050H,00BH,0CDH,0B1H,005H,03EH,020H,0DFH,0E1H,0C3H,063H,005H,036H,000H,021H,01EH,001H,03EH,00DH,032H,027H,000H,0DFH,03EH,00AH
            db  0DFH,03AH,026H,000H,03DH,032H,027H,000H,0C8H,0F5H,0AFH,0DFH,0F1H,0C3H,0A4H,005H,023H,07EH,0B7H,0C8H,023H,0FEH,022H,0C8H,0DFH,0FEH,00DH,0CCH,098H,005H,0C3H,0B1H
            db  005H,03AH,027H,000H,0FEH,038H,0D4H,098H,005H,0D2H,0EDH,005H,0D6H,00EH,0D2H,0CCH,005H,02FH,0C3H,0E4H,005H,0CDH,096H,004H,0CFH,029H,02BH,0E5H,03AH,027H,000H,02FH
            db  083H,0D2H,0EDH,005H,03CH,047H,03EH,020H,0DFH,005H,0C2H,0E8H,005H,0E1H,0D7H,0C3H,068H,005H,0E5H,02AH,01AH,001H,01EH,016H,023H,07DH,0B4H,0CAH,0DBH,001H,0CDH,0CAH
            db  002H,0C3H,009H,006H,0E5H,02AH,075H,001H,0F6H,0AFH,032H,06AH,001H,0E3H,001H,0CFH,02CH,0CDH,029H,007H,0E3H,0D5H,07EH,0FEH,02CH,0CAH,02EH,006H,0B7H,0C2H,0D6H,001H
            db  03AH,06AH,001H,0B7H,023H,0C2H,044H,006H,03EH,03FH,0DFH,0CDH,0CAH,002H,0D1H,023H,0CDH,015H,005H,0E3H,02BH,0D7H,0C2H,00FH,006H,0D1H,03AH,06AH,001H,0B7H,0C8H,0EBH
            db  0C2H,07CH,004H,0E1H,0F7H,079H,0B0H,01EH,006H,0CAH,0DBH,001H,023H,0D7H,0FEH,083H,0C2H,043H,006H,0C1H,0C3H,02EH,006H,0CDH,029H,007H,022H,06BH,001H,0CDH,09AH,001H
            db  0F9H,0D5H,07EH,023H,0F5H,0D5H,01EH,000H,0C2H,0DBH,001H,0CDH,01DH,00AH,0E3H,0E5H,0CDH,012H,008H,0E1H,0CDH,037H,00AH,0E1H,0CDH,02EH,00AH,0E5H,0CDH,05AH,00AH,0E1H
            db  0C1H,090H,0CDH,02EH,00AH,0CAH,091H,006H,0EBH,022H,01AH,001H,069H,060H,0C3H,02BH,004H,0F9H,02AH,06BH,001H,0C3H,02FH,004H,0CFH,028H,02BH,016H,000H,0D5H,00EH,001H
            db  0CDH,0BEH,001H,0CDH,0D4H,006H,022H,06DH,001H,02AH,06DH,001H,0C1H,07EH,016H,000H,0D6H,098H,0D8H,0FEH,004H,0D0H,05FH,007H,083H,05FH,021H,0F4H,000H,019H,078H,056H
            db  0BAH,0D0H,023H,0C5H,001H,0A9H,006H,0C5H,04AH,0CDH,010H,00AH,051H,0F7H,02AH,06DH,001H,0C3H,09DH,006H,0D7H,0DAH,0C1H,00AH,0CDH,08EH,004H,0D2H,001H,007H,0FEH,098H
            db  0CAH,0D4H,006H,0FEH,02EH,0CAH,0C1H,00AH,0FEH,099H,0CAH,0F8H,006H,0D6H,09FH,0D2H,00BH,007H,0CDH,098H,006H,0CFH,029H,0C9H,0CDH,0D4H,006H,0E5H,0CDH,008H,00AH,0E1H
            db  0C9H,0CDH,029H,007H,0E5H,0EBH,0CDH,01DH,00AH,0E1H,0C9H,006H,000H,007H,04FH,0C5H,0D7H,0CDH,0F2H,006H,0E3H,011H,0FFH,006H,0D5H,001H,043H,000H,009H,0F7H,0C9H,02BH
            db  0D7H,0C8H,0CFH,02CH,001H,01FH,007H,0C5H,0F6H,0AFH,032H,069H,001H,046H,0CDH,08EH,004H,0DAH,0D6H,001H,0AFH,04FH,0D7H,0D2H,03CH,007H,04FH,0D7H,0D6H,028H,0CAH,098H
            db  007H,0E5H,02AH,071H,001H,0EBH,02AH,06FH,001H,0E7H,0CAH,060H,007H,079H,096H,023H,0C2H,055H,007H,078H,096H,023H,0CAH,090H,007H,023H,023H,023H,023H,0C3H,049H,007H
            db  0E1H,0E3H,0D5H,011H,004H,007H,0E7H,0D1H,0CAH,093H,007H,0E3H,0E5H,0C5H,001H,006H,000H,02AH,073H,001H,0E5H,009H,0C1H,0E5H,0CDH,0AFH,001H,0E1H,022H,073H,001H,060H
            db  069H,022H,071H,001H,02BH,036H,000H,0E7H,0C2H,084H,007H,0D1H,073H,023H,072H,023H,0EBH,0E1H,0C9H,032H,07AH,001H,0E1H,0C9H,0C5H,03AH,069H,001H,0F5H,0CDH,096H,004H
            db  0CFH,029H,0F1H,032H,069H,001H,0E3H,0EBH,029H,029H,0E5H,02AH,071H,001H,001H,0C1H,009H,0EBH,0E5H,02AH,073H,001H,0E7H,0EBH,0D1H,0CAH,0DBH,007H,0F7H,0E3H,0E7H,0E1H
            db  0F7H,0C2H,0AFH,007H,03AH,069H,001H,0B7H,01EH,012H,0C2H,0DBH,001H,0D1H,01BH,0E3H,0E7H,01EH,010H,0D2H,0DBH,001H,0D1H,019H,0D1H,0EBH,0C9H,073H,023H,072H,023H,011H
            db  02CH,000H,03AH,069H,001H,0B7H,0CAH,0EFH,007H,0D1H,0D5H,013H,013H,013H,013H,0D5H,073H,023H,072H,023H,0E5H,019H,0CDH,0C7H,001H,022H,073H,001H,0D1H,02BH,036H,000H
            db  0E7H,0C2H,0FDH,007H,0C3H,0CDH,007H,050H,01EH,000H,006H,090H,0C3H,0F8H,009H,021H,019H,00CH,0CDH,02EH,00AH,0C3H,020H,008H,0C1H,0D1H,0CDH,008H,00AH,021H,0C1H,0D1H
            db  078H,0B7H,0C8H,03AH,07AH,001H,0B7H,0CAH,020H,00AH,090H,0D2H,03AH,008H,02FH,03CH,0EBH,0CDH,010H,00AH,0EBH,0CDH,020H,00AH,0C1H,0D1H,0F5H,0CDH,045H,00AH,067H,0F1H
            db  0CDH,0D7H,008H,0B4H,021H,077H,001H,0F2H,05BH,008H,0CDH,0B7H,008H,0D2H,08CH,008H,023H,034H,0CAH,0B2H,008H,0CDH,0E4H,008H,0C3H,08CH,008H,0AFH,090H,047H,07EH,09BH
            db  05FH,023H,07EH,09AH,057H,023H,07EH,099H,04FH,0DCH,0C3H,008H,026H,000H,079H,0B7H,0FAH,08CH,008H,0FEH,0E0H,0CAH,0CCH,009H,025H,078H,087H,047H,0CDH,09EH,008H,07CH
            db  0F2H,073H,008H,021H,07AH,001H,086H,077H,0D2H,0CCH,009H,0C8H,078H,021H,07AH,001H,0B7H,0FCH,0A8H,008H,046H,023H,07EH,0E6H,080H,0A9H,04FH,0C3H,020H,00AH,07BH,017H
            db  05FH,07AH,017H,057H,079H,08FH,04FH,0C9H,01CH,0C0H,014H,0C0H,00CH,0C0H,00EH,080H,034H,0C0H,01EH,00AH,0C3H,0DBH,001H,07EH,083H,05FH,023H,07EH,08AH,057H,023H,07EH
            db  089H,04FH,0C9H,021H,07BH,001H,07EH,02FH,077H,0AFH,06FH,090H,047H,07DH,09BH,05FH,07DH,09AH,057H,07DH,099H,04FH,0C9H,006H,000H,03CH,06FH,0AFH,02DH,0C8H,0CDH,0E4H
            db  008H,0C3H,0DBH,008H,079H,01FH,04FH,07AH,01FH,057H,07BH,01FH,05FH,078H,01FH,047H,0C9H,0C1H,0D1H,0EFH,0C8H,02EH,000H,0CDH,0A9H,009H,079H,032H,025H,009H,0EBH,022H
            db  020H,009H,001H,000H,000H,050H,058H,021H,06CH,008H,0E5H,021H,013H,009H,0E5H,0E5H,021H,077H,001H,07EH,023H,0E5H,02EH,008H,01FH,067H,079H,0D2H,027H,009H,0E5H,021H
            db  000H,000H,019H,0D1H,0CEH,000H,0EBH,0CDH,0E5H,008H,02DH,07CH,0C2H,018H,009H,0E1H,0C9H,0CDH,010H,00AH,001H,020H,084H,011H,000H,000H,0CDH,020H,00AH,0C1H,0D1H,0EFH
            db  0CAH,0D9H,001H,02EH,0FFH,0CDH,0A9H,009H,034H,034H,02BH,07EH,032H,06EH,009H,02BH,07EH,032H,06AH,009H,02BH,07EH,032H,066H,009H,041H,0EBH,0AFH,04FH,057H,05FH,032H
            db  071H,009H,0E5H,0C5H,07DH,0D6H,000H,06FH,07CH,0DEH,000H,067H,078H,0DEH,000H,047H,03EH,000H,0DEH,000H,03FH,0D2H,07FH,009H,032H,071H,009H,0F1H,0F1H,037H,0D2H,0C1H
            db  0E1H,079H,03CH,03DH,01FH,0FAH,08DH,008H,017H,0CDH,09EH,008H,029H,078H,017H,047H,03AH,071H,009H,017H,032H,071H,009H,079H,0B2H,0B3H,0C2H,062H,009H,0E5H,021H,07AH
            db  001H,035H,0E1H,0C2H,062H,009H,0C3H,0B2H,008H,078H,0B7H,0CAH,0C8H,009H,07DH,021H,07AH,001H,0AEH,080H,047H,01FH,0A8H,078H,0F2H,0C7H,009H,0C6H,080H,077H,0CAH,02FH
            db  009H,0CDH,045H,00AH,077H,02BH,0C9H,0B7H,0E1H,0FAH,0B2H,008H,0AFH,032H,07AH,001H,0C9H,0CDH,02BH,00AH,078H,0B7H,0C8H,0C6H,002H,0DAH,0B2H,008H,047H,0CDH,020H,008H
            db  021H,07AH,001H,034H,0C0H,0C3H,0B2H,008H,03AH,079H,001H,0FEH,02FH,017H,09FH,0C0H,03CH,0C9H,0EFH,006H,088H,011H,000H,000H,021H,07AH,001H,04FH,070H,006H,000H,023H
            db  036H,080H,017H,0C3H,069H,008H,0EFH,0F0H,021H,079H,001H,07EH,0EEH,080H,077H,0C9H,0EBH,02AH,077H,001H,0E3H,0E5H,02AH,079H,001H,0E3H,0E5H,0EBH,0C9H,0CDH,02EH,00AH
            db  0EBH,022H,077H,001H,060H,069H,022H,079H,001H,0EBH,0C9H,021H,077H,001H,05EH,023H,056H,023H,04EH,023H,046H,023H,0C9H,011H,077H,001H,006H,004H,01AH,077H,013H,023H
            db  005H,0C2H,03CH,00AH,0C9H,021H,079H,001H,07EH,007H,037H,01FH,077H,03FH,01FH,023H,023H,077H,079H,007H,037H,01FH,04FH,01FH,0AEH,0C9H,078H,0B7H,0CAH,028H,000H,021H
            db  0ECH,009H,0E5H,0EFH,079H,0C8H,021H,079H,001H,0AEH,079H,0F8H,0CDH,072H,00AH,01FH,0A9H,0C9H,023H,078H,0BEH,0C0H,02BH,079H,0BEH,0C0H,02BH,07AH,0BEH,0C0H,02BH,07BH
            db  096H,0C0H,0E1H,0E1H,0C9H,047H,04FH,057H,05FH,0B7H,0C8H,0E5H,0CDH,02BH,00AH,0CDH,045H,00AH,0AEH,067H,0FCH,0A9H,00AH,03EH,098H,090H,0CDH,0D7H,008H,07CH,017H,0DCH
            db  0A8H,008H,006H,000H,0DCH,0C3H,008H,0E1H,0C9H,01BH,07AH,0A3H,03CH,0C0H,00BH,0C9H,021H,07AH,001H,07EH,0FEH,098H,0D0H,0CDH,085H,00AH,036H,098H,079H,017H,0C3H,069H
            db  008H,02BH,0CDH,0CCH,009H,047H,057H,05FH,02FH,04FH,0D7H,0DAH,012H,00BH,0FEH,02EH,0CAH,0F2H,00AH,0FEH,045H,0C2H,0F6H,00AH,0D7H,015H,0FEH,099H,0CAH,0E6H,00AH,014H
            db  0FEH,098H,0CAH,0E6H,00AH,02BH,0D7H,0DAH,031H,00BH,014H,0C2H,0F6H,00AH,0AFH,093H,05FH,00CH,00CH,0CAH,0CAH,00AH,0E5H,07BH,090H,0F4H,00AH,00BH,0F2H,005H,00BH,0F5H
            db  0CDH,031H,009H,0F1H,03CH,0C2H,0F9H,00AH,0E1H,0C9H,0C8H,0F5H,0CDH,0D1H,009H,0F1H,03DH,0C9H,0D5H,057H,078H,089H,047H,0C5H,0E5H,0D5H,0CDH,0D1H,009H,0F1H,0D6H,030H
            db  0CDH,010H,00AH,0CDH,0F3H,009H,0C1H,0D1H,0CDH,020H,008H,0E1H,0C1H,0D1H,0C3H,0CAH,00AH,07BH,007H,007H,083H,007H,086H,0D6H,030H,05FH,0C3H,0E6H,00AH,0E5H,021H,090H
            db  001H,0CDH,0B1H,005H,0E1H,0EBH,0AFH,006H,098H,0CDH,0F8H,009H,021H,0B0H,005H,0E5H,021H,07CH,001H,0E5H,0EFH,036H,020H,0F2H,05CH,00BH,036H,02DH,023H,036H,030H,0CAH
            db  005H,00CH,0E5H,0FCH,008H,00AH,0AFH,0F5H,0CDH,00BH,00CH,001H,043H,091H,011H,0F8H,04FH,0CDH,05AH,00AH,0E2H,088H,00BH,0F1H,0CDH,00BH,00BH,0F5H,0C3H,06BH,00BH,0CDH
            db  031H,009H,0F1H,03CH,0F5H,0CDH,00BH,00CH,0CDH,00FH,008H,03CH,0CDH,085H,00AH,0CDH,020H,00AH,001H,006H,002H,0F1H,081H,0FAH,0A3H,00BH,0FEH,007H,0D2H,0A3H,00BH,03CH
            db  047H,03EH,001H,03DH,0E1H,0F5H,011H,01DH,00CH,005H,036H,02EH,0CCH,035H,00AH,0C5H,0E5H,0D5H,0CDH,02BH,00AH,0E1H,006H,02FH,004H,07BH,096H,05FH,023H,07AH,09EH,057H
            db  023H,079H,09EH,04FH,02BH,02BH,0D2H,0B8H,00BH,0CDH,0B7H,008H,023H,0CDH,020H,00AH,0EBH,0E1H,070H,023H,0C1H,00DH,0C2H,0A9H,00BH,005H,0CAH,0E9H,00BH,02BH,07EH,0FEH
            db  030H,0CAH,0DDH,00BH,0FEH,02EH,0C4H,035H,00AH,0F1H,0CAH,008H,00CH,036H,045H,023H,036H,02BH,0F2H,0F9H,00BH,036H,02DH,02FH,03CH,006H,02FH,004H,0D6H,00AH,0D2H,0FBH
            db  00BH,0C6H,03AH,023H,070H,023H,077H,023H,071H,0E1H,0C9H,001H,074H,094H,011H,0F7H,023H,0CDH,05AH,00AH,0E1H,0E2H,07FH,00BH,0E9H,000H,000H,000H,080H,0A0H,086H,001H
            db  010H,027H,000H,0E8H,003H,000H,064H,000H,000H,00AH,000H,000H,001H,000H,000H,0EFH,0FAH,0A6H,004H,0C8H,021H,07AH,001H,07EH,01FH,0F5H,0E5H,03EH,040H,017H,077H,021H
            db  07CH,001H,0CDH,037H,00AH,03EH,004H,0F5H,0CDH,010H,00AH,021H,07CH,001H,0CDH,02EH,00AH,0CDH,03FH,009H,0C1H,0D1H,0CDH,020H,008H,001H,000H,080H,051H,059H,0CDH,0F3H
            db  008H,0F1H,03DH,0C2H,047H,00CH,0E1H,0F1H,0C6H,0C0H,086H,077H,0C9H,0EFH,021H,09CH,00CH,0CDH,01DH,00AH,0C8H,001H,035H,098H,011H,07AH,044H,0CDH,0F3H,008H,001H,028H
            db  068H,011H,046H,0B1H,0CDH,020H,008H,0CDH,02BH,00AH,07BH,059H,04FH,036H,080H,02BH,046H,036H,080H,0CDH,06CH,008H,021H,09CH,00CH,0C3H,037H,00AH,052H,0C7H,04FH,080H
            db  0CDH,010H,00AH,001H,049H,083H,011H,0DBH,00FH,0CDH,020H,00AH,0C1H,0D1H,0CDH,03FH,009H,0CDH,010H,00AH,0CDH,0B0H,00AH,0C1H,0D1H,0CDH,01AH,008H,001H,000H,07FH,051H
            db  059H,0CDH,01AH,008H,0EFH,037H,0F2H,0CEH,00CH,0CDH,00FH,008H,0EFH,0B7H,0F5H,0F4H,008H,00AH,001H,000H,07FH,051H,059H,0CDH,020H,008H,0F1H,0D4H,008H,00AH,0CDH,010H
            db  00AH,0CDH,02BH,00AH,0CDH,0F3H,008H,0CDH,010H,00AH,021H,00EH,00DH,0CDH,01DH,00AH,0C1H,0D1H,03EH,004H,0F5H,0D5H,0C5H,0E5H,0CDH,0F3H,008H,0E1H,0CDH,02EH,00AH,0E5H
            db  0CDH,020H,008H,0E1H,0C1H,0D1H,0F1H,03DH,0C2H,0F4H,00CH,0C3H,0F1H,008H,0BAH,0D7H,01EH,086H,064H,026H,099H,087H,058H,034H,023H,087H,0E0H,05DH,0A5H,086H,0DAH,00FH
;           db  049H,083H,000H,000H,0DBH,0FFH,0E6H,0F0H,00FH,00FH,0FEH,03CH,0C8H,0FEH,038H,037H,0C2H,03DH,00DH,021H,0FFH,00FH,04EH,02BH,07EH,0E6H,0F0H,00FH,00FH,0F5H,06FH,026H
            db  049H,083H,000H,000H,03EH,000H,0E6H,0F0H,00FH,00FH,0FEH,03CH,0C8H,0FEH,038H,037H,0C2H,03DH,00DH,021H,0FFH,00FH,04EH,02BH,07EH,0E6H,0F0H,00FH,00FH,0F5H,06FH,026H            
;                                   ^^^^ ^^^^
            db  000H,011H,0A2H,00DH,019H,07EH,023H,056H,023H,046H,023H,05EH,067H,0F1H,0F5H,07CH,0DAH,054H,00DH,079H,032H,0A0H,00DH,0F1H,021H,0BEH,00DH,0E5H,00EH,0FFH,0FEH,010H
            db  021H,000H,000H,022H,08FH,003H,0CAH,078H,00DH,0FEH,008H,0D0H,0C6H,011H,0F5H,03EH,003H,0CDH,09FH,00DH,0F1H,0C3H,09FH,00DH,0AFH,0CDH,09FH,00DH,0CDH,09BH,00DH,0CDH
            db  09BH,00DH,02FH,00EH,001H,0CDH,09BH,00DH,0E5H,02AH,09FH,00DH,02EH,0DBH,022H,08FH,003H,0E1H,03EH,02CH,035H,0CDH,09FH,00DH,035H,035H,035H,021H,0A0H,00DH,034H,0D3H
            db  010H,0C9H,010H,0CAH,001H,002H,010H,0CAH,001H,002H,000H,0C2H,001H,080H,006H,0C2H,001H,080H,020H,0CAH,080H,080H,004H,0CAH,002H,001H,024H,0CAH,040H,040H,062H,068H
            db  022H,096H,003H,07CH,0E6H,0C8H,067H,022H,084H,004H,0EBH,022H,087H,003H,03AH,0A0H,00DH,032H,094H,003H,032H,082H,004H,03CH,032H,09BH,003H,081H,032H,085H,003H,03CH
            db  032H,08DH,003H,0C9H,000H,000H,021H,040H,00FH,0F9H,022H,018H,001H,0CDH,024H,00DH,021H,0FFH,0FFH,022H,01AH,001H,0CDH,098H,005H,021H,0EEH,00EH,0CDH,0B1H,005H,0CDH
            db  0CAH,002H,0DAH,0F9H,00DH,0D7H,0B7H,0C2H,01DH,00EH,021H,0FAH,00EH,023H,07CH,0B5H,0CAH,029H,00EH,07EH,02FH,077H,0BEH,0CAH,00DH,00EH,0C3H,029H,00EH,021H,01FH,001H
            db  0CDH,0ABH,004H,07EH,0B7H,0C2H,0D6H,001H,0EBH,02BH,0E5H,021H,0DFH,00EH,0CDH,0B1H,005H,0CDH,0CAH,002H,0DAH,02BH,00EH,0D7H,0B7H,03EH,048H,05FH,0CAH,048H,00EH,0CDH
            db  0ABH,004H,07BH,0FEH,00FH,0DAH,02BH,00EH,032H,07CH,003H,0D6H,00EH,0D2H,04BH,00EH,0C6H,01CH,02FH,03CH,083H,032H,0C5H,005H,021H,0BFH,00EH,0F7H,011H,0D3H,00EH,0E7H
            db  0CAH,06FH,00EH,0F7H,0E3H,0CDH,0B1H,005H,0CDH,0CAH,002H,0D7H,0E1H,0FEH,059H,0D1H,0CAH,084H,00EH,0FEH,04EH,0C2H,058H,00EH,0F7H,0E3H,011H,0A6H,004H,073H,023H,072H
            db  0E1H,0C3H,05BH,00EH,0EBH,036H,000H,023H,022H,01CH,001H,0E3H,011H,040H,00FH,0E7H,0DAH,0D3H,001H,0D1H,0F9H,022H,018H,001H,0EBH,0CDH,0C7H,001H,07BH,095H,06FH,07AH
            db  09CH,067H,001H,0F0H,0FFH,009H,0CDH,045H,00BH,021H,027H,001H,0CDH,0B1H,005H,021H,0B1H,005H,022H,005H,002H,0CDH,09EH,002H,021H,001H,002H,022H,002H,000H,0E9H,022H
            db  00DH,0D3H,00EH,04FH,000H,0A0H,00CH,0D7H,00EH,04DH,000H,06DH,00CH,0DBH,00EH,04BH,000H,02FH,00CH,053H,049H,04EH,000H,052H,04EH,044H,000H,053H,051H,052H,000H,054H
            db  045H,052H,04DH,049H,04EH,041H,04CH,020H,057H,049H,044H,054H,048H,000H,04DH,045H,04DH,04FH,052H,059H,020H,053H,049H,05AH,045H,000H,000H,000H,000H,000H,000H,000H
;           db  0F3H,0AFH,0D3H,022H,02FH,0D3H,023H,03EH,02CH,0D3H,022H,03EH,003H,0D3H,010H,0DBH,0FFH,0E6H,010H,00FH,00FH,0C6H,011H,0D3H,010H,031H,000H,010H,0DBH,0FFH,0E6H,00FH            
            db  0F3H,0AFH,0D3H,022H,02FH,0D3H,023H,03EH,02CH,0D3H,022H,03EH,003H,0D3H,010H,03EH,000H,0E6H,010H,00FH,00FH,0C6H,011H,0D3H,010H,031H,000H,010H,03EH,000H,0E6H,00FH
;                                                                                          ^^^^ ^^^^                                                        ^^^^ ^^^^
            db  0FEH,007H,0F2H,08AH,00FH,021H,0ACH,00FH,006H,000H,04FH,087H,081H,04FH,009H,07EH,032H,0A6H,00FH,03DH,032H,09FH,00FH,023H,074H,00DH,051H,003H,0D3H,002H,002H,00EH
            db  00FH,0CDH,09EH,00FH,0FEH,03CH,0CAH,058H,00FH,0FEH,078H,0C2H,041H,00FH,0CDH,09EH,00FH,04FH,0CDH,09EH,00FH,069H,067H,0E9H,0CDH,09EH,00FH,04FH,006H,000H,0CDH,09EH
            db  00FH,05FH,0CDH,09EH,00FH,057H,07AH,0FEH,00FH,03EH,04FH,0CAH,08CH,00FH,0CDH,09EH,00FH,0EBH,077H,0BEH,03EH,04DH,0C2H,08CH,00FH,023H,0EBH,00DH,0C2H,066H,00FH,048H
            db  0CDH,09EH,00FH,0B9H,0CAH,041H,00FH,03EH,043H,001H,03EH,049H,032H,000H,000H,022H,001H,000H,0FBH,0D3H,001H,0D3H,011H,0D3H,005H,0D3H,023H,0C3H,093H,00FH,0DBH,010H
            db  0E6H,001H,0CAH,09EH,00FH,0DBH,011H,0F5H,080H,047H,0F1H,0C9H,011H,0CAH,001H,011H,0CAH,001H,001H,0C2H,001H,007H,0C2H,001H,021H,0CAH,080H,005H,0CAH,002H,025H,0CAH
            db  040H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H
            db  000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,002H,000H,055H,00FH
basic4kend:

            ;Altair 8K BASIC 4.0 image
basic8k:    db  0F3H,0C3H,00BH,01AH,038H,007H,0E5H,00DH,07EH,0E3H,0BEH,023H,0E3H,0C2H,0C6H,002H,023H,07EH,0FEH,03AH,0D0H,0C3H,053H,006H,0F5H,03AH,0D1H,001H,0B7H,0C3H,02DH,005H
            db  07CH,092H,0C0H,07DH,093H,0C9H,001H,000H,03AH,056H,002H,0B7H,0C2H,069H,014H,0C9H,000H,000H,000H,000H,000H,000H,000H,000H,0C9H,00EH,002H,00DH,00AH,042H,052H,045H
            db  041H,04BH,000H,073H,014H,031H,015H,087H,014H,04DH,007H,0C3H,00DH,0FAH,010H,0F1H,00DH,0FFH,016H,0DCH,017H,01DH,013H,04BH,017H,076H,018H,07CH,018H,0D9H,018H,0EEH
            db  018H,0F9H,011H,041H,010H,06FH,00EH,0D2H,010H,050H,010H,061H,010H,071H,010H,0A1H,010H,0ABH,010H,0C5H,04EH,044H,0C6H,04FH,052H,0CEH,045H,058H,054H,0C4H,041H,054H
            db  041H,0C9H,04EH,050H,055H,054H,0C4H,049H,04DH,0D2H,045H,041H,044H,0CCH,045H,054H,0C7H,04FH,054H,04FH,0D2H,055H,04EH,0C9H,046H,0D2H,045H,053H,054H,04FH,052H,045H
            db  0C7H,04FH,053H,055H,042H,0D2H,045H,054H,055H,052H,04EH,0D2H,045H,04DH,0D3H,054H,04FH,050H,0CFH,055H,054H,0CFH,04EH,0CEH,055H,04CH,04CH,0D7H,041H,049H,054H,0C4H
            db  045H,046H,0D0H,04FH,04BH,045H,0D0H,052H,049H,04EH,054H,0C3H,04FH,04EH,054H,0CCH,049H,053H,054H,0C3H,04CH,045H,041H,052H,0C3H,04CH,04FH,041H,044H,0C3H,053H,041H
            db  056H,045H,0CEH,045H,057H,0D4H,041H,042H,028H,0D4H,04FH,0C6H,04EH,0D3H,050H,043H,028H,0D4H,048H,045H,04EH,0CEH,04FH,054H,0D3H,054H,045H,050H,0ABH,0ADH,0AAH,0AFH
            db  0DEH,0C1H,04EH,044H,0CFH,052H,0BEH,0BDH,0BCH,0D3H,047H,04EH,0C9H,04EH,054H,0C1H,042H,053H,0D5H,053H,052H,0C6H,052H,045H,0C9H,04EH,050H,0D0H,04FH,053H,0D3H,051H
            db  052H,0D2H,04EH,044H,0CCH,04FH,047H,0C5H,058H,050H,0C3H,04FH,053H,0D3H,049H,04EH,0D4H,041H,04EH,0C1H,054H,04EH,0D0H,045H,045H,04BH,0CCH,045H,04EH,0D3H,054H,052H
            db  024H,0D6H,041H,04CH,0C1H,053H,043H,0C3H,048H,052H,024H,0CCH,045H,046H,054H,024H,0D2H,049H,047H,048H,054H,024H,0CDH,049H,044H,024H,080H,081H,006H,0B7H,005H,03AH
            db  00AH,0FAH,007H,05EH,009H,03FH,00CH,088H,009H,011H,008H,0B9H,007H,09CH,007H,07FH,008H,05EH,006H,0A8H,007H,0D5H,007H,0FCH,007H,07FH,006H,005H,011H,063H,008H,0C0H
            db  006H,00BH,011H,0F9H,00DH,000H,012H,09BH,008H,0ADH,006H,06DH,005H,073H,007H,085H,011H,059H,011H,0B0H,003H,079H,0EBH,015H,079H,01CH,012H,07CH,059H,013H,07CH,0B7H
            db  013H,07FH,008H,017H,050H,09CH,00BH,046H,09BH,00BH,04EH,046H,053H,04EH,052H,047H,04FH,044H,046H,043H,04FH,056H,04FH,04DH,055H,04CH,042H,053H,044H,044H,02FH,030H
            db  049H,044H,054H,04DH,04FH,053H,04CH,053H,053H,054H,043H,04EH,055H,046H,04DH,04FH,000H,000H,073H,01CH,0FEH,0FFH,010H,01CH,02CH,000H,000H,000H,000H,000H,000H,000H
            db  000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H
            db  000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,056H,005H,047H,005H,068H,006H,00CH,006H,07FH,019H,0A8H,019H,000H,000H,000H,000H,000H,000H
            db  000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H
            db  000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H
            db  000H,000H,000H,000H,000H,020H,045H,052H,052H,04FH,052H,000H,020H,049H,04EH,020H,000H,04FH,04BH,00DH,00AH,000H,021H,004H,000H,039H,07EH,023H,0FEH,081H,0C0H,04EH
            db  023H,046H,023H,0E5H,069H,060H,07AH,0B3H,0EBH,0CAH,08EH,002H,0EBH,0E7H,001H,00DH,000H,0E1H,0C8H,009H,0C3H,07AH,002H,0CDH,0AFH,002H,0C5H,0E3H,0C1H,0E7H,07EH,002H
            db  0C8H,00BH,02BH,0C3H,09DH,002H,0E5H,02AH,04FH,002H,006H,000H,009H,009H,03EH,0E5H,03EH,0D4H,095H,06FH,03EH,0FFH,09CH,067H,039H,0E1H,0D8H,01EH,00CH,0C3H,0D1H,002H
            db  02AH,03EH,002H,022H,0D4H,001H,01EH,002H,001H,01EH,014H,001H,01EH,000H,001H,01EH,012H,0CDH,0D5H,003H,032H,0D1H,001H,0CDH,0E0H,008H,021H,0AAH,001H,057H,03EH,03FH
            db  0DFH,019H,07EH,0DFH,0D7H,0DFH,021H,065H,002H,0CDH,0E3H,00EH,02AH,0D4H,001H,07CH,0A5H,03CH,0C4H,0FCH,015H,03EH,0C1H,0AFH,032H,0D1H,001H,0CDH,0E0H,008H,021H,071H
            db  002H,0CDH,00BH,01AH,021H,0FFH,0FFH,022H,0D4H,001H,0CDH,0D4H,004H,0DAH,004H,003H,0AFH,032H,041H,002H,0D7H,03CH,03DH,0CAH,004H,003H,0F5H,0CDH,052H,007H,0D5H,0CDH
            db  0F8H,003H,047H,0D1H,0F1H,0D2H,030H,006H,0D5H,0C5H,0D7H,0B7H,0F5H,0CDH,092H,003H,0DAH,039H,003H,0F1H,0F5H,0CAH,0D0H,007H,0B7H,0C5H,0D2H,04EH,003H,0EBH,02AH,04BH
            db  002H,01AH,002H,003H,013H,0E7H,0C2H,041H,003H,060H,069H,022H,04BH,002H,0D1H,0F1H,0CAH,075H,003H,02AH,04BH,002H,0E3H,0C1H,009H,0E5H,0CDH,097H,002H,0E1H,022H,04BH
            db  002H,0EBH,074H,0D1H,023H,023H,073H,023H,072H,023H,011H,0D9H,001H,01AH,077H,023H,013H,0B7H,0C2H,06DH,003H,0CDH,0BCH,003H,023H,0EBH,062H,06BH,07EH,023H,0B6H,0CAH
            db  004H,003H,023H,023H,023H,0AFH,0BEH,023H,0C2H,086H,003H,0EBH,073H,023H,072H,0C3H,07AH,003H,02AH,0D6H,001H,044H,04DH,07EH,023H,0B6H,02BH,0C8H,023H,023H,07EH,023H
            db  066H,06FH,0E7H,060H,069H,07EH,023H,066H,06FH,03FH,0C8H,03FH,0D0H,0C3H,095H,003H,0C0H,02AH,0D6H,001H,0AFH,077H,023H,077H,023H,022H,04BH,002H,02AH,0D6H,001H,02BH
            db  022H,043H,002H,02AH,026H,002H,022H,03AH,002H,0CDH,05EH,006H,02AH,04BH,002H,022H,04DH,002H,022H,04FH,002H,0C1H,02AH,0D2H,001H,0F9H,021H,02AH,002H,022H,028H,002H
            db  0AFH,06FH,067H,022H,049H,002H,032H,040H,002H,0E5H,0C5H,02AH,043H,002H,0C9H,03EH,03FH,0DFH,03EH,020H,0DFH,0C3H,0D4H,004H,0AFH,032H,025H,002H,00EH,005H,011H,0D9H
            db  001H,07EH,0FEH,020H,0CAH,07EH,004H,047H,0FEH,022H,0CAH,09EH,004H,0B7H,0CAH,0A5H,004H,03AH,025H,002H,0B7H,07EH,0C2H,07EH,004H,0FEH,03FH,03EH,096H,0CAH,07EH,004H
            db  07EH,0FEH,030H,0DAH,02BH,004H,0FEH,03CH,0DAH,07EH,004H,0D5H,011H,072H,000H,0C5H,001H,07AH,004H,0C5H,006H,07FH,07EH,0FEH,061H,0DAH,044H,004H,0FEH,07BH,0D2H,044H
            db  004H,0E6H,05FH,077H,04EH,0EBH,023H,0B6H,0F2H,046H,004H,004H,07EH,0E6H,07FH,0C8H,0B9H,0C2H,046H,004H,0EBH,0E5H,013H,01AH,0B7H,0FAH,076H,004H,04FH,078H,0FEH,088H
            db  0C2H,065H,004H,0D7H,02BH,023H,07EH,0FEH,061H,0DAH,06EH,004H,0E6H,05FH,0B9H,0CAH,056H,004H,0E1H,0C3H,044H,004H,048H,0F1H,0EBH,0C9H,0EBH,079H,0C1H,0D1H,023H,012H
            db  013H,00CH,0D6H,03AH,0CAH,08CH,004H,0FEH,049H,0C2H,08FH,004H,032H,025H,002H,0D6H,054H,0C2H,001H,004H,047H,07EH,0B7H,0CAH,0A5H,004H,0B8H,0CAH,07EH,004H,023H,012H
            db  00CH,013H,0C3H,095H,004H,021H,0D8H,001H,012H,013H,012H,013H,012H,0C9H,03AH,0D0H,001H,0B7H,03EH,05CH,032H,0D0H,001H,0C2H,0C0H,004H,005H,0CAH,0D4H,004H,0DFH,03EH
            db  005H,02BH,0CAH,0D0H,004H,07EH,0DFH,0C3H,0DDH,004H,005H,02BH,0DFH,0C2H,0DDH,004H,0DFH,0CDH,0EDH,008H,021H,0D9H,001H,006H,001H,0AFH,032H,0D0H,001H,0CDH,056H,005H
            db  04FH,0FEH,07FH,0CAH,0AEH,004H,03AH,0D0H,001H,0B7H,0CAH,0F4H,004H,03EH,05CH,0DFH,0AFH,032H,0D0H,001H,079H,0FEH,007H,0CAH,01AH,005H,0FEH,003H,0CCH,0EDH,008H,037H
            db  0C8H,0FEH,00DH,0CAH,0E8H,008H,0FEH,015H,0CAH,0D0H,004H,0FEH,040H,0CAH,0D0H,004H,0FEH,05FH,0CAH,0CAH,004H,0FEH,020H,0DAH,0DDH,004H,078H,0FEH,048H,03EH,007H,0D2H
            db  029H,005H,079H,071H,032H,041H,002H,023H,004H,0DFH,0C3H,0DDH,004H,03AH,0D1H,001H,0B7H,0C2H,014H,00FH,0F1H,0F5H,0FEH,020H,0DAH,047H,005H,03AH,027H,000H,0FEH,048H
            db  0CCH,0EDH,008H,03CH,032H,027H,000H,0DBH,000H,0E6H,0C8H,0C2H,047H,005H,0F1H,0D3H,001H,0F5H,0DBH,013H,0F1H,0C9H,0DBH,000H,0E6H,001H,0C2H,056H,005H,0DBH,001H,0E6H
            db  07FH,0FEH,00FH,0C0H,03AH,0D1H,001H,02FH,032H,0D1H,001H,0AFH,0C9H,0CDH,052H,007H,0C0H,0C1H,0CDH,092H,003H,0C5H,0E1H,04EH,023H,046H,023H,078H,0B1H,0CAH,0F7H,002H
            db  0CDH,068H,006H,0C5H,0CDH,0EDH,008H,05EH,023H,056H,023H,0E5H,0EBH,0CDH,004H,016H,03EH,020H,0E1H,0DFH,07EH,0B7H,023H,0CAH,076H,005H,0F2H,093H,005H,0D6H,07FH,04FH
            db  011H,073H,000H,01AH,013H,0B7H,0F2H,0A3H,005H,00DH,0C2H,0A3H,005H,0DFH,01AH,013H,0B7H,0F2H,0ADH,005H,0C3H,094H,005H,03EH,064H,032H,040H,002H,0CDH,011H,008H,0E3H
            db  0CDH,076H,002H,0D1H,0C2H,0C9H,005H,009H,0F9H,0EBH,00EH,008H,0CDH,0A6H,002H,0E5H,0CDH,0FAH,007H,0E3H,0E5H,02AH,0D4H,001H,0E3H,0CDH,086H,00AH,0CFH,09EH,0CDH,083H
            db  00AH,0E5H,0CDH,0ACH,014H,0E1H,0C5H,0D5H,001H,000H,081H,051H,05AH,07EH,0FEH,0A3H,03EH,001H,0C2H,0FFH,005H,0D7H,0CDH,083H,00AH,0E5H,0CDH,0ACH,014H,0EFH,0E1H,0C5H
            db  0D5H,0F5H,033H,0E5H,02AH,043H,002H,0E3H,006H,081H,0C5H,033H,0DBH,000H,0E6H,001H,0CCH,06DH,006H,022H,043H,002H,07EH,0FEH,03AH,0CAH,030H,006H,0B7H,0C2H,0C6H,002H
            db  023H,07EH,023H,0B6H,0CAH,089H,006H,023H,05EH,023H,056H,0EBH,022H,0D4H,001H,0EBH,0D7H,011H,00CH,006H,0D5H,0C8H,0D6H,080H,0DAH,011H,008H,0FEH,01DH,0D2H,0C6H,002H
            db  007H,04FH,006H,000H,0EBH,021H,05BH,001H,009H,04EH,023H,046H,0C5H,0EBH,023H,07EH,0FEH,03AH,0D0H,0FEH,020H,0CAH,04EH,006H,0FEH,030H,03FH,03CH,03DH,0C9H,0EBH,02AH
            db  0D6H,001H,02BH,022H,051H,002H,0EBH,0C9H,0DBH,000H,0E6H,001H,0C0H,0CDH,056H,005H,0FEH,003H,0CAH,07FH,006H,0FEH,011H,0CAH,068H,006H,0FEH,013H,0CAH,06DH,006H,0C0H
            db  0F6H,0C0H,022H,043H,002H,021H,0F6H,0FFH,0C1H,02AH,0D4H,001H,0F5H,07DH,0A4H,03CH,0CAH,09CH,006H,022H,047H,002H,02AH,043H,002H,022H,049H,002H,0AFH,032H,0D1H,001H
            db  0CDH,0E0H,008H,0F1H,021H,03BH,000H,0C2H,0E9H,002H,0C3H,0F7H,002H,02AH,049H,002H,07CH,0B5H,01EH,020H,0CAH,0D1H,002H,0EBH,02AH,047H,002H,022H,0D4H,001H,0EBH,0C9H
            db  0CDH,031H,011H,0C0H,03CH,0FEH,048H,0D2H,04DH,007H,032H,026H,000H,0C9H,006H,0FFH,0D7H,078H,032H,043H,002H,03EH,001H,032H,040H,002H,0C3H,044H,00CH,032H,040H,002H
            db  044H,04DH,00BH,00BH,00BH,00BH,03AH,043H,002H,0B7H,0F5H,0EBH,019H,0EBH,04EH,006H,000H,009H,009H,023H,006H,0D2H,0FAH,003H,007H,078H,0CDH,04AH,011H,0CDH,04AH,011H
            db  0C3H,010H,007H,00EH,004H,0CDH,040H,011H,0B8H,0C2H,003H,007H,00DH,0C2H,005H,007H,0CDH,086H,00AH,0E7H,0CAH,025H,007H,0F1H,0F5H,07EH,0F4H,04DH,011H,0FCH,040H,011H
            db  077H,023H,0C3H,013H,007H,0F1H,0E1H,0C9H,07EH,0FEH,041H,0D8H,0FEH,05BH,03FH,0C9H,0D7H,0CDH,083H,00AH,0EFH,0FAH,04DH,007H,03AH,056H,002H,0FEH,090H,0DAH,006H,015H
            db  001H,080H,090H,011H,000H,000H,0E5H,0CDH,0DBH,014H,0E1H,051H,0C8H,01EH,008H,0C3H,0D1H,002H,02BH,011H,000H,000H,0D7H,0D0H,0E5H,0F5H,021H,098H,019H,0E7H,0DAH,0C6H
            db  002H,062H,06BH,019H,029H,019H,029H,0F1H,0D6H,030H,05FH,016H,000H,019H,0EBH,0E1H,0C3H,056H,007H,0CAH,0C0H,003H,0CDH,031H,007H,02BH,0D7H,0C0H,0E5H,02AH,026H,002H
            db  07DH,093H,05FH,07CH,09AH,057H,0DAH,0BBH,002H,02AH,04BH,002H,001H,028H,000H,009H,0E7H,0D2H,0BBH,002H,0EBH,022H,0D2H,001H,0E1H,0C3H,0C0H,003H,0CAH,0BCH,003H,0CDH
            db  0C0H,003H,001H,00CH,006H,0C3H,0B8H,007H,00EH,003H,0CDH,0A6H,002H,0C1H,0E5H,0E5H,02AH,0D4H,001H,0E3H,03EH,08CH,0F5H,033H,0C5H,0CDH,052H,007H,0CDH,0FCH,007H,0E5H
            db  02AH,0D4H,001H,0E7H,0E1H,023H,0DCH,095H,003H,0D4H,092H,003H,060H,069H,02BH,0D8H,01EH,00EH,0C3H,0D1H,002H,0C0H,016H,0FFH,0CDH,076H,002H,0F9H,0FEH,08CH,01EH,004H
            db  0C2H,0D1H,002H,0E1H,022H,0D4H,001H,023H,07CH,0B5H,0C2H,0F4H,007H,03AH,041H,002H,0B7H,0C2H,0F6H,002H,021H,00CH,006H,0E3H,03EH,0E1H,001H,03AH,00EH,000H,006H,000H
            db  079H,048H,047H,07EH,0B7H,0C8H,0B8H,0C8H,023H,0FEH,022H,0CAH,000H,008H,0C3H,003H,008H,0CDH,044H,00CH,0CFH,0ACH,0D5H,03AH,024H,002H,0F5H,0CDH,094H,00AH,0F1H,0E3H
            db  022H,043H,002H,01FH,0CDH,088H,00AH,0CAH,05CH,008H,0E5H,02AH,053H,002H,0E5H,023H,023H,05EH,023H,056H,02AH,0D6H,001H,0E7H,0D2H,04BH,008H,02AH,0D2H,001H,0E7H,0D1H
            db  0D2H,053H,008H,02AH,04BH,002H,0E7H,0D2H,053H,008H,03EH,0D1H,0CDH,032H,010H,0EBH,0CDH,07FH,00EH,0CDH,032H,010H,0E1H,0CDH,0BBH,014H,0E1H,0C9H,0E5H,0CDH,0B8H,014H
            db  0D1H,0E1H,0C9H,0CDH,031H,011H,07EH,047H,0FEH,08CH,0CAH,070H,008H,0CFH,088H,02BH,04BH,00DH,078H,0CAH,036H,006H,0CDH,053H,007H,0FEH,02CH,0C0H,0C3H,071H,008H,0CDH
            db  094H,00AH,07EH,0FEH,088H,0CAH,08AH,008H,0CFH,0A1H,02BH,0CDH,086H,00AH,0EFH,0CAH,0FCH,007H,0D7H,0DAH,0B9H,007H,0C3H,035H,006H,02BH,0D7H,0CAH,0EDH,008H,0C8H,0FEH
            db  09DH,0CAH,016H,009H,0FEH,0A0H,0CAH,016H,009H,0E5H,0FEH,02CH,0CAH,002H,009H,0FEH,03BH,0CAH,035H,009H,0C1H,0CDH,094H,00AH,0E5H,03AH,024H,002H,0B7H,0C2H,0D9H,008H
            db  0CDH,00FH,016H,0CDH,0A3H,00EH,02AH,053H,002H,03AH,027H,000H,086H,0FEH,048H,0D4H,0EDH,008H,0CDH,0E6H,00EH,03EH,020H,0DFH,0AFH,0C4H,0E6H,00EH,0E1H,0C3H,099H,008H
            db  03AH,027H,000H,0B7H,0C8H,0C3H,0EDH,008H,036H,000H,021H,0D8H,001H,03EH,00DH,0DFH,03EH,00AH,0DFH,03AH,026H,000H,03DH,032H,027H,000H,0C8H,0F5H,0AFH,0DFH,0F1H,0C3H
            db  0F6H,008H,03AH,027H,000H,0FEH,038H,0D4H,0EDH,008H,0D2H,035H,009H,0D6H,00EH,0D2H,00DH,009H,02FH,0C3H,02CH,009H,0F5H,0CDH,030H,011H,0CFH,029H,02BH,0F1H,0D6H,0A0H
            db  0E5H,0CAH,027H,009H,03AH,027H,000H,02FH,083H,0D2H,035H,009H,03CH,047H,03EH,020H,0DFH,005H,0C2H,030H,009H,0E1H,0D7H,0C3H,09EH,008H,03FH,052H,045H,044H,04FH,020H
            db  046H,052H,04FH,04DH,020H,053H,054H,041H,052H,054H,00DH,00AH,000H,03AH,042H,002H,0B7H,0C2H,0C0H,002H,0C1H,021H,03AH,009H,0CDH,0E3H,00EH,0C3H,0EBH,003H,0FEH,022H
            db  03EH,000H,032H,0D1H,001H,0C2H,072H,009H,0CDH,0A4H,00EH,0CFH,03BH,0E5H,0CDH,0E6H,00EH,03EH,0E5H,0CDH,052H,00EH,0CDH,0EFH,003H,0C1H,0DAH,086H,006H,023H,07EH,0B7H
            db  02BH,0C5H,0CAH,0F9H,007H,0C3H,08DH,009H,0E5H,02AH,051H,002H,0F6H,0AFH,032H,042H,002H,0E3H,001H,0CFH,02CH,0CDH,044H,00CH,0E3H,0D5H,07EH,0FEH,02CH,0CAH,0BBH,009H
            db  03AH,042H,002H,0B7H,0C2H,018H,00AH,03EH,03FH,0DFH,0CDH,0EFH,003H,0D1H,0C1H,0DAH,086H,006H,023H,07EH,0B7H,02BH,0C5H,0CAH,0F9H,007H,0D5H,03AH,024H,002H,0B7H,0CAH
            db  0DBH,009H,0D7H,057H,047H,0FEH,022H,0CAH,0CFH,009H,016H,03AH,006H,02CH,02BH,0CDH,0A7H,00EH,0EBH,021H,0E4H,009H,0E3H,0D5H,0C3H,02AH,008H,0D7H,0CDH,065H,015H,0E3H
            db  0CDH,0B8H,014H,0E1H,02BH,0D7H,0CAH,0EEH,009H,0FEH,02CH,0C2H,04DH,009H,0E3H,02BH,0D7H,0C2H,093H,009H,0D1H,03AH,042H,002H,0B7H,0EBH,0C2H,063H,006H,0D5H,0B6H,021H
            db  007H,00AH,0C4H,0E3H,00EH,0E1H,0C9H,03FH,045H,058H,054H,052H,041H,020H,049H,047H,04EH,04FH,052H,045H,044H,00DH,00AH,000H,0CDH,0FAH,007H,0B7H,0C2H,031H,00AH,023H
            db  07EH,023H,0B6H,01EH,006H,0CAH,0D1H,002H,023H,05EH,023H,056H,0EBH,022H,03EH,002H,0EBH,0D7H,0FEH,083H,0C2H,018H,00AH,0C3H,0BBH,009H,011H,000H,000H,0C4H,044H,00CH
            db  022H,043H,002H,0CDH,076H,002H,0C2H,0CCH,002H,0F9H,0D5H,07EH,023H,0F5H,0D5H,0CDH,09EH,014H,0E3H,0E5H,0CDH,012H,012H,0E1H,0CDH,0B8H,014H,0E1H,0CDH,0AFH,014H,0E5H
            db  0CDH,0DBH,014H,0E1H,0C1H,090H,0CDH,0AFH,014H,0CAH,075H,00AH,0EBH,022H,0D4H,001H,069H,060H,0C3H,008H,006H,0F9H,02AH,043H,002H,07EH,0FEH,02CH,0C2H,00CH,006H,0D7H
            db  0CDH,03DH,00AH,0CDH,094H,00AH,0F6H,037H,03AH,024H,002H,08FH,0E8H,01EH,018H,0C3H,0D1H,002H,0CFH,028H,02BH,016H,000H,0D5H,00EH,001H,0CDH,0A6H,002H,0CDH,009H,00BH
            db  022H,045H,002H,02AH,045H,002H,0C1H,078H,0FEH,078H,0D4H,086H,00AH,07EH,016H,000H,0D6H,0ABH,0DAH,0CAH,00AH,0FEH,003H,0D2H,0CAH,00AH,0FEH,001H,017H,0AAH,0BAH,057H
            db  0DAH,0C6H,002H,022H,03CH,002H,0D7H,0C3H,0B0H,00AH,07AH,0B7H,0C2H,0C3H,00BH,07EH,022H,03CH,002H,0D6H,0A4H,0D8H,0FEH,007H,0D0H,05FH,03AH,024H,002H,03DH,0B3H,07BH
            db  0CAH,0C9H,00FH,007H,083H,05FH,021H,095H,001H,019H,078H,056H,0BAH,0D0H,023H,0CDH,086H,00AH,0C5H,001H,0A3H,00AH,0C5H,043H,04AH,0CDH,091H,014H,058H,051H,04EH,023H
            db  046H,023H,0C5H,02AH,03CH,002H,0C3H,097H,00AH,0AFH,032H,024H,002H,0D7H,01EH,024H,0CAH,0D1H,002H,0DAH,065H,015H,0CDH,028H,007H,0D2H,056H,00BH,0FEH,0A4H,0CAH,009H
            db  00BH,0FEH,02EH,0CAH,065H,015H,0FEH,0A5H,0CAH,045H,00BH,0FEH,022H,0CAH,0A4H,00EH,0FEH,0A2H,0CAH,023H,00CH,0FEH,09FH,0CAH,016H,00EH,0D6H,0AEH,0D2H,067H,00BH,0CDH
            db  092H,00AH,0CFH,029H,0C9H,016H,07DH,0CDH,097H,00AH,02AH,045H,002H,0E5H,0CDH,089H,014H,0CDH,086H,00AH,0E1H,0C9H,0CDH,044H,00CH,0E5H,0EBH,022H,053H,002H,03AH,024H
            db  002H,0B7H,0CCH,09EH,014H,0E1H,0C9H,006H,000H,007H,04FH,0C5H,0D7H,079H,0FEH,029H,0DAH,08AH,00BH,0CDH,092H,00AH,0CFH,02CH,0CDH,087H,00AH,0EBH,02AH,053H,002H,0E3H
            db  0E5H,0EBH,0CDH,031H,011H,0EBH,0E3H,0C3H,092H,00BH,0CDH,03FH,00BH,0E3H,011H,051H,00BH,0D5H,001H,043H,000H,009H,04EH,023H,066H,069H,0E9H,0F6H,0AFH,0F5H,0CDH,086H
            db  00AH,0CDH,038H,007H,0F1H,0EBH,0C1H,0E3H,0EBH,0CDH,0A1H,014H,0F5H,0CDH,038H,007H,0F1H,0C1H,079H,021H,0E4H,00DH,0C2H,0BEH,00BH,0A3H,04FH,078H,0A2H,0E9H,0B3H,04FH
            db  078H,0B2H,0E9H,021H,0D5H,00BH,03AH,024H,002H,01FH,07AH,017H,05FH,016H,064H,078H,0BAH,0D0H,0C3H,0F2H,00AH,0D7H,00BH,079H,0B7H,01FH,0C1H,0D1H,0F5H,0CDH,088H,00AH
            db  021H,019H,00CH,0E5H,0CAH,0DBH,014H,0AFH,032H,024H,002H,0D5H,0CDH,016H,010H,07EH,023H,023H,04EH,023H,046H,0D1H,0C5H,0F5H,0CDH,01AH,010H,0CDH,0AFH,014H,0F1H,057H
            db  0E1H,07BH,0B2H,0C8H,07AH,0D6H,001H,0D8H,0AFH,0BBH,03CH,0D0H,015H,01DH,00AH,0BEH,023H,003H,0CAH,001H,00CH,03FH,0C3H,06FH,014H,03CH,08FH,0C1H,0A0H,0C6H,0FFH,09FH
            db  0C3H,074H,014H,016H,05AH,0CDH,097H,00AH,0CDH,086H,00AH,0CDH,038H,007H,07BH,02FH,04FH,07AH,02FH,0CDH,0E4H,00DH,0C1H,0C3H,0A3H,00AH,02BH,0D7H,0C8H,0CFH,02CH,001H
            db  03AH,00CH,0C5H,0F6H,0AFH,032H,023H,002H,046H,0CDH,028H,007H,0DAH,0C6H,002H,0AFH,04FH,032H,024H,002H,0D7H,0DAH,05EH,00CH,0CDH,028H,007H,0DAH,069H,00CH,04FH,0D7H
            db  0DAH,05FH,00CH,0CDH,028H,007H,0D2H,05FH,00CH,0D6H,024H,0C2H,076H,00CH,03CH,032H,024H,002H,00FH,081H,04FH,0D7H,03AH,040H,002H,03DH,0CAH,00DH,00DH,0F2H,086H,00CH
            db  07EH,0D6H,028H,0CAH,0E7H,00CH,0AFH,032H,040H,002H,0E5H,02AH,04DH,002H,0EBH,02AH,04BH,002H,0E7H,0CAH,0A9H,00CH,079H,096H,023H,0C2H,09EH,00CH,078H,096H,023H,0CAH
            db  0D9H,00CH,023H,023H,023H,023H,0C3H,092H,00CH,0E1H,0E3H,0D5H,011H,059H,00BH,0E7H,0D1H,0CAH,0DCH,00CH,0E3H,0E5H,0C5H,001H,006H,000H,02AH,04FH,002H,0E5H,009H,0C1H
            db  0E5H,0CDH,097H,002H,0E1H,022H,04FH,002H,060H,069H,022H,04DH,002H,02BH,036H,000H,0E7H,0C2H,0CDH,00CH,0D1H,073H,023H,072H,023H,0EBH,0E1H,0C9H,032H,056H,002H,021H
            db  070H,002H,022H,053H,002H,0E1H,0C9H,0E5H,02AH,023H,002H,0E3H,057H,0D5H,0C5H,0CDH,030H,007H,0C1H,0F1H,0EBH,0E3H,0E5H,0EBH,03CH,057H,07EH,0FEH,02CH,0CAH,0EDH,00CH
            db  0CFH,029H,022H,045H,002H,0E1H,022H,023H,002H,01EH,000H,0D5H,011H,0E5H,0F5H,02AH,04DH,002H,03EH,019H,0EBH,02AH,04FH,002H,0EBH,0E7H,0CAH,041H,00DH,07EH,0B9H,023H
            db  0C2H,025H,00DH,07EH,0B8H,023H,05EH,023H,056H,023H,0C2H,013H,00DH,03AH,023H,002H,0B7H,0C2H,0CFH,002H,0F1H,0CAH,0DDH,006H,096H,0CAH,09DH,00DH,01EH,010H,0C3H,0D1H
            db  002H,011H,004H,000H,0F1H,0CAH,04DH,007H,071H,023H,070H,023H,04FH,0CDH,0A6H,002H,023H,023H,022H,03CH,002H,071H,023H,03AH,023H,002H,017H,079H,001H,00BH,000H,0D2H
            db  064H,00DH,0C1H,003H,071H,023H,070H,023H,0F5H,0E5H,0CDH,04AH,015H,0EBH,0E1H,0F1H,03DH,0C2H,05CH,00DH,0F5H,042H,04BH,0EBH,019H,0DAH,03CH,00DH,0CDH,0AFH,002H,022H
            db  04FH,002H,02BH,036H,000H,0E7H,0C2H,082H,00DH,003H,057H,02AH,03CH,002H,05EH,0EBH,029H,009H,0EBH,02BH,02BH,073H,023H,072H,023H,0F1H,0DAH,0BFH,00DH,047H,04FH,07EH
            db  023H,016H,0E1H,05EH,023H,056H,023H,0E3H,0F5H,0E7H,0D2H,03CH,00DH,0E5H,0CDH,04AH,015H,0D1H,019H,0F1H,03DH,044H,04DH,0C2H,0A2H,00DH,029H,029H,0C1H,009H,0EBH,02AH
            db  045H,002H,0C9H,02AH,04FH,002H,0EBH,021H,000H,000H,039H,03AH,024H,002H,0B7H,0CAH,0DFH,00DH,0CDH,016H,010H,0CDH,022H,00FH,02AH,0D2H,001H,0EBH,02AH,03AH,002H,07DH
            db  093H,04FH,07CH,09AH,041H,050H,01EH,000H,021H,024H,002H,073H,006H,090H,0C3H,079H,014H,03AH,027H,000H,047H,0AFH,0C3H,0E5H,00DH,0CDH,060H,00EH,0CDH,052H,00EH,001H
            db  0FAH,007H,0C5H,0D5H,0CFH,028H,0CDH,044H,00CH,0CDH,086H,00AH,0CFH,029H,0CFH,0ACH,044H,04DH,0E3H,0C3H,04CH,00EH,0CDH,060H,00EH,0D5H,0CDH,03FH,00BH,0CDH,086H,00AH
            db  0E3H,05EH,023H,056H,023H,07EH,023H,066H,06FH,04EH,023H,046H,023H,0C5H,04EH,023H,046H,0C5H,02BH,02BH,02BH,0E5H,0E7H,0D5H,01EH,022H,0CAH,0D1H,002H,0CDH,0B8H,014H
            db  0E1H,0CDH,083H,00AH,02BH,0D7H,0C2H,0C6H,002H,0E1H,0D1H,0C1H,071H,023H,070H,0C3H,09DH,00EH,0E5H,02AH,0D4H,001H,023H,07CH,0B5H,0E1H,0C0H,01EH,016H,0C3H,0D1H,002H
            db  0CFH,09FH,03EH,080H,032H,040H,002H,0B6H,047H,0CDH,049H,00CH,0C3H,086H,00AH,0CDH,086H,00AH,0CDH,00FH,016H,0CDH,0A3H,00EH,0CDH,016H,010H,001H,06DH,010H,0C5H,07EH
            db  023H,023H,0E5H,0CDH,0FAH,00EH,0E1H,04EH,023H,046H,0CDH,097H,00EH,0E5H,06FH,0CDH,009H,010H,0D1H,0C9H,0CDH,0FAH,00EH,021H,036H,002H,0E5H,077H,023H,023H,073H,023H
            db  072H,0E1H,0C9H,02BH,006H,022H,050H,0E5H,00EH,0FFH,023H,07EH,00CH,0B7H,0CAH,0B9H,00EH,0BAH,0CAH,0B9H,00EH,0B8H,0C2H,0AAH,00EH,0FEH,022H,0CCH,04EH,006H,0E3H,023H
            db  0EBH,079H,0CDH,097H,00EH,011H,036H,002H,02AH,028H,002H,022H,053H,002H,03EH,001H,032H,024H,002H,0CDH,0BBH,014H,0E7H,022H,028H,002H,0E1H,07EH,0C0H,01EH,01EH,0C3H
            db  0D1H,002H,023H,0CDH,0A3H,00EH,0CDH,016H,010H,0CDH,0AFH,014H,01CH,01DH,0C8H,00AH,0DFH,0FEH,00DH,0CCH,0F3H,008H,003H,0C3H,0EDH,00EH,0B7H,00EH,0F1H,0F5H,02AH,0D2H
            db  001H,0EBH,02AH,03AH,002H,02FH,04FH,006H,0FFH,009H,023H,0E7H,0DAH,016H,00FH,022H,03AH,002H,023H,0EBH,0F1H,0C9H,0F1H,01EH,01AH,0CAH,0D1H,002H,0BFH,0F5H,001H,0FCH
            db  00EH,0C5H,02AH,026H,002H,022H,03AH,002H,021H,000H,000H,0E5H,02AH,0D2H,001H,0E5H,021H,02AH,002H,0EBH,02AH,028H,002H,0EBH,0E7H,001H,033H,00FH,0C2H,07FH,00FH,02AH
            db  04BH,002H,0EBH,02AH,04DH,002H,0EBH,0E7H,0CAH,056H,00FH,07EH,023H,023H,0B7H,0CDH,082H,00FH,0C3H,042H,00FH,0C1H,0EBH,02AH,04FH,002H,0EBH,0E7H,0CAH,0A4H,00FH,0CDH
            db  0AFH,014H,07BH,0E5H,009H,0B7H,0F2H,055H,00FH,022H,03CH,002H,0E1H,04EH,006H,000H,009H,009H,023H,0EBH,02AH,03CH,002H,0EBH,0E7H,0CAH,056H,00FH,001H,073H,00FH,0C5H
            db  0F6H,080H,07EH,023H,023H,05EH,023H,056H,023H,0F0H,0B7H,0C8H,044H,04DH,02AH,03AH,002H,0E7H,060H,069H,0D8H,0E1H,0E3H,0E7H,0E3H,0E5H,060H,069H,0D0H,0C1H,0F1H,0F1H
            db  0E5H,0D5H,0C5H,0C9H,0D1H,0E1H,07DH,0B4H,0C8H,02BH,046H,02BH,04EH,0E5H,02BH,02BH,06EH,026H,000H,009H,050H,059H,02BH,044H,04DH,02AH,03AH,002H,0CDH,09AH,002H,0E1H
            db  071H,023H,070H,069H,060H,02BH,0C3H,025H,00FH,0C5H,0E5H,02AH,053H,002H,0E3H,0CDH,009H,00BH,0E3H,0CDH,087H,00AH,07EH,0E5H,02AH,053H,002H,0E5H,086H,01EH,01CH,0DAH
            db  0D1H,002H,0CDH,094H,00EH,0D1H,0CDH,01AH,010H,0E3H,0CDH,019H,010H,0E5H,02AH,038H,002H,0EBH,0CDH,000H,010H,0CDH,000H,010H,021H,0A6H,00AH,0E3H,0E5H,0C3H,0C5H,00EH
            db  0E1H,0E3H,07EH,023H,023H,04EH,023H,046H,06FH,02CH,02DH,0C8H,00AH,012H,003H,013H,0C3H,00AH,010H,0CDH,087H,00AH,02AH,053H,002H,0EBH,0CDH,032H,010H,0EBH,0C0H,0D5H
            db  050H,059H,01BH,04EH,02AH,03AH,002H,0E7H,0C2H,030H,010H,047H,009H,022H,03AH,002H,0E1H,0C9H,02AH,028H,002H,02BH,046H,02BH,04EH,02BH,02BH,0E7H,0C0H,022H,028H,002H
            db  0C9H,001H,0F4H,00DH,0C5H,0CDH,013H,010H,0AFH,057H,032H,024H,002H,07EH,0B7H,0C9H,001H,0F4H,00DH,0C5H,0CDH,045H,010H,0CAH,04DH,007H,023H,023H,05EH,023H,056H,01AH
            db  0C9H,03EH,001H,0CDH,094H,00EH,0CDH,034H,011H,02AH,038H,002H,073H,0C1H,0C3H,0C5H,00EH,0CDH,0EDH,010H,0AFH,0E3H,04FH,0E5H,07EH,0B8H,0DAH,07FH,010H,078H,011H,00EH
            db  000H,0C5H,0CDH,0FAH,00EH,0C1H,0E1H,0E5H,023H,023H,046H,023H,066H,068H,006H,000H,009H,044H,04DH,0CDH,097H,00EH,06FH,0CDH,009H,010H,0D1H,0CDH,01AH,010H,0C3H,0C5H
            db  00EH,0CDH,0EDH,010H,0D1H,0D5H,01AH,090H,0C3H,075H,010H,0EBH,07EH,0CDH,0F0H,010H,0C5H,01EH,0FFH,0FEH,029H,0CAH,0BDH,010H,0CFH,02CH,0CDH,031H,011H,0CFH,029H,0F1H
            db  0E3H,001H,077H,010H,0C5H,03DH,0BEH,006H,000H,0D0H,04FH,07EH,091H,0BBH,047H,0D8H,043H,0C9H,0CDH,045H,010H,0CAH,087H,012H,05FH,023H,023H,07EH,023H,066H,06FH,0E5H
            db  019H,046H,072H,0E3H,0C5H,07EH,0CDH,065H,015H,0C1H,0E1H,070H,0C9H,0EBH,0CFH,029H,0C1H,0D1H,0C5H,043H,004H,005H,0C0H,0C3H,04DH,007H,0CDH,034H,011H,032H,001H,011H
            db  0DBH,000H,0C3H,0F4H,00DH,0CDH,024H,011H,0D3H,000H,0C9H,0CDH,024H,011H,0F5H,01EH,000H,02BH,0D7H,0CAH,01BH,011H,0CFH,02CH,0CDH,031H,011H,0C1H,0DBH,000H,0ABH,0A0H
            db  0CAH,01CH,011H,0C9H,0CDH,031H,011H,032H,01DH,011H,032H,009H,011H,0CFH,02CH,006H,0D7H,0CDH,083H,00AH,0CDH,034H,007H,07AH,0B7H,0C2H,04DH,007H,02BH,0D7H,07BH,0C9H
            db  0DBH,006H,0E6H,001H,0C2H,040H,011H,0DBH,007H,0C9H,0CDH,04DH,011H,0F5H,0DBH,006H,0E6H,080H,0C2H,04EH,011H,0F1H,0D3H,007H,0C9H,006H,001H,0FEH,0A6H,0CAH,0D0H,006H
            db  0CDH,094H,00AH,0E5H,0CDH,054H,010H,03EH,0D3H,0CDH,04DH,011H,0CDH,04AH,011H,01AH,0CDH,04DH,011H,02AH,0D6H,001H,0EBH,02AH,04BH,002H,01AH,013H,0CDH,04DH,011H,0E7H
            db  0C2H,07AH,011H,0E1H,0C9H,0FEH,0A6H,0CAH,0CEH,006H,0D6H,096H,0CAH,091H,011H,0AFH,001H,02FH,023H,0F5H,0CDH,094H,00AH,0CDH,054H,010H,01AH,06FH,0F1H,0B7H,067H,022H
            db  053H,002H,0CCH,0B1H,003H,02AH,053H,002H,0EBH,006H,003H,0CDH,040H,011H,0D6H,0D3H,0C2H,0A9H,011H,005H,0C2H,0ABH,011H,0CDH,040H,011H,093H,0C2H,0A9H,011H,02AH,0D6H
            db  001H,006H,003H,0CDH,040H,011H,05FH,096H,0A2H,0C2H,0E6H,011H,073H,0CDH,0AFH,002H,07EH,0B7H,023H,0C2H,0C1H,011H,005H,0C2H,0C3H,011H,022H,04BH,002H,021H,071H,002H
            db  0CDH,0E3H,00EH,0C3H,075H,003H,021H,0EFH,011H,0CDH,0E3H,00EH,0C3H,0F6H,002H,04EH,04FH,020H,047H,04FH,04FH,044H,00DH,00AH,000H,0CDH,038H,007H,01AH,0C3H,0F4H,00DH
            db  0CDH,083H,00AH,0CDH,038H,007H,0D5H,0CFH,02CH,0CDH,031H,011H,0D1H,012H,0C9H,021H,0E4H,016H,0CDH,0AFH,014H,0C3H,021H,012H,0CDH,0AFH,014H,021H,0C1H,0D1H,0CDH,089H
            db  014H,078H,0B7H,0C8H,03AH,056H,002H,0B7H,0CAH,0A1H,014H,090H,0D2H,03BH,012H,02FH,03CH,0EBH,0CDH,091H,014H,0EBH,0CDH,0A1H,014H,0C1H,0D1H,0FEH,019H,0D0H,0F5H,0CDH
            db  0C6H,014H,067H,0F1H,0CDH,0E8H,012H,0B4H,021H,053H,002H,0F2H,061H,012H,0CDH,0C8H,012H,0D2H,0A7H,012H,023H,034H,0CAH,0C3H,012H,02EH,001H,0CDH,0FEH,012H,0C3H,0A7H
            db  012H,0AFH,090H,047H,07EH,09BH,05FH,023H,07EH,09AH,057H,023H,07EH,099H,04FH,0DCH,0D4H,012H,068H,063H,0AFH,047H,079H,0B7H,0C2H,094H,012H,04AH,054H,065H,06FH,078H
            db  0D6H,008H,0FEH,0E0H,0C2H,075H,012H,0AFH,032H,056H,002H,0C9H,005H,029H,07AH,017H,057H,079H,08FH,04FH,0F2H,08CH,012H,078H,05CH,045H,0B7H,0CAH,0A7H,012H,021H,056H
            db  002H,086H,077H,0D2H,087H,012H,0C8H,078H,021H,056H,002H,0B7H,0FCH,0B9H,012H,046H,023H,07EH,0E6H,080H,0A9H,04FH,0C3H,0A1H,014H,01CH,0C0H,014H,0C0H,00CH,0C0H,00EH
            db  080H,034H,0C0H,01EH,00AH,0C3H,0D1H,002H,07EH,083H,05FH,023H,07EH,08AH,057H,023H,07EH,089H,04FH,0C9H,021H,057H,002H,07EH,02FH,077H,0AFH,06FH,090H,047H,07DH,09BH
            db  05FH,07DH,09AH,057H,07DH,099H,04FH,0C9H,006H,000H,0D6H,008H,0DAH,0F7H,012H,043H,05AH,051H,00EH,000H,0C3H,0EAH,012H,0C6H,009H,06FH,0AFH,02DH,0C8H,079H,01FH,04FH
            db  07AH,01FH,057H,07BH,01FH,05FH,078H,01FH,047H,0C3H,0FAH,012H,000H,000H,000H,081H,003H,0AAH,056H,019H,080H,0F1H,022H,076H,080H,045H,0AAH,038H,082H,0EFH,0EAH,04DH
            db  007H,021H,056H,002H,07EH,001H,035H,080H,011H,0F3H,004H,090H,0F5H,070H,0D5H,0C5H,0CDH,021H,012H,0C1H,0D1H,004H,0CDH,0B9H,013H,021H,00CH,013H,0CDH,018H,012H,021H
            db  010H,013H,0CDH,0ACH,017H,001H,080H,080H,011H,000H,000H,0CDH,021H,012H,0F1H,0CDH,0E5H,015H,001H,031H,080H,011H,018H,072H,021H,0C1H,0D1H,0EFH,0C8H,02EH,000H,0CDH
            db  029H,014H,079H,032H,092H,013H,0EBH,022H,08DH,013H,001H,000H,000H,050H,058H,021H,072H,012H,0E5H,021H,07BH,013H,0E5H,0E5H,021H,053H,002H,07EH,023H,0B7H,0CAH,0A6H
            db  013H,0E5H,0EBH,01EH,008H,01FH,057H,079H,0D2H,093H,013H,0D5H,011H,000H,000H,019H,0D1H,0CEH,000H,01FH,04FH,07CH,01FH,067H,07DH,01FH,06FH,078H,01FH,047H,01DH,07AH
            db  0C2H,085H,013H,0EBH,0E1H,0C9H,043H,05AH,051H,04FH,0C9H,0CDH,091H,014H,001H,020H,084H,011H,000H,000H,0CDH,0A1H,014H,0C1H,0D1H,0EFH,0CAH,0C9H,002H,02EH,0FFH,0CDH
            db  029H,014H,034H,034H,02BH,07EH,032H,0E8H,013H,02BH,07EH,032H,0E4H,013H,02BH,07EH,032H,0E0H,013H,041H,0EBH,0AFH,04FH,057H,05FH,032H,0EBH,013H,0E5H,0C5H,07DH,0D6H
            db  000H,06FH,07CH,0DEH,000H,067H,078H,0DEH,000H,047H,03EH,000H,0DEH,000H,03FH,0D2H,0F9H,013H,032H,0EBH,013H,0F1H,0F1H,037H,0D2H,0C1H,0E1H,079H,03CH,03DH,01FH,0FAH
            db  0A8H,012H,017H,07BH,017H,05FH,07AH,017H,057H,079H,017H,04FH,029H,078H,017H,047H,03AH,0EBH,013H,017H,032H,0EBH,013H,079H,0B2H,0B3H,0C2H,0DCH,013H,0E5H,021H,056H
            db  002H,035H,0E1H,0C2H,0DCH,013H,0C3H,0C3H,012H,078H,0B7H,0CAH,04BH,014H,07DH,021H,056H,002H,0AEH,080H,047H,01FH,0A8H,078H,0F2H,04AH,014H,0C6H,080H,077H,0CAH,0A4H
            db  013H,0CDH,0C6H,014H,077H,02BH,0C9H,0EFH,02FH,0E1H,0B7H,0E1H,0F2H,087H,012H,0C3H,0C3H,012H,0CDH,0ACH,014H,078H,0B7H,0C8H,0C6H,002H,0DAH,0C3H,012H,047H,0CDH,021H
            db  012H,021H,056H,002H,034H,0C0H,0C3H,0C3H,012H,03AH,055H,002H,0FEH,02FH,017H,09FH,0C0H,03CH,0C9H,0EFH,006H,088H,011H,000H,000H,021H,056H,002H,04FH,070H,006H,000H
            db  023H,036H,080H,017H,0C3H,06FH,012H,0EFH,0F0H,021H,055H,002H,07EH,0EEH,080H,077H,0C9H,0EBH,02AH,053H,002H,0E3H,0E5H,02AH,055H,002H,0E3H,0E5H,0EBH,0C9H,0CDH,0AFH
            db  014H,0EBH,022H,053H,002H,060H,069H,022H,055H,002H,0EBH,0C9H,021H,053H,002H,05EH,023H,056H,023H,04EH,023H,046H,023H,0C9H,011H,053H,002H,006H,004H,01AH,077H,013H
            db  023H,005H,0C2H,0BDH,014H,0C9H,021H,055H,002H,07EH,007H,037H,01FH,077H,03FH,01FH,023H,023H,077H,079H,007H,037H,01FH,04FH,01FH,0AEH,0C9H,078H,0B7H,0CAH,028H,000H
            db  021H,06DH,014H,0E5H,0EFH,079H,0C8H,021H,055H,002H,0AEH,079H,0F8H,0CDH,0F3H,014H,01FH,0A9H,0C9H,023H,078H,0BEH,0C0H,02BH,079H,0BEH,0C0H,02BH,07AH,0BEH,0C0H,02BH
            db  07BH,096H,0C0H,0E1H,0E1H,0C9H,047H,04FH,057H,05FH,0B7H,0C8H,0E5H,0CDH,0ACH,014H,0CDH,0C6H,014H,0AEH,067H,0FCH,02AH,015H,03EH,098H,090H,0CDH,0E8H,012H,07CH,017H
            db  0DCH,0B9H,012H,006H,000H,0DCH,0D4H,012H,0E1H,0C9H,01BH,07AH,0A3H,03CH,0C0H,00BH,0C9H,021H,056H,002H,07EH,0FEH,098H,03AH,053H,002H,0D0H,07EH,0CDH,006H,015H,036H
            db  098H,07BH,0F5H,079H,017H,0CDH,06FH,012H,0F1H,0C9H,021H,000H,000H,078H,0B1H,0C8H,03EH,010H,029H,0DAH,03CH,00DH,0EBH,029H,0EBH,0D2H,060H,015H,009H,0DAH,03CH,00DH
            db  03DH,0C2H,052H,015H,0C9H,0FEH,02DH,0F5H,0CAH,071H,015H,0FEH,02BH,0CAH,071H,015H,02BH,0CDH,087H,012H,047H,057H,05FH,02FH,04FH,0D7H,0DAH,0CEH,015H,0FEH,02EH,0CAH
            db  0A9H,015H,0FEH,045H,0C2H,0ADH,015H,0D7H,0E5H,021H,09DH,015H,0E3H,015H,0FEH,0A5H,0C8H,0FEH,02DH,0C8H,014H,0FEH,02BH,0C8H,0FEH,0A4H,0C8H,0F1H,02BH,0D7H,0DAH,0F0H
            db  015H,014H,0C2H,0ADH,015H,0AFH,093H,05FH,00CH,00CH,0CAH,079H,015H,0E5H,07BH,090H,0F4H,0C6H,015H,0F2H,0BCH,015H,0F5H,0CDH,0ABH,013H,0F1H,03CH,0C2H,0B0H,015H,0D1H
            db  0F1H,0CCH,089H,014H,0EBH,0C9H,0C8H,0F5H,0CDH,052H,014H,0F1H,03DH,0C9H,0D5H,057H,078H,089H,047H,0C5H,0E5H,0D5H,0CDH,052H,014H,0F1H,0D6H,030H,0CDH,0E5H,015H,0E1H
            db  0C1H,0D1H,0C3H,079H,015H,0CDH,091H,014H,0CDH,074H,014H,0C1H,0D1H,0C3H,021H,012H,07BH,007H,007H,083H,007H,086H,0D6H,030H,05FH,0C3H,09DH,015H,0E5H,021H,06CH,002H
            db  0CDH,0E3H,00EH,0E1H,0EBH,0AFH,006H,098H,0CDH,079H,014H,021H,0E2H,00EH,0E5H,021H,058H,002H,0E5H,0EFH,036H,020H,0F2H,01BH,016H,036H,02DH,023H,036H,030H,0CAH,0D0H
            db  016H,0E5H,0FCH,089H,014H,0AFH,0F5H,0CDH,0D6H,016H,001H,043H,091H,011H,0F8H,04FH,0CDH,0DBH,014H,0E2H,047H,016H,0F1H,0CDH,0C7H,015H,0F5H,0C3H,02AH,016H,0CDH,0ABH
            db  013H,0F1H,03CH,0F5H,0CDH,0D6H,016H,0CDH,00FH,012H,03CH,0CDH,006H,015H,0CDH,0A1H,014H,001H,006H,003H,0F1H,081H,03CH,0FAH,063H,016H,0FEH,008H,0D2H,063H,016H,03CH
            db  047H,03EH,002H,03DH,03DH,0E1H,0F5H,011H,0E8H,016H,005H,0C2H,074H,016H,036H,02EH,023H,036H,030H,023H,005H,036H,02EH,0CCH,0B6H,014H,0C5H,0E5H,0D5H,0CDH,0ACH,014H
            db  0E1H,006H,02FH,004H,07BH,096H,05FH,023H,07AH,09EH,057H,023H,079H,09EH,04FH,02BH,02BH,0D2H,083H,016H,0CDH,0C8H,012H,023H,0CDH,0A1H,014H,0EBH,0E1H,070H,023H,0C1H
            db  00DH,0C2H,074H,016H,005H,0CAH,0B4H,016H,02BH,07EH,0FEH,030H,0CAH,0A8H,016H,0FEH,02EH,0C4H,0B6H,014H,0F1H,0CAH,0D3H,016H,036H,045H,023H,036H,02BH,0F2H,0C4H,016H
            db  036H,02DH,02FH,03CH,006H,02FH,004H,0D6H,00AH,0D2H,0C6H,016H,0C6H,03AH,023H,070H,023H,077H,023H,071H,0E1H,0C9H,001H,074H,094H,011H,0F7H,023H,0CDH,0DBH,014H,0E1H
            db  0E2H,03EH,016H,0E9H,000H,000H,000H,080H,0A0H,086H,001H,010H,027H,000H,0E8H,003H,000H,064H,000H,000H,00AH,000H,000H,001H,000H,000H,021H,089H,014H,0E3H,0E9H,0CDH
            db  091H,014H,021H,0E4H,016H,0CDH,09EH,014H,0C1H,0D1H,0EFH,078H,0CAH,04BH,017H,0F2H,016H,017H,0B7H,0CAH,0C9H,002H,0B7H,0CAH,088H,012H,0D5H,0C5H,079H,0F6H,07FH,0CDH
            db  0ACH,014H,0F2H,033H,017H,0D5H,0C5H,0CDH,031H,015H,0C1H,0D1H,0F5H,0CDH,0DBH,014H,0E1H,07CH,01FH,0E1H,022H,055H,002H,0E1H,022H,053H,002H,0DCH,0FAH,016H,0CCH,089H
            db  014H,0D5H,0C5H,0CDH,01DH,013H,0C1H,0D1H,0CDH,05BH,013H,0CDH,091H,014H,001H,038H,081H,011H,03BH,0AAH,0CDH,05BH,013H,03AH,056H,002H,0FEH,088H,0D2H,047H,014H,0CDH
            db  031H,015H,0C6H,080H,0C6H,002H,0DAH,047H,014H,0F5H,021H,00CH,013H,0CDH,012H,012H,0CDH,052H,013H,0F1H,0C1H,0D1H,0F5H,0CDH,01EH,012H,0CDH,089H,014H,021H,08BH,017H
            db  0CDH,0BBH,017H,011H,000H,000H,0C1H,04AH,0C3H,05BH,013H,008H,040H,02EH,094H,074H,070H,04FH,02EH,077H,06EH,002H,088H,07AH,0E6H,0A0H,02AH,07CH,050H,0AAH,0AAH,07EH
            db  0FFH,0FFH,07FH,07FH,000H,000H,080H,081H,000H,000H,000H,081H,0CDH,091H,014H,011H,059H,013H,0D5H,0E5H,0CDH,0ACH,014H,0CDH,05BH,013H,0E1H,0CDH,091H,014H,07EH,023H
            db  0CDH,09EH,014H,006H,0F1H,0C1H,0D1H,03DH,0C8H,0D5H,0C5H,0F5H,0E5H,0CDH,05BH,013H,0E1H,0CDH,0AFH,014H,0E5H,0CDH,021H,012H,0E1H,0C3H,0C4H,017H,0EFH,021H,045H,018H
            db  0FAH,03BH,018H,021H,066H,018H,0CDH,09EH,014H,021H,045H,018H,0C8H,086H,0E6H,007H,006H,000H,077H,023H,087H,087H,04FH,009H,0CDH,0AFH,014H,0CDH,05BH,013H,03AH,044H
            db  018H,03CH,0E6H,003H,006H,000H,0FEH,001H,088H,032H,044H,018H,021H,066H,018H,087H,087H,04FH,009H,0CDH,012H,012H,0CDH,0ACH,014H,07BH,059H,0EEH,04FH,04FH,036H,080H
            db  02BH,046H,036H,080H,021H,043H,018H,034H,07EH,0D6H,0ABH,0C2H,032H,018H,077H,00CH,015H,01CH,0CDH,072H,012H,021H,066H,018H,0C3H,0B8H,014H,077H,02BH,077H,02BH,077H
            db  0C3H,016H,018H,000H,000H,000H,035H,04AH,0CAH,099H,039H,01CH,076H,098H,022H,095H,0B3H,098H,00AH,0DDH,047H,098H,053H,0D1H,099H,099H,00AH,01AH,09FH,098H,065H,0BCH
            db  0CDH,098H,0D6H,077H,03EH,098H,052H,0C7H,04FH,080H,068H,0B1H,046H,068H,099H,0E9H,092H,069H,010H,0D1H,075H,068H,021H,0BCH,018H,0CDH,012H,012H,0CDH,091H,014H,001H
            db  049H,083H,011H,0DBH,00FH,0CDH,0A1H,014H,0C1H,0D1H,0CDH,0B9H,013H,0CDH,091H,014H,0CDH,031H,015H,0C1H,0D1H,0CDH,01EH,012H,021H,0C0H,018H,0CDH,018H,012H,0EFH,037H
            db  0F2H,0A8H,018H,0CDH,00FH,012H,0EFH,0B7H,0F5H,0F4H,089H,014H,021H,0C0H,018H,0CDH,012H,012H,0F1H,0D4H,089H,014H,021H,0C4H,018H,0C3H,0ACH,017H,0DBH,00FH,049H,081H
            db  000H,000H,000H,07FH,005H,0BAH,0D7H,01EH,086H,064H,026H,099H,087H,058H,034H,023H,087H,0E0H,05DH,0A5H,086H,0DAH,00FH,049H,083H,0CDH,091H,014H,0CDH,07CH,018H,0C1H
            db  0E1H,0CDH,091H,014H,0EBH,0CDH,0A1H,014H,0CDH,076H,018H,0C3H,0B7H,013H,0EFH,0FCH,0FAH,016H,0FCH,089H,014H,03AH,056H,002H,0FEH,081H,0DAH,009H,019H,001H,000H,081H
            db  051H,059H,0CDH,0B9H,013H,021H,018H,012H,0E5H,021H,013H,019H,0CDH,0ACH,017H,021H,0BCH,018H,0C9H,009H,04AH,0D7H,03BH,078H,002H,06EH,084H,07BH,0FEH,0C1H,02FH,07CH
;           db  074H,031H,09AH,07DH,084H,03DH,05AH,07DH,0C8H,07FH,091H,07EH,0E4H,0BBH,04CH,07EH,06CH,0AAH,0AAH,07FH,000H,000H,000H,081H,000H,000H,0DBH,0FFH,0E6H,0F0H,00FH,00FH
            db  074H,031H,09AH,07DH,084H,03DH,05AH,07DH,0C8H,07FH,091H,07EH,0E4H,0BBH,04CH,07EH,06CH,0AAH,0AAH,07FH,000H,000H,000H,081H,000H,000H,03EH,000H,0E6H,0F0H,00FH,00FH
;                                                                                                                                                 ^^^^ ^^^^
            db  0FEH,03CH,0C8H,0FEH,038H,037H,0C2H,053H,019H,021H,0FFH,01FH,04EH,02BH,07EH,0E6H,0F0H,00FH,00FH,0F5H,06FH,026H,000H,011H,0B8H,019H,019H,07EH,023H,056H,023H,046H
            db  023H,05EH,067H,0F1H,0F5H,07CH,0DAH,06AH,019H,079H,032H,0B6H,019H,0F1H,021H,0D4H,019H,0E5H,00EH,0FFH,0FEH,010H,021H,000H,000H,022H,052H,005H,0CAH,08EH,019H,0FEH
            db  008H,0D0H,0C6H,011H,0F5H,03EH,003H,0CDH,0B5H,019H,0F1H,0C3H,0B5H,019H,0AFH,0CDH,0B5H,019H,0CDH,0B1H,019H,0CDH,0B1H,019H,02FH,00EH,001H,0CDH,0B1H,019H,0E5H,02AH
            db  0B5H,019H,02EH,0DBH,022H,052H,005H,0E1H,03EH,02CH,035H,0CDH,0B5H,019H,035H,035H,035H,021H,0B6H,019H,034H,0D3H,010H,0C9H,010H,0CAH,001H,002H,010H,0CAH,001H,002H
            db  000H,0C2H,001H,080H,006H,0C2H,001H,080H,020H,0CAH,080H,080H,004H,0CAH,002H,001H,024H,0CAH,040H,040H,062H,068H,022H,059H,005H,07CH,0E6H,0C8H,067H,022H,06BH,006H
            db  0EEH,00CH,067H,022H,00FH,006H,0EBH,022H,04AH,005H,03AH,0B6H,019H,032H,057H,005H,032H,069H,006H,032H,00DH,006H,03CH,032H,05EH,005H,081H,032H,048H,005H,03CH,032H
            db  050H,005H,0C9H,000H,000H,021H,030H,01BH,0CDH,0E3H,00EH,021H,00FH,01CH,0F9H,022H,0D2H,001H,0CDH,03AH,019H,021H,0FFH,0FFH,022H,0D4H,001H,0AFH,032H,0D1H,001H,0CDH
            db  0EDH,008H,021H,02AH,002H,022H,028H,002H,021H,068H,01BH,0CDH,0E3H,00EH,0CDH,0EFH,003H,0DAH,028H,01AH,0D7H,0FEH,041H,0CAH,005H,01AH,0B7H,0C2H,051H,01AH,021H,0C9H
            db  01BH,023H,07CH,0B5H,0CAH,05DH,01AH,07EH,02FH,077H,0BEH,0CAH,041H,01AH,0C3H,05DH,01AH,021H,0D9H,001H,0CDH,052H,007H,07EH,0B7H,0C2H,0C6H,002H,0EBH,02BH,0E5H,021H
            db  059H,01BH,0CDH,0E3H,00EH,0CDH,0EFH,003H,0DAH,05FH,01AH,0D7H,0B7H,03EH,048H,05FH,0CAH,081H,01AH,0CDH,052H,007H,07AH,0B7H,0C2H,05FH,01AH,07BH,0FEH,00FH,0DAH,05FH
            db  01AH,032H,03FH,005H,032H,0C6H,006H,032H,0CEH,008H,0D6H,00EH,0D2H,08AH,01AH,0C6H,01CH,02FH,03CH,083H,032H,006H,009H,011H,0CFH,0FFH,0E1H,022H,026H,002H,019H,0D2H
            db  0BBH,002H,02BH,0E5H,021H,01BH,01BH,0CDH,0E3H,00EH,0CDH,0EFH,003H,0DAH,0A4H,01AH,0D7H,021H,04DH,007H,0FEH,059H,011H,038H,019H,0CAH,0DDH,01AH,0FEH,041H,0CAH,0C6H
            db  01AH,0FEH,04EH,0C2H,0A4H,01AH,011H,0EEH,018H,022H,05FH,000H,0FEH,041H,0CAH,0DDH,01AH,022H,059H,000H,022H,05DH,000H,022H,05BH,000H,011H,076H,018H,0EBH,036H,000H
            db  023H,022H,0D6H,001H,0E3H,011H,00FH,01CH,0E7H,0DAH,0BBH,002H,0D1H,0F9H,022H,0D2H,001H,0EBH,0CDH,0AFH,002H,07BH,095H,06FH,07AH,09CH,067H,001H,0F0H,0FFH,009H,0CDH
            db  0EDH,008H,0CDH,004H,016H,021H,074H,01BH,0CDH,0E3H,00EH,021H,0E3H,00EH,022H,002H,003H,0CDH,0B1H,003H,021H,0F7H,002H,022H,002H,000H,0E9H,057H,041H,04EH,054H,020H
            db  053H,049H,04EH,02DH,043H,04FH,053H,02DH,054H,041H,04EH,02DH,041H,054H,04EH,000H,00DH,00AH,00AH,057H,052H,049H,054H,054H,045H,04EH,020H,046H,04FH,052H,020H,052H
            db  04FH,059H,041H,04CH,054H,049H,045H,053H,020H,042H,059H,020H,04DH,049H,043H,052H,04FH,02DH,053H,04FH,046H,054H,00DH,00AH,000H,054H,045H,052H,04DH,049H,04EH,041H
            db  04CH,020H,057H,049H,044H,054H,048H,000H,04DH,045H,04DH,04FH,052H,059H,020H,053H,049H,05AH,045H,000H,020H,042H,059H,054H,045H,053H,020H,046H,052H,045H,045H,00DH
            db  00AH,041H,04CH,054H,041H,049H,052H,020H,042H,041H,053H,049H,043H,020H,052H,045H,056H,02EH,020H,034H,02EH,030H,00DH,00AH,05BH,045H,049H,047H,048H,054H,02DH,04BH
            db  020H,056H,045H,052H,053H,049H,04FH,04EH,05DH,00DH,00AH,043H,04FH,050H,059H,052H,049H,047H,048H,054H,020H,031H,039H,037H,036H,020H,042H,059H,020H,04DH,049H,054H
            db  053H,020H,049H,04EH,043H,02EH,00DH,00AH,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H
            db  000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H
basic8kend:

;*********************************************************
;*                                                       *
;*               8085 system monitor                     *
;*                                                       *
;*********************************************************

core    equ      07FEFH          ;top of utility ram
pcloc   equ      core-2
stack   equ      core-28         ;(core-1CH) stack location
ram     equ      stack-256       ;requires 256 bytes of ram

;*********************************************************
;*                                                       *
;*               global constants                        *
;*                                                       *
;*********************************************************
comma   equ       ','            ;comma
ctrlc   equ       3              ;control c abort
ctrlh   equ       8              ;video terminal backspace
ctrlo   equ       15             ;control o suppress output
ctrlq   equ       17             ;continue output command
ctrls   equ       19             ;stop output command
ctrlz   equ       26             ;end of ascii char. in 'asc' psuedo op

; program entry point...
; initialize everything
monitor:

; locate the stack at the top of specified ram memory, set
; the user register save area and exit template
        lxi     h,core-2        ;place debug entrance and exit template
        mvi     b,endx-exitc    ;in RAM, B has length of template
        lxi     d,endx          ;point to template end
bg1:    dcx     d               ;move pointer down
        ldax    d               ;load a with data
        dcx     h               ;move memory pointer down
        mov     m,a             ;write data
        dcr     b               ;end of template?
        jnz     bg1             ;loop till done

; type sign-on message
        xra     a               ;zero a
        sta     lstsupflag      ;clear list suppression flag
        lxi     h,m0            ;type entry
        call    crlfmg          ;message
        jmp     reset           ;continue elsewhere

;*********************************************************
;*               next monitor command                    *
;*                                                       *
;* this is the re-entry point after each command has     *
;* been executed                                         *
;*********************************************************
next:   lxi     sp,stack        ;restore stack pointer
        call    crlf            ;turn up a new line
        mvi     a,'-'           ;send the prompt
        call    type            ;type it
        mvi     a,'>'           ;get other half
        call    chrspc          ;type it and a space
nxt1:   call    chin            ;get command char
        mov     b,a             ;and save command

; check for some legal non-alpha commands
        cpi     '-'             ;examine previous location
        jz      lstlc           ;
        cpi     '.'             ;examine current location
        jz      locat           ;
        cpi     lf              ;examine next location
        jz      nxloc
        cpi     '?'
        jz      srch4

; ignore non-alpha characters
        sui     41H             ;at least an 'A'
        jc      nxt1            ;jump if <A
        cpi     1BH             ;and not greater than 'Z'
        jnc     nxt1            ;jump if >Z

; search operation table for command
srch4:  lxi     h,optab         ;fetch table vector
srch5:  mov     a,m             ;get table command byte
        cpi     0FFH            ;check for end of table
        jz      illeg           ;must be illegal input
        cmp     b               ;compair to input
        jz      fnd5            ;found command
        inx     h               ;bump to
        inx     h               ;next
        inx     h               ;command
        inx     h               ;
        inx     h               ;
        jmp     srch5           ;and continue

; undefined command, type error message
illeg:  lxi     h,m2            ;undefined
illeg1: call    msg             ;message
        jmp     reset           ;clean up and try again

; found command, now fetch address and execute command
fnd3:   inx     h               ;bump to low address byte
        mov     e,m             ;get addr vector
        inx     h
        mov     d,m
        xchg                    ;msg ptr to HL
        pchl                    ;goto command processor

fnd5:   inx     h               ;bump to low address byte
        call    ilodm           ;get vector to BC, msg to DE
        xchg                    ;msg ptr to HL
        call    msg             ;send the message
        mov     h,b             ;vector to HL
        mov     l,c
        pchl                    ;goto command processor

;*************************************************
;*                                               *
;*       operation decode/dispatch               *
;*                                               *
;*************************************************
optab:  db       '?'             ;command
        dw       help            ;display help (hints, really)
        dw       m75

        db       'A'             ;command
        dw       getad           ;to get address
        dw       m32

        db       'D'             ;command
        dw       dumper          ;to dump memory
        dw       m27

        db       'F'             ;command
        dw       fill            ;to fill memory
        dw       m30

        db       'H'             ;command
        dw       help            ;display help (hints, really)
        dw       m70

        db       'I'             ;command
        dw       inport          ;get input from port
        dw       m72

        db       'J'             ;command
        dw       jump            ;to jump to memory location
        dw       m27

        db       'L'             ;command
        dw       load            ;load memory
        dw       m14

        db       'M'             ;command
        dw       move            ;to move area of memory
        dw       m29

        db       'O'             ; command
        dw       outport         ;output to port
        dw       m73

        db       'P'             ;command
        dw       pcmd            ;to punch intel hex tape
        dw       m51

        db       'R'             ;command
        dw       rcmd            ;to examine/modify/display registers
        dw       m66

        db       'T'             ;command
        dw       test2           ;to test memory
        dw       m31

        db       'S'             ;command
        dw       search          ;to search byte location
        dw       m34

        db       'Z'             ;command
        dw       zap             ;to zap (zero) a block of memory
        dw       m13

        db       'X'             ;command
        dw       quit            ;exit program
        dw       m0

        db       0FFH            ;end of table code

;*************************************************
;*                                               *
;*       sub operation decode/dispatch routines  *
;*                                               *
;*************************************************
; the 'D' command comes here to read a second character

; 'DH' will result in a hex dump
;     >< DUMP HEX XXXX YYYY

; 'DS' will result in a symbolic dump (disassembly)
;     >< DUMP SYMB XXXX YYYY

dumper: call    space           ;space over one
        lxi     h,dtab          ;point to dump option table
dumpda: call    chin            ;get second command char
        mov     b,a             ;save command in B
        jmp     srch5           ;search table for command

; dump command decode/dispatch table
dtab:   db       'H'             ;command to
        dw       dump            ;dump bytes
        dw       m35             ;msg pointer
        db       'S'             ;command to
        dw       dmpsym          ;dump symbolic
        dw       m38             ;msg pointer
        db       0FFH            ;end of table indicator

;*********************************
; the 'L' command comes here to read a second character
;
; intel hex format loader and a symbolic loader are
; available and are selected by the choice of the second
; command character. the symbolic loader is a one-pass
; assembler with a macro-command capability.
load:   lxi     h,ltab          ;point to loader option table
        jmp     dumpda          ;search table for command

; loader decode/dispatch table
ltab:   db       'H'             ;command to read
        dw       hexin           ;intel hex format
        dw       m35             ;msg pointer
        db       'S'             ;command to
        dw       losym           ;load symbolic
        dw       m38             ;msg pointer
        db       0FFH            ;end of table

;*********************************
; the 'P' command comes here to read a second character
;
; intel hex format puncher.
;
; the second character specifies 'H' for hex, 'E' for
; end-of-file
pcmd:   lxi     h,ptab          ;point to loader option table
        jmp     dumpda

; puncher dispatch table
ptab:   db       'H'             ;command to punch
        dw       punhex          ;intel hex format
        dw       m35             ;msg pointer
        db       'E'             ;command to punch eof
        dw       pend
        dw       m67             ;msg pointer
        db       0FFH            ;end of table

;*********************************
; the 'R' command comes here to read a second character
rcmd:   lxi     h,rtab          ;point to command table
        jmp     dumpda          ;search table for command

; register command decode/dispatch table
rtab:   db       'M'             ;examine/modify
        dw       modreg
        dw       m68+8
        db       'E'             ;examine/modify
        dw       modreg
        dw       m68
        db       'D'             ;display
        dw       regx
        dw       m69
        db       0FFH            ;end of table

;*********************************
; the 'S' command comes here to read a second character
;
; 'SH' will result in a list of the addresses of the byte ZZ
;
;     >< SEARCH HEX XXXX YYYY ZZ
;
; 'SS' will result in a symbolic list (disassembly in radix selec
;     >< SEARCH SYMB XXXX YYYY ZZ
;
search: lxi     h,srchtab        ;point to search option table
        jmp     dumpda           ;search table for command

; search command decode/dispatch table
srchtab:db       'H'             ;command to
        dw       srchb           ;search bytes
        dw       m35             ;msg pointer
        db       'S'             ;command to
        dw       srchs           ;search symbolic
        dw       m38             ;msg pointer
        db       0FFH            ;end of table indicator

;*********************************************************
;**                                                     **
;**              utility subroutines                    **
;**                                                     **
;*********************************************************

;*********************************************************
;*               character input routine                 *
;*********************************************************
chin:   mvi     a,0FFH          ;set echo
        sta     echo            ;flag on
chin1:  call    ascin           ;get character & strip parity
        push    psw             ;save data
        lda     echo            ;and check
        ana     a               ; echo flag
        jnz     chin2           ;echo set
        pop     psw             ;echo not set
        ret                     ; so return

chin2:  pop     psw             ;restore data and echo
        cpi     ' '             ;check for control
        cnc     type            ;echo if >= space
        ret                     ;return

;*********************************************************
;*                       crlfmg                          *
;*********************************************************
; sends a carriage return and line feed then the message
; pointed to by hl
crlfmg: call    crlf            ;turn up a new line

;*********************************************************
;*   message print routine                               *
;*                                                       *
;* end of msg (eom) can be any one of the following:     *
;*   (1) offh/0FFH                                       *
;*   (2) zero                                            *
;*   (3) bit 8 of last char = 1                          *
;*********************************************************
msg:    push    psw             ;save PSW
        push    h               ;save HL
mnxt:   mov     a,m             ;get a character
        cpi     0FFH            ;check for 0FFH/0FFH/-1 eom
        jz      mdone           ;done if offh eom found
        ora     a               ;to check for zero terminator
        jz      mdone           ;done if zero eom found
        ral                     ;rotate bit 8 into carry
        jc      mlast           ;done if bit 8 = 1 eom found
        rar                     ;restore char
        call    type            ;type the character
        inx     h               ;bump mem vector
        jmp     mnxt            ;and continue

mlast:  rar                     ;restore character
        ani     7FH             ;strip off bit 8
        call    type            ;type the character & exit

mdone:  pop     h               ;restore HL
        pop     psw             ;and PSW
        ret                     ;exit to caller
;
;*********************************************************
;* routine to begin a new line                           *
;*********************************************************
crlf:   push    psw             ;save a and flags
        mvi     a,cr            ;get a CR
        call    type            ;send it
        mvi     a,lf            ;get a LF
        jmp     spac1           ;continue elsewhere

;*********************************************************
;* print char in 'A' then a space char                   *
;*********************************************************
chrspc: call    type            ;print char in 'A'

;*********************************************************
;* routine to type one space                             *
;*********************************************************
space:  push    psw             ;save A, PSW
spac0:  mvi     a,' '           ;get a space
spac1:  call    type            ;and do it
        pop     psw             ;restore psw
        ret                     ;and return

;*********************************************************
;* type a space and then the value in A in hex on tty    *                                                         *
;*********************************************************
sthxb:  call   space            ;type a space

;*********************************************************
;* type value in A in hex on tty                         *
;*********************************************************
thxb:   push    psw             ;save A, PSW
        rrc                     ;shift
        rrc                     ; to
        rrc                     ;  left
        rrc                     ;   nibble
        call    thxn            ;type hex nibble
        pop     psw             ;restore data
        call    thxn            ;type right nibble
        ret                     ; and exit

;*********************************************************
;* routine to type one ascii character representing      *
;* bits 3-0 fo 'A' in hex                                *
;*********************************************************
thxn:   push    psw             ;save A, PSW
        ani     0FH             ;isolate nibble b3>b0
        cpi     10              ;see if >9
        jc      $+5             ;nibble <=9
        adi     7               ;adjust alpha char
        adi     '0'             ; add in ascii 0
        jmp     spac1           ;type nibble, pop PSW, ret

;*********************************************************
; routine to type a space, then contents of hl in hex    *
;*********************************************************
sthxw:  call    space           ;type a space

;*********************************************************
;* routine to type a word in hex                         *
;*********************************************************
thxw:   push    psw             ;save PSW
        mov     a,h             ;get high byte
        call    thxb            ; and type it
        mov     a,l             ;get low byte
        call    thxb            ; and type it
        pop     psw             ;restore PSW
        ret                     ; and return

;*********************************************************
;* routine to get one hex character from tty             *
;*                                                       *
;* if the character entered is 0 to 9 or A to F then     *
;* 'A' will be set to the binary value 0 to F and        *
;* the carry will be reset.                              *
;*                                                       *
;* if the character is not a valid hex digit             *
;* then the 'A' register will contain the ascii char     *
;* and the carry will be set to a 1.                     *
;*********************************************************
ghxn:   call    chin1           ;get character in
                                ;(chin1 in case no echo)
        cpi     '0'             ;return if
        rc                      ; < '0'
        cpi     ':'             ;see if numeric
        jc      ghx1            ;char is 0 to 9
        cpi     'A'             ;see if a to f
        rc                      ;char ':' to '0'
        cpi     'G'             ;see if > 'F'
        cmc                     ;invert carry sense
        rc                      ;char > 'F'
        sui     7               ;char is A to F so adjust
ghx1:   sui     '0'             ;adjust to binary
        ret                     ;and exit

;*********************************************************
; routine to type a space then get a hex byte to 'a'.    *
; this routine is different from 'ghxb' in that it will  *
; force a correct byte entry.                            *
;*********************************************************
spcby:  call    space           ;type a space

;*********************************************************
;* routine to get a hex byte to 'a'.                     *
;* will force a correct entry.                           *
;*********************************************************
inpby:  call    ghxb            ;get hex byte
        rnc                     ;return if legal entry
        push    h               ;save hl
        lxi     h,m46           ;send the 'num?'
        call    msg             ; message
        pop     h               ;restore HL
        jmp     inpby           ;do it again

;*********************************************************
;* routine to type a space and get a hex byte to 'a'     *
;*********************************************************
sghxb:  call    space           ;type a space

;*********************************************************
;* routine to get one hex byte from tty                  *
;*********************************************************
ghxb:   call    ghxn            ;get left nibble
        rc                      ;leave if non-hex
        push    b               ;save BC
        rlc                     ;shift
        rlc                     ; to
        rlc                     ;  left
        rlc                     ;   nibble
        mov     b,a             ;and save in B
        call    ghxn            ;get right nibble
        jc      $+4             ;jump if non-hex
        add     b               ;add in left nibble
        pop     b               ;restore BC
        ret                     ;and exit

;*********************************************************
;* routine to send a space and force input of legal hex  *
;* word.  exits with word in hl pair.                    *
;*********************************************************
spcwd:  call    space           ;send a space

;*********************************************************
;* routine to force input of legal hex word to hl.       *
;*********************************************************
inpwd:  call    ghxw            ;get hex word
        rnc                     ;return if legal
        lxi     h,m46           ;send the 'num'
        call    msg             ; message
        jmp     inpwd           ;do it again

;*********************************************************
;* type a space and then get a hex word from tty         *
;*********************************************************
sghxw:  call    space           ;type a space

;*********************************************************
;* routine to get a hex word from tty                    *
;*                                                       *
;* if input value is valid hex then value will be in hl  *
;*   with all other registers preserved and carry reset. *
;*                                                       *
;* if input if invalid, hl will be partially modified    *
;*   and carry will be set and 'a' will have the         *
;*   illegal non-hex character                           *
;*********************************************************
ghxw:   stc             ;set and
        cmc             ;clear carry
        push    psw     ;save status
        call    ghxb    ;get high hex byte
        mov     h,a     ;and set to H
        jnc     ghx2    ;jump if valid
        pop     psw     ;restore status
        mov     a,h     ;set to bad character
        stc             ;set carry
        ret             ; and exit

ghx2:   call    ghxb    ;get low hex byte
        mov     l,a     ; and set to L
        jnc     ghx3    ;jump if valid
        pop     psw     ;invalid, restore status
        mov     a,l     ;set 'A' to bad char
        stc             ; set carry
        ret             ;  and return

ghx3:   pop     psw     ;all ok
        ret             ;so ret with HL set to word

;*********************************************************
;* routine to store a byte in memory with read-back chk  *
;*                                                       *
;* if read-back check fails, and appropriate error       *
;* message will be typed, and control returned to        *
;* the user.                                             *
;*********************************************************
store:  mov     m,a             ;store the byte
        cmp     m               ;read-back check
        rz                      ;return if read-back ok
memwe:  push    h               ;error, save vector
        lxi     h,m4            ;type error
        call    crlfmg          ; message
        pop     h               ;restore vector
        call    thxw            ; and type address
        jmp     next            ;and return to exec

;*********************************************************
;*               memory examine/modify routines          *
;* the following routines handle memory examines and     *
;* modifies. the address of the memory location          *
;* currently being accessed is in 'adr'.                 *
;*                                                       *
;* to initialize 'adr', the monitor command 'A' is used. *
;*                                                       *
;*       >< ADDR 1234                                    *
;*                                                       *
;* will set the 'adr' to the value 1234 (hex)            *
;*                                                       *
;* the routine will then return the carriage, type the   *
;* value of 'adr' and it's contents in hex, and wait     *
;* for one of the following inputs:                      *
;*                                                       *
;* - a valid hex byte to replace the value typed in      *
;*   which case the routine will 'store' the byte,       *
;*   increment 'adr', and do the next address            *
;*                                                       *
;* - a line-feed or space will cause the next address    *
;*   to be acessed with-out modifying the current one    *
;*                                                       *
;* - a carriage-return will return control to the        *
;*   monitor.                                            *
;*                                                       *
;* - a minus sign will cause the 'adr' to be             *
;*   decremented by one.                                 *
;*                                                       *
;* the lf and '-' may be entered as a monitor command    *
;* also and will perform the same function.              *
;*                                                       *
;* in addition, the command '.' from the monitor will    *
;* cause the contents of the current 'adr' to be typed   *
;* as if the command 'A' with 'adr' had been entered.    *
;*                                                       *
;*********************************************************
getad:  call    spcwd           ;get address
        jnc     gta1            ;jump if valid

illch:  lxi     h,m3            ;illegal input
        jmp     illeg1          ; message and back to monitor.

gta1:   shld    adr             ;save 'adr'

locat:                          ;from command '.' also
        call    crlf            ;turn up a new line
        lhld    adr             ;fetch 'adr'
        call    thxw            ; and print it
        call    space           ;space
        call    opmeby          ;get byte & print in hex
        call    sghxb           ;send space & get command or hex byte
        jc      nonhx           ;non-hex input
        call    store           ;store the new value

nxloc:                          ;from command 'LF' also
        lhld    adr             ;access
        inx     h               ; next
        jmp     gta1            ;and continue

nonhx:  cpi     cr              ;if cr
        jz      next            ; return to user
        cpi     lf              ;if lf
        jz      nxloc           ; access next 'adr'
        cpi     ' '             ;if space
        jz      nxloc           ; access next 'adr'
        cpi     '-'             ;if - access last
        jnz     illch           ;not cr, lf, or - so illegal

lstlc:                          ;from command '-' also
        lhld    adr             ;decrement
        dcx     h               ; 'adr'
        jmp     gta1            ;and continue

;*********************************************************
;*       compares HL to DE and exits if HL>DE            *
;*********************************************************
hiloex: call    hilow
        jc      reset
        ret

;*********************************************************
;*               compare HL to DE                       *
;*                                                       *
;* if HL<=DE then carry=0                                *
;* if HL>DE  then carry=1                                *
;* the routine also increments hl.                       *
;*********************************************************
hilow:  push    b               ;save bc
        mov     b,a             ;save a
        inx     h               ;increment hl
        mov     a,h             ;test for hl=0
        ora     l               ;if yes, set zero flag
        stc                     ; and carry
        jz      hilo1           ;  and go exit
        mov     a,e             ;else
        sub     l               ; compare
        mov     a,d             ;  hl with
        sbb     h               ;    de
hilo1:  mov     a,b             ;restore a
        pop     b               ;  and bc
        ret

;*********************************************************
;*                       aparam                          *
;*********************************************************
aparam: xra     a               ;enter here for HL=beg, DE=end

;*********************************************************
;* this routine gets beginning and ending addressed for  *
;* various utility routines.                             *
;*                                                       *
;* calling sequence ...                                  *
;*                                                       *
;*       ...             ;value in a                     *
;*       call    param   ;get parameters                 *
;*       ...             ;return here with hl & de       *
;*                       ;containing addresses as        *
;*                       ;follows:                       *
;*                                                       *
;* depending upon contents of the a register upon entry: *
;*                                                       *
;* a = 00  ...  exits with hl=beg  de=end                *
;* a = 01  ...  exits with hl=beg  de=block size         *
;*                                                       *
;*********************************************************
param:  push    psw             ;save option
        call    pu3             ;get beg. addr to DE
        call    pu3             ;HL=beg, DE=end
        pop     psw             ;restore option & set flags
        ora     a               ;to set flags
        rz                      ; return if A=0

; a=01 ... exits with start addr in hl and block size in de.
; exits to monitor if start addr > end addr
param1: xchg                    ;HL=end, DE=beg.
        push    d               ;beg. addr to stack
        mov     a,l             ;lsb of end addr to A
        sub     e               ;subtract lsb of start addr
        mov     e,a             ;put lsb difference in E
        mov     a,h             ;msb of end addr to A
        sbb     d               ;subtract w/borrow msb of start addr
        mov     d,a             ;put msb of diff in D
        pop     h               ;get start adrs again
        jc      illeg           ;if end adr.lt.begin quit
        inx     d               ;add one to correct count
        ret                     ;return .. hl=start, de=size

;*********************************************************
;* sends a space then accepts a hex address. the address *
;* is checked for illegal hex characters and is returned *
;* in the de register pair. previous contents of DE are  *
;* returned in HL.                                       *
;*********************************************************
pu3:    call    spcwd           ;send a space & get hex word
        xchg                    ;put in de
        ret

;*********************************************************
;* this routine is used to load four characters from     *
;* memory into registers.                                *
;*********************************************************
ilodm:  mov     c,m             ;fetch character to C
        inx     h               ;
        mov     b,m             ;fetch character to B
        inx     h               ;
        mov     e,m             ;fetch character to E
        inx     h               ;
        mov     d,m             ;fetch character to D
        ret

;*********************************************************
;*       ok? (okhuh) - routine to verify operation       *
;*********************************************************
okhuh:  push    psw             ;save psw
        push    h               ; and hl
        lxi     h,m7            ;adr of 'ok?' msg
        call    msg             ;print it
        lxi     h,m8            ;possible abort
        call    chin            ;get answer
        cpi     'Y'             ; 'y' ?
        jnz     illeg1          ;no, go abort
        pop     h               ;restore hl
        pop     psw             ; and psw
        ret                     ;  and leave

;*********************************************************
; delay is a software delay routine. delay time is       *
; controlled by the value in the b-c register pair       *
;*********************************************************
dly:    dcr     c               ;decrement inner loop
        push    h               ;push-pop to
        pop     h               ;  burn up time
        jnz     dly             ;inner loop return
        dcr     b               ;decrement outer loop
        jnz     dly             ;outer loop return
        ret

;*********************************************************
;* routine to output a comma                             *
;*********************************************************
opcom:  push    psw
        mvi     a,comma
        jmp     spac1

;*********************************************************
;* output memory byte addressed by hl                    *
;*********************************************************
opmeby: mov     a,m             ;get mem byte
        jmp     thxb            ;type hex byte & return

;*********************************************************
;* input char from terminal and check for space, "," or  *
;* <cr> entered. carry=1 implies <cr> entered. zero set  *
;* if space or comma entered. not zero if none of the    *
;* three legal delimiters were entered.                  *
;*********************************************************
pchk:   call    chin            ;get a character
p2c:    cpi     ' '             ;space?
        rz                      ;return if so
        cpi     ','             ;a comma?
        rz                      ;return if so
        cpi     cr              ;a cr?
        stc                     ;set carry
        rz                      ;return if cr
        cmc                     ;clear carry if not cr
        ret                     ;return non zero if invalid

;*********************************************************
;* print a slash character.                              *
;*********************************************************
pslash: push    psw             ;
        mvi     a,'/'           ;load a slash
        jmp     spac1           ;cont elsewhere

;*********************************************************
;* print equals sign.                                    *
;*********************************************************
pequ:   push    psw
        mvi     a,'='
        jmp     spac1           ;cont elsewhere

;*********************************************************
;*                 input from I/O port                   *
;*********************************************************
inport: mvi     a,0DBH          ; 'in' opcode
        sta     iosub           ; store the opcode
        mvi     a,0C9H          ; 'ret' opcode
        sta     iosub+2         ; store the opcode
        call    spcby           ; get the port address
        sta     iosub+1         ; store the port address
        call    iosub           ; call the i/o subroutine
        call    sthxb           ; print the contents of the accumulator as two hex digits
        jmp     next            ; go back for the next command

;*********************************************************
;*                 output to I/O port                    *
;*********************************************************
outport:mvi     a,0D3H          ; 'out' opcode
        sta     iosub           ; store the opcode
        mvi     a,0C9H          ; 'ret' opcode
        sta     iosub+2         ; store the opcode
        call    spcby           ; get the port address as two hex digits
        sta     iosub+1         ; store the port address
        call    spcby           ; get value to output into the accumulator as two hex digits
        call    iosub           ; call the i/o subroutine
        jmp     next            ; go back for next command

;*********************************************************
;*       routine to dump a block of memory to tty        *
;*                                                       *
;* this routine will dump a block of memory on the tty   *
;* 16 bytes per line with the address at the start of    *
;* each line.                                            *
;*********************************************************
dump:   call    aparam          ;get parameters
dmret:  call    crlf            ;turn up a new line
        call    thxw            ;type vector address
        call    space           ;space
        push    h               ;keep for ascii dump part
dmnxt:  call    space           ;
        call    opmeby          ;get data and display
        call    hiloex          ;check for all done, h=h+1
        mov     a,l             ;check for mod 16
        ani     15              ; address
        jz      dmasc           ;continue with ascii dump
        jmp     dmnxt           ; continue if not

; add ascii part after hex
; 16 bytes hex followed by 16 characters ascii
dmasc:  call    space           ;space
        call    space
        pop     h               ;get mem location
dmasc1: mov     a,m             ;get byte
        cpi     '~'             ;if > '~' = '.'
        jnc     dmasc3
        cpi     ' '             ;if < ' ' = '.'
        jnc     dmasc2
dmasc3: mvi     a,'.'           ;non printable ascii set '.'
dmasc2: call    type            ;type ascii
        call    hiloex          ;check for all done, h=h+1
        mov     a,l             ;check for mod 16
        ani     15              ; address
        jz      dmret           ;done, new line
        jmp     dmasc1          ; continue if not

;*********************************************************
;*       command 'J' - direct jump to address            *
;*********************************************************
jump:   call    spcwd           ;get hex address
        pchl                    ;then jump to it

;*********************************************************
;*                                                       *
;* command 'M' - move memory block                       *
;*                                                       *
;* will move the block of memory starting at             *
;* XXXX and ending at and including YYYY to the          *
;* block starting at ZZZZ.                               *
;*********************************************************
move:   call    aparam          ;get parameters
        push    h               ;save beg. addr on stack
        call    spcwd             ;get dest. addr. (ZZZZ)
        xthl                    ;de=-YYYY, top=ZZZZ, hl=XXXX
        call    okhuh
mov1:   mov     a,m             ;get thru XXXX
        xthl                    ;hl=ZZZZ, top=XXXX
        call    store           ;checked store
        inx     h               ;bump ZZZZ
        xthl                    ;restore
        call    hiloex          ;check for end
        jmp     mov1            ; and continue

;*********************************************************
;* command 'Z' - zero a block of memory                  *
;*                                                       *
;* will cause memory locations XXXX thru YYYY            *
;* inclusive to be filled with zeros (00 hex).           *
;*********************************************************
zap:    call    aparam          ;get parameters
        xra     a               ;get a zero
        jmp     fill0           ;go fill with zeros

;*********************************************************
;* command 'F' - fill a block of memory with a value     *
;*                                                       *
;* will cause memory locations XXXX thru YYYY            *
;* inclusive to be set to the value VV (hex).            *
;*********************************************************
fill:   call    aparam          ;get parameters
        call    spcby           ;send a space get vv --> 'a'
fill0:  call    okhuh           ;see if ok to proceed
fill1:  call    store           ;stuff it
        call    hiloex          ;see if done
        jmp     fill1           ;  and continue

;*********************************************************
;*       routines to punch or load memory on tty         *
;*                                                       *
;* these routines work with data in the intel hex        *
;* format. the format consists of a record reader.       *
;* up to 16 bytes of data, and a record checksum.        *
;*                                                       *
;* record format:                                        *
;*                                                       *
;* header character ':'                                  *
;* hex-ascii byte count, two charaters                   *
;* hex-ascii load address, four characters hhll          *
;* hex-ascii record type, two charaters 00 for data      *
;*                                                       *
;* data bytes in hex-ascii, two charaters each           *
;*                                                       *
;* hex-ascii checksum, two characters                    *
;*                                                       *
;* the checksum is calculated such that the              *
;* sum of all the two character byte fields              *
;* will be zero.                                         *
;*                                                       *
;* the eof record may contain an execution address       *
;* in the load address field.  the load routine will     *
;* transfer control to this address after reading the    *
;* tape if the address is non-zero.                      *
;*********************************************************

;*********************************************************
;*               Intel hex puncher                       *
;*********************************************************
punhex: call    aparam          ;get parameters
        ;call    pwait           ;type prompt and wait

; hl has low address, de has high address
pn0:    mov     a,l
        adi     16
        mov     c,a
        mov     a,h
        aci     0
        mov     b,a
        mov     a,e
        sub     c
        mov     c,a
        mov     a,d             ;
        sbb     b               ;
        jc      pn1             ;record length = 16
        mvi     a,16            ;
        jmp     pn2             ;
pn1:    mov     a,c             ;last record
        adi     17              ;
pn2:    ora     a               ;
        jz      pdone           ;
        push    d               ;save high
        mov     e,a             ;e=length
        mvi     d,0             ;clear checksum
        call    crlf            ;turn up a new line
        mvi     a,':'           ;punch hdr
        call    type            ;
        mov     a,e             ;
        call    pbyte           ;punch length
        mov     a,h             ;punch block addr
        call    pbyte           ;
        mov     a,l             ;
        call    pbyte           ;
        xra     a               ;
        call    pbyte           ;punch record type
pn3:    mov     a,m             ;get data
        inx     h               ;increment pointer
        call    pbyte           ;punch data
        dcr     e               ;decr count
        jnz     pn3             ;continue
        xra     a               ;calculate
        sub     d               ; checksum
        call    pbyte           ;and punch it
        pop     d               ;restore high address
        jmp     pn0             ;and continue

pbyte:  call    thxb            ;
        add     d               ;add to sum
        mov     d,a             ;
        ret                     ;

pdone:  call    crlf            ;turn up a new line
pdone1: jmp     next            ;back to monitor

; routine to type 'pause' message
; and wait for tty go-ahead
pwait:  push    h               ;save h
        lxi     h,m5            ;prompt
        call    msg             ; message
        pop     h               ;
        call    getch           ;wait for go-ahead
        ret                     ;  and then leave

;*********************************************************
;*               routine to punch eof record             *
;*********************************************************
pend:   call    sghxw           ;type space & get bias or c/r
        jnc     pend1           ; address
        lxi     h,0             ;set 0 address
        cpi     cr              ;check for cr reply
        jnz     illch           ; others illegal
pend1:  ;call    pwait           ;prompt pause
        call    crlf            ;turn up a new line
        mvi     a,':'
        call    type            ;type hdr :
        xra     a
        mov     d,a             ;zero checksum
        call    pbyte           ;and output zero length
        mov     a,h
        call    pbyte           ;execution
        mov     a,l
        call    pbyte           ; address
        mvi     a,1             ;record type
        call    pbyte
        xra     a
        sub     d               ;calculate checksum
        call    pbyte           ; and punch it
        jmp     pdone1

;*********************************************************
;*               Intel hex loader                        *
;*********************************************************
hexin:  call    sghxw           ;type space & get bias or c/r
        jnc     ldq             ;bias address entered
        lxi     h,0             ;bias 0
        cpi     cr              ;check for cr
        jnz     illch           ;others n.g.
        push    h
        lxi     h,m74
        call    msg
        pop     h

ldq:    push    h               ;save bias
        xra     a               ;kill
        sta     echo            ; tty0 echo

ld0:    pop     h               ;get bias
        push    h               ;and restore
        call    ascin           ;get input
        mvi     b,':'
        sub     b               ;check for record mark
        jnz     ld0

        mov     d,a             ;clear checksum
        call    byte            ;get length
        jz      ld2             ;zero all done

        mov     e,a             ;save length
        call    byte            ;get high address
        push    psw             ; and save
        call    byte            ;get low address
        pop     b               ;fetch msbyte
        mov     c,a             ;bc has address
        push    b               ;save vector
        xthl                    ; to hl
        shld    blkad           ;save block address
        xthl                    ; in case of error
        pop     b               ;restore
        dad     b               ;add to bias
        call    byte            ;get type
ld1:    call    byte            ;get data
        call    store
        inx     h
        dcr     e
        jnz     ld1             ;continue
        call    byte            ;get checksum
        jnz     error           ; jsl
        mvi     a,'.'           ; jsl
        call    putch           ; jsl
        jmp     ld0             ; jsl

; error...
; this is an error exit routine
; control returned to the monitor.
error:  lxi     h,m6            ;get the error message addr
        call    crlfmg          ;send it
        lhld    blkad           ;get block addr
        call    thxw            ; and type it
        jmp     next            ;back to user

ld2:    call    byte            ;get msb of xeqad
        mov     h,a
        call    byte
        mov     l,a
        ora     h
        jz      next            ;mon if no xeqad
        pchl                    ;go to routine

byte:   call    ghxb            ;get two characters
        mov     c,a             ;save a in c
        add     d               ;add checksum
        mov     d,a             ;new checksum to d
        mov     a,c             ;restore a
        ret                     ;return

;***********************************************************
;* command 'T' - all bit patterns memory test              *
;* this test is exhaustive, writing all bit pattern        *
;* combinations in each of the addresses.                  *
;* an error free check of 4k of ram takes about 30 seconds.*
;***********************************************************
test2:  xra     a               ;want hl=beg, de=end
        sta     errfl           ;error indicator flag
        call    param           ;get parameters
        call    okhuh           ;get the go ahead
test21: mvi     b,0FFH          ;init. test pattern
test22: mov     m,b             ;write the pattern
        mov     a,m             ;read it back
        cmp     b               ;compair with original
        cnz     tsterr          ;if error, go display it
        ;call    check           ;see if we have interupt
        dcr     b               ;decrement pattern
        mov     a,b             ;for end of pattern check
        cpi     0FFH            ;thru this location ?
        jnz     test22          ;loop back if not done
        call    hilow           ;inc hl/see if test done
        jnc     test21          ;branch if not done
        jmp     tstext          ;all tests exit here

; memory test error subroutine
;  upon entry:
;       hl = addr of error
;        a = read byte
;        b = write byte
tsterr: push    b               ;save bc
        mov     b,a             ;save a
        lda     errfl           ;get error flag
        ora     a               ;to set zero flag
        mov     a,b             ;restore a
        pop     b               ;  and bc
        jnz     te1             ;don't halt if <> 1st error
        push    psw             ;save a
        mvi     a,1             ;set error flag to non-zero
        sta     errfl           ;
        pop     psw             ;restore a
        call    crlf            ;turn up a new line
te1:    push    h               ;save hl
        lxi     h,m9            ;get message addr
        call    crlfmg          ;send msg
        pop     h               ;restore hl
        call    thxw            ;type addr of bad check
        call    space           ;space over once
        push    psw             ;save bad reading
        mov     a,b             ;get ref byte
        call    thxb            ;convert to hex and print it
        call    space           ;space again
        pop     psw             ;get bad reading back
        call    thxb            ;convert and print it
        ret                     ;return

; all memory test routines exit here. if no errors were
; discovered, a 'TEST OK' message is printed.
tstext: lda     errfl           ;get error flag
        ora     a               ;set zero flag for testing
        jnz     next            ;return if errors were found
        lxi     h,m10           ;get perfect test msg
        call    msg             ;send it
        jmp     next            ;return to user

;*********************************************************
;* loads memory from 8080 assembly language symbolic     *
;* inputs.                                               *
;*                                                       *
;* in addition to standard 8080 mneumonics, several      *
;* macro commands are available which reference          *
;* monitor utility routines:                             *
;*                                                       *
;* LST - provides disassembly listing of code entered    *
;*       so far.                                         *
;*                                                       *
;* RUN - jumps to entered address or last 'org' in this  *
;*       assembly. type space & cr or addr.              *
;*                                                       *
;* BYT - enter bytes mode                                *
;*                                                       *
;* ORG - new start address for assembler                 *
;*                                                       *
;* ASC - enter ascii mode (exit with ^z)                 *
;*                                                       *
;* MSG - type out msg at given location                  *
;*       (lxi h,adr  call msg)                           *
;*                                                       *
;* TTI - char. input                     (call ascin)    *
;*                                                       *
;* TTE - char. input/echo                (call chin)     *
;*                                                       *
;* TTO - char. output                    (call type)     *
;*                                                       *
;* BIN - binary input                    (call getch)    *
;*                                                       *
;* LBY - list hex byte                   (call lstby)    *
;*                                                       *
;* LWD - list hex word                   (call thxw)     *
;*                                                       *
;* IPW - input hex word                  (call ghxw)     *
;*                                                       *
;* NWL - new line macro                  (call crlf)     *
;*                                                       *
;* SPC - space macro                     (call space)    *
;*                                                       *
;* DLY - delay macro                     (call dly)      *
;*                                                       *
;*********************************************************
losym:  call    ghxw            ;get starting addr to hl
        jnc     losym1          ;skip down if addr ok
        cpi     cr              ;c/r enetered ?
        jz      as0             ;yes, open previous addr
        jmp     illch           ;no, gripe and exit

losym1: shld    preadr          ;save start address
losym3: shld    clp             ;hl to clp
as0:    xra     a               ;clear a-reg and use
        sta     adflag          ; to clear assembler/dissassembler flag
        sta     exflag          ;  and exception flag
        dcx     h               ;get right address for listing
        shld    finclp          ;save as final address for dmpsym
        inx     h               ;restore h
        call    opclp           ;o/p cr/lf, clp, colon, and tab
as1:    lxi     sp,stack        ;reset stack
        call    ipnabt          ;i/p nonblank, first letter
        jz      as1
        cpi     cr              ;exit losym command ?
        jz      reset           ;exit if so
        ani     3fh             ;mask
        mov     c,a             ;store in c
        call    ipnabt          ;i/p alphabetic, second letter
        jnc     err             ;illegal entry, type ?? and start over
        ani     3fh             ;else mask character
        mov     d,a             ;save 2nd char in d
        call    ipnabt          ;i/p third character
        jnc     set2lt          ;jump if only 2 characters
        ani     3fh             ;mask 3rd character
        mov     e,a             ;save 3rd char in e
        call    ipnabt          ;i/p 4th char if there is one
        jnc     setbct          ;jump if 3 characters
        call    ipnabt          ;
        jc      err             ;
        lxi     h,fourtbl       ;
        mvi     b,11            ;
sw4lt:  call    losymb          ;
        inx     h               ;
        jnz     sw4lt           ;
err:    lxi     h,m3            ;point to ?? message
        call    crlfmg          ;send the message
        jmp     as0             ;

; check for macro instruction (psuedo operation)
setbct: lxi     h,psopr         ;point to the psuedo op table
        mvi     b,16            ;<<threetbl-psopr>/5> length of pseudo opr table/5
trimat: call    exgcmd          ;
        inx     h               ;
        jnz     trimat          ;
        mvi     b,35h           ;
tmat2:  call    tmat3           ;
        jnz     tmat2           ;
        jmp     err             ;

set2lt: lxi     h,twotbl        ;
        mvi     b,0fh           ;
        mov     e,d             ;
        mov     d,c             ;
twomat: call    let2ma          ;
        jnz     twomat          ;
        jmp     err             ;
;
exgcmd: call    mskihl          ;
        mov     e,m             ;match found, load cmd address
        inx     h               ;and execute the command
        mov     d,m             ;
        xchg                    ;
        pchl                    ;branch to command routine

tmat3:  call    mskihl          ;
rstest: mov     a,m             ;
        cpi     0FFH            ;
        jnz     ldcx1           ;
        call    inpwd           ;input a hex word
        mov     a,l             ;
        ori     0c7h            ;
        mov     e,a             ;
nodata: call    hlclp2          ;
        jmp     as0             ;

let2ma: call    go2ltr          ;
        jmp     ldcx2           ;
;
mskihl: call    mask            ;
        cmp     c               ;
        jnz     linx            ;
go2ltr: call    mask            ;
        cmp     d               ;
        jnz     linx1           ;
        call    mask            ;
        cmp     e               ;
        jnz     linx2           ;
        mov     e,m             ;
        ret                     ;

linx:   inx     h
linx1:  inx     h
linx2:  inx     h
        inx     sp
        inx     sp
        dcr     b
        ret

mask:   mvi     a,3fh
        ana     m
        inx     h
        ret

ipnabt: call    ascin           ;input ascii char
        cpi     '-'             ;minus is back-up (was rubout)
        jnz     ttyio0          ;if so back up pointer
        call    decclp          ;
        jmp     as0             ;

ttyio0: cpi     lf              ;line feed?
        jz      ibls            ;if so increment pointer
        call    type
        cpi     ' '
        push    b
        mov     b,a
        rlc
        rlc
        mov     a,b
        pop     b
        ret

losymb: call    mskihl
        inx     h
        mov     e,m
        dcx     h
ldcx1:  dcx     h
ldcx2:  dcx     h
        dcx     h
        mov     a,m
        ani     0c0h
        rlc
        rlc
        inx     h
        jz      hl2ipb
        dcr     a
        jz      dstarg
        dcr     a
        jz      ipdest
onearg: call    matarg
        ana     e
        mov     e,a
        jmp     waitna

desub:  inx     h
        call    matarg
        mov     b,a
        mov     a,m
        ani     40h
        mov     a,b
        jz      lrlc
        ori     1
lrlc:   rlc
        rlc
        rlc
        ana     e
        mov     e,a
        dcx     h
        ret

ipdest: call    desub
souarg: call    ipnabt
        jc      souarg
        jmp     onearg

dstarg: call    desub
waitna: call    ipnabt
        jc      waitna
hl2ipb: mov     a,m
        ani     0c0h
        jz      nodata
        rlc
        jnc     dat1by
        call    by2buf
        mov     m,d
        call    incclp
iblsby: mov     m,c
ibls:   call    incclp
        jmp     as0

by2buf: call    inpwd           ;input a hex word
        mov     c,h
        mov     d,l
hlclp2: lhld    clp
        mov     m,e
        jmp     incclp

dat1by: call    inpwd           ;input a hex word
        mov     c,l
        call    hlclp2
        jmp     iblsby

matarg: call    ipnabt
        jnc     err             ;*****register error?
        push    b
        lxi     b,regtbl
        call    swarta
        inx     b
        ldax    b
        pop     b
        ret

;********************************************************
;* used in 'losym' to load ascii characters with the    *
;* 'asc' macro exit ascii mode with a control-z (^z)    *
;* puts a zero at the end of the message entered        *
;********************************************************
ascii:  lhld    clp             ;get clp
        call    crlf            ;turn up a new line
asci1:  call    ascin           ;get character
        cpi     7fh             ;rubout?
        jz      asci3           ;back up if so
        cpi     ctrlz           ;control-z? (ends ascii input)
        jz      asci4           ;
        mov     m,a             ;store ascii in memory
        inx     h               ;increment pointer
asci2:  call    type            ;type the character
        jmp     asci1           ;next character
;
; ctrl-h backs up the pointer on conrac units
asci3:  mvi     a,ctrlh         ;back up the cursor
        call    chrspc          ; and space over the character there
        mvi     a,ctrlh         ;set up to back up again
        dcx     h               ;back up clp
        jmp     asci2           ;go back up and continue
;
asci4:  mvi     m,0             ;store a zero for eom
asci5:  inx     h               ;increment clp
asci6:  shld    clp             ;store clp
losym0: lhld    clp             ;get clp
        jmp     losym3          ;back to assembler

; following routines are used by assembler macro instructions
; they reference utility routines thru worm holes to
; ensure operability of those routines if monitor is
; relocated.
; if additional macros are defined, be sure to change
; the table length in 'setbct'.
prtmg:  lhld    clp
        mvi     m,21h           ;lxi h, instruction
        inx     h
        xchg                    ;save hl in de
        call    ghxw            ;get message adr to hl
        xchg                    ;pntr back to hl, msg addr to de
        mov     m,e             ;
        inx     h
        mov     m,d             ;message address loaded
        inx     h
        lxi     d,msg           ;get message subroutine adr
        shld    clp             ;save program adr in clp
ldcall: lhld    clp             ;get clp
        mvi     m,0cdh          ;store call message
        inx     h
        mov     m,e
        inx     h
        mov     m,d             ;message addr saved
        jmp     asci5
;
mtto:   lxi     d,type          ;load console output call
        jmp     ldcall
;
mtti:   lxi     d,ascin         ;load call terminal input macro
        jmp     ldcall
;
mtte:   lxi     d,chin          ;load call terminal input/echo macro
        jmp     ldcall
;
mbin:   lxi     d,getch         ;load call terminal input macro
        jmp     ldcall
;
mlby:   lxi     d,thxb          ;load list byte call
        jmp     ldcall
;
mlwd:   lxi     d,thxw          ;load list word call
        jmp     ldcall
;
mipw:   lxi     d,ghxw          ;load input word call
        jmp     ldcall
;
mnwl:   lxi     d,crlf          ;new line macro
        jmp     ldcall
;
mspc:   lxi     d,space         ;macro print a space char
        jmp     ldcall
;
mdly:   lxi     d,dly           ;macro long delay
        jmp     ldcall
;
mrun:   call    ghxw            ;get addr or cr
        jnc     mrun1           ;skip if not a cr
        cpi     cr              ;cr entered?
        jnz     illeg           ;illegal if not
        lhld    preadr          ;on cr, get addr of last org
mrun1:  pchl                    ;execute the addr

;*********************************************************
;* dump symbolically the contents of memory command      *
;* (disassembles)                                        *
;* clp = current pointer                                 *
;*********************************************************
dmpsym: call    ghxw            ;get starting addr to hl
        jnc     dsym0           ;skip down if addr ok
        cpi     cr              ;was cr entered
        jz      dsym1           ;send previous dump is so
        jmp     illch           ;go gripe about illegal address

dsym0:  shld    clp             ;start address to clp
        shld    preadr          ;save as new init address
        call    spcwd             ;get another address
        shld    finclp          ;store final address
        jmp     next0           ;begin dump

dsym1:  lhld    preadr          ;get previous init address
        shld    clp             ;save initial address in clp
next0:  call    lmnum
xnext:  lxi     sp,stack        ;reset stack in case byte was found
        call    endchk          ;check for end of block?
        jmp     next0

lmnum:  xra     a               ;clear exception flag
        sta     exflag
        mvi     a,'D'           ;set assembler/disassembler flag
        sta     adflag          ;
        call    opclp           ;output current address
        call    rstst           ;call restart and mask test
next1:  call    except          ;call exception test
        call    ptmneu          ;output mneumonic
        call    space           ;print a space
        lda     augflag         ;arguement flag to acc
        ana     a               ;test arguement flag
        jz      skipco          ;no arguement? then skip comma and arg print routine
        call    prtarg          ;else print arguement
        lda     dbflag          ;get data bytes flag
        ana     a               ;test data bytes flag
        rz                      ;no data bytes - go get next instruction
        call    opcom           ;else print a comma
skipco: lda     dbflag          ;get data bytes flag
        ana     a               ;test data bytes flag
        rz                      ;no data bytes? go get next instruction
        call    incclp          ;increment clp and return new clp to hl
        dcr     a               ;check for one or two data bytes
        jz      opmeby          ;db flag = 1 therefore output one byte
                                ;else output two db's
        mov     e,m             ;memory at clp to e register
        call    incclp          ;increment clp and return clp to hl
        mov     d,m             ;memory at clp to d register
        lxi     h,tabend        ;point to shld,lhld,sta,lxi,and lda table
skip1:  inx     h               ;point to instruction
        lda     lastopc         ;get current opc
        cmp     m               ;is it in table
        mov     a,m             ;save byte in table in 'a'
        xchg
        jz      thxw            ;yes, just list the word, no breakpoint
        xchg
        dcr     a               ;see if end of table, last byte is a 1
        jnz     skip1           ;no, get next one
        xchg                    ;de <--> hl
        shld    opcadr          ;save address aug. for debug
        jmp     thxw            ;print the 2 db's as an address & return

opclp:  lhld    clp             ;clp to hl
opadr:  call    crlf            ;turn up a new line
        call    thxw            ;print clp
pcol:   mvi     a,':'           ; with a colon following
        jmp     chrspc          ;type char, space & return

rstst:  mov     a,m             ;restart and match test
        ani     0c7h
        cpi     0c7h
        jnz     match
        push    h               ;save hl
        mov     a,m             ;get rst instruction
        ani     38h             ;mask call address
        mov     l,a             ;save in hl
        mvi     h,0
        shld    opcadr          ;save bp2 register
        pop     h
        lxi     d,tabend
        mvi     a,3
        sta     matchflag
        rar
        sta     augflag
        ret

match:  lxi     d,twotbl        ;match test
        mvi     a,2
mat0:   sta     matchflag
        mov     b,a
        call    argtst
        mov     a,b
        add     e
        mov     e,a
        jnc     mat1
        inr     d
mat1:   ldax    d
        cmp     c
        rz
        call    mat2
        mov     a,b
        jmp     mat0

mat2:   cpi     0f3h
        jz      setf24
        cpi     0e9h
        jz      setf23
        cpi     0FFH
        jz      tf24
        cpi     0e3h
        inx     d
        rnz
pdb:    lda     adflag          ;get assembler/disassembler flag
        ora     a               ;test flag
        jz      err             ;its assembler error
        call    type            ;else its data byte under disassember
        mvi     a,'B'
        call    chrspc
        lhld    clp
        call    opmeby
        jmp     xnext

tf24:   inr     b
        lxi     d,fourtbl
        ret

setf23: dcr     b
        lxi     d,threetbl
        ret

setf24: inr     b
        inr     b
        lxi     d,fourt1
        ret

argtst: ldax    d               ;arguement test
        rlc
        rlc
        ani     3
        sta     augflag
        jnz     arg0
        mov     c,m
        ret

arg0:   dcr     a
        jnz     s12arg
        mov     a,m
        ori     38h
        mov     c,a
        ret

s12arg: dcr     a
        jnz     oargs
        mov     a,m
        ori     3fh
        mov     c,a
        ret

oargs:  mov     a,m
        ori     7
        mov     c,a
        ret

except: adi     0c5h            ;exception test
        jz      noexcp
        inr     a
        jz      noexcp
        inr     a
        jnz     hltest
noexcp: mov     a,m
        cpi     3ah
        rz
        ani     08h
        rz
        mvi     a,0f7h
        sta     exflag
noex1:  lda     matchflag
        add     e
        mov     e,a
        jnc     noex2
        inr     d
noex2:  inx     d
        ret

hltest: mov     a,m             ;get character
        cpi     76h             ;hlt instruction?
        rnz                     ;return if not
        sub     a
        sta     augflag
        jmp     noex1

ptmneu: call    goto1l
        lda     matchflag
        mov     c,a
        call    print
        inx     d
        dcr     c
        ldax    d
        ani     0c0h
        rlc
        rlc
        sta     dbflag
        call    print
ptm0:   dcr     c
        rz
        inx     d
        call    print
        jmp     ptm0

goto1l: lda     matchflag
g0:     dcx     d
        dcr     a
        jnz     g0
        ret

print:  ldax    d
        ani     3fh
        ori     40h
opcall: call    type
        dcr     b
        ret

prtarg: dcr     a               ;print arguement subroutine
        jz      de1arg
        dcr     a
        jz      pt2arg
prtar1: mov     a,m
        ori     0f8h
prtar2: push    b
        lxi     b,regtbl+1
        call    swarta
        dcx     b
        ldax    b
        pop     b
        jmp     opcall

de1arg: mov     a,m
        ani     0c7h
        cpi     0c7h
        jnz     sptest
        mov     a,m
        ani     38h
        jmp     thxb

pt2arg: call    sptest
        call    opcom
        dcr     b
        jmp     prtar1

swarta: push    d
        mvi     e,0ah
        mov     d,a
cmpbys: ldax    b
        cmp     d
        jnz     ldcre
        pop     d
        ret

prtsp:  lxi     h,m25           ;point to 'sp' msg
        jmp     msg             ;print it & return

sptest: mov     a,m
        ani     0f5h
        cpi     31h
        jz      prtsp
        ani     0f1h
        cpi     0f1h
        jnz     spt1
prtpsw: lxi     h,pswmg
        jmp     msg

spt1:   mov     c,m
        lda     exflag
        cpi     0f7h
        jnz     genarg
        ana     c
        mov     c,a
genarg: mvi     a,38h
        ana     c
        rrc
        rrc
        rrc
        ori     0f8h
        jmp     prtar2

ldcre:  dcr     e
        jz      pdb
        inx     b
        inx     b
        jmp     cmpbys

endchk: lhld    finclp
        xchg
        lhld    clp
        call    hilow
        shld    clp

        jc      reset
        ret

incclp: lhld    clp
inhlsp: inx     h               ;increment hl and save in clp
inc0:   shld    clp
        ret

decclp: lhld    clp
        dcx     h
        jmp     inc0

; 'BYT' is a macro command used by lodsym
byt:    call    incclp          ;get clp, increment it and resave
byt1:   dcx     h
        call    opadr
        call    opmeby
        call    pslash
        call    ascin           ;input ascii
        cpi     '-'             ;back up pointer ? (was rubout)
        jz      byt1            ;if so back up pointer
        cpi     lf              ;line feed?
        jz      byt2            ;if so advance pointer
        call    p2c             ;check delimiter
        jc      asci6           ;exited to reset before
        jz      byt2
        call    type
        call    ghxb            ;get hex byte
        mov     m,a             ;store it

; do we need jc exit for non-hex byte???
        cpi     cr
        jz      asci6           ;exited to reset before
byt2:   inx     h
        shld    clp             ;save as clp
        jmp     byt

;*********************************************************
;* lists addresses of all occurences of ZZ between       *
;* addresses XXXX and YYYY.                              *
;*                                                       *
;* space can be substituted for <cr> at end of line if   *
;* in symbolic mode.                                     *
;*********************************************************
srchs:  call    ghxw            ;start addr to HL
        jc      illch           ;gripe & exit if illegal
        xchg                    ;put addr in DE
        call    pu3             ;start to HL, end to de
        call    sghxb           ;send space, get desired byte
        jc      illch           ;gripe & exit if bad byte
        mov     c,a             ;put byte in 'C'
ver1:   mov     a,m             ;get memory byte
        cmp     c               ;compare to C
        jnz     ver4            ;no match?
ver3:   shld    clp             ;save found addr in clp
        push    h
        push    d
        push    b               ;save all status
        call    lmnum           ;print the db symbolically
        pop     b
        pop     d
        pop     h
ver4:   call    hiloex          ;increment pointer
        jmp     ver1            ;look again

; the following search routine prints only the addresses
; of occurance.
srchb:  call    aparam          ;get parameters
        call    sghxb           ;send space, get byte to find
        mov     b,a             ;save copy in B
        call    crlf            ;turn up a new line
vfb1:   mov     a,m             ;read a byte
        cmp     b               ;compare with ref
        jnz     vfb2            ;skip down if no match
        call    sthxw           ;space vice CRLF to save paper...then type address
vfb2:   call    hiloex          ;incr pntr & check if done
        jmp     vfb1            ;keep searching if not done

;*********************************************************
;* examine and modify cpu registers command...           *
;*                                                       *
;* space after a register value will cause next register *
;* to be displayed. <cr> will end the register command.  *
;*********************************************************
regx:   call    dspreg
        jmp     reset

;*********************************************************
;*               modify registers                        *
;*********************************************************
modreg: lxi     h,actbl         ;point to start of table
        call    pchk            ;input character
        jc      errore          ;can't be CR
x0:     cmp     m               ;check against table
        jz      x1              ;jump if a match
        push    psw
        mov     a,m             ;get table entry
        ora     a               ;set flags
        jm      errore          ;exit if end of table without match
        inx     h               ;point to next
        inx     h               ; table entry
        inx     h
        pop     psw
        jmp     x0              ;check next entry

x1:     call    crlf            ;turn up a new line
        mov     a,m             ;get reg from table
        call    chrspc          ;print char and a space
        mvi     a,'='           ;print equals and
        call    chrspc          ;a space.
        inx     h
        mov     a,m             ;get reg location pointer
        xchg                    ;save hl
        mov     l,a
        mvi     h,0
        dad     sp
        xchg                    ;get hl back
        inx     h
        mov     b,m             ;get #bytes per register
        inx     h
        ldax    d
        dcr     b
        jz      x2              ;print  8 bit reg
        call    ds3             ;print 16 bit reg
        jmp     x3

x2:     call    thxb            ;print hex byte
x3:     inr     b
        call    pslash          ;print slash
        call    ghxb            ;get a hex byte
        jnc     x31             ;skip down if valid hex
        cpi     ' '             ;space entered?
        jz      x5              ;open next loc if so
        jmp     reset           ;exit if CR, etc.

x31:    push    b
        dcr     b               ;check if all 2 or 4 bytes gotten
        jz      x32             ;jmp if all bytes entered already
        push    psw             ;save 1st byte entered
        call    ghxb            ;need a 2nd byte
        jc      reset           ;exit if bad entry
        stax    d
        inx     d
        pop     psw             ;restore 1st byte
x32:    stax    d
x4:     pop     b
x5:     mov     a,m             ;get table entry
        ora     a               ;set flags
        jm      x6              ;start at beginning if at end of table
        mov     a,b             ;get character typed
        cpi     cr              ;is it a CR?
        jz      reset           ;exit if so
        jmp     x1              ;next register
x6:     lxi     h,actbl         ;set at start of table
        call    crlf            ;print a blank line
        jmp     x1              ;next line

;*********************************************************
;*               display registers                       *
;*********************************************************
dspreg: lxi     h,actbl         ;full register display
        call    crlf
        jmp     ds1+3

ds1:    call    space
        mov     a,m
        inx     h
        ora     a
        rm                      ;return
        call    type
        call    pequ
        mov     e,m
        inx     h
        push    h
        mvi     d,0
        lxi     h,stack
        dad     d
        xchg
        pop     h
        mov     b,m
        inx     h
        dcr     b
        ldax    d
        jz      ds2
        call    ds3
        jmp     ds1

ds2:    call    thxb
        jmp     ds1

ds3:    push    h               ;it's double precision, display as address
        ldax    d
        mov     h,a
        dcx     d
        ldax    d
        mov     l,a
        call    thxw
        pop     h
        ret

quit:   jmp     8000H

;*********************************************************
;*       display help                                    *
;*********************************************************
help:   lxi     h,m71           ;type help
        call    crlfmg          ;message
        jmp     reset           ;continue elsewhere

;*********************************************************
;*               monitor I/O routines                    *
;*********************************************************
putch:  push psw
putch1: in acia_status
        rrc
        rrc
        jnc  putch1
        pop psw
        out acia_data           ;output the character to the transmit register
        ret

getch:  in acia_status
        rrc
        jnc getch               ; go back and try again if the receive register is empty
        in acia_data            ; input the character from the receive register
        cpi 'a'
        rc                      ; return if already upper case
        sui 20h                 ; else, convert to upper case
        ret

poll:   in acia_status
        ani 00000001b           ; check for receiver buffer ready
        ret

; routine to check for software interupt
check:  call    poll            ;check for input data
        rz                      ;return if none

; ascii input routine
ascin:  call    getch           ;get a character
        ani     7fh             ;strip off parity
        push    psw             ;save it temporarily
        call    check2          ;check for special characters
        pop     psw             ;restore char
        ret                     ;return

check2: cpi     ctrls           ;check for ctrl-s halt
        jz      hang            ;go hang up if so
        cpi     ctrlc           ;ctrl-C abort?
        jz      abort           ;abort if so
        cpi     ctrlo           ;suppress printing?
        rnz
        lda     lstsupflag      ;get list suppression flag
        inr     a               ;reverse its state
        sta     lstsupflag      ;store it again
        ret                     ;return

; hangs up here when control-s is typed to suppress output
; only an abort command or control-s will continue output
hang:   call    getch           ;get character
        ani     7fh             ;strip parity
        cpi     ctrlc           ;ctrl-C abort?
        jz      abort           ;abort if yes
        cpi     ctrls           ;another ctrl-S?
        jnz     hang            ;continue hanging if not
        ret                     ;back to normal

abort:  mvi     a,5eh           ;('^') echo the ctrl-C
        call    type            ; with a
        mvi     a,'C'           ;  ^C
        call    type            ;
        xra     a               ;get a zero
        sta     tmpa            ;
        jmp     reset           ;continue below

; error...
; this is an error exit routine. the stack is reinitialized and
; control returned to the monitor.
; need to combine with illch, move 'reset' up below 'abort'
; and general clean-up.
errore: lxi     h,m3
rescon: call    msg             ;output crlf the message

reset:  xra     a               ;clear a
        sta     lstsupflag      ;clear list suppression flag
        jmp     next            ;return to user

; pan...
; punch a null character
pan:    xra     a               ;load a with a null

; routine to type a character
; calling sequence
;       lda     char            ;character in 'A' register
;       call    type            ;type it
;       ....                    ;returns here
type:   push    psw             ;save contents of 'A'
        call    check           ;check for abort first
        lda     lstsupflag      ;get list suppression flag
        rrc                     ;to check bit 0
        jnc     type0           ;jump if not suppressed
        pop     psw             ;restore a
        ret                     ;and return

type0:  pop     psw             ;get char back
        call    putch           ;send the character
        cpi     lf              ;was it a line feed
        rnz                     ;return if not
        push    b               ;save bc
        push    psw             ;and the character
        mvi     b,2             ;number of fills to b
        xra     a               ;get fill character
type1:  call    putch           ;send a fill character
        dcr     b               ;decrement counter
        jnz     type1           ;loop back if not done
type2:  pop     psw             ;restore a and flags
        pop     b               ;restore b
        ret                     ;return

; exit code template
; restores machine state and returns to program execution.
exitc:  pop     d               ;restore registers
        pop     b
        pop     psw
        pop     h
        sphl                    ;set stack pointer
        ei                      ;enable interupts
        lxi     h,0             ;zero HL

hlx     equ      $-2
        jmp     0

pcx     equ      $-2

t1a:    dw       0ffffh
        db       0
        dw       0ffffh
        db       0

; displacement of register location from sp (level 0)
endx:
aloc    equ      5
bloc    equ      3
cloc    equ      2
dloc    equ      1
eloc    equ      0
floc    equ      4
hloc    equ      hlx-exitc+11q
lloc    equ      hlx-exitc+10q
ploc    equ      pcx-exitc+11q
sloc    equ      7
tloc    equ      t1a-exitc+10q

; table for accessing registers
actbl:
        db       'A',aloc,1
        db       'B',bloc,1
        db       'C',cloc,1
        db       'D',dloc,1
        db       'E',eloc,1
        db       'F',floc,1
        db       'H',hloc,1
        db       'L',lloc,1
        db       'M',hloc,2
        db       'P',ploc,2
        db       'S',sloc,2
        db       0FFH

; assem/dissem look-up tables
fourtbl:
        db       03H,81H,8CH,0CH,0CDH    ;CALL
        db       50H,15H,93H,08H,0FDH    ;PUSH
        db       18H,03H,88H,07H,0EBH    ;XCHG
        db       13H,88H,8CH,04H,22H     ;SHLD
        db       0CH,88H,8CH,04H,2AH     ;LHLD
        db       53H,14H,81H,18H,3AH     ;STAX
        db       4CH,04H,0C1H,18H,3AH    ;LDAX
        db       18H,03H,88H,07H,0EBH    ;XCHG
        db       18H,14H,88H,0CH,0E3H    ;XTHL
fourt1: db       13H,10H,88H,0CH,0F9H    ;SPHL
        db       10H,03H,88H,0CH,0E9H    ;PCHL

twotbl: db       0AH,83H,0DAH            ;JC    two letter look up table
        db       0AH,9AH,0CAH            ;JZ
        db       0AH,90H,0F2H            ;JP
        db       0AH,8DH,0FAH            ;JM
        db       03H,83H,0DCH            ;CC
        db       03H,9AH,0CCH            ;CZ
        db       03H,90H,0F4H            ;CP
        db       03H,8DH,0FCH            ;CM
        db       12H,03H,0D8H            ;RC
        db       12H,1AH,0C8H            ;RZ
        db       12H,10H,0F0H            ;RP
        db       12H,0DH,0F8H            ;RM
        db       09H,4EH,0dbH            ;IN
        db       05H,09H,0FBH            ;EI
        db       04H,09H,0F3H            ;DI

regtbl: db       'B',0F8H,'C',0F9H       ;register look up table
        db       'D',0FAH,'E',0FBH
        db       'H',0FCH,'L',0FDH,'M',0FEH
        db       'S',0FEH,'P',0FEH,'A',0FFH

; command branch table for assembler
; note: if any macros are added to or deleted from this table,
;       change the table length in 'setbct'
psopr:  db       "LST"           ;list code so far
        dw       dsym1
        db       "RUN"           ;jump to address
        dw       mrun
        db       "BYT"           ;enter bytes mode
        dw       byt
        db       "ORG"           ;new start address for assembler
        dw       losym
        db       "ASC"           ;enter ascii input mode
        dw       ascii
        db       "PRT"           ;print macro, call message etc
        dw       prtmg
        db       "TTI"           ;console input macro
        dw       mtti
        db       "TTO"           ;console output macro
        dw       mtto
        db       "TTE"           ;console input/echo macro
        dw       mtte
        db       "BIN"           ;binary input macro
        dw       mbin
        db       "LBY"           ;list byte macro
        dw       mlby
        db       "LWD"           ;list word macro
        dw       mlwd
        db       "IPW"           ;input word macro
        dw       mipw
        db       "NWL"           ;new line macro
        dw       mnwl
        db       "SPC"           ;macro print a space char
        dw       mspc
        db       "DLY"           ;macro long delay
        dw       mdly

; assem/dissem look-up tables
threetbl:
        db       03h,0dh,03h,3fh         ;CMC
        db       8dh,0fh,16h,7fh         ;MOV
        db       08h,0ch,14h,76h         ;HLT
        db       4dh,56h,09h,3eh         ;MVI
        db       49h,0eh,12h,3ch         ;INR
        db       44h,03h,12h,3dh         ;DCR
        db       0c1h,04h,04h,87h        ;ADD
        db       0c1h,04h,03h,8fh        ;ADC
        db       0d3h,15h,02h,97h        ;SUB
        db       0d3h,02h,02h,9fh        ;SBB
        db       0c1h,0eh,01h,0a7h       ;ANA
        db       0d8h,12h,01h,0afh       ;XRA
        db       0cfh,12h,01h,0b7h       ;ORA
        db       0c3h,0dh,10h,0bfh       ;CMP
        db       01h,44h,09h,0c6h        ;ADI
        db       01h,43h,09h,0ceh        ;ACI
        db       13h,55h,09h,0d6h        ;SUI
        db       13h,42h,09h,0deh        ;SBI
        db       01h,4eh,09h,0e6h        ;ANI
        db       18h,52h,09h,0eeh        ;XRI
        db       0fh,52h,09h,0f6h        ;ORI
        db       03h,50h,09h,0feh        ;CPI
        db       12h,0ch,03h,07h         ;RLC
        db       12h,12h,03h,0fh         ;RRC
        db       12h,01h,0ch,17h         ;RAL
        db       12h,01h,12h,1fh         ;RAR
        db       0ah,8dh,10h,0c3h        ;JMP
        db       0ah,8eh,03h,0d2h        ;JNC
        db       0ah,8eh,1ah,0c2h        ;JNZ
        db       0ah,90h,05h,0eah        ;JPE
        db       0ah,90h,0fh,0e2h        ;JPO
        db       03h,8eh,03h,0d4h        ;CNC
        db       03h,8eh,1ah,0c4h        ;CNZ
        db       03h,90h,05h,0ech        ;CPE
        db       03h,90h,0fh,0e4h        ;CPO
        db       12h,05h,14h,0c9h        ;RET
        db       12h,0eh,03h,0d0h        ;RNC
        db       12h,0eh,1ah,0c0h        ;RNZ
        db       12h,10h,05h,0e8h        ;RPE
        db       12h,10h,0fh,0e0h        ;RPO
        db       0fh,55h,14h,0d3h        ;OUT
        db       0ch,84h,01h,3ah         ;LDA
        db       50h,0fh,10h,0f9h        ;POP
        db       13h,94h,01h,32h         ;STA
        db       4ch,98h,09h,39h         ;LXI
        db       44h,01h,44h,39h         ;DAD
        db       49h,0eh,18h,3bh         ;INX
        db       44h,03h,58h,3bh         ;DCX
        db       03h,0dh,01h,2fh         ;CMA
        db       13h,14h,03h,37h         ;STC
        db       04h,01h,01h,27h         ;DAA
        db       0eh,0fh,10h,00h         ;NOP
        db       52h,13h,14h             ;RST
tabend: db       0FFH

; table containing codes for SHLD LHLD STA LDA LXI
        db       22h,2ah,32h,3ah,39h,21h,31h,11h,01h
        db       0

;*********************************************************
;*                                                       *
;*               system messages                         *
;*                                                       *
;*********************************************************
m0:     db       cr,lf,lf,"8080/8085 System Monitor",cr,lf,lf
        db       "Type \"H\" or \"?\" for help with commands.",lf+80h
m2:     db       " is undefine",'d'+80h
m3:     db       " ?",'?'+80h
m4:     db       "MEMORY WRITE ERROR AT",' '+80H
m5:     db       " PAUSE",' '+80H
m6:     db       "CHECKSUM ERROR, BLOCK",' '+80H
m7:     db       "  OK?",' '+80H
m8:     db       " ABORTED..",'.'+80H
m9:     db       "ERROR AT ADDR/WRITE/READ -",' '+80H
m10:    db       "  TEST O",'K'+80H
m13:    db       'A','P'+80H
m14:    db       "OAD",' '+80H
m25:    db       'S','P'+80H
m27:    db       "UM",'P'+80H
m29:    db       "OV",'E'+80H
m30:    db       "IL",'L'+80H
m31:    db       "ES",'T'+80H
m32:    db       "DDRES",'S'+80H
m34:    db       "EARCH",' '+80H
m35:    db       'E','X'+80H
m38:    db       "YMBOLIC",' '+80H
m46:    db       " NUM?",' '+80H
m51:    db       "UNCH",' '+80H
m64:    db       "ULL",'S'+80H
m66:    db       "EGISTER",' '+80H
m67:    db       'O','F'+80H
m68:    db       "XAMINE/MODIFY",' '+80H
m69:    db       "ISPLA",'Y'+80H
m70:    db       "EL",'P'+80H
m71:    db       cr,lf,"ADDRESS XXXX"
        db       cr,lf,"DUMP HEX XXXX YYYY"
        db       cr,lf,"DUMP SYMBOLIC XXXX YYYY"
        db       cr,lf,"FILL XXXX YYYY ZZ"
        db       cr,lf,"IN XX"
        db       cr,lf,"JUMP XXXX"
        db       cr,lf,"LOAD HEX XXXX"
        db       cr,lf,"LOAD SYMBOLIC XXXX"
        db       cr,lf,"MOVE XXXX YYYY ZZZZ"
        db       cr,lf,"OUT XX YY"
        db       cr,lf,"PUNCH EOF"
        db       cr,lf,"PUNCH HEX XXXX YYYY"
        db       cr,lf,"REGISTER DISPLAY"
        db       cr,lf,"REGISTER MODIFY X"
        db       cr,lf,"SEARCH HEX XXXX YYYY ZZ"
        db       cr,lf,"SEARCH SYMBOLIC XXXX YYYY ZZ"
        db       cr,lf,"TEST XXXX YYYY"
        db       cr,lf,"ZAP XXXX YYY",'Y'+80H
m72:    db       'N'+80H
m73:    db       "U",'T'+80H
m74:    db       cr,lf,"READY FOR HEX DOWNLOAD..",'.'+80H
m75:    db       80H

pswmg:  db       "PS",'W'+80H

rxlst:  db       "AFBCDEHL",0            ;register list

endrom  equ      $                       ;boundry marker

        org      ram                     ;some ram scratchpad area

; system ram area
iosub:      ds  3               ;input/output subroutine goes here
tmpa:       ds  1               ;temp storage location
echo:       ds  1               ;chin echo flag
adr:        ds  2               ;examine/modify address
xeqad:      ds  2               ;execution address
blkad:      ds  2               ;load block address
errfl:      ds  1               ;memory test error flag
begadr:     ds  2               ;temp start addr storage
endadr:     ds  2               ;temp stop  addr storage

; following are ram areas for multi-radix assembly/disassembly
clp:        ds  2               ;current location pointer
finclp:     ds  2               ;final current location pointer
opcadr:     ds  2               ;opcode addr
preadr:     ds  2               ;previous opcode address
adflag:     ds  1
exflag:     ds  1
dbflag:     ds  1
augflag:    ds  1
matchflag:  ds  2
tempword:   ds  3
lastopc:    ds  3               ;last op
lstsupflag: ds  1               ;suppress listing flag

            org 0C000H
                ; Altair ROM BASIC Ver. 4.1
basic16k:       db  0F3H,0C3H,008H,0C0H,024H,0EDH,042H,0EDH,006H,09EH,021H,040H,000H,011H,064H,0C5H,01AH,077H,023H,013H,005H,0C2H,010H,0C0H,0F9H,0CDH,09BH,0C8H,021H,000H,000H,0CDH
;               db  0F3H,0F9H,0CDH,08BH,0D2H,032H,01CH,003H,032H,0D7H,003H,032H,05EH,003H,0DBH,0FFH,0FEH,0FFH,0CAH,000H,0E0H,01FH,01FH,0DAH,0EDH,0E0H,021H,0F4H,0C0H,0CDH,0A8H,0DCH
                db  0F3H,0F9H,0CDH,08BH,0D2H,032H,01CH,003H,032H,0D7H,003H,032H,05EH,003H,03EH,000H,0FEH,0FFH,0CAH,000H,0E0H,01FH,01FH,0DAH,0EDH,0E0H,021H,0F4H,0C0H,0CDH,0A8H,0DCH
;                                                                                         ^^^^ ^^^^
                db  0CDH,0B8H,0C8H,0CDH,02CH,0CDH,0B7H,0C2H,05DH,0C0H,021H,03BH,004H,023H,07CH,0B5H,0CAH,06FH,0C0H,07EH,02FH,077H,0BEH,0CAH,04DH,0C0H,0C3H,06FH,0C0H,0CDH,008H,0D7H
                db  0B7H,0C2H,07BH,0C6H,0EBH,02BH,03EH,0D9H,046H,077H,0BEH,070H,0C2H,03AH,0C0H,02BH,011H,02CH,001H,0CDH,0CBH,0CBH,0DAH,05BH,0C6H,011H,0CEH,0FFH,022H,02EH,003H,019H
                db  022H,0D5H,000H,0CDH,05BH,0C8H,02AH,0D5H,000H,011H,018H,0FCH,019H,0E5H,0CDH,0FCH,0FBH,021H,0B9H,0C0H,0CDH,0A8H,0DCH,0E1H,0CDH,007H,0F3H,021H,0ABH,0C0H,0CDH,0A8H
                db  0DCH,0F3H,031H,01FH,002H,0CDH,09BH,0C8H,0C3H,006H,0C7H,020H,042H,059H,054H,045H,053H,020H,046H,052H,045H,045H,00DH,00AH,000H,041H,04CH,054H,041H,049H,052H,020H
                db  052H,04FH,04DH,020H,042H,041H,053H,049H,043H,020H,056H,045H,052H,020H,020H,034H,02EH,031H,009H,009H,00DH,00AH,043H,04FH,050H,059H,052H,049H,047H,048H,054H,020H
                db  031H,039H,037H,037H,020H,042H,059H,020H,04DH,049H,054H,053H,020H,049H,04EH,043H,02EH,00DH,00AH,000H,04DH,045H,04DH,04FH,052H,059H,020H,053H,049H,05AH,045H,000H
                db  0FCH,0CDH,043H,0CCH,050H,0D4H,07EH,0D0H,05BH,0D3H,06EH,0D8H,089H,0D3H,0A4H,0D0H,02CH,0D0H,0FEH,0CFH,0D0H,0D1H,0D6H,0CDH,014H,0D0H,063H,0D0H,080H,0D0H,0FAH,0CDH
                db  01AH,0D2H,0D1H,0CFH,00DH,0E1H,05AH,0C8H,005H,0D1H,05EH,0CEH,06EH,0E0H,0FEH,0DAH,07FH,0E3H,04BH,0CEH,0B1H,0E2H,0EEH,0E2H,068H,0E0H,011H,0D2H,008H,0E1H,08CH,0E0H
                db  0A3H,0E0H,080H,0D0H,070H,0CEH,071H,0CEH,076H,0CEH,0BBH,0CEH,071H,0E5H,08FH,0D1H,052H,0D1H,05CH,0E2H,09AH,0D1H,046H,0E7H,03BH,0CFH,03EH,0CFH,041H,0CFH,044H,0CFH
                db  013H,0D3H,0A6H,0DEH,0D7H,0DEH,0E1H,0DEH,026H,0ECH,0E8H,0EDH,011H,0ECH,0C5H,0F7H,0AAH,0F8H,025H,0F9H,09CH,0EAH,019H,0F8H,01FH,0F9H,086H,0F9H,09BH,0F9H,07CH,0DAH
                db  05CH,0E0H,0A6H,0DAH,032H,0DEH,032H,0DCH,0FDH,0DEH,03EH,0DEH,04EH,0DEH,078H,0E3H,089H,0DEH,026H,0DCH,02CH,0DCH,0A0H,0DAH,024H,0EDH,05AH,0EDH,086H,0EDH,0D5H,0EDH
                db  0D4H,0C1H,0E5H,0C1H,0E6H,0C1H,014H,0C2H,03DH,0C2H,05FH,0C2H,06BH,0C2H,07AH,0C2H,07FH,0C2H,095H,0C2H,096H,0C2H,097H,0C2H,0BDH,0C2H,0C5H,0C2H,0D4H,0C2H,0E0H,0C2H
                db  0F1H,0C2H,0F2H,0C2H,01EH,0C3H,049H,0C3H,060H,0C3H,069H,0C3H,073H,0C3H,07DH,0C3H,081H,0C3H,082H,0C3H,04EH,0C4H,0F7H,042H,0D3H,006H,054H,0CEH,00EH,053H,0C3H,015H
                db  055H,054H,0CFH,0ABH,000H,000H,04FH,04EH,053H,04FH,04CH,0C5H,0A0H,04FH,04EH,0D4H,09AH,04CH,045H,041H,0D2H,092H,04CH,04FH,041H,0C4H,09CH,053H,041H,056H,0C5H,09BH
                db  049H,04EH,0D4H,01CH,053H,04EH,0C7H,01DH,044H,042H,0CCH,01EH,04FH,0D3H,00CH,048H,052H,0A4H,016H,000H,041H,054H,0C1H,084H,049H,0CDH,086H,045H,046H,053H,054H,0D2H
                db  0ADH,045H,046H,049H,04EH,0D4H,0AEH,045H,046H,053H,04EH,0C7H,0AFH,045H,046H,044H,042H,0CCH,0B0H,045H,0C6H,098H,045H,04CH,045H,054H,0C5H,0AAH,000H,04EH,0C4H,081H
                db  04CH,053H,0C5H,0A2H,052H,041H,053H,0C5H,0A6H,044H,049H,0D4H,0A7H,052H,052H,04FH,0D2H,0A8H,052H,0CCH,0D6H,052H,0D2H,0D7H,058H,0D0H,00BH,051H,0D6H,0FAH,000H,04FH
                db  0D2H,082H,0CEH,0D3H,052H,0C5H,00FH,049H,0D8H,01FH,000H,04FH,054H,0CFH,089H,04FH,020H,054H,0CFH,089H,04FH,053H,055H,0C2H,08DH,000H,045H,058H,0A4H,01AH,000H,04EH
                db  050H,055H,0D4H,085H,0C6H,08BH,04EH,053H,054H,0D2H,0DAH,04EH,0D4H,005H,04EH,0D0H,010H,04DH,0D0H,0FBH,000H,000H,000H,045H,0D4H,088H,049H,04EH,0C5H,0B1H,050H,052H
                db  049H,04EH,0D4H,09EH,049H,053H,0D4H,093H,04CH,049H,053H,0D4H,09FH,050H,04FH,0D3H,01BH,04FH,0C7H,00AH,045H,0CEH,012H,045H,046H,054H,0A4H,001H,000H,04FH,0C4H,0FCH
                db  049H,044H,0A4H,003H,000H,045H,058H,0D4H,083H,055H,04CH,0CCH,096H,045H,0D7H,094H,04FH,0D4H,0D5H,000H,055H,0D4H,09DH,0CEH,095H,0D2H,0F8H,043H,054H,0A4H,019H,000H
                db  04FH,04BH,0C5H,099H,052H,049H,04EH,0D4H,091H,04FH,0D3H,011H,045H,045H,0CBH,017H,000H,000H,045H,041H,0C4H,087H,055H,0CEH,08AH,045H,053H,054H,04FH,052H,0C5H,08CH
                db  045H,054H,055H,052H,0CEH,08EH,045H,0CDH,08FH,045H,053H,055H,04DH,0C5H,0A9H,049H,047H,048H,054H,0A4H,002H,04EH,0C4H,008H,045H,04EH,055H,0CDH,0ACH,000H,054H,04FH
                db  0D0H,090H,057H,041H,0D0H,0A5H,050H,043H,0A8H,0D4H,054H,045H,0D0H,0D1H,047H,0CEH,004H,051H,0D2H,007H,049H,0CEH,009H,054H,052H,0A4H,013H,054H,052H,049H,04EH,047H
                db  0A4H,0D8H,050H,041H,043H,045H,0A4H,018H,000H,052H,04FH,0CEH,0A3H,052H,04FH,046H,0C6H,0A4H,041H,042H,0A8H,0D0H,0CFH,0CEH,048H,045H,0CEH,0CFH,041H,0CEH,00DH,000H
                db  053H,049H,04EH,0C7H,0D9H,053H,0D2H,0D2H,000H,041H,0CCH,014H,041H,052H,050H,054H,0D2H,0DCH,000H,049H,044H,054H,0C8H,0A1H,041H,049H,0D4H,097H,000H,04FH,0D2H,0F9H
                db  000H,000H,000H,0ABH,0F2H,0ADH,0F3H,0AAH,0F4H,0AFH,0F5H,0DEH,0F6H,0DCH,0FDH,0A7H,0DBH,0BEH,0EFH,0BDH,0F0H,0BCH,0F1H,000H,079H,079H,07CH,07CH,07FH,050H,046H,03CH
                db  032H,028H,07AH,07BH,086H,0EDH,000H,000H,024H,0EDH,0A1H,0EDH,05AH,0EDH,083H,0EFH,07CH,0EFH,0BEH,0F0H,006H,0F1H,01DH,0EDH,0A0H,0E9H,09DH,0E9H,0DAH,0EAH,038H,0EBH
                db  0B0H,0ECH,08EH,0EEH,082H,0EEH,0AEH,0EEH,04AH,0D6H,0DDH,0ECH,000H,04EH,045H,058H,054H,020H,057H,049H,054H,048H,04FH,055H,054H,020H,046H,04FH,052H,000H,053H,059H
                db  04EH,054H,041H,058H,020H,045H,052H,052H,04FH,052H,000H,052H,045H,054H,055H,052H,04EH,020H,057H,049H,054H,048H,04FH,055H,054H,020H,047H,04FH,053H,055H,042H,000H
                db  04FH,055H,054H,020H,04FH,046H,020H,044H,041H,054H,041H,000H,049H,04CH,04CH,045H,047H,041H,04CH,020H,046H,055H,04EH,043H,054H,049H,04FH,04EH,020H,043H,041H,04CH
                db  04CH,000H,04FH,056H,045H,052H,046H,04CH,04FH,057H,000H,04FH,055H,054H,020H,04FH,046H,020H,04DH,045H,04DH,04FH,052H,059H,000H,055H,04EH,044H,045H,046H,049H,04EH
                db  045H,044H,020H,04CH,049H,04EH,045H,020H,04EH,055H,04DH,042H,045H,052H,000H,053H,055H,042H,053H,043H,052H,049H,050H,054H,020H,04FH,055H,054H,020H,04FH,046H,020H
                db  052H,041H,04EH,047H,045H,000H,052H,045H,044H,049H,04DH,045H,04EH,053H,049H,04FH,04EH,045H,044H,020H,041H,052H,052H,041H,059H,000H,044H,049H,056H,049H,053H,049H
                db  04FH,04EH,020H,042H,059H,020H,05AH,045H,052H,04FH,000H,049H,04CH,04CH,045H,047H,041H,04CH,020H,044H,049H,052H,045H,043H,054H,000H,054H,059H,050H,045H,020H,04DH
                db  049H,053H,04DH,041H,054H,043H,048H,000H,04FH,055H,054H,020H,04FH,046H,020H,053H,054H,052H,049H,04EH,047H,020H,053H,050H,041H,043H,045H,000H,053H,054H,052H,049H
                db  04EH,047H,020H,054H,04FH,04FH,020H,04CH,04FH,04EH,047H,000H,053H,054H,052H,049H,04EH,047H,020H,046H,04FH,052H,04DH,055H,04CH,041H,020H,054H,04FH,04FH,020H,043H
                db  04FH,04DH,050H,04CH,045H,058H,000H,043H,041H,04EH,027H,054H,020H,043H,04FH,04EH,054H,049H,04EH,055H,045H,000H,055H,04EH,044H,045H,046H,049H,04EH,045H,044H,020H
                db  055H,053H,045H,052H,020H,046H,055H,04EH,043H,054H,049H,04FH,04EH,000H,04EH,04FH,020H,052H,045H,053H,055H,04DH,045H,000H,052H,045H,053H,055H,04DH,045H,020H,057H
                db  049H,054H,048H,04FH,055H,054H,020H,045H,052H,052H,04FH,052H,000H,055H,04EH,050H,052H,049H,04EH,054H,041H,042H,04CH,045H,020H,045H,052H,052H,04FH,052H,000H,04DH
                db  049H,053H,053H,049H,04EH,047H,020H,04FH,050H,045H,052H,041H,04EH,044H,000H,04CH,049H,04EH,045H,020H,042H,055H,046H,046H,045H,052H,020H,04FH,056H,045H,052H,046H
                db  04CH,04FH,057H,000H,0C3H,0A1H,0C0H,0C3H,090H,000H,0C3H,098H,000H,0C3H,0A2H,000H,0D3H,000H,0C9H,0D6H,000H,06FH,07CH,0DEH,000H,067H,078H,0DEH,000H,047H,03EH,000H
                db  0C9H,000H,000H,000H,035H,04AH,0CAH,099H,039H,01CH,076H,098H,022H,095H,0B3H,098H,00AH,0DDH,047H,098H,053H,0D1H,099H,099H,00AH,01AH,09FH,098H,065H,0BCH,0CDH,098H
                db  0D6H,077H,03EH,098H,052H,0C7H,04FH,080H,0C3H,08BH,0E8H,0C3H,08EH,0E8H,0C3H,091H,0E8H,0DBH,000H,0C9H,0DBH,000H,0E6H,001H,0C6H,0FFH,09FH,0C9H,0CDH,043H,000H,0B7H
                db  0CAH,098H,000H,0DBH,001H,0C9H,0F5H,0DBH,000H,0E6H,080H,0CAH,0A3H,000H,0F1H,0D3H,001H,0F5H,0DBH,013H,0F1H,0C9H,089H,0CFH,089H,0CFH,089H,0CFH,089H,0CFH,089H,0CFH
                db  089H,0CFH,089H,0CFH,089H,0CFH,089H,0CFH,089H,0CFH,001H,000H,000H,000H,000H,000H,000H,000H,000H,000H,000H,048H,038H,000H,000H,03BH,004H,0FEH,0FFH,0D8H,003H,020H
                db  049H,04EH,020H,000H,04FH,04BH,00DH,00AH,000H,042H,052H,045H,041H,04BH,000H,021H,004H,000H,039H,07EH,023H,0FEH,082H,0C0H,04EH,023H,046H,023H,0E5H,069H,060H,07AH
                db  0B3H,0EBH,0CAH,029H,0C6H,0EBH,0CDH,0CBH,0CBH,001H,00EH,000H,0E1H,0C8H,009H,0C3H,013H,0C6H,0CDH,04CH,0C6H,0C5H,0E3H,0C1H,0CDH,0CBH,0CBH,07EH,002H,0C8H,00BH,02BH
                db  0C3H,038H,0C6H,0E5H,02AH,07AH,003H,006H,000H,009H,009H,03EH,0E5H,03EH,0CAH,095H,06FH,03EH,0FFH,09CH,0DAH,05BH,0C6H,067H,039H,0E1H,0D8H,01EH,007H,0C3H,08CH,0C6H
                db  02AH,0D7H,000H,07CH,0A5H,03CH,0CAH,00AH,0CEH,03AH,06FH,003H,0B7H,01EH,013H,0C2H,08CH,0C6H,0C3H,00AH,0CEH,02AH,057H,003H,022H,0D7H,000H,01EH,002H,001H,01EH,00BH
                db  001H,01EH,001H,001H,01EH,00AH,001H,01EH,012H,001H,01EH,014H,02AH,0D7H,000H,022H,067H,003H,022H,069H,003H,001H,09EH,0C6H,02AH,065H,003H,0C3H,0A2H,0C8H,0C1H,07BH
                db  04BH,032H,0C7H,000H,02AH,063H,003H,022H,06BH,003H,0EBH,02AH,067H,003H,07CH,0A5H,03CH,0CAH,0BBH,0C6H,022H,072H,003H,0EBH,022H,074H,003H,02AH,06DH,003H,07CH,0B5H
                db  0EBH,021H,06FH,003H,0CAH,0D0H,0C6H,0A6H,0C2H,0D0H,0C6H,035H,0EBH,0C3H,0E7H,0CCH,0AFH,077H,059H,032H,0D4H,000H,0CDH,07EH,0D2H,021H,0CCH,0C3H,07BH,0FEH,018H,0DAH
                db  0E4H,0C6H,01EH,015H,0CDH,080H,0D0H,01DH,023H,0C2H,0E4H,0C6H,0E5H,02AH,067H,003H,0E3H,0CDH,0A8H,0DCH,0E1H,011H,0FEH,0FFH,0CDH,0CBH,0CBH,0CAH,008H,0C0H,07CH,0A5H
                db  03CH,0C4H,0FFH,0F2H,03EH,0C1H,0CDH,084H,000H,0AFH,032H,0D4H,000H,0CDH,07EH,0D2H,021H,004H,0C6H,0CDH,0A8H,0DCH,03AH,0C7H,000H,0D6H,002H,0CCH,063H,0E5H,021H,0FFH
                db  0FFH,022H,0D7H,000H,03AH,05EH,003H,0B7H,0CAH,069H,0C7H,02AH,05FH,003H,0E5H,0CDH,007H,0F3H,0D1H,0D5H,0CDH,03AH,0C8H,03EH,02AH,0DAH,03EH,0C7H,03EH,020H,0CDH,0DCH
                db  0CBH,0CDH,006H,0CBH,0D1H,0D2H,04FH,0C7H,0AFH,032H,05EH,003H,0C3H,006H,0C7H,02AH,061H,003H,019H,0DAH,048H,0C7H,0D5H,011H,0F9H,0FFH,0CDH,0CBH,0CBH,0D1H,0D2H,048H
                db  0C7H,022H,05FH,003H,0F6H,0FFH,0C3H,01FH,0E7H,0CDH,006H,0CBH,0DAH,01EH,0C7H,0CDH,02CH,0CDH,03CH,03DH,0CAH,01EH,0C7H,0F5H,0CDH,099H,0CFH,0CDH,0E5H,0CAH,07EH,0FEH
                db  020H,0CCH,068H,0ECH,0D5H,0CDH,0C5H,0C8H,0D1H,0F1H,022H,063H,003H,0D2H,00CH,0CDH,0D5H,0C5H,0CDH,02CH,0CDH,0B7H,0F5H,0EBH,022H,069H,003H,0EBH,0CDH,03AH,0C8H,0DAH
                db  0A8H,0C7H,0F1H,0F5H,0CAH,05EH,0D0H,0B7H,0C5H,0F5H,0E5H,0CDH,083H,0E8H,0E1H,0F1H,0C1H,0C5H,0DCH,084H,0E2H,0D1H,0F1H,0D5H,0CAH,0E7H,0C7H,0D1H,02AH,076H,003H,0E3H
                db  0C1H,0E5H,009H,0E5H,0CDH,032H,0C6H,0E1H,022H,076H,003H,0EBH,074H,0C1H,0D1H,0E5H,023H,023H,073H,023H,072H,023H,011H,0DBH,000H,00BH,00BH,00BH,00BH,01AH,077H,023H
                db  013H,00BH,079H,0B0H,0C2H,0DDH,0C7H,0D1H,0CDH,0F1H,0C7H,0CDH,06BH,0C8H,0C3H,01EH,0C7H,062H,06BH,07EH,023H,0B6H,0C8H,023H,023H,023H,07EH,0B7H,0CAH,012H,0C8H,0FEH
                db  020H,0D2H,0F9H,0C7H,0FEH,00BH,0DAH,0F9H,0C7H,0CDH,02DH,0CDH,0CDH,02CH,0CDH,0C3H,0FBH,0C7H,023H,0EBH,073H,023H,072H,0C3H,0F1H,0C7H,011H,000H,000H,0D5H,0CAH,02DH
                db  0C8H,0D1H,0CDH,08EH,0CFH,0D5H,0CAH,036H,0C8H,0CDH,0D1H,0CBH,0F3H,011H,0FAH,0FFH,0C4H,08EH,0CFH,0C2H,07BH,0C6H,0EBH,0D1H,0E3H,0E5H,02AH,0D9H,000H,044H,04DH,07EH
                db  023H,0B6H,02BH,0C8H,023H,023H,07EH,023H,066H,06FH,0CDH,0CBH,0CBH,060H,069H,07EH,023H,066H,06FH,03FH,0C8H,03FH,0D0H,0C3H,03DH,0C8H,0C0H,02AH,0D9H,000H,0CDH,071H
                db  0CEH,032H,05DH,003H,077H,023H,077H,023H,022H,076H,003H,02AH,0D9H,000H,02BH,022H,05BH,003H,03EH,01AH,021H,07EH,003H,036H,004H,023H,03DH,0C2H,077H,0C8H,032H,06FH
                db  003H,06FH,067H,022H,06DH,003H,022H,074H,003H,02AH,02EH,003H,022H,053H,003H,0CDH,0D6H,0CDH,02AH,076H,003H,022H,078H,003H,022H,07AH,003H,0C1H,02AH,0D5H,000H,022H
                db  065H,003H,0F9H,021H,032H,003H,022H,030H,003H,0CDH,084H,000H,0AFH,067H,06FH,032H,059H,003H,0E5H,0C5H,02AH,05BH,003H,0C9H,03EH,03FH,0CDH,0DCH,0CBH,03EH,020H,0CDH
                db  0DCH,0CBH,0C3H,006H,0CBH,0AFH,032H,021H,003H,032H,020H,003H,001H,03BH,001H,011H,0DBH,000H,07EH,0FEH,022H,0CAH,08AH,0CAH,0FEH,020H,0CAH,060H,0CAH,0B7H,0CAH,093H
                db  0CAH,03AH,020H,003H,0B7H,07EH,0C2H,060H,0CAH,0FEH,03FH,03EH,091H,0CAH,060H,0CAH,0D5H,0C5H,011H,083H,0C3H,0CDH,0B3H,0CAH,0CDH,07BH,0CFH,0DAH,0A3H,0C9H,0E5H,021H
                db  0A0H,0C1H,0D6H,041H,087H,04FH,006H,000H,009H,05EH,023H,056H,0E1H,023H,0E5H,0CDH,0B3H,0CAH,04FH,01AH,0E6H,07FH,0CAH,0BEH,0CAH,0B9H,023H,0C2H,03FH,0C9H,01AH,013H
                db  0B7H,0F2H,00FH,0C9H,0F1H,01AH,0B7H,0FAH,04AH,0C9H,0C1H,0D1H,0F6H,080H,0F5H,03EH,0FFH,0CDH,0A8H,0CAH,0AFH,032H,021H,003H,0F1H,0CDH,0A8H,0CAH,0C3H,0D2H,0C8H,0E1H
                db  01AH,013H,0B7H,0F2H,040H,0C9H,013H,0C3H,00EH,0C9H,02BH,0F5H,0FEH,0A2H,0CAH,07CH,0C9H,001H,07CH,0C9H,0C5H,0FEH,0ABH,0C8H,0FEH,0ACH,0C8H,0FEH,0AAH,0C8H,0FEH,0A7H
                db  0C8H,0FEH,0A9H,0C8H,0FEH,0D6H,0C8H,0FEH,08AH,0C8H,0FEH,093H,0C8H,0FEH,09FH,0C8H,0FEH,089H,0C8H,0FEH,0CFH,0C8H,0FEH,08DH,0C8H,0F1H,0AFH,0C2H,03EH,001H,032H,021H
                db  003H,0F1H,0FEH,07EH,0C1H,0D1H,0FEH,0A2H,0C2H,090H,0C9H,0F5H,0CDH,0A6H,0CAH,0F1H,0FEH,0DBH,0C2H,02EH,0CAH,0F5H,0CDH,0A6H,0CAH,03EH,08FH,0CDH,0A8H,0CAH,0F1H,0F5H
                db  0C3H,08CH,0CAH,07EH,0FEH,02EH,0CAH,0B3H,0C9H,0FEH,03AH,0D2H,04DH,0CAH,0FEH,030H,0DAH,04DH,0CAH,03AH,021H,003H,0B7H,07EH,0C1H,0D1H,0FAH,060H,0CAH,0CAH,0DFH,0C9H
                db  0FEH,02EH,0CAH,060H,0CAH,03EH,00EH,0CDH,0A8H,0CAH,0D5H,0CDH,099H,0CFH,0CDH,0E5H,0CAH,0E3H,0EBH,07DH,0CDH,0A8H,0CAH,07CH,0E1H,0CDH,0A8H,0CAH,0C3H,0D2H,0C8H,0D5H
                db  0C5H,07EH,0CDH,091H,0F1H,0CDH,0E5H,0CAH,0C1H,0D1H,0E5H,03AH,01FH,003H,0FEH,002H,0C2H,00BH,0CAH,02AH,0A6H,003H,07CH,0B7H,03EH,002H,0C2H,00BH,0CAH,07DH,065H,02EH
                db  00FH,0FEH,00AH,0D2H,0D3H,0C9H,0C6H,011H,0C3H,0D8H,0C9H,0F5H,00FH,0C6H,01BH,0CDH,0A8H,0CAH,021H,0A6H,003H,0CDH,008H,0D8H,0DAH,01EH,0CAH,021H,0A2H,003H,0F1H,0F5H
                db  07EH,0CDH,0A8H,0CAH,0F1H,023H,03DH,0C2H,01FH,0CAH,0E1H,0C3H,0D2H,0C8H,0FEH,026H,0C2H,060H,0CAH,0E5H,0CDH,02CH,0CDH,0FEH,048H,0E1H,03EH,00BH,0C2H,041H,0CAH,03EH
                db  00CH,0CDH,0A8H,0CAH,0D5H,0C5H,0CDH,00DH,0D7H,0C1H,0C3H,0D1H,0C9H,011H,082H,0C3H,013H,01AH,0E6H,07FH,0CAH,0C7H,0CAH,0BEH,013H,01AH,0C2H,050H,0CAH,0C3H,0D9H,0CAH
                db  023H,0F5H,0CDH,0A8H,0CAH,0F1H,0D6H,03AH,0CAH,072H,0CAH,0FEH,04AH,0C2H,078H,0CAH,03EH,001H,032H,020H,003H,032H,021H,003H,0D6H,055H,0C2H,0D2H,0C8H,0F5H,07EH,0B7H
                db  0E3H,07CH,0E1H,0CAH,093H,0CAH,0BEH,0CAH,060H,0CAH,0F5H,07EH,023H,0CDH,0A8H,0CAH,0C3H,07EH,0CAH,021H,040H,001H,07DH,091H,04FH,07CH,098H,047H,021H,0DAH,000H,0AFH
                db  012H,013H,012H,013H,012H,0C9H,03EH,03AH,012H,013H,00BH,079H,0B0H,0C0H,01EH,017H,0C3H,08CH,0C6H,07EH,0FEH,061H,0D8H,0FEH,07BH,0D0H,0E6H,05FH,077H,0C9H,0E1H,02BH
                db  03DH,032H,021H,003H,0C3H,083H,0C9H,07EH,0FEH,020H,0D2H,0D9H,0CAH,0FEH,009H,0CAH,0D9H,0CAH,0FEH,00AH,0CAH,0D9H,0CAH,03EH,020H,0F5H,03AH,021H,003H,03CH,0CAH,07EH
                db  0C9H,03DH,0C3H,07EH,0C9H,02BH,07EH,0FEH,020H,0CAH,0E5H,0CAH,0FEH,009H,0CAH,0E5H,0CAH,0FEH,00AH,0CAH,0E5H,0CAH,023H,0C9H,0CDH,028H,0CCH,0FEH,001H,0C2H,050H,0CBH
                db  036H,000H,0C3H,00EH,0CBH,070H,0CDH,028H,0CCH,0FEH,001H,0C2H,045H,0CBH,0CDH,08BH,0D2H,021H,0FFH,0FFH,0C3H,08EH,0E5H,03AH,0D3H,000H,0B7H,03EH,05CH,032H,0D3H,000H
                db  0C2H,02BH,0CBH,005H,0CAH,005H,0CBH,0CDH,0DCH,0CBH,03EH,005H,02BH,0CAH,03FH,0CBH,07EH,0CDH,0DCH,0CBH,0C3H,0F8H,0CAH,005H,02BH,0CDH,0DCH,0CBH,0C2H,0F8H,0CAH,0CDH
                db  0DCH,0CBH,0CDH,08BH,0D2H,021H,01AH,002H,006H,001H,0F5H,0AFH,032H,0D3H,000H,0F1H,04FH,0FEH,07FH,0CAH,017H,0CBH,03AH,0D3H,000H,0B7H,0CAH,066H,0CBH,03EH,05CH,0CDH
                db  0DCH,0CBH,0AFH,032H,0D3H,000H,079H,0FEH,007H,0CAH,0A1H,0CBH,0FEH,003H,0CCH,034H,0CEH,037H,0C8H,0FEH,00DH,0CAH,086H,0D2H,0FEH,009H,0CAH,0A1H,0CBH,0FEH,00AH,0C2H
                db  08AH,0CBH,005H,0CAH,006H,0CBH,004H,0C3H,0A1H,0CBH,0FEH,015H,0CCH,034H,0CEH,0CAH,006H,0CBH,0FEH,040H,0CAH,03FH,0CBH,0FEH,05FH,0CAH,037H,0CBH,0FEH,020H,0DAH,0F8H
                db  0CAH,078H,03CH,03EH,007H,0CAH,0ACH,0CBH,079H,071H,023H,004H,0CDH,0DCH,0CBH,0D6H,00AH,0C2H,0F8H,0CAH,032H,01DH,003H,03EH,00DH,0CDH,0DCH,0CBH,0CDH,028H,0CCH,0B7H
                db  0CAH,0BCH,0CBH,0FEH,00DH,0CAH,0F8H,0CAH,0C3H,050H,0CBH,07CH,092H,0C0H,07DH,093H,0C9H,07EH,0E3H,0BEH,023H,0E3H,0CAH,02CH,0CDH,0C3H,07BH,0C6H,0F5H,03AH,0D4H,000H
                db  0B7H,03AH,0CAH,000H,0B7H,0C2H,08AH,000H,03AH,0D4H,000H,0B7H,0C2H,0DDH,0DCH,0F1H,0C5H,0F5H,0FEH,009H,0C2H,007H,0CCH,03EH,020H,0CDH,0DCH,0CBH,03AH,01DH,003H,0E6H
                db  007H,0C2H,0F7H,0CBH,0F1H,0C1H,0C9H,0FEH,020H,0DAH,01BH,0CCH,03AH,0D1H,000H,047H,03AH,01DH,003H,0B8H,0CCH,08BH,0D2H,03CH,032H,01DH,003H,0F1H,0C1H,0F5H,0F1H,0F5H
                db  0C5H,04FH,0CDH,049H,000H,0C1H,0F1H,0C9H,0C5H,0CDH,046H,000H,0C1H,0E6H,07FH,0FEH,00FH,0C0H,03AH,0D4H,000H,0B7H,0CCH,032H,0CEH,02FH,032H,0D4H,000H,0B7H,0CAH,032H
                db  0CEH,0AFH,0C9H,03EH,064H,032H,059H,003H,0CDH,0A4H,0D0H,0E3H,0CDH,00FH,0C6H,0D1H,0C2H,058H,0CCH,009H,0F9H,022H,065H,003H,0EBH,00EH,008H,0CDH,043H,0C6H,0E5H,0CDH
                db  07EH,0D0H,0E3H,0E5H,02AH,0D7H,000H,0E3H,0CDH,0D1H,0CBH,0CEH,0CDH,008H,0D8H,0CAH,0A5H,0EDH,0D2H,0A5H,0EDH,0F5H,0CDH,0D7H,0D4H,0F1H,0E5H,0F2H,094H,0CCH,0CDH,024H
                db  0EDH,0E3H,011H,001H,000H,07EH,0FEH,0D1H,0CCH,0BCH,0E0H,0D5H,0E5H,0EBH,0CDH,03CH,0ECH,0C3H,0B7H,0CCH,0CDH,05AH,0EDH,0CDH,05EH,0ECH,0E1H,0C5H,0D5H,001H,000H,081H
                db  051H,05AH,07EH,0FEH,0D1H,03EH,001H,0C2H,0B8H,0CCH,0CDH,0D8H,0D4H,0E5H,0CDH,05AH,0EDH,0CDH,05EH,0ECH,0CDH,0EFH,0EBH,0E1H,0C5H,0D5H,04FH,0CDH,008H,0D8H,047H,0C5H
                db  0E5H,02AH,05BH,003H,0E3H,006H,082H,0C5H,033H,0CDH,043H,000H,0B7H,0C4H,0E5H,0CDH,022H,063H,003H,0EBH,021H,000H,000H,039H,022H,065H,003H,0EBH,07EH,0FEH,03AH,0CAH
                db  00CH,0CDH,0B7H,0C2H,07BH,0C6H,023H,07EH,023H,0B6H,0CAH,060H,0C6H,023H,05EH,023H,056H,0EBH,022H,0D7H,000H,03AH,0A0H,003H,0B7H,0CAH,00BH,0CDH,0D5H,03EH,05BH,0CDH
                db  0DCH,0CBH,0CDH,007H,0F3H,03EH,05DH,0CDH,0DCH,0CBH,0D1H,0EBH,0CDH,02CH,0CDH,011H,0C9H,0CCH,0D5H,0C8H,0D6H,081H,0DAH,0A4H,0D0H,0FEH,031H,0D2H,0BEH,0DFH,007H,04FH
                db  006H,000H,0EBH,021H,000H,0C1H,009H,04EH,023H,046H,0C5H,0EBH,023H,07EH,0FEH,03AH,0D0H,0FEH,020H,0CAH,02CH,0CDH,0D2H,09FH,0CDH,0FEH,00BH,0DAH,095H,0CDH,0FEH,01EH
                db  0C2H,048H,0CDH,03AH,024H,003H,0B7H,0C9H,0FEH,010H,0C2H,053H,0CDH,02AH,022H,003H,0C3H,02DH,0CDH,0F5H,023H,032H,024H,003H,0D6H,01CH,0D2H,07AH,0CDH,0D6H,0F5H,0D2H
                db  069H,0CDH,0FEH,0FEH,0C2H,079H,0CDH,07EH,023H,0E5H,06FH,026H,000H,022H,026H,003H,03EH,002H,032H,025H,003H,0E1H,0C3H,08CH,0CDH,0AFH,03CH,007H,032H,025H,003H,0D5H
                db  0C5H,011H,026H,003H,0EBH,047H,0CDH,077H,0ECH,0EBH,0C1H,0D1H,022H,022H,003H,0F1H,021H,0A5H,0CDH,0B7H,0C9H,0FEH,00BH,0D2H,09FH,0CDH,0FEH,009H,0D2H,02CH,0CDH,0FEH
                db  030H,03FH,03CH,03DH,0C9H,01EH,010H,03AH,024H,003H,0FEH,00FH,0D2H,0C7H,0CDH,0FEH,00DH,0DAH,0C7H,0CDH,02AH,026H,003H,0C2H,0C1H,0CDH,023H,023H,023H,05EH,023H,056H
                db  0EBH,0CDH,060H,0EFH,0C3H,04DH,0CDH,021H,026H,003H,03AH,025H,003H,032H,01FH,003H,0CDH,098H,0ECH,0C3H,04DH,0CDH,0EBH,02AH,0D9H,000H,02BH,022H,07CH,003H,0EBH,0C9H
                db  0CDH,043H,000H,0B7H,0C8H,0CDH,028H,0CCH,0FEH,003H,0CCH,034H,0CEH,0CAH,0FAH,0CDH,0FEH,011H,0CAH,0E0H,0CDH,0FEH,013H,0CAH,0E5H,0CDH,0C0H,0F6H,0C0H,022H,063H,003H
                db  021H,032H,003H,022H,030H,003H,021H,0F6H,0FFH,0C1H,02AH,0D7H,000H,0E5H,0F5H,07DH,0A4H,03CH,0CAH,01EH,0CEH,022H,072H,003H,02AH,063H,003H,022H,074H,003H,0AFH,032H
                db  0D4H,000H,0CDH,084H,000H,0CDH,07EH,0D2H,0F1H,021H,009H,0C6H,0C2H,0F1H,0C6H,0C3H,005H,0C7H,03EH,00FH,0F5H,0D6H,003H,0C2H,03DH,0CEH,032H,0D4H,000H,03EH,05EH,0CDH
                db  0DCH,0CBH,0F1H,0C6H,040H,0CDH,0DCH,0CBH,0C3H,08BH,0D2H,02AH,074H,003H,07CH,0B5H,01EH,011H,0CAH,08CH,0C6H,0EBH,02AH,072H,003H,022H,0D7H,000H,0EBH,0C9H,0CDH,0DEH
                db  0E0H,0C0H,03AH,0D1H,000H,047H,07BH,03CH,0B8H,0D2H,089H,0CFH,032H,0C6H,000H,0C9H,03EH,0AFH,032H,0A0H,003H,0C9H,0CDH,073H,0D8H,0D5H,0E5H,021H,098H,003H,0CDH,073H
                db  0ECH,02AH,078H,003H,0E3H,0CDH,008H,0D8H,0F5H,0CDH,0D1H,0CBH,02CH,0CDH,073H,0D8H,0C1H,0CDH,008H,0D8H,0B8H,0C2H,0A5H,0EDH,0E3H,0EBH,0E5H,02AH,078H,003H,0CDH,0CBH
                db  0CBH,0C2H,089H,0CFH,0D1H,0E1H,0E3H,0D5H,0CDH,073H,0ECH,0E1H,011H,098H,003H,0CDH,073H,0ECH,0E1H,0C9H,006H,0FFH,0CDH,02CH,0CDH,078H,0FEH,0AFH,032H,05BH,003H,03EH
                db  001H,032H,059H,003H,0C3H,073H,0D8H,032H,059H,003H,044H,04DH,00BH,00BH,00BH,00BH,00BH,03AH,05BH,003H,0B7H,0C2H,0F8H,0CEH,019H,0EBH,02AH,07AH,003H,0CDH,0CBH,0CBH
                db  01AH,002H,013H,003H,0C2H,0DDH,0CEH,00BH,060H,069H,022H,07AH,003H,0E1H,07EH,0FEH,02CH,0C0H,0CDH,02CH,0CDH,0C3H,0BBH,0CEH,0F5H,0EBH,019H,0EBH,04EH,006H,000H,009H
                db  009H,023H,006H,0D2H,0FAH,011H,0CFH,078H,0CDH,0A2H,0E2H,0CDH,0A2H,0E2H,0C3H,01EH,0CFH,00EH,004H,0CDH,098H,0E2H,0B8H,0C2H,011H,0CFH,00DH,0C2H,013H,0CFH,0CDH,008H
                db  0D8H,0CAH,0A5H,0EDH,0CDH,0CBH,0CBH,0CAH,038H,0CFH,0F1H,0F5H,07EH,0F4H,0A5H,0E2H,0FCH,098H,0E2H,077H,023H,0C3H,024H,0CFH,0F1H,0E1H,0C9H,01EH,003H,001H,01EH,002H
                db  001H,01EH,004H,001H,01EH,008H,0CDH,07AH,0CFH,001H,07BH,0C6H,0C5H,0D8H,0D6H,041H,04FH,047H,0CDH,02CH,0CDH,0FEH,0F3H,0C2H,067H,0CFH,0CDH,02CH,0CDH,0CDH,07AH,0CFH
                db  0D8H,0D6H,041H,047H,0CDH,02CH,0CDH,078H,091H,0D8H,03CH,0E3H,021H,07EH,003H,006H,000H,009H,073H,023H,03DH,0C2H,072H,0CFH,0E1H,0C9H,07EH,0FEH,041H,0D8H,0FEH,05BH
                db  03FH,0C9H,0CDH,02CH,0CDH,0CDH,0BFH,0E0H,0F0H,01EH,005H,0C3H,08CH,0C6H,07EH,0FEH,02EH,0EBH,02AH,069H,003H,0EBH,0CAH,02CH,0CDH,02BH,0CDH,02CH,0CDH,0FEH,00EH,0CAH
                db  0A4H,0CFH,0FEH,00DH,0EBH,02AH,026H,003H,0EBH,0CAH,02CH,0CDH,02BH,011H,000H,000H,0CDH,02CH,0CDH,0D0H,0E5H,0F5H,021H,098H,019H,0CDH,0CBH,0CBH,0DAH,07BH,0C6H,062H
                db  06BH,019H,029H,019H,029H,0F1H,0D6H,030H,05FH,016H,000H,019H,0EBH,0E1H,0C3H,0B0H,0CFH,0CAH,06FH,0C8H,0CDH,085H,0CFH,02BH,0CDH,02CH,0CDH,0C0H,0E5H,02AH,02EH,003H
                db  07DH,093H,05FH,07CH,09AH,057H,0DAH,05BH,0C6H,02AH,076H,003H,001H,028H,000H,009H,0CDH,0CBH,0CBH,0D2H,05BH,0C6H,0EBH,022H,0D5H,000H,0E1H,0C3H,06FH,0C8H,0CAH,06BH
                db  0C8H,0FEH,00EH,0CAH,00BH,0D0H,0FEH,00DH,0C2H,07BH,0C6H,0CDH,06FH,0C8H,001H,0C9H,0CCH,0C3H,02BH,0D0H,00EH,003H,0CDH,043H,0C6H,0CDH,099H,0CFH,0C1H,0E5H,0E5H,02AH
                db  0D7H,000H,0E3H,03EH,08DH,0F5H,033H,0C5H,0C3H,02FH,0D0H,0C5H,0CDH,099H,0CFH,03AH,024H,003H,0FEH,00DH,0EBH,0C8H,0EBH,0E5H,02AH,022H,003H,0E3H,0CDH,080H,0D0H,0E5H
                db  02AH,0D7H,000H,0CDH,0CBH,0CBH,0E1H,023H,0DCH,03DH,0C8H,0D4H,03AH,0C8H,0D2H,05EH,0D0H,00BH,03EH,00DH,032H,05DH,003H,0E1H,0CDH,07AH,0E8H,060H,069H,0C9H,01EH,008H
                db  0C3H,08CH,0C6H,0C0H,016H,0FFH,0CDH,00FH,0C6H,0F9H,022H,065H,003H,0FEH,08DH,01EH,003H,0C2H,08CH,0C6H,0E1H,022H,0D7H,000H,021H,0C9H,0CCH,0E3H,03EH,0E1H,001H,03AH
                db  00EH,000H,006H,000H,079H,048H,047H,02BH,0CDH,02CH,0CDH,0B7H,0C8H,0B8H,0C8H,023H,0FEH,022H,0CAH,084H,0D0H,03CH,0CAH,088H,0D0H,0D6H,08CH,0C2H,087H,0D0H,0B8H,08AH
                db  057H,0C3H,087H,0D0H,0CDH,073H,0D8H,0D5H,0CDH,008H,0D8H,0F5H,0CDH,0D1H,0CBH,0F0H,0EBH,022H,05BH,003H,0EBH,0C3H,0BDH,0D0H,0D5H,0CDH,008H,0D8H,0F5H,0CDH,0D7H,0D4H
                db  0F1H,0E3H,0C6H,003H,0CDH,0E7H,0DBH,0CDH,0A5H,0ECH,0E5H,0C2H,0FFH,0D0H,02AH,0A6H,003H,0E5H,023H,05EH,023H,056H,02AH,0D9H,000H,0CDH,0CBH,0CBH,0D2H,0F3H,0D0H,02AH
                db  0D5H,000H,0CDH,0CBH,0CBH,0D1H,0D2H,0FBH,0D0H,02AH,076H,003H,0CDH,0CBH,0CBH,0D2H,0FBH,0D0H,03EH,0D1H,0CDH,022H,0DEH,0EBH,0CDH,03FH,0DCH,0CDH,022H,0DEH,0E3H,0CDH
                db  073H,0ECH,0D1H,0E1H,0C9H,0FEH,0A8H,0C2H,034H,0D1H,0CDH,02CH,0CDH,0CDH,0D1H,0CBH,089H,0CDH,099H,0CFH,07AH,0B3H,0CAH,022H,0D1H,0CDH,038H,0C8H,050H,059H,0E1H,0D2H
                db  05EH,0D0H,0EBH,022H,06DH,003H,0EBH,0D8H,03AH,06FH,003H,0B7H,0C8H,03AH,0C7H,000H,05FH,0C3H,095H,0C6H,0CDH,0DEH,0E0H,07EH,047H,0FEH,08DH,0CAH,043H,0D1H,0CDH,0D1H
                db  0CBH,089H,02BH,04BH,00DH,078H,0CAH,014H,0CDH,0CDH,09AH,0CFH,0FEH,02CH,0C0H,0C3H,044H,0D1H,011H,06FH,003H,01AH,0B7H,0CAH,08AH,0C6H,03CH,032H,0C7H,000H,012H,07EH
                db  0FEH,083H,0CAH,072H,0D1H,0CDH,099H,0CFH,0C0H,07AH,0B3H,0C2H,02FH,0D0H,03CH,0C3H,076H,0D1H,0CDH,02CH,0CDH,0C0H,02AH,06BH,003H,0EBH,02AH,067H,003H,022H,0D7H,000H
                db  0EBH,0C0H,07EH,0B7H,0C2H,08BH,0D1H,023H,023H,023H,023H,023H,0C3H,07EH,0D0H,0CDH,0DEH,0E0H,0C0H,0B7H,0CAH,089H,0CFH,0C3H,08CH,0C6H,011H,00AH,000H,0D5H,0CAH,0BCH
                db  0D1H,0CDH,08EH,0CFH,0EBH,0E3H,0CAH,0BDH,0D1H,0EBH,0CDH,0D1H,0CBH,02CH,0EBH,02AH,061H,003H,0EBH,0CAH,0BCH,0D1H,0CDH,099H,0CFH,0C2H,07BH,0C6H,0EBH,07CH,0B5H,0CAH
                db  089H,0CFH,022H,061H,003H,032H,05EH,003H,0E1H,022H,05FH,003H,0C1H,0C3H,01EH,0C7H,0CDH,0D7H,0D4H,07EH,0FEH,02CH,0CCH,02CH,0CDH,0FEH,089H,0CAH,0E3H,0D1H,0CDH,0D1H
                db  0CBH,0CFH,02BH,0E5H,0CDH,030H,0ECH,0E1H,0CAH,0FBH,0D1H,0CDH,02CH,0CDH,0FEH,00EH,0CAH,02CH,0D0H,0FEH,00DH,0CAH,02CH,0D0H,0C3H,013H,0CDH,016H,001H,0CDH,07EH,0D0H
                db  0B7H,0C8H,0CDH,02CH,0CDH,0FEH,0A2H,0C2H,0FDH,0D1H,015H,0C2H,0FDH,0D1H,0C3H,0EBH,0D1H,03EH,001H,032H,0CAH,000H,02BH,0CDH,02CH,0CDH,0CCH,08BH,0D2H,0CAH,00EH,0D3H
                db  0FEH,0D9H,0CAH,0A8H,0E3H,0FEH,0D0H,0CAH,0D7H,0D2H,0FEH,0D4H,0CAH,0D7H,0D2H,0E5H,0FEH,02CH,0CAH,0ADH,0D2H,0FEH,03BH,0CAH,007H,0D3H,0C1H,0CDH,0D7H,0D4H,0E5H,0CDH
                db  008H,0D8H,0CAH,077H,0D2H,0CDH,015H,0F3H,0CDH,061H,0DCH,02AH,0A6H,003H,03AH,0CAH,000H,0B7H,0CAH,062H,0D2H,03AH,0C9H,000H,086H,0E5H,021H,0CFH,000H,0BEH,0E1H,0C3H
                db  06BH,0D2H,03AH,0D1H,000H,047H,03AH,01DH,003H,086H,0B8H,0D4H,08BH,0D2H,0CDH,0ABH,0DCH,03EH,020H,0CDH,0DCH,0CBH,0B7H,0CCH,0ABH,0DCH,0E1H,0C3H,016H,0D2H,03AH,01DH
                db  003H,0B7H,0C8H,0C3H,08BH,0D2H,036H,000H,021H,019H,002H,03AH,0CAH,000H,0B7H,0C2H,087H,000H,03EH,00DH,0CDH,0DCH,0CBH,03EH,00AH,0CDH,0DCH,0CBH,03AH,0C6H,000H,03DH
                db  032H,01DH,003H,0C8H,0F5H,0AFH,0CDH,0DCH,0CBH,0F1H,0C3H,09FH,0D2H,03AH,0CAH,000H,0B7H,0CAH,0C0H,0D2H,03AH,0C9H,000H,0E5H,021H,0D0H,000H,0BEH,0E1H,0C3H,0C8H,0D2H
                db  03AH,0D2H,000H,047H,03AH,01DH,003H,0B8H,0D4H,08BH,0D2H,0D2H,007H,0D3H,0D6H,00EH,0D2H,0CEH,0D2H,02FH,0C3H,0FCH,0D2H,0F5H,0CDH,0DBH,0E0H,0CDH,0D1H,0CBH,029H,02BH
                db  0F1H,0D6H,0D4H,0E5H,0CAH,0F7H,0D2H,03AH,0CAH,000H,0B7H,0CAH,0F4H,0D2H,03AH,0C9H,000H,0C3H,0F7H,0D2H,03AH,01DH,003H,02FH,083H,0D2H,007H,0D3H,03CH,047H,03EH,020H
                db  0CDH,0DCH,0CBH,005H,0C2H,000H,0D3H,0E1H,0CDH,02CH,0CDH,0C3H,01DH,0D2H,0AFH,032H,0CAH,000H,0C9H,0CDH,0D1H,0CBH,085H,0CDH,05FH,0D3H,0CDH,073H,0D8H,0CDH,0A1H,0EDH
                db  0D5H,0E5H,0CDH,006H,0CBH,0D1H,0C1H,0DAH,007H,0CEH,0C5H,0D5H,006H,000H,0CDH,064H,0DCH,0E1H,0AFH,0C3H,0C1H,0D0H,03FH,052H,045H,044H,04FH,020H,046H,052H,04FH,04DH
                db  020H,053H,054H,041H,052H,054H,00DH,00AH,000H,03AH,05AH,003H,0B7H,0C2H,075H,0C6H,0C1H,021H,036H,0D3H,0CDH,0A8H,0DCH,02AH,063H,003H,0C9H,001H,074H,0D3H,0C5H,0FEH
                db  022H,03EH,000H,032H,0D4H,000H,0C0H,0CDH,062H,0DCH,0CDH,0D1H,0CBH,03BH,0E5H,0CDH,0ABH,0DCH,0E1H,0C9H,0E5H,0CDH,0B8H,0C8H,0C1H,0DAH,007H,0CEH,023H,07EH,0B7H,02BH
                db  0C5H,0CAH,07DH,0D0H,036H,02CH,0C3H,08EH,0D3H,0E5H,02AH,07CH,003H,0F6H,0AFH,032H,05AH,003H,0E3H,0C3H,09AH,0D3H,0CDH,0D1H,0CBH,02CH,0CDH,073H,0D8H,0E3H,0D5H,07EH
                db  0FEH,02CH,0CAH,0C2H,0D3H,03AH,05AH,003H,0B7H,0C2H,02CH,0D4H,03EH,03FH,0CDH,0DCH,0CBH,0CDH,0B8H,0C8H,0D1H,0C1H,0DAH,007H,0CEH,023H,07EH,0B7H,02BH,0C5H,0CAH,07DH
                db  0D0H,0D5H,0CDH,008H,0D8H,0F5H,0C2H,0E5H,0D3H,0CDH,02CH,0CDH,057H,047H,0FEH,022H,0CAH,0D8H,0D3H,016H,03AH,006H,02CH,02BH,0CDH,065H,0DCH,0F1H,0EBH,021H,0F4H,0D3H
                db  0E3H,0D5H,0C3H,0C2H,0D0H,0CDH,02CH,0CDH,0F1H,0F5H,001H,0DBH,0D3H,0C5H,0DAH,091H,0F1H,0D2H,08AH,0F1H,02BH,0CDH,02CH,0CDH,0CAH,000H,0D4H,0FEH,02CH,0C2H,049H,0D3H
                db  0E3H,02BH,0CDH,02CH,0CDH,0C2H,096H,0D3H,0D1H,03AH,05AH,003H,0B7H,0EBH,0C2H,0DBH,0CDH,0D5H,0B6H,021H,01BH,0D4H,0C4H,0A8H,0DCH,0E1H,0C9H,03FH,045H,058H,054H,052H
                db  041H,020H,049H,047H,04EH,04FH,052H,045H,044H,00DH,00AH,000H,0CDH,07EH,0D0H,0B7H,0C2H,045H,0D4H,023H,07EH,023H,0B6H,01EH,004H,0CAH,08CH,0C6H,023H,05EH,023H,056H
                db  0EBH,022H,057H,003H,0EBH,0CDH,02CH,0CDH,0FEH,084H,0C2H,02CH,0D4H,0C3H,0C2H,0D3H,011H,000H,000H,0C4H,073H,0D8H,022H,05BH,003H,0CDH,00FH,0C6H,0C2H,081H,0C6H,0F9H
                db  022H,065H,003H,0D5H,07EH,023H,0F5H,0D5H,07EH,023H,0B7H,0FAH,085H,0D4H,0CDH,050H,0ECH,0E3H,0E5H,0CDH,094H,0E9H,0E1H,0CDH,06AH,0ECH,0E1H,0CDH,061H,0ECH,0E5H,0CDH
                db  0B0H,0ECH,0C3H,0AEH,0D4H,023H,023H,023H,023H,04EH,023H,046H,023H,0E3H,05EH,023H,056H,0E5H,069H,060H,0CDH,08EH,0EEH,03AH,01FH,003H,0FEH,004H,0CAH,042H,0EAH,0EBH
                db  0E1H,072H,02BH,073H,0E1H,0D5H,05EH,023H,056H,023H,0E3H,0CDH,0DDH,0ECH,0E1H,0C1H,090H,0CDH,061H,0ECH,0CAH,0C0H,0D4H,0EBH,022H,0D7H,000H,069H,060H,0C3H,0C5H,0CCH
                db  0F9H,022H,065H,003H,02AH,05BH,003H,07EH,0FEH,02CH,0C2H,0C9H,0CCH,0CDH,02CH,0CDH,0CDH,053H,0D4H,0CDH,0D1H,0CBH,028H,02BH,016H,000H,0D5H,00EH,001H,0CDH,043H,0C6H
                db  0CDH,059H,0D6H,022H,070H,003H,02AH,070H,003H,0C1H,07EH,016H,000H,0D6H,0EFH,0DAH,009H,0D5H,0FEH,003H,0D2H,009H,0D5H,0FEH,001H,017H,0AAH,0BAH,057H,0DAH,07BH,0C6H
                db  022H,055H,003H,0CDH,02CH,0CDH,0C3H,0EDH,0D4H,07AH,0B7H,0C2H,09EH,0D5H,07EH,022H,055H,003H,0D6H,0F2H,0D8H,0FEH,00CH,0D0H,05FH,03AH,01FH,003H,0D6H,003H,0B3H,0CAH
                db  0B8H,0DDH,021H,098H,0C3H,019H,078H,056H,0BAH,0D0H,0C5H,001H,0E6H,0D4H,0C5H,07AH,0FEH,07FH,0CAH,084H,0D5H,0FEH,051H,0DAH,092H,0D5H,0E6H,0FEH,0FEH,07AH,0CAH,092H
                db  0D5H,021H,0A6H,003H,0B7H,03AH,01FH,003H,03DH,03DH,03DH,0CAH,060H,0DBH,04EH,023H,046H,0C5H,0FAH,072H,0D5H,023H,04EH,023H,046H,0C5H,0F5H,0B7H,0E2H,071H,0D5H,0F1H
                db  023H,0DAH,067H,0D5H,021H,0A2H,003H,04EH,023H,046H,023H,0C5H,04EH,023H,046H,0C5H,006H,0F1H,0DAH,06AH,0DBH,0C6H,003H,04BH,047H,0C5H,001H,0BBH,0D5H,0C5H,02AH,055H
                db  003H,0C3H,0DAH,0D4H,0CDH,05AH,0EDH,0CDH,043H,0ECH,001H,0D1H,0F7H,016H,07FH,0C3H,07DH,0D5H,0D5H,0CDH,024H,0EDH,0D1H,0E5H,001H,019H,0D8H,0C3H,07DH,0D5H,078H,0FEH
                db  064H,0D0H,0C5H,0D5H,011H,004H,064H,021H,0E6H,0D7H,0E5H,0CDH,008H,0D8H,0C2H,041H,0D5H,02AH,0A6H,003H,0E5H,001H,0B9H,0D7H,0C3H,07DH,0D5H,0C1H,079H,032H,020H,003H
                db  078H,0FEH,008H,0CAH,0EEH,0D5H,03AH,01FH,003H,0FEH,008H,0CAH,016H,0D6H,057H,078H,0FEH,004H,0CAH,02AH,0D6H,07AH,0FEH,003H,0CAH,0A5H,0EDH,0D2H,035H,0D6H,021H,0C2H
                db  0C3H,006H,000H,009H,009H,04EH,023H,046H,0D1H,02AH,0A6H,003H,0C5H,0C9H,0CDH,086H,0EDH,0CDH,09EH,0ECH,0E1H,022H,0A4H,003H,0E1H,022H,0A2H,003H,0C1H,0D1H,0CDH,053H
                db  0ECH,0CDH,086H,0EDH,021H,0AEH,0C3H,03AH,020H,003H,007H,0C5H,04FH,006H,000H,009H,0C1H,07EH,023H,066H,06FH,0E9H,0C5H,0CDH,09EH,0ECH,0F1H,032H,01FH,003H,0FEH,004H
                db  0CAH,0FCH,0D5H,0E1H,022H,0A6H,003H,0C3H,001H,0D6H,0CDH,05AH,0EDH,0C1H,0D1H,021H,0B8H,0C3H,0C3H,007H,0D6H,0E1H,0CDH,043H,0ECH,0CDH,07AH,0EDH,0CDH,05EH,0ECH,0E1H
                db  022H,0A8H,003H,0E1H,022H,0A6H,003H,0C3H,02FH,0D6H,0E5H,0EBH,0CDH,07AH,0EDH,0E1H,0CDH,043H,0ECH,0CDH,07AH,0EDH,0C3H,036H,0EBH,0CDH,02CH,0CDH,01EH,016H,0CAH,08CH
                db  0C6H,0DAH,091H,0F1H,0FEH,020H,0DAH,0A7H,0CDH,03CH,0CAH,070H,0D7H,03DH,0CDH,07AH,0CFH,0D2H,0F8H,0D6H,0FEH,0F2H,0CAH,059H,0D6H,0FEH,0F3H,0CAH,0EAH,0D6H,0FEH,022H
                db  0CAH,062H,0DCH,0FEH,0D5H,0CAH,0F3H,0D7H,0FEH,026H,0CAH,00DH,0D7H,0FEH,0D7H,0C2H,09EH,0D6H,0CDH,02CH,0CDH,03AH,0C7H,000H,0E5H,0CDH,0A9H,0DAH,0E1H,0C9H,0FEH,0D6H
                db  0C2H,0AFH,0D6H,0CDH,02CH,0CDH,0E5H,02AH,067H,003H,0CDH,060H,0EFH,0E1H,0C9H,0FEH,0DCH,0C2H,0CEH,0D6H,0CDH,02CH,0CDH,0CDH,0D1H,0CBH,028H,0CDH,073H,0D8H,0CDH,0D1H
                db  0CBH,029H,0E5H,0EBH,07CH,0B5H,0CAH,089H,0CFH,0CDH,042H,0EDH,0E1H,0C9H,0FEH,0D2H,0CAH,0AFH,0DAH,0FEH,0DAH,0CAH,026H,0DFH,0FEH,0D8H,0CAH,05EH,0DEH,0FEH,0D3H,0CAH
                db  027H,0DBH,0CDH,0D3H,0D4H,0CDH,0D1H,0CBH,029H,0C9H,016H,07DH,0CDH,0DAH,0D4H,02AH,070H,003H,0E5H,0CDH,015H,0ECH,0E1H,0C9H,0CDH,073H,0D8H,0E5H,0EBH,022H,0A6H,003H
                db  0CDH,008H,0D8H,0C4H,098H,0ECH,0E1H,0C9H,0FEH,026H,0C2H,099H,0CFH,011H,000H,000H,0CDH,02CH,0CDH,0FEH,04FH,0CAH,04AH,0D7H,0FEH,048H,0C2H,049H,0D7H,006H,005H,023H
                db  0CDH,07AH,0CFH,0EBH,0D2H,034H,0D7H,0FEH,03AH,0D2H,06BH,0D7H,0D6H,030H,0DAH,06BH,0D7H,0C3H,03BH,0D7H,0FEH,047H,0D2H,06BH,0D7H,0D6H,037H,029H,029H,029H,029H,0B5H
                db  06FH,005H,0CAH,042H,0EAH,0EBH,0C3H,01FH,0D7H,02BH,0CDH,02CH,0CDH,0EBH,0D2H,06BH,0D7H,0FEH,038H,0D2H,07BH,0C6H,001H,042H,0EAH,0C5H,029H,0D8H,029H,0D8H,029H,0D8H
                db  0C1H,006H,000H,0D6H,030H,04FH,009H,0EBH,0C3H,04AH,0D7H,0CDH,042H,0EDH,0EBH,0C9H,023H,07EH,0D6H,081H,006H,000H,007H,04FH,0C5H,0CDH,02CH,0CDH,079H,0FEH,005H,0D2H
                db  09BH,0D7H,0CDH,0D3H,0D4H,0CDH,0D1H,0CBH,02CH,0CDH,0A1H,0EDH,0EBH,02AH,0A6H,003H,0E3H,0E5H,0EBH,0CDH,0DEH,0E0H,0EBH,0E3H,0C3H,0B0H,0D7H,0CDH,0E2H,0D6H,0E3H,07DH
                db  0FEH,00CH,0DAH,0ACH,0D7H,0FEH,01BH,0E5H,0DCH,05AH,0EDH,0E1H,011H,0F6H,0D6H,0D5H,001H,062H,0C1H,009H,04EH,023H,066H,069H,0E9H,0CDH,001H,0DEH,07EH,023H,04EH,023H
                db  046H,0D1H,0C5H,0F5H,0CDH,008H,0DEH,0D1H,05EH,023H,04EH,023H,046H,0E1H,07BH,0B2H,0C8H,07AH,0D6H,001H,0D8H,0AFH,0BBH,03CH,0D0H,015H,01DH,00AH,0BEH,023H,003H,0CAH
                db  0CEH,0D7H,03FH,0C3H,0FAH,0EBH,03CH,08FH,0C1H,0A0H,0C6H,0FFH,09FH,0CDH,029H,0ECH,0C3H,0E6H,0D4H,016H,05AH,0CDH,0DAH,0D4H,0CDH,024H,0EDH,07DH,02FH,06FH,07CH,02FH
                db  067H,022H,0A6H,003H,0C1H,0C3H,0E6H,0D4H,03AH,01FH,003H,0FEH,008H,0D2H,015H,0D8H,0D6H,003H,0B7H,037H,0C9H,0D6H,003H,0B7H,0C9H,0C5H,0CDH,024H,0EDH,0F1H,0D1H,0FEH
                db  07AH,0CAH,06AH,0EFH,0FEH,07BH,0CAH,005H,0EFH,001H,0ABH,0DAH,0C5H,0FEH,046H,0C2H,038H,0D8H,07BH,0B5H,06FH,07CH,0B2H,0C9H,0FEH,050H,0C2H,043H,0D8H,07BH,0A5H,06FH
                db  07CH,0A2H,0C9H,0FEH,03CH,0C2H,04EH,0D8H,07BH,0ADH,06FH,07CH,0AAH,0C9H,0FEH,032H,0C2H,05BH,0D8H,07BH,0ADH,02FH,06FH,07CH,0AAH,02FH,0C9H,07DH,02FH,0A3H,02FH,06FH
                db  07CH,02FH,0A2H,02FH,0C9H,02BH,0CDH,02CH,0CDH,0C8H,0CDH,0D1H,0CBH,02CH,001H,065H,0D8H,0C5H,0F6H,0AFH,032H,01EH,003H,046H,0CDH,07AH,0CFH,0DAH,07BH,0C6H,0AFH,04FH
                db  0CDH,02CH,0CDH,0DAH,08CH,0D8H,0CDH,07AH,0CFH,0DAH,099H,0D8H,04FH,0CDH,02CH,0CDH,0DAH,08DH,0D8H,0CDH,07AH,0CFH,0D2H,08DH,0D8H,011H,0C0H,0D8H,0D5H,016H,002H,0FEH
                db  025H,0C8H,014H,0FEH,024H,0C8H,014H,0FEH,021H,0C8H,016H,008H,0FEH,023H,0C8H,078H,0D6H,041H,0E6H,07FH,05FH,016H,000H,0E5H,021H,07EH,003H,019H,056H,0E1H,02BH,0C9H
                db  07AH,032H,01FH,003H,0CDH,02CH,0CDH,03AH,059H,003H,03DH,0CAH,0A6H,0D9H,0F2H,0DCH,0D8H,07EH,0D6H,028H,0CAH,073H,0D9H,0D6H,033H,0CAH,073H,0D9H,0AFH,032H,059H,003H
                db  0E5H,0D5H,02AH,076H,003H,0EBH,02AH,078H,003H,0CDH,0CBH,0CBH,0E1H,0CAH,012H,0D9H,01AH,06FH,0BCH,013H,0C2H,003H,0D9H,01AH,0B9H,0C2H,003H,0D9H,013H,01AH,0B8H,0CAH
                db  059H,0D9H,03EH,013H,013H,0E5H,026H,000H,019H,0C3H,0E5H,0D8H,057H,05FH,0F1H,0F1H,0E3H,0C9H,07CH,0E1H,0E3H,0F5H,0D5H,011H,0BEH,0D6H,0CDH,0CBH,0CBH,0CAH,00CH,0D9H
                db  011H,0FBH,0D6H,0CDH,0CBH,0CBH,0D1H,0CAH,05CH,0D9H,0F1H,0E3H,0E5H,0C5H,04FH,006H,000H,0C5H,003H,003H,003H,02AH,07AH,003H,0E5H,009H,0C1H,0E5H,0CDH,032H,0C6H,0E1H
                db  022H,07AH,003H,060H,069H,022H,078H,003H,02BH,036H,000H,0CDH,0CBH,0CBH,0C2H,048H,0D9H,0D1H,073H,023H,0D1H,073H,023H,072H,0EBH,013H,0E1H,0C9H,032H,0A9H,003H,0C1H
                db  067H,06FH,022H,0A6H,003H,0CDH,008H,0D8H,0C2H,026H,0EBH,021H,003H,0C6H,022H,0A6H,003H,0E1H,0C9H,0E5H,02AH,01EH,003H,0E3H,057H,0D5H,0C5H,0CDH,082H,0CFH,0C1H,0F1H
                db  0EBH,0E3H,0E5H,0EBH,03CH,057H,07EH,0FEH,02CH,0CAH,079H,0D9H,0FEH,05DH,0C2H,097H,0D9H,0CDH,02CH,0CDH,0C3H,09BH,0D9H,0CDH,0D1H,0CBH,029H,022H,070H,003H,0E1H,022H
                db  01EH,003H,01EH,000H,0D5H,011H,0E5H,0F5H,02AH,078H,003H,03EH,019H,0EBH,02AH,07AH,003H,0EBH,0CDH,0CBH,0CBH,03AH,01FH,003H,0CAH,0E6H,0D9H,0BEH,023H,0C2H,0C9H,0D9H
                db  07EH,0B9H,023H,0C2H,0CAH,0D9H,07EH,0B8H,03EH,023H,023H,05EH,023H,056H,023H,0C2H,0ACH,0D9H,03AH,01EH,003H,0B7H,0C2H,084H,0C6H,0F1H,0CAH,0C7H,0CEH,096H,0CAH,043H
                db  0DAH,01EH,009H,0C3H,08CH,0C6H,077H,023H,05FH,016H,000H,0F1H,0CAH,089H,0CFH,071H,023H,070H,023H,04FH,0CDH,043H,0C6H,023H,023H,022H,055H,003H,071H,023H,03AH,01EH
                db  003H,017H,079H,001H,00BH,000H,0D2H,00BH,0DAH,0C1H,003H,071H,023H,070H,023H,0F5H,0CDH,062H,0EEH,0F1H,03DH,0C2H,003H,0DAH,0F5H,042H,04BH,0EBH,019H,0DAH,0E1H,0D9H
                db  0CDH,04CH,0C6H,022H,07AH,003H,02BH,036H,000H,0CDH,0CBH,0CBH,0C2H,026H,0DAH,003H,057H,02AH,055H,003H,05EH,0EBH,029H,009H,0EBH,02BH,02BH,073H,023H,072H,023H,0F1H
                db  0DAH,078H,0DAH,047H,04FH,07EH,023H,016H,0E1H,05EH,023H,056H,023H,0E3H,0F5H,0CDH,0CBH,0CBH,0D2H,0E1H,0D9H,0CDH,062H,0EEH,019H,0F1H,03DH,044H,04DH,0C2H,048H,0DAH
                db  03AH,01FH,003H,044H,04DH,029H,0D6H,004H,0DAH,070H,0DAH,029H,0CAH,075H,0DAH,029H,0B7H,0E2H,075H,0DAH,009H,0C1H,009H,0EBH,02AH,070H,003H,0C9H,02AH,07AH,003H,0EBH
                db  021H,000H,000H,039H,0CDH,008H,0D8H,0C2H,097H,0DAH,0CDH,004H,0DEH,0CDH,0EBH,0DCH,02AH,0D5H,000H,0EBH,02AH,053H,003H,07DH,093H,06FH,07CH,09AH,067H,0C3H,060H,0EFH
                db  03AH,0C9H,000H,0C3H,0A9H,0DAH,03AH,01DH,003H,06FH,0AFH,067H,0C3H,042H,0EDH,0CDH,0CEH,0DAH,0D5H,0CDH,0E2H,0D6H,0E3H,04EH,023H,046H,021H,026H,0EBH,0E5H,0C5H,03AH
                db  01FH,003H,0F5H,0FEH,003H,0CCH,004H,0DEH,0F1H,0EBH,021H,0A6H,003H,0C9H,0CDH,02CH,0CDH,001H,000H,000H,0FEH,01BH,0D2H,0E6H,0DAH,0FEH,011H,0DAH,0E6H,0DAH,0CDH,02CH
                db  0CDH,03AH,026H,003H,017H,04FH,0EBH,021H,0B2H,000H,009H,0EBH,0C9H,0CDH,0CEH,0DAH,0D5H,0CDH,0D1H,0CBH,0F0H,0CDH,0BFH,0E0H,0E3H,073H,023H,072H,0E1H,0C9H,0FEH,0D2H
                db  0CAH,0EDH,0DAH,0CDH,018H,0DCH,0CDH,00AH,0DCH,0EBH,073H,023H,072H,0EBH,07EH,0FEH,028H,0C2H,07EH,0D0H,0CDH,02CH,0CDH,0CDH,073H,0D8H,07EH,0FEH,029H,0CAH,07EH,0D0H
                db  0CDH,0D1H,0CBH,02CH,0C3H,017H,0DBH,0CDH,018H,0DCH,03AH,01FH,003H,0B7H,0F5H,022H,070H,003H,0EBH,07EH,023H,066H,06FH,0B4H,0CAH,087H,0C6H,07EH,0FEH,028H,0C2H,0A0H
                db  0DBH,0CDH,02CH,0CDH,022H,055H,003H,0C3H,04EH,0DBH,0CDH,0D1H,0CBH,02CH,00EH,004H,0CDH,043H,0C6H,03EH,080H,032H,059H,003H,0CDH,073H,0D8H,0EBH,037H,0C3H,045H,0D5H
                db  0D2H,0A5H,0EDH,0D5H,0EBH,0CDH,087H,0DCH,0D1H,0AFH,0E5H,0F5H,0EBH,07EH,0FEH,029H,0C2H,04AH,0DBH,02AH,070H,003H,0CDH,0D1H,0CBH,028H,0E5H,02AH,055H,003H,0CDH,073H
                db  0D8H,0E3H,0CDH,0B8H,0D0H,07EH,0FEH,029H,0CAH,097H,0DBH,0CDH,0D1H,0CBH,02CH,0E3H,0CDH,0D1H,0CBH,02CH,0C3H,07EH,0DBH,0CDH,02CH,0CDH,0E3H,0CDH,0D1H,0CBH,029H,03EH
                db  0D5H,0CDH,0D1H,0CBH,0F0H,0CDH,0D7H,0D4H,02BH,0CDH,02CH,0CDH,0C2H,07BH,0C6H,0D1H,0CDH,008H,0D8H,0CAH,0F6H,0DBH,0F1H,0CAH,0FAH,0DBH,0D2H,0DAH,0DBH,0E1H,0C1H,070H
                db  02BH,071H,0FAH,0B6H,0DBH,02BH,0C1H,070H,02BH,071H,0E2H,0B6H,0DBH,02BH,0C1H,070H,02BH,071H,02BH,0C1H,070H,02BH,071H,0C3H,0B6H,0DBH,0D5H,0F5H,0CDH,008H,0D8H,011H
                db  050H,003H,0CCH,087H,0DCH,0F1H,0FEH,0E5H,0E6H,007H,021H,0A4H,0C3H,04FH,006H,000H,009H,0CDH,0B3H,0D7H,0E1H,0C9H,021H,050H,003H,0E5H,0CDH,022H,0DEH,07EH,022H,030H
                db  003H,0E1H,077H,023H,071H,023H,070H,0C3H,0B6H,0DBH,0E5H,02AH,0D7H,000H,023H,07CH,0B5H,0E1H,0C0H,01EH,00CH,0C3H,08CH,0C6H,0CDH,0D1H,0CBH,0D3H,03EH,080H,032H,059H
                db  003H,0B6H,047H,0C3H,078H,0D8H,0CDH,074H,0F7H,0C3H,035H,0DCH,0CDH,077H,0F7H,0C3H,035H,0DCH,0CDH,015H,0F3H,0CDH,061H,0DCH,0CDH,004H,0DEH,001H,05AH,0DEH,0C5H,07EH
                db  023H,0E5H,0CDH,0C1H,0DCH,0E1H,04EH,023H,046H,0CDH,056H,0DCH,0E5H,06FH,0CDH,0F7H,0DDH,0D1H,0C9H,0CDH,0C1H,0DCH,021H,050H,003H,0E5H,077H,023H,073H,023H,072H,0E1H
                db  0C9H,02BH,006H,022H,050H,0E5H,00EH,0FFH,023H,07EH,00CH,0B7H,0CAH,077H,0DCH,0BAH,0CAH,077H,0DCH,0B8H,0C2H,068H,0DCH,0FEH,022H,0CCH,02CH,0CDH,0E3H,023H,0EBH,079H
                db  0CDH,056H,0DCH,011H,050H,003H,03EH,0D5H,02AH,030H,003H,022H,0A6H,003H,03EH,003H,032H,01FH,003H,0CDH,073H,0ECH,011H,053H,003H,0CDH,0CBH,0CBH,022H,030H,003H,0E1H
                db  07EH,0C0H,01EH,010H,0C3H,08CH,0C6H,023H,0CDH,061H,0DCH,0CDH,004H,0DEH,0CDH,063H,0ECH,014H,015H,0C8H,00AH,0CDH,0DCH,0CBH,0FEH,00DH,0CCH,09CH,0D2H,003H,0C3H,0B2H
                db  0DCH,0B7H,00EH,0F1H,0F5H,02AH,0D5H,000H,0EBH,02AH,053H,003H,02FH,04FH,006H,0FFH,009H,023H,0CDH,0CBH,0CBH,0DAH,0DFH,0DCH,022H,053H,003H,023H,0EBH,0F1H,0C9H,0F1H
                db  01EH,00EH,0CAH,08CH,0C6H,0BFH,0F5H,001H,0C3H,0DCH,0C5H,02AH,02EH,003H,022H,053H,003H,021H,000H,000H,0E5H,02AH,0D5H,000H,0E5H,021H,032H,003H,0EBH,02AH,030H,003H
                db  0EBH,0CDH,0CBH,0CBH,022H,0D0H,0CCH,001H,0FCH,0DCH,0C2H,060H,0DDH,02AH,076H,003H,0EBH,02AH,078H,003H,0EBH,0CDH,0CBH,0CBH,0CAH,030H,0DDH,07EH,023H,023H,023H,0FEH
                db  003H,0C2H,028H,0DDH,0CDH,061H,0DDH,0AFH,05FH,016H,000H,019H,0C3H,010H,0DDH,0C1H,0EBH,02AH,07AH,003H,0EBH,0CDH,0CBH,0CBH,0CAH,085H,0DDH,07EH,023H,0CDH,061H,0ECH
                db  0E5H,009H,0FEH,003H,0C2H,02FH,0DDH,022H,055H,003H,0E1H,04EH,006H,000H,009H,009H,023H,0EBH,02AH,055H,003H,0EBH,0CDH,0CBH,0CBH,0CAH,030H,0DDH,001H,051H,0DDH,002H
                db  0C5H,0AFH,0B6H,023H,05EH,023H,056H,023H,0C8H,044H,04DH,02AH,053H,003H,0CDH,0CBH,0CBH,060H,069H,0D8H,0E1H,0E3H,0CDH,0CBH,0CBH,0E3H,0E5H,060H,069H,0D0H,0C1H,0F1H
                db  0F1H,0E5H,0D5H,0C5H,0C9H,0D1H,0E1H,07DH,0B4H,0C8H,02BH,046H,02BH,04EH,0E5H,02BH,06EH,026H,000H,009H,050H,059H,02BH,044H,04DH,02AH,053H,003H,07CH,0B8H,0C2H,0A6H
                db  0DDH,07DH,0B9H,0CAH,0ACH,0DDH,0CDH,035H,0C6H,0C3H,0AEH,0DDH,042H,04BH,0E1H,071H,023H,070H,069H,060H,02BH,0C3H,0EEH,0DCH,0C5H,0E5H,02AH,0A6H,003H,0E3H,0CDH,059H
                db  0D6H,0E3H,0CDH,0A1H,0EDH,07EH,0E5H,02AH,0A6H,003H,0E5H,086H,01EH,00FH,0DAH,08CH,0C6H,0CDH,053H,0DCH,0D1H,0CDH,008H,0DEH,0E3H,0CDH,007H,0DEH,0E5H,02AH,051H,003H
                db  0EBH,0CDH,0EFH,0DDH,0CDH,0EFH,0DDH,021H,0E9H,0D4H,0E3H,0E5H,0C3H,083H,0DCH,0E1H,0E3H,07EH,023H,04EH,023H,046H,06FH,02CH,02DH,0C8H,00AH,012H,003H,013H,0C3H,0F8H
                db  0DDH,0CDH,0A1H,0EDH,02AH,0A6H,003H,0EBH,0CDH,022H,0DEH,0EBH,0C0H,0D5H,050H,059H,01BH,04EH,02AH,053H,003H,0CDH,0CBH,0CBH,0C2H,020H,0DEH,047H,009H,022H,053H,003H
                db  0E1H,0C9H,02AH,030H,003H,02BH,046H,02BH,04EH,02BH,0CDH,0CBH,0CBH,0C0H,022H,030H,003H,0C9H,001H,0A9H,0DAH,0C5H,0CDH,001H,0DEH,0AFH,057H,07EH,0B7H,0C9H,001H,0A9H
                db  0DAH,0C5H,0CDH,036H,0DEH,0CAH,089H,0CFH,023H,05EH,023H,056H,01AH,0C9H,03EH,001H,0CDH,053H,0DCH,0CDH,0E1H,0E0H,02AH,051H,003H,073H,0C1H,0C3H,083H,0DCH,0CDH,02CH
                db  0CDH,0CDH,0D1H,0CBH,028H,0CDH,0DEH,0E0H,0D5H,0CDH,0D1H,0CBH,02CH,0CDH,0D7H,0D4H,0CDH,0D1H,0CBH,029H,0E3H,0E5H,0CDH,008H,0D8H,0CAH,082H,0DEH,0CDH,0E1H,0E0H,0C3H
                db  085H,0DEH,0CDH,042H,0DEH,0D1H,0CDH,08EH,0DEH,0CDH,0E1H,0E0H,03EH,020H,0F5H,07BH,0CDH,053H,0DCH,05FH,0F1H,01CH,01DH,0CAH,05AH,0DEH,02AH,051H,003H,077H,023H,01DH
                db  0C2H,09DH,0DEH,0C3H,05AH,0DEH,0CDH,017H,0DFH,0AFH,0E3H,04FH,03EH,0E5H,0E5H,07EH,0B8H,0DAH,0B6H,0DEH,078H,011H,00EH,000H,0C5H,0CDH,0C1H,0DCH,0C1H,0E1H,0E5H,023H
                db  046H,023H,066H,068H,006H,000H,009H,044H,04DH,0CDH,056H,0DCH,06FH,0CDH,0F7H,0DDH,0D1H,0CDH,008H,0DEH,0C3H,083H,0DCH,0CDH,017H,0DFH,0D1H,0D5H,01AH,090H,0C3H,0AAH
                db  0DEH,0EBH,07EH,0CDH,01CH,0DFH,0C5H,0CDH,049H,0E0H,0F1H,0E3H,001H,0AEH,0DEH,0C5H,03DH,0BEH,006H,000H,0D0H,04FH,07EH,091H,0BBH,047H,0D8H,043H,0C9H,0CDH,036H,0DEH
                db  0CAH,0A9H,0DAH,05FH,023H,07EH,023H,066H,06FH,0E5H,019H,046H,072H,0E3H,0C5H,07EH,0CDH,08AH,0F1H,0C1H,0E1H,070H,0C9H,0EBH,0CDH,0D1H,0CBH,029H,0C1H,0D1H,0C5H,043H
                db  004H,005H,0C0H,0C3H,089H,0CFH,0CDH,02CH,0CDH,0CDH,0D3H,0D4H,0CDH,008H,0D8H,03EH,001H,0F5H,0CAH,048H,0DFH,0F1H,0CDH,0E1H,0E0H,0B7H,0CAH,089H,0CFH,0F5H,0CDH,0D1H
                db  0CBH,02CH,0CDH,0D7H,0D4H,0CDH,0A1H,0EDH,0CDH,0D1H,0CBH,02CH,0E5H,02AH,0A6H,003H,0E3H,0CDH,0D7H,0D4H,0CDH,0D1H,0CBH,029H,0E5H,0CDH,001H,0DEH,0EBH,0C1H,0E1H,0F1H
                db  0C5H,001H,026H,0EBH,0C5H,001H,0A9H,0DAH,0C5H,0F5H,0D5H,0CDH,007H,0DEH,0D1H,0F1H,047H,03DH,04FH,0BEH,03EH,000H,0D0H,07EH,0B7H,0C8H,01AH,0B7H,078H,0C8H,07EH,023H
                db  046H,023H,066H,068H,006H,000H,009H,091H,047H,0C5H,0D5H,0E3H,04EH,023H,05EH,023H,056H,0E1H,0E5H,0D5H,0C5H,01AH,0BEH,0C2H,0B3H,0DFH,013H,00DH,0CAH,0AAH,0DFH,023H
                db  005H,0C2H,095H,0DFH,0D1H,0D1H,0C1H,0D1H,0AFH,0C9H,0E1H,0D1H,0D1H,0C1H,078H,094H,081H,03CH,0C9H,0C1H,0D1H,0E1H,023H,005H,0CAH,0A7H,0DFH,0C3H,092H,0DFH,0FEH,07EH
                db  0C2H,07BH,0C6H,023H,07EH,0FEH,083H,0C2H,07BH,0C6H,023H,0CDH,0D1H,0CBH,028H,0CDH,073H,0D8H,0CDH,0A1H,0EDH,0E5H,0D5H,0EBH,023H,05EH,023H,056H,02AH,0D5H,000H,0CDH
                db  0CBH,0CBH,0DAH,0EFH,0DFH,0E1H,0E5H,0CDH,03FH,0DCH,0E1H,0E5H,0CDH,073H,0ECH,0E1H,0E3H,0CDH,0D1H,0CBH,02CH,0CDH,0DEH,0E0H,0B7H,0CAH,089H,0CFH,0F5H,07EH,0CDH,049H
                db  0E0H,0D5H,0CDH,0D1H,0CBH,0F0H,0CDH,0D7H,0D4H,0E5H,0CDH,001H,0DEH,0EBH,0E1H,0C1H,0F1H,047H,0E3H,0E5H,021H,026H,0EBH,0E3H,079H,0B7H,0C8H,07EH,090H,0DAH,089H,0CFH
                db  03CH,0B9H,0DAH,026H,0E0H,079H,048H,00DH,006H,000H,0D5H,023H,05EH,023H,066H,06BH,009H,047H,0D1H,0EBH,04EH,023H,07EH,023H,066H,06FH,0EBH,079H,0B7H,0C8H,01AH,077H
                db  013H,023H,00DH,0C8H,005H,0C2H,03EH,0E0H,0C9H,01EH,0FFH,0FEH,029H,0CAH,057H,0E0H,0CDH,0D1H,0CBH,02CH,0CDH,0DEH,0E0H,0CDH,0D1H,0CBH,029H,0C9H,0CDH,0E1H,0E0H,032H
                db  08EH,000H,0CDH,08DH,000H,0C3H,0A9H,0DAH,0CDH,0CBH,0E0H,0C3H,04CH,000H,0CDH,0CBH,0E0H,0F5H,01EH,000H,02BH,0CDH,02CH,0CDH,0CAH,082H,0E0H,0CDH,0D1H,0CBH,02CH,0CDH
                db  0DEH,0E0H,0C1H,0CDH,08DH,000H,0ABH,0A0H,0CAH,083H,0E0H,0C9H,0CDH,0CBH,0E0H,0E5H,03AH,04DH,000H,04FH,07BH,0FEH,007H,0D2H,089H,0CFH,007H,007H,007H,007H,0CDH,008H
                db  0FAH,0E1H,0C9H,0CDH,0DEH,0E0H,0FEH,00FH,0DAH,089H,0CFH,032H,0D1H,000H,0D6H,00EH,0D2H,0AEH,0E0H,0C6H,01CH,02FH,03CH,083H,032H,0D2H,000H,0C9H,0CDH,02CH,0CDH,0CDH
                db  0D7H,0D4H,0E5H,0CDH,024H,0EDH,0EBH,0E1H,07AH,0B7H,0C9H,0CDH,0DEH,0E0H,032H,08EH,000H,032H,04DH,000H,0CDH,0D1H,0CBH,02CH,0C3H,0DEH,0E0H,0CDH,02CH,0CDH,0CDH,0D7H
                db  0D4H,0CDH,0C2H,0E0H,0C2H,089H,0CFH,02BH,0CDH,02CH,0CDH,07BH,0C9H,02AH,0D9H,000H,022H,076H,003H,021H,000H,080H,05EH,023H,056H,023H,023H,022H,0D9H,000H,0EBH,022H
                db  02EH,003H,022H,0D5H,000H,0C3H,06BH,0C8H,03EH,002H,032H,0CAH,000H,0C1H,0CDH,01AH,0C8H,0C5H,021H,0FFH,0FFH,022H,0D7H,000H,0CDH,08BH,0D2H,0E1H,0D1H,04EH,023H,046H
                db  023H,078H,0B1H,0CAH,006H,0C7H,0CDH,0E0H,0CDH,0C5H,04EH,023H,046H,023H,0C5H,0E3H,0EBH,0CDH,0CBH,0CBH,0C1H,0DAH,005H,0C7H,0E3H,0E5H,0C5H,0EBH,022H,069H,003H,0CDH
                db  007H,0F3H,03EH,020H,0E1H,0CDH,0DCH,0CBH,0CDH,05CH,0E1H,021H,01AH,002H,001H,012H,0E1H,0C5H,07EH,0B7H,0C8H,0CDH,029H,0E7H,023H,0C3H,052H,0E1H,001H,01AH,002H,016H
                db  0FFH,0C3H,068H,0E1H,003H,023H,015H,0C8H,07EH,0B7H,002H,0C8H,0FEH,00BH,0DAH,076H,0E1H,0FEH,020H,0DAH,0CDH,0E1H,0B7H,0F2H,064H,0E1H,03CH,07EH,0C2H,083H,0E1H,023H
                db  07EH,0E6H,07FH,023H,0FEH,0DBH,0C2H,091H,0E1H,00BH,00BH,00BH,00BH,014H,014H,014H,014H,0FEH,0A2H,0CCH,0D3H,0EDH,0E5H,0C5H,0D5H,021H,0D3H,0C1H,047H,00EH,040H,00CH
                db  023H,054H,05DH,07EH,0B7H,0CAH,09FH,0E1H,023H,0F2H,0A3H,0E1H,07EH,0B8H,0C2H,0A0H,0E1H,0EBH,079H,0D1H,0C1H,0FEH,05BH,0C2H,0BCH,0E1H,07EH,023H,05FH,0E6H,07FH,002H
                db  003H,015H,0CAH,062H,0E6H,0B3H,0F2H,0BAH,0E1H,0E1H,0C3H,068H,0E1H,02BH,0CDH,02CH,0CDH,0D5H,0C5H,0F5H,0CDH,0A7H,0CDH,0F1H,001H,0ECH,0E1H,0C5H,0FEH,00BH,0CAH,074H
                db  0F7H,0FEH,00CH,0CAH,077H,0F7H,02AH,026H,003H,0C3H,015H,0F3H,0C1H,0D1H,03AH,024H,003H,01EH,04FH,0FEH,00BH,0CAH,0FFH,0E1H,0FEH,00CH,01EH,048H,0C2H,00AH,0E2H,03EH
                db  026H,002H,003H,015H,0C8H,07BH,002H,003H,015H,0C8H,03AH,025H,003H,0FEH,004H,01EH,000H,0DAH,01BH,0E2H,01EH,021H,0CAH,01BH,0E2H,01EH,023H,07EH,0FEH,020H,0CCH,068H
                db  0ECH,07EH,023H,0B7H,0CAH,04DH,0E2H,002H,003H,015H,0C8H,03AH,025H,003H,0FEH,004H,0DAH,021H,0E2H,00BH,00AH,003H,0C2H,03EH,0E2H,0FEH,02EH,0CAH,048H,0E2H,0FEH,044H
                db  0CAH,048H,0E2H,0FEH,045H,0C2H,021H,0E2H,01EH,000H,0C3H,021H,0E2H,07BH,0B7H,0CAH,056H,0E2H,002H,003H,015H,0C8H,02AH,022H,003H,0C3H,068H,0E1H,0F5H,0E5H,0CDH,083H
                db  0E8H,0E1H,0F1H,0CDH,01AH,0C8H,0D1H,0C5H,0C5H,0CDH,03AH,0C8H,0D2H,089H,0CFH,054H,05DH,0E3H,0E5H,0CDH,0CBH,0CBH,0D2H,089H,0CFH,021H,004H,0C6H,0CDH,0A8H,0DCH,0C1H
                db  021H,0E7H,0C7H,0E3H,0EBH,02AH,076H,003H,01AH,002H,003H,013H,0CDH,0CBH,0CBH,0C2H,088H,0E2H,060H,069H,022H,076H,003H,0C9H,0DBH,006H,0E6H,001H,0C2H,098H,0E2H,0DBH
                db  007H,0C9H,0CDH,0A5H,0E2H,0F5H,0DBH,006H,0E6H,080H,0C2H,0A6H,0E2H,0F1H,0D3H,007H,0C9H,006H,001H,0FEH,0F4H,0CAH,0B6H,0CEH,0CDH,0D7H,0D4H,0E5H,0CDH,042H,0DEH,03EH
                db  0D3H,006H,00AH,0CDH,0A5H,0E2H,005H,0C2H,0C3H,0E2H,01AH,0CDH,0A5H,0E2H,0CDH,083H,0E8H,02AH,0D9H,000H,0EBH,02AH,076H,003H,01AH,013H,0CDH,0A5H,0E2H,0CDH,0CBH,0CBH
                db  0C2H,0D8H,0E2H,02EH,007H,0CDH,0A5H,0E2H,02DH,0C2H,0E5H,0E2H,0E1H,0C9H,0FEH,0F4H,0CAH,0B4H,0CEH,0D6H,091H,0CAH,0FAH,0E2H,0AFH,001H,02FH,023H,0F5H,0CDH,0D7H,0D4H
                db  0CDH,042H,0DEH,01AH,06FH,0F1H,0B7H,067H,022H,0A6H,003H,0CCH,05BH,0C8H,0CDH,083H,0E8H,02AH,0A6H,003H,0EBH,006H,00AH,0CDH,098H,0E2H,0D6H,0D3H,0C2H,015H,0E3H,005H
                db  0C2H,017H,0E3H,0CDH,098H,0E2H,093H,0C2H,015H,0E3H,02AH,0D9H,000H,006H,00AH,0CDH,098H,0E2H,05FH,096H,0A2H,0C2H,05AH,0E3H,073H,0CDH,04CH,0C6H,07EH,0B7H,023H,0C2H
                db  02DH,0E3H,005H,0C2H,02FH,0E3H,001H,0F9H,0FFH,009H,022H,076H,003H,021H,004H,0C6H,0CDH,0A8H,0DCH,02AH,0D9H,000H,0E5H,0C3H,0E7H,0C7H,023H,0EBH,02AH,076H,003H,0CDH
                db  0CBH,0CBH,0DAH,04DH,0E3H,021H,06EH,0E3H,0CDH,0A8H,0DCH,0C3H,005H,0C7H,04EH,04FH,020H,047H,04FH,04FH,044H,00DH,00AH,000H,0CDH,091H,0E3H,07EH,0C3H,0A9H,0DAH,0CDH
                db  0D7H,0D4H,0E5H,0CDH,091H,0E3H,0E3H,0CDH,0D1H,0CBH,02CH,0CDH,0DEH,0E0H,0D1H,012H,0C9H,001H,024H,0EDH,0C5H,0CDH,008H,0D8H,0F8H,03AH,0A9H,003H,0FEH,090H,0C0H,001H
                db  080H,091H,011H,000H,000H,0C3H,0A0H,0E9H,0CDH,0D8H,0D4H,0CDH,0A1H,0EDH,0CDH,0D1H,0CBH,03BH,0EBH,02AH,0A6H,003H,0C3H,0C2H,0E3H,03AH,05AH,003H,0B7H,0CAH,089H,0CFH
                db  0D1H,0EBH,0E5H,0AFH,032H,05AH,003H,0BAH,0F5H,0D5H,046H,0B0H,0CAH,089H,0CFH,023H,04EH,023H,066H,069H,0C3H,0F6H,0E3H,058H,0E5H,00EH,002H,07EH,023H,0FEH,05CH,0CAH
                db  025H,0E5H,0FEH,020H,0C2H,0ECH,0E3H,00CH,005H,0C2H,0DBH,0E3H,0E1H,043H,03EH,05CH,0CDH,059H,0E5H,0CDH,0DCH,0CBH,0AFH,05FH,057H,0CDH,059H,0E5H,057H,07EH,023H,0FEH
                db  021H,0CAH,022H,0E5H,0FEH,023H,0CAH,048H,0E4H,005H,0CAH,00EH,0E5H,0FEH,02BH,03EH,008H,0CAH,0F9H,0E3H,02BH,07EH,023H,0FEH,02EH,0CAH,067H,0E4H,0FEH,05CH,0CAH,0D7H
                db  0E3H,0BEH,0C2H,0F0H,0E3H,0FEH,024H,0CAH,041H,0E4H,0FEH,02AH,0C2H,0F0H,0E3H,078H,0FEH,002H,023H,0DAH,039H,0E4H,07EH,0FEH,024H,03EH,020H,0C2H,045H,0E4H,005H,01CH
                db  0FEH,0AFH,0C6H,010H,023H,01CH,082H,057H,01CH,00EH,000H,005H,0CAH,09DH,0E4H,07EH,023H,0FEH,02EH,0CAH,072H,0E4H,0FEH,023H,0CAH,048H,0E4H,0FEH,02CH,0C2H,07EH,0E4H
                db  07AH,0F6H,040H,057H,0C3H,048H,0E4H,07EH,0FEH,023H,03EH,02EH,0C2H,0F0H,0E3H,00EH,001H,023H,00CH,005H,0CAH,09DH,0E4H,07EH,023H,0FEH,023H,0CAH,072H,0E4H,0D5H,011H
                db  09BH,0E4H,0D5H,054H,05DH,0FEH,05EH,0C0H,0BEH,0C0H,023H,0BEH,0C0H,023H,0BEH,0C0H,023H,078H,0D6H,004H,0D8H,0D1H,0D1H,047H,014H,023H,0CAH,0EBH,0D1H,07AH,02BH,01CH
                db  0E6H,008H,0C2H,0BDH,0E4H,01DH,078H,0B7H,0CAH,0BDH,0E4H,07EH,0D6H,02DH,0CAH,0B8H,0E4H,0FEH,0FEH,0C2H,0BDH,0E4H,03EH,008H,0C6H,004H,082H,057H,005H,0E1H,0F1H,0CAH
                db  019H,0E5H,0C5H,0D5H,0CDH,0D7H,0D4H,0D1H,0C1H,0C5H,0E5H,043H,078H,081H,0FEH,019H,0D2H,089H,0CFH,07AH,0F6H,080H,0CDH,016H,0F3H,0CDH,0A8H,0DCH,0E1H,02BH,0CDH,02CH
                db  0CDH,037H,0CAH,0F5H,0E4H,032H,05AH,003H,0FEH,03BH,0CAH,0F2H,0E4H,0FEH,02CH,0C2H,07BH,0C6H,0CDH,02CH,0CDH,0C1H,0EBH,0E1H,0E5H,0F5H,0D5H,07EH,090H,023H,04EH,023H
                db  066H,069H,016H,000H,05FH,019H,078H,0B7H,0C2H,0F6H,0E3H,0C3H,014H,0E5H,0CDH,059H,0E5H,0CDH,0DCH,0CBH,0E1H,0F1H,0C2H,0B9H,0E3H,0DCH,08BH,0D2H,0E3H,0CDH,007H,0DEH
                db  0E1H,0C9H,00EH,001H,03EH,0F1H,005H,0CDH,059H,0E5H,0E1H,0F1H,0CAH,019H,0E5H,0C5H,0CDH,0D7H,0D4H,0CDH,0A1H,0EDH,0C1H,0C5H,0E5H,02AH,0A6H,003H,041H,00EH,000H,0C5H
                db  0CDH,0ADH,0DEH,0CDH,0ABH,0DCH,02AH,0A6H,003H,0F1H,096H,047H,03EH,020H,004H,005H,0CAH,0DCH,0E4H,0CDH,0DCH,0CBH,0C3H,04FH,0E5H,0F5H,07AH,0B7H,03EH,02BH,0C4H,0DCH
                db  0CBH,0F1H,0C9H,032H,0C7H,000H,02AH,067H,003H,0B4H,0A5H,03CH,0EBH,0C8H,0C3H,075H,0E5H,0CDH,08EH,0CFH,0C0H,0E1H,0EBH,022H,069H,003H,0EBH,0CDH,03AH,0C8H,0D2H,05EH
                db  0D0H,060H,069H,023H,023H,04EH,023H,046H,023H,0C5H,0CDH,05CH,0E1H,0E1H,0E5H,07CH,0A5H,03CH,03EH,021H,0CCH,0DCH,0CBH,0C4H,007H,0F3H,03EH,020H,0CDH,0DCH,0CBH,021H
                db  01AH,002H,0E5H,00EH,0FFH,00CH,07EH,0B7H,023H,0C2H,0A5H,0E5H,0E1H,047H,016H,000H,0CDH,028H,0CCH,0D6H,030H,0DAH,0C8H,0E5H,0FEH,00AH,0D2H,0C8H,0E5H,05FH,07AH,007H
                db  007H,082H,007H,083H,057H,0C3H,0B0H,0E5H,0E5H,021H,0AEH,0E5H,0E3H,015H,014H,0C2H,0D3H,0E5H,014H,0FEH,04FH,0CAH,006H,0E7H,0FEH,0DDH,0CAH,014H,0E7H,0FEH,0F0H,0CAH
                db  02AH,0E6H,0FEH,031H,0DAH,0E9H,0E5H,0D6H,020H,0FEH,021H,0CAH,038H,0E7H,0FEH,01CH,0CAH,064H,0E6H,0FEH,023H,0CAH,03DH,0E6H,0FEH,019H,0CAH,0A4H,0E6H,0FEH,014H,0CAH
                db  06EH,0E6H,0FEH,013H,0CAH,08BH,0E6H,0FEH,015H,0CAH,017H,0E7H,0FEH,028H,0CAH,09FH,0E6H,0FEH,01BH,0CAH,037H,0E6H,0FEH,018H,0CAH,09CH,0E6H,0FEH,011H,03EH,007H,0C2H
                db  0DCH,0CBH,0C1H,0D1H,0CDH,08BH,0D2H,0C3H,076H,0E5H,07EH,0B7H,0C8H,004H,0CDH,029H,0E7H,023H,015H,0C2H,02AH,0E6H,0C9H,0E5H,021H,085H,0E6H,0E3H,037H,0F5H,0CDH,028H
                db  0CCH,05FH,0F1H,0F5H,0DCH,085H,0E6H,07EH,0B7H,0CAH,062H,0E6H,0CDH,029H,0E7H,0F1H,0F5H,0DCH,0CDH,0E6H,0DAH,059H,0E6H,023H,004H,07EH,0BBH,0C2H,047H,0E6H,015H,0C2H
                db  047H,0E6H,0F1H,0C9H,0CDH,052H,0E1H,0CDH,08BH,0D2H,0C1H,0C3H,08DH,0E5H,07EH,0B7H,0C8H,03EH,05CH,0CDH,029H,0E7H,07EH,0B7H,0CAH,085H,0E6H,0CDH,029H,0E7H,0CDH,0CDH
                db  0E6H,015H,0C2H,076H,0E6H,03EH,05CH,0CDH,0DCH,0CBH,0C9H,07EH,0B7H,0C8H,0CDH,028H,0CCH,077H,0CDH,029H,0E7H,023H,004H,015H,0C2H,08BH,0E6H,0C9H,036H,000H,048H,016H
                db  0FFH,0CDH,02AH,0E6H,0CDH,028H,0CCH,0B7H,0CAH,0A4H,0E6H,0FEH,07FH,0CAH,0BDH,0E6H,0FEH,00DH,0CAH,014H,0E7H,0FEH,01BH,0C8H,0FEH,05FH,0C2H,0DDH,0E6H,03EH,05FH,005H
                db  004H,0CAH,0E5H,0E6H,0CDH,029H,0E7H,02BH,005H,011H,0A4H,0E6H,0D5H,0E5H,00DH,07EH,0B7H,037H,0CAH,026H,0EBH,023H,07EH,02BH,077H,023H,0C3H,0CFH,0E6H,0F5H,079H,0FEH
                db  0FFH,0DAH,0EDH,0E6H,0F1H,03EH,007H,0CDH,0DCH,0CBH,0C3H,0A4H,0E6H,090H,00CH,004H,0C5H,0EBH,06FH,026H,000H,019H,044H,04DH,023H,0CDH,035H,0C6H,0C1H,0F1H,077H,0CDH
                db  029H,0E7H,023H,0C3H,0A4H,0E6H,078H,0B7H,0C8H,005H,02BH,07EH,0CDH,029H,0E7H,015H,0C2H,006H,0E7H,0C9H,0CDH,052H,0E1H,0CDH,08BH,0D2H,0C1H,0D1H,07AH,0A3H,03CH,021H
                db  019H,002H,0C8H,037H,023H,0F5H,0C3H,084H,0C7H,0CDH,0DCH,0CBH,0FEH,00AH,0C0H,03EH,00DH,0CDH,0DCH,0CBH,0AFH,0C3H,099H,0D2H,0C1H,0D1H,07AH,0A3H,03CH,021H,01AH,002H
                db  0CAH,086H,0D2H,0C3H,006H,0C7H,001H,00AH,000H,0C5H,050H,058H,0CAH,07CH,0E7H,0FEH,02CH,0CAH,05EH,0E7H,0D5H,0CDH,08EH,0CFH,042H,04BH,0D1H,0CAH,07CH,0E7H,0CDH,0D1H
                db  0CBH,02CH,0CDH,08EH,0CFH,0CAH,07CH,0E7H,0F1H,0CDH,0D1H,0CBH,02CH,0D5H,0CDH,099H,0CFH,0C2H,07BH,0C6H,07AH,0B3H,0CAH,089H,0CFH,0EBH,0E3H,0EBH,0C5H,0CDH,03AH,0C8H
                db  0D1H,0D5H,0C5H,0CDH,03AH,0C8H,060H,069H,0D1H,0CDH,0CBH,0CBH,0EBH,0DAH,089H,0CFH,0D1H,0C1H,0F1H,0E5H,0D5H,0C3H,0A8H,0E7H,009H,0DAH,089H,0CFH,0EBH,0E5H,021H,0F9H
                db  0FFH,0CDH,0CBH,0CBH,0E1H,0DAH,089H,0CFH,0D5H,05EH,07BH,023H,056H,0B2H,0EBH,0D1H,0CAH,0BBH,0E7H,07EH,023H,0B6H,02BH,0EBH,0C2H,098H,0E7H,0C5H,0CDH,0DEH,0E7H,0C1H
                db  0D1H,0E1H,0D5H,05EH,07BH,023H,056H,0B2H,0CAH,0D9H,0E7H,0EBH,0E3H,0EBH,023H,073H,023H,072H,0EBH,009H,0EBH,0E1H,0C3H,0C2H,0E7H,001H,005H,0C7H,0C5H,0FEH,0F6H,0AFH
                db  032H,05DH,003H,02AH,0D9H,000H,02BH,023H,07EH,023H,0B6H,0C8H,023H,05EH,023H,056H,0CDH,02CH,0CDH,0B7H,0CAH,0E7H,0E7H,04FH,03AH,05DH,003H,0B7H,079H,0CAH,05FH,0E8H
                db  0FEH,0A8H,0C2H,021H,0E8H,0CDH,02CH,0CDH,0FEH,089H,0C2H,0F3H,0E7H,0CDH,02CH,0CDH,0FEH,00EH,0C2H,0F3H,0E7H,0D5H,0CDH,0A4H,0CFH,07AH,0B3H,0C2H,02AH,0E8H,0C3H,04AH
                db  0E8H,0FEH,00EH,0C2H,0F0H,0E7H,0D5H,0CDH,0A4H,0CFH,0E5H,0CDH,03AH,0C8H,00BH,03EH,00DH,0DAH,073H,0E8H,0CDH,07EH,0D2H,021H,04FH,0E8H,0D5H,0CDH,0A8H,0DCH,0E1H,0CDH
                db  007H,0F3H,0C1H,0E1H,0E5H,0C5H,0CDH,0FFH,0F2H,0E1H,0D1H,02BH,0C3H,0F0H,0E7H,055H,04EH,044H,045H,046H,049H,04EH,045H,044H,020H,04CH,049H,04EH,045H,020H,000H,0FEH
                db  00DH,0C2H,0F0H,0E7H,0D5H,0CDH,0A4H,0CFH,0EBH,023H,023H,023H,04EH,023H,046H,0EBH,0E5H,03EH,00EH,021H,049H,0E8H,0E5H,02AH,022H,003H,0E5H,02BH,070H,02BH,071H,02BH
                db  077H,0E1H,0C9H,03AH,05DH,003H,0B7H,0C8H,0C3H,0DFH,0E7H,0C3H,025H,0E9H,0C3H,031H,0E9H,0F1H,0E5H,0D5H,0F5H,0FEH,020H,03AH,0C9H,000H,0FAH,0C6H,0E8H,0CAH,0E0H,0E8H
                db  0CDH,0F8H,0E8H,0F1H,0F5H,087H,02FH,0D3H,031H,0CDH,087H,0E9H,03EH,0DFH,0D3H,033H,0F6H,020H,0D3H,033H,03AH,0C9H,000H,021H,0CFH,000H,0BEH,0C2H,0DCH,0E8H,03EH,00DH
                db  0CDH,0DCH,0CBH,0C3H,052H,0E9H,0F1H,0F5H,0FEH,009H,0CCH,079H,0E9H,0FEH,00DH,0CAH,03DH,0E9H,0FEH,00AH,0CAH,052H,0E9H,0FEH,00CH,0CAH,06BH,0E9H,0F1H,0D1H,0E1H,0C9H
                db  03CH,021H,0CFH,000H,0BEH,0CAH,0BEH,0E8H,032H,0C9H,000H,011H,00CH,000H,02AH,0CDH,000H,019H,022H,0CDH,000H,0C3H,0DCH,0E8H,03CH,032H,0C9H,000H,011H,00CH,000H,02AH
                db  0CDH,000H,019H,0EBH,02AH,0CBH,000H,019H,022H,0CBH,000H,0CDH,087H,0E9H,07BH,02FH,0D3H,031H,07AH,02FH,0D3H,033H,0EEH,040H,0D3H,033H,0F6H,040H,0D3H,033H,021H,000H
                db  000H,022H,0CDH,000H,0C9H,03AH,0C9H,000H,0B7H,0C4H,031H,0E9H,0AFH,032H,0CAH,000H,0C9H,03EH,00DH,0CDH,0DCH,0CBH,03EH,00AH,0CDH,0DCH,0CBH,0AFH,0C9H,02AH,0CBH,000H
                db  07CH,0F6H,008H,067H,0EBH,0CDH,00BH,0E9H,022H,0CBH,000H,07CH,032H,0C9H,000H,0C3H,0DCH,0E8H,021H,010H,000H,0CDH,087H,0E9H,07DH,02FH,0D3H,031H,07CH,02FH,0D3H,033H
                db  0EEH,080H,0D3H,033H,0F6H,080H,0D3H,033H,0C3H,0DCH,0E8H,0CDH,087H,0E9H,03EH,0DAH,0D3H,037H,03EH,0FAH,0D3H,037H,0C3H,0DCH,0E8H,03EH,020H,0CDH,0DCH,0CBH,03AH,0C9H
                db  000H,0E6H,007H,0C2H,079H,0E9H,0C9H,0DBH,035H,0EEH,080H,0E6H,090H,0CAH,087H,0E9H,0C9H,021H,012H,0F7H,0CDH,061H,0ECH,0C3H,0A0H,0E9H,0CDH,061H,0ECH,0CDH,01EH,0ECH
                db  078H,0B7H,0C8H,03AH,0A9H,003H,0B7H,0CAH,053H,0ECH,090H,0D2H,0BAH,0E9H,02FH,03CH,0EBH,0CDH,043H,0ECH,0EBH,0CDH,053H,0ECH,0C1H,0D1H,0FEH,019H,0D0H,0F5H,0CDH,080H
                db  0ECH,067H,0F1H,0CDH,067H,0EAH,0B4H,021H,0A6H,003H,0F2H,0E0H,0E9H,0CDH,047H,0EAH,0D2H,026H,0EAH,023H,034H,0CAH,042H,0EAH,02EH,001H,0CDH,07DH,0EAH,0C3H,026H,0EAH
                db  0AFH,090H,047H,07EH,09BH,05FH,023H,07EH,09AH,057H,023H,07EH,099H,04FH,0DCH,053H,0EAH,068H,063H,0AFH,047H,079H,0B7H,0C2H,013H,0EAH,04AH,054H,065H,06FH,078H,0D6H
                db  008H,0FEH,0E0H,0C2H,0F4H,0E9H,0AFH,032H,0A9H,003H,0C9H,005H,029H,07AH,017H,057H,079H,08FH,04FH,0F2H,00BH,0EAH,078H,05CH,045H,0B7H,0CAH,026H,0EAH,021H,0A9H,003H
                db  086H,077H,0D2H,006H,0EAH,0C8H,078H,021H,0A9H,003H,0B7H,0FCH,038H,0EAH,046H,023H,07EH,0E6H,080H,0A9H,04FH,0C3H,053H,0ECH,01CH,0C0H,014H,0C0H,00CH,0C0H,00EH,080H
                db  034H,0C0H,01EH,006H,0C3H,08CH,0C6H,07EH,083H,05FH,023H,07EH,08AH,057H,023H,07EH,089H,04FH,0C9H,021H,0AAH,003H,07EH,02FH,077H,0AFH,06FH,090H,047H,07DH,09BH,05FH
                db  07DH,09AH,057H,07DH,099H,04FH,0C9H,006H,000H,0D6H,008H,0DAH,076H,0EAH,043H,05AH,051H,00EH,000H,0C3H,069H,0EAH,0C6H,009H,06FH,0AFH,02DH,0C8H,079H,01FH,04FH,07AH
                db  01FH,057H,07BH,01FH,05FH,078H,01FH,047H,0C3H,079H,0EAH,000H,000H,000H,081H,003H,0AAH,056H,019H,080H,0F1H,022H,076H,080H,045H,0AAH,038H,082H,0CDH,0EFH,0EBH,0B7H
                db  0EAH,089H,0CFH,021H,0A9H,003H,07EH,001H,035H,080H,011H,0F3H,004H,090H,0F5H,070H,0D5H,0C5H,0CDH,0A0H,0E9H,0C1H,0D1H,004H,0CDH,038H,0EBH,021H,08BH,0EAH,0CDH,09AH
                db  0E9H,021H,08FH,0EAH,0CDH,07AH,0F8H,001H,080H,080H,011H,000H,000H,0CDH,0A0H,0E9H,0F1H,0CDH,0E0H,0F2H,001H,031H,080H,011H,018H,072H,0CDH,0EFH,0EBH,0C8H,02EH,000H
                db  0CDH,0ADH,0EBH,079H,032H,0D4H,003H,0EBH,022H,0D5H,003H,001H,000H,000H,050H,058H,021H,0F1H,0E9H,0E5H,021H,0FCH,0EAH,0E5H,0E5H,021H,0A6H,003H,07EH,023H,0B7H,0CAH
                db  028H,0EBH,0E5H,02EH,008H,01FH,067H,079H,0D2H,016H,0EBH,0E5H,02AH,0D5H,003H,019H,0EBH,0E1H,03AH,0D4H,003H,089H,01FH,04FH,07AH,01FH,057H,07BH,01FH,05FH,078H,01FH
                db  047H,02DH,07CH,0C2H,005H,0EBH,0E1H,0C9H,043H,05AH,051H,04FH,0C9H,0CDH,043H,0ECH,021H,0F9H,0F0H,0CDH,050H,0ECH,0C1H,0D1H,0CDH,0EFH,0EBH,0CAH,07EH,0C6H,02EH,0FFH
                db  0CDH,0ADH,0EBH,034H,034H,02BH,07EH,032H,058H,000H,02BH,07EH,032H,054H,000H,02BH,07EH,032H,050H,000H,041H,0EBH,0AFH,04FH,057H,05FH,032H,05BH,000H,0E5H,0C5H,07DH
                db  0CDH,04FH,000H,0DEH,000H,03FH,0D2H,070H,0EBH,032H,05BH,000H,0F1H,0F1H,037H,0D2H,0C1H,0E1H,079H,03CH,03DH,01FH,0FAH,027H,0EAH,017H,07BH,017H,05FH,07AH,017H,057H
                db  079H,017H,04FH,029H,078H,017H,047H,03AH,05BH,000H,017H,032H,05BH,000H,079H,0B2H,0B3H,0C2H,05DH,0EBH,0E5H,021H,0A9H,003H,035H,0E1H,0C2H,05DH,0EBH,0C3H,042H,0EAH
                db  03EH,0FFH,02EH,0AFH,021H,0B2H,003H,04EH,023H,0AEH,047H,02EH,000H,078H,0B7H,0CAH,0D1H,0EBH,07DH,021H,0A9H,003H,0AEH,080H,047H,01FH,0A8H,078H,0F2H,0D0H,0EBH,0C6H
                db  080H,077H,0CAH,026H,0EBH,0CDH,080H,0ECH,077H,02BH,0C9H,0CDH,0EFH,0EBH,02FH,0E1H,0B7H,0E1H,0F2H,006H,0EAH,0C3H,042H,0EAH,0CDH,05EH,0ECH,078H,0B7H,0C8H,0C6H,002H
                db  0DAH,042H,0EAH,047H,0CDH,0A0H,0E9H,021H,0A9H,003H,034H,0C0H,0C3H,042H,0EAH,03AH,0A9H,003H,0B7H,0C8H,03AH,0A8H,003H,0FEH,02FH,017H,09FH,0C0H,03CH,0C9H,006H,088H
                db  011H,000H,000H,021H,0A9H,003H,04FH,070H,006H,000H,023H,036H,080H,017H,0C3H,0EEH,0E9H,0CDH,030H,0ECH,0F0H,0CDH,008H,0D8H,0FAH,055H,0EFH,0CAH,0A5H,0EDH,021H,0A8H
                db  003H,07EH,0EEH,080H,077H,0C9H,0CDH,030H,0ECH,06FH,017H,09FH,067H,0C3H,042H,0EDH,0CDH,008H,0D8H,0CAH,0A5H,0EDH,0F2H,0EFH,0EBH,02AH,0A6H,003H,07CH,0B5H,0C8H,07CH
                db  0C3H,0F9H,0EBH,0EBH,02AH,0A6H,003H,0E3H,0E5H,02AH,0A8H,003H,0E3H,0E5H,0EBH,0C9H,0CDH,061H,0ECH,0EBH,022H,0A6H,003H,060H,069H,022H,0A8H,003H,0EBH,0C9H,021H,0A6H
                db  003H,05EH,023H,056H,023H,04EH,023H,046H,023H,0C9H,011H,0A6H,003H,006H,004H,0C3H,077H,0ECH,0EBH,03AH,01FH,003H,047H,01AH,077H,013H,023H,005H,0C2H,077H,0ECH,0C9H
                db  021H,0A8H,003H,07EH,007H,037H,01FH,077H,03FH,01FH,023H,023H,077H,079H,007H,037H,01FH,04FH,01FH,0AEH,0C9H,021H,0ACH,003H,011H,072H,0ECH,0C3H,0A4H,0ECH,021H,0ACH
                db  003H,011H,073H,0ECH,0D5H,011H,0A6H,003H,0CDH,008H,0D8H,0D8H,011H,0A2H,003H,0C9H,078H,0B7H,0CAH,0EFH,0EBH,021H,0F8H,0EBH,0E5H,0CDH,0EFH,0EBH,079H,0C8H,021H,0A8H
                db  003H,0AEH,079H,0F8H,0CDH,0CAH,0ECH,01FH,0A9H,0C9H,023H,078H,0BEH,0C0H,02BH,079H,0BEH,0C0H,02BH,07AH,0BEH,0C0H,02BH,07BH,096H,0C0H,0E1H,0E1H,0C9H,07AH,0ACH,07CH
                db  0FAH,0F9H,0EBH,0BAH,0C2H,0FAH,0EBH,07DH,093H,0C2H,0FAH,0EBH,0C9H,021H,0ACH,003H,0CDH,073H,0ECH,011H,0B3H,003H,01AH,0B7H,0CAH,0EFH,0EBH,021H,0F8H,0EBH,0E5H,0CDH
                db  0EFH,0EBH,01BH,01AH,04FH,0C8H,021H,0A8H,003H,0AEH,079H,0F8H,013H,023H,006H,008H,01AH,096H,0C2H,0C7H,0ECH,01BH,02BH,005H,0C2H,010H,0EDH,0C1H,0C9H,0CDH,0F3H,0ECH
                db  0C2H,0F8H,0EBH,0C9H,0CDH,008H,0D8H,02AH,0A6H,003H,0F8H,0CAH,0A5H,0EDH,0D4H,064H,0EDH,021H,042H,0EAH,0E5H,03AH,0A9H,003H,0FEH,090H,0D2H,04BH,0EDH,0CDH,0AAH,0EDH
                db  0EBH,0D1H,022H,0A6H,003H,03EH,002H,032H,01FH,003H,0C9H,001H,080H,090H,011H,000H,000H,0CDH,0B0H,0ECH,0C0H,061H,06AH,0C3H,041H,0EDH,0CDH,008H,0D8H,0E0H,0FAH,077H
                db  0EDH,0CAH,0A5H,0EDH,0CDH,05EH,0ECH,0CDH,09CH,0EDH,078H,0B7H,0C8H,0CDH,080H,0ECH,021H,0A5H,003H,046H,0C3H,026H,0EAH,02AH,0A6H,003H,0CDH,09CH,0EDH,07CH,055H,01EH
                db  000H,006H,090H,0C3H,003H,0ECH,0CDH,008H,0D8H,0D0H,0CAH,0A5H,0EDH,0FCH,077H,0EDH,021H,000H,000H,022H,0A2H,003H,022H,0A4H,003H,03EH,008H,001H,03EH,004H,0C3H,047H
                db  0EDH,0CDH,008H,0D8H,0C8H,01EH,00DH,0C3H,08CH,0C6H,047H,04FH,057H,05FH,0B7H,0C8H,0E5H,0CDH,05EH,0ECH,0CDH,080H,0ECH,0AEH,067H,0FCH,0CEH,0EDH,03EH,098H,090H,0CDH
                db  067H,0EAH,07CH,017H,0DCH,038H,0EAH,006H,000H,0DCH,053H,0EAH,0E1H,0C9H,01BH,07AH,0A3H,03CH,0C0H,00BH,0C9H,0CDH,008H,0D8H,0F8H,0CDH,0EFH,0EBH,0F2H,0E8H,0EDH,0CDH
                db  01EH,0ECH,0CDH,0E8H,0EDH,0C3H,015H,0ECH,0CDH,008H,0D8H,0F8H,0D2H,00EH,0EEH,0CAH,0A5H,0EDH,0CDH,035H,0EDH,021H,0A9H,003H,07EH,0FEH,098H,03AH,0A6H,003H,0D0H,07EH
                db  0CDH,0AAH,0EDH,036H,098H,07BH,0F5H,079H,017H,0CDH,0EEH,0E9H,0F1H,0C9H,021H,0A9H,003H,07EH,0FEH,090H,0DAH,024H,0EDH,0C2H,02FH,0EEH,04FH,02BH,07EH,0EEH,080H,006H
                db  006H,02BH,0B6H,005H,0C2H,021H,0EEH,0B7H,021H,000H,080H,0CAH,042H,0EDH,079H,0FEH,0B8H,0D0H,0F5H,0CDH,05EH,0ECH,0CDH,080H,0ECH,0AEH,02BH,036H,0B8H,0F5H,0FCH,057H
                db  0EEH,021H,0A8H,003H,03EH,0B8H,090H,0CDH,07FH,0F0H,0F1H,0FCH,032H,0F0H,0AFH,032H,0A1H,003H,0F1H,0D0H,0C3H,0E6H,0EFH,021H,0A2H,003H,07EH,035H,0B7H,023H,0CAH,05AH
                db  0EEH,0C9H,0E5H,021H,000H,000H,078H,0B1H,0CAH,07FH,0EEH,03EH,010H,029H,0DAH,0E1H,0D9H,0EBH,029H,0EBH,0D2H,07BH,0EEH,009H,0DAH,0E1H,0D9H,03DH,0C2H,06DH,0EEH,0EBH
                db  0E1H,0C9H,07CH,017H,09FH,047H,0CDH,04BH,0EFH,079H,098H,0C3H,091H,0EEH,07CH,017H,09FH,047H,0E5H,07AH,017H,09FH,019H,088H,00FH,0ACH,0F2H,041H,0EDH,0C5H,0EBH,0CDH
                db  07AH,0EDH,0F1H,0E1H,0CDH,043H,0ECH,0EBH,0CDH,065H,0EFH,0C3H,0E6H,0F2H,07CH,0B5H,0CAH,042H,0EDH,0E5H,0D5H,0CDH,03FH,0EFH,0C5H,044H,04DH,021H,000H,000H,03EH,010H
                db  029H,0DAH,0E6H,0EEH,0EBH,029H,0EBH,0D2H,0CEH,0EEH,009H,0DAH,0E6H,0EEH,03DH,0C2H,0C0H,0EEH,0C1H,0D1H,07CH,0B7H,0FAH,0DEH,0EEH,0D1H,078H,0C3H,047H,0EFH,0EEH,080H
                db  0B5H,0CAH,0F7H,0EEH,0EBH,001H,0C1H,0E1H,0CDH,07AH,0EDH,0E1H,0CDH,043H,0ECH,0CDH,07AH,0EDH,0C1H,0D1H,0C3H,0DAH,0EAH,078H,0B7H,0C1H,0FAH,042H,0EDH,0D5H,0CDH,07AH
                db  0EDH,0D1H,0C3H,01EH,0ECH,07CH,0B5H,0CAH,07EH,0C6H,0CDH,03FH,0EFH,0C5H,0EBH,0CDH,04BH,0EFH,044H,04DH,021H,000H,000H,03EH,011H,0F5H,0B7H,0C3H,028H,0EFH,0F5H,0E5H
                db  009H,0D2H,027H,0EFH,0F1H,037H,03EH,0E1H,07BH,017H,05FH,07AH,017H,057H,07DH,017H,06FH,07CH,017H,067H,0F1H,03DH,0C2H,01EH,0EFH,0EBH,0C1H,0D5H,0C3H,0D4H,0EEH,07CH
                db  0AAH,047H,0CDH,046H,0EFH,0EBH,07CH,0B7H,0F2H,042H,0EDH,0AFH,04FH,095H,06FH,079H,09CH,067H,0C3H,042H,0EDH,02AH,0A6H,003H,0CDH,04BH,0EFH,07CH,0EEH,080H,0B5H,0C0H
                db  0EBH,0CDH,09CH,0EDH,0AFH,006H,098H,0C3H,003H,0ECH,0D5H,0CDH,005H,0EFH,0AFH,082H,01FH,067H,07BH,01FH,06FH,0CDH,045H,0EDH,0F1H,0C3H,047H,0EFH,021H,0B2H,003H,07EH
                db  0EEH,080H,077H,021H,0B3H,003H,07EH,0B7H,0C8H,047H,02BH,04EH,011H,0A9H,003H,01AH,0B7H,0CAH,095H,0ECH,090H,0D2H,0AFH,0EFH,02FH,03CH,0F5H,00EH,008H,023H,0E5H,01AH
                db  046H,077H,078H,012H,01BH,02BH,00DH,0C2H,09FH,0EFH,0E1H,046H,02BH,04EH,0F1H,0FEH,039H,0D0H,0F5H,0CDH,080H,0ECH,023H,036H,000H,047H,0F1H,021H,0B2H,003H,0CDH,07FH
                db  0F0H,03AH,0ABH,003H,032H,0A1H,003H,078H,0B7H,0F2H,0DDH,0EFH,0CDH,046H,0F0H,0D2H,020H,0F0H,0EBH,034H,0CAH,042H,0EAH,0CDH,0ABH,0F0H,0C3H,020H,0F0H,0CDH,059H,0F0H
                db  021H,0AAH,003H,0DCH,06CH,0F0H,0AFH,047H,03AH,0A8H,003H,0B7H,0C2H,00FH,0F0H,021H,0A1H,003H,00EH,008H,056H,077H,07AH,023H,00DH,0C2H,0F4H,0EFH,078H,0D6H,008H,0FEH
                db  0C0H,0C2H,0E7H,0EFH,0C3H,006H,0EAH,005H,021H,0A1H,003H,0CDH,0B3H,0F0H,0B7H,0F2H,007H,0F0H,078H,0B7H,0CAH,020H,0F0H,021H,0A9H,003H,086H,077H,0D2H,006H,0EAH,0C8H
                db  03AH,0A1H,003H,0B7H,0FCH,032H,0F0H,021H,0AAH,003H,07EH,0E6H,080H,02BH,02BH,0AEH,077H,0C9H,021H,0A2H,003H,006H,007H,034H,0C0H,023H,005H,0C2H,037H,0F0H,034H,0CAH
                db  042H,0EAH,02BH,036H,080H,0C9H,021H,0ACH,003H,011H,0A2H,003H,00EH,007H,0AFH,01AH,08EH,012H,013H,023H,00DH,0C2H,04FH,0F0H,0C9H,021H,0ACH,003H,011H,0A2H,003H,00EH
                db  007H,0AFH,01AH,09EH,012H,013H,023H,00DH,0C2H,062H,0F0H,0C9H,07EH,02FH,077H,021H,0A1H,003H,006H,008H,0AFH,04FH,079H,09EH,077H,023H,005H,0C2H,076H,0F0H,0C9H,071H
                db  0E5H,0D6H,008H,0DAH,096H,0F0H,0E1H,0E5H,011H,000H,008H,04EH,073H,059H,02BH,015H,0C2H,08BH,0F0H,0C3H,081H,0F0H,0C6H,009H,057H,0AFH,0E1H,015H,0C8H,0E5H,01EH,008H
                db  07EH,01FH,077H,02BH,01DH,0C2H,0A0H,0F0H,0C3H,099H,0F0H,021H,0A8H,003H,016H,001H,0C3H,09DH,0F0H,00EH,008H,07EH,017H,077H,023H,00DH,0C2H,0B5H,0F0H,0C9H,0CDH,0EFH
                db  0EBH,0C8H,0CDH,0A3H,0EBH,0CDH,05DH,0F1H,071H,013H,006H,007H,01AH,013H,0B7H,0D5H,0CAH,0ECH,0F0H,00EH,008H,0C5H,01FH,047H,0DCH,046H,0F0H,0CDH,0ABH,0F0H,078H,0C1H
                db  00DH,0C2H,0D5H,0F0H,0D1H,005H,0C2H,0CCH,0F0H,0C3H,0E6H,0EFH,021H,0A8H,003H,0CDH,087H,0F0H,0C3H,0E4H,0F0H,000H,000H,000H,000H,000H,000H,020H,084H,011H,0F5H,0F0H
                db  021H,0ACH,003H,0CDH,073H,0ECH,03AH,0B3H,003H,0B7H,0CAH,07EH,0C6H,0CDH,0A0H,0EBH,034H,034H,0CDH,05DH,0F1H,021H,0D6H,003H,071H,041H,011H,0CFH,003H,021H,0ACH,003H
                db  0CDH,05FH,0F0H,01AH,099H,03FH,0DAH,034H,0F1H,011H,0CFH,003H,021H,0ACH,003H,0CDH,04CH,0F0H,0AFH,0DAH,012H,004H,03AH,0A8H,003H,03CH,03DH,01FH,0FAH,023H,0F0H,017H
                db  021H,0A2H,003H,00EH,007H,0CDH,0B5H,0F0H,021H,0CFH,003H,0CDH,0B3H,0F0H,078H,0B7H,0C2H,01AH,0F1H,021H,0A9H,003H,035H,0C2H,01AH,0F1H,0C3H,042H,0EAH,079H,032H,0B2H
                db  003H,02BH,011H,0D5H,003H,001H,000H,007H,07EH,012H,071H,01BH,02BH,005H,0C2H,068H,0F1H,0C9H,0CDH,09EH,0ECH,0EBH,02BH,07EH,0B7H,0C8H,0C6H,002H,0DAH,042H,0EAH,077H
                db  0E5H,0CDH,083H,0EFH,0E1H,034H,0C0H,0C3H,042H,0EAH,0CDH,006H,0EAH,0CDH,099H,0EDH,0F6H,0AFH,0EBH,001H,0FFH,000H,060H,068H,0CCH,042H,0EDH,0EBH,07EH,0FEH,026H,0CAH
                db  00DH,0D7H,0FEH,02DH,0F5H,0CAH,0AEH,0F1H,0FEH,02BH,0CAH,0AEH,0F1H,02BH,0CDH,02CH,0CDH,0DAH,078H,0F2H,0FEH,02EH,0CAH,028H,0F2H,0FEH,045H,0C2H,0CBH,0F1H,0E5H,0CDH
                db  02CH,0CDH,0FEH,04CH,0E1H,03EH,000H,0C2H,0DFH,0F1H,07EH,0FEH,025H,0CAH,035H,0F2H,0FEH,023H,0CAH,03FH,0F2H,0FEH,021H,0CAH,040H,0F2H,0FEH,044H,0C2H,008H,0F2H,0B7H
                db  0CDH,046H,0F2H,0E5H,021H,0FBH,0F1H,0E3H,0CDH,02CH,0CDH,015H,0FEH,0F3H,0C8H,0FEH,02DH,0C8H,014H,0FEH,0F2H,0C8H,0FEH,02BH,0C8H,02BH,0F1H,0CDH,02CH,0CDH,0DAH,0EBH
                db  0F2H,014H,0C2H,008H,0F2H,0AFH,093H,05FH,0E5H,07BH,090H,0F4H,055H,0F2H,0FCH,065H,0F2H,0C2H,00BH,0F2H,0E1H,0F1H,0E5H,0CCH,015H,0ECH,0E1H,0CDH,008H,0D8H,0E8H,0E5H
                db  021H,026H,0EBH,0E5H,0CDH,04BH,0EDH,0C9H,0CDH,008H,0D8H,00CH,0C2H,008H,0F2H,0DCH,046H,0F2H,0C3H,0AEH,0F1H,0CDH,008H,0D8H,0F2H,07BH,0C6H,023H,0C3H,008H,0F2H,0B7H
                db  0CDH,046H,0F2H,0C3H,03BH,0F2H,0E5H,0D5H,0C5H,0F5H,0CCH,05AH,0EDH,0F1H,0C4H,086H,0EDH,0C1H,0D1H,0E1H,0C9H,0C8H,0F5H,0CDH,008H,0D8H,0F5H,0E4H,0D8H,0EBH,0F1H,0ECH
                db  072H,0F1H,0F1H,03DH,0C9H,0D5H,0E5H,0F5H,0CDH,008H,0D8H,0F5H,0E4H,02DH,0EBH,0F1H,0ECH,0FDH,0F0H,0F1H,0E1H,0D1H,03CH,0C9H,0D5H,078H,089H,047H,0C5H,0E5H,07EH,0D6H
                db  030H,0F5H,0CDH,008H,0D8H,0F2H,0B1H,0F2H,02AH,0A6H,003H,011H,0CDH,00CH,0CDH,0CBH,0CBH,0D2H,0ADH,0F2H,054H,05DH,029H,029H,019H,029H,0F1H,04FH,009H,07CH,0B7H,0FAH
                db  0ABH,0F2H,022H,0A6H,003H,0E1H,0C1H,0D1H,0C3H,0AEH,0F1H,079H,0F5H,0CDH,077H,0EDH,037H,0D2H,0CDH,0F2H,001H,074H,094H,011H,000H,024H,0CDH,0B0H,0ECH,0F2H,0CAH,0F2H
                db  0CDH,0D8H,0EBH,0F1H,0CDH,0E0H,0F2H,0C3H,0A5H,0F2H,0CDH,090H,0EDH,0CDH,072H,0F1H,0CDH,09EH,0ECH,0F1H,0CDH,0FEH,0EBH,0CDH,090H,0EDH,0CDH,083H,0EFH,0C3H,0A5H,0F2H
                db  0CDH,043H,0ECH,0CDH,0FEH,0EBH,0C1H,0D1H,0C3H,0A0H,0E9H,07BH,0FEH,00AH,0D2H,0FAH,0F2H,007H,007H,083H,007H,086H,0D6H,030H,05FH,0FAH,01EH,032H,0C3H,0FBH,0F1H,0E5H
                db  021H,0FFH,0C5H,0CDH,0A8H,0DCH,0E1H,0CDH,042H,0EDH,0AFH,0CDH,098H,0F3H,0B6H,0CDH,032H,0F3H,0C3H,0A7H,0DCH,0AFH,0CDH,098H,0F3H,0E6H,008H,0CAH,020H,0F3H,036H,02BH
                db  0EBH,0CDH,030H,0ECH,0EBH,0F2H,032H,0F3H,036H,02DH,0C5H,0E5H,0CDH,015H,0ECH,0E1H,0C1H,0B4H,023H,036H,030H,03AH,055H,003H,057H,017H,03AH,01FH,003H,0DAH,004H,0F4H
                db  0CAH,0FCH,0F3H,0FEH,004H,0D2H,0A1H,0F3H,001H,000H,000H,0CDH,0BFH,0F6H,021H,0B5H,003H,046H,00EH,020H,03AH,055H,003H,05FH,0E6H,020H,0CAH,065H,0F3H,078H,0B9H,00EH
                db  02AH,0C2H,065H,0F3H,041H,071H,0CDH,02CH,0CDH,0CAH,085H,0F3H,0FEH,045H,0CAH,085H,0F3H,0FEH,044H,0CAH,085H,0F3H,0FEH,030H,0CAH,065H,0F3H,0FEH,02CH,0CAH,065H,0F3H
                db  0FEH,02EH,0C2H,088H,0F3H,02BH,036H,030H,07BH,0E6H,010H,0CAH,091H,0F3H,02BH,036H,024H,07BH,0E6H,004H,0C0H,02BH,070H,0C9H,032H,055H,003H,021H,0B5H,003H,036H,020H
                db  0C9H,0FEH,005H,0E5H,0DEH,000H,017H,057H,014H,0CDH,07AH,0F5H,001H,000H,003H,082H,0FAH,0BCH,0F3H,014H,0BAH,0D2H,0BCH,0F3H,03CH,047H,03EH,002H,0D6H,002H,0E1H,0F5H
                db  0CDH,018H,0F6H,036H,030H,0CCH,068H,0ECH,0CDH,02CH,0F6H,02BH,07EH,0FEH,030H,0CAH,0CBH,0F3H,0FEH,02EH,0C4H,068H,0ECH,0F1H,0CAH,0FDH,0F3H,0F5H,0CDH,008H,0D8H,03EH
                db  022H,08FH,077H,023H,0F1H,036H,02BH,0F2H,0EEH,0F3H,036H,02DH,02FH,03CH,006H,02FH,004H,0D6H,00AH,0D2H,0F0H,0F3H,0C6H,03AH,023H,070H,023H,077H,023H,036H,000H,0EBH
                db  021H,0B5H,003H,0C9H,023H,0C5H,0FEH,004H,07AH,0D2H,07EH,0F4H,01FH,0DAH,01BH,0F5H,001H,003H,006H,0CDH,010H,0F6H,0D1H,07AH,0D6H,005H,0F4H,0ECH,0F5H,0CDH,0BFH,0F6H
                db  07BH,0B7H,0CCH,0C9H,0EBH,03DH,0F4H,0ECH,0F5H,0E5H,0CDH,04EH,0F3H,0E1H,0CAH,033H,0F4H,070H,023H,036H,000H,021H,0B4H,003H,023H,03AH,070H,003H,095H,092H,0C8H,07EH
                db  0FEH,020H,0CAH,038H,0F4H,0FEH,02AH,0CAH,038H,0F4H,02BH,0E5H,0F5H,001H,04CH,0F4H,0C5H,0CDH,02CH,0CDH,0FEH,02DH,0C8H,0FEH,02BH,0C8H,0FEH,024H,0C8H,0C1H,0FEH,030H
                db  0C2H,076H,0F4H,023H,0CDH,02CH,0CDH,0D2H,076H,0F4H,02BH,001H,02BH,077H,0F1H,0CAH,06CH,0F4H,0C1H,0C3H,039H,0F4H,0F1H,0CAH,076H,0F4H,0E1H,036H,025H,0C9H,0E5H,01FH
                db  0DAH,022H,0F5H,0CAH,09AH,0F4H,011H,016H,0F7H,0CDH,0EDH,0ECH,016H,010H,0FAH,0A8H,0F4H,0E1H,0C1H,0CDH,015H,0F3H,02BH,036H,025H,0C9H,001H,00EH,0B6H,011H,0CAH,01BH
                db  0CDH,0B0H,0ECH,0F2H,091H,0F4H,016H,006H,0CDH,0EFH,0EBH,0C4H,07AH,0F5H,0E1H,0C1H,0FAH,0CDH,0F4H,0C5H,05FH,078H,092H,093H,0F4H,0ECH,0F5H,0CDH,003H,0F6H,0CDH,02CH
                db  0F6H,0B3H,0C4H,0FCH,0F5H,0B3H,0C4H,018H,0F6H,0D1H,0C3H,020H,0F4H,05FH,079H,0B7H,0C4H,063H,0F2H,083H,0FAH,0D8H,0F4H,0AFH,0C5H,0F5H,0FCH,065H,0F2H,0FAH,0DAH,0F4H
                db  0C1H,07BH,090H,0C1H,05FH,082H,078H,0FAH,0F6H,0F4H,092H,093H,0F4H,0ECH,0F5H,0C5H,0CDH,003H,0F6H,0C3H,007H,0F5H,0CDH,0ECH,0F5H,079H,0CDH,01CH,0F6H,04FH,0AFH,092H
                db  093H,0CDH,0ECH,0F5H,0C5H,047H,04FH,0CDH,02CH,0F6H,0C1H,0B1H,0C2H,012H,0F5H,02AH,070H,003H,083H,03DH,0F4H,0ECH,0F5H,050H,0C3H,029H,0F4H,0E5H,0D5H,0CDH,077H,0EDH
                db  0D1H,0AFH,0CAH,028H,0F5H,01EH,010H,001H,01EH,006H,0CDH,0EFH,0EBH,037H,0C4H,07AH,0F5H,0E1H,0C1H,0F5H,079H,0B7H,0F5H,0C4H,063H,0F2H,080H,04FH,07AH,0E6H,004H,0FEH
                db  001H,09FH,057H,081H,04FH,093H,0F5H,0C5H,0FCH,065H,0F2H,0FAH,048H,0F5H,0C1H,0F1H,0C5H,0F5H,0FAH,056H,0F5H,0AFH,02FH,03CH,080H,03CH,082H,047H,00EH,000H,0CDH,02CH
                db  0F6H,0F1H,0F4H,0F5H,0F5H,0C1H,0F1H,0CCH,0C9H,0EBH,0F1H,0DAH,071H,0F5H,083H,090H,092H,0C5H,0CDH,0DBH,0F3H,0EBH,0D1H,0C3H,029H,0F4H,0D5H,0AFH,0F5H,0CDH,008H,0D8H
                db  0E2H,09EH,0F5H,03AH,0A9H,003H,0FEH,091H,0D2H,09EH,0F5H,011H,0F6H,0F6H,021H,0ACH,003H,0CDH,073H,0ECH,0CDH,0BEH,0F0H,0F1H,0D6H,00AH,0F5H,0C3H,083H,0F5H,0CDH,0CFH
                db  0F5H,0CDH,008H,0D8H,0EAH,0B3H,0F5H,001H,043H,091H,011H,0F9H,04FH,0CDH,0B0H,0ECH,0C3H,0B9H,0F5H,011H,0FEH,0F6H,0CDH,0EDH,0ECH,0F2H,0CCH,0F5H,0F1H,0CDH,056H,0F2H
                db  0F5H,0C3H,0A1H,0F5H,0F1H,0CDH,065H,0F2H,0F5H,0CDH,0CFH,0F5H,0F1H,0D1H,0C9H,0CDH,008H,0D8H,0EAH,0E1H,0F5H,001H,074H,094H,011H,0F8H,023H,0CDH,0B0H,0ECH,0C3H,0E7H
                db  0F5H,011H,006H,0F7H,0CDH,0EDH,0ECH,0E1H,0F2H,0C4H,0F5H,0E9H,0B7H,0C8H,03DH,036H,030H,023H,0C3H,0EDH,0F5H,0C2H,0FCH,0F5H,0C8H,0CDH,018H,0F6H,036H,030H,023H,03DH
                db  0C3H,0F8H,0F5H,07BH,082H,03CH,047H,03CH,0D6H,003H,0D2H,008H,0F6H,0C6H,005H,04FH,03AH,055H,003H,0E6H,040H,0C0H,04FH,0C9H,005H,0C2H,024H,0F6H,036H,02EH,022H,070H
                db  003H,023H,048H,0C9H,00DH,0C0H,036H,02CH,023H,00EH,003H,0C9H,0D5H,0CDH,008H,0D8H,0E2H,077H,0F6H,0C5H,0E5H,0CDH,09EH,0ECH,021H,00EH,0F7H,0CDH,098H,0ECH,0CDH,083H
                db  0EFH,0AFH,0CDH,032H,0EEH,0E1H,0C1H,011H,01EH,0F7H,03EH,00AH,0CDH,018H,0F6H,0C5H,0F5H,0E5H,0D5H,006H,02FH,004H,0E1H,0E5H,0CDH,05CH,0F0H,0D2H,055H,0F6H,0E1H,0CDH
                db  049H,0F0H,0EBH,0E1H,070H,023H,0F1H,0C1H,03DH,0C2H,04CH,0F6H,0C5H,0E5H,021H,0A2H,003H,0CDH,050H,0ECH,0C3H,083H,0F6H,0C5H,0E5H,0CDH,091H,0E9H,03CH,0CDH,0AAH,0EDH
                db  0CDH,053H,0ECH,0E1H,0C1H,0AFH,011H,064H,0F7H,03FH,0CDH,018H,0F6H,0C5H,0F5H,0E5H,0D5H,0CDH,05EH,0ECH,0E1H,006H,02FH,004H,07BH,096H,05FH,023H,07AH,09EH,057H,023H
                db  079H,09EH,04FH,02BH,02BH,0D2H,097H,0F6H,0CDH,047H,0EAH,023H,0CDH,053H,0ECH,0EBH,0E1H,070H,023H,0F1H,0C1H,0DAH,089H,0F6H,013H,013H,03EH,004H,0C3H,0C5H,0F6H,0D5H
                db  011H,06AH,0F7H,03EH,005H,0CDH,018H,0F6H,0C5H,0F5H,0E5H,0EBH,04EH,023H,046H,0C5H,023H,0E3H,0EBH,02AH,0A6H,003H,006H,02FH,004H,07DH,093H,06FH,07CH,09AH,067H,0D2H
                db  0D8H,0F6H,019H,022H,0A6H,003H,0D1H,0E1H,070H,023H,0F1H,0C1H,03DH,0C2H,0C5H,0F6H,0CDH,018H,0F6H,077H,0D1H,0C9H,000H,000H,000H,000H,0F9H,002H,015H,0A2H,0FDH,0FFH
                db  09FH,031H,0A9H,05FH,063H,0B2H,0FEH,0FFH,003H,0BFH,0C9H,01BH,00EH,0B6H,000H,000H,000H,000H,000H,000H,000H,080H,000H,000H,004H,0BFH,0C9H,01BH,00EH,0B6H,000H,080H
                db  0C6H,0A4H,07EH,08DH,003H,000H,040H,07AH,010H,0F3H,05AH,000H,000H,0A0H,072H,04EH,018H,009H,000H,000H,010H,0A5H,0D4H,0E8H,000H,000H,000H,0E8H,076H,048H,017H,000H
                db  000H,000H,0E4H,00BH,054H,002H,000H,000H,000H,0CAH,09AH,03BH,000H,000H,000H,000H,0E1H,0F5H,005H,000H,000H,000H,080H,096H,098H,000H,000H,000H,000H,040H,042H,00FH
                db  000H,000H,000H,000H,0A0H,086H,001H,010H,027H,000H,010H,027H,0E8H,003H,064H,000H,00AH,000H,001H,000H,0AFH,047H,0C2H,006H,001H,0C5H,0CDH,091H,0E3H,0C1H,011H,0B4H
                db  003H,0D5H,0AFH,012H,005H,004H,00EH,006H,0CAH,093H,0F7H,00EH,004H,029H,08FH,029H,08FH,029H,08FH,029H,08FH,0B7H,0C2H,0A4H,0F7H,079H,03DH,0CAH,0A4H,0F7H,01AH,0B7H
                db  0CAH,0B0H,0F7H,0AFH,0C6H,030H,0FEH,03AH,0DAH,0ADH,0F7H,0C6H,007H,012H,013H,012H,0AFH,00DH,0CAH,0BDH,0F7H,005H,004H,0CAH,08FH,0F7H,0C3H,08DH,0F7H,012H,0E1H,0C9H
                db  021H,01EH,0ECH,0E3H,0E9H,0CDH,043H,0ECH,021H,012H,0F7H,0CDH,050H,0ECH,0C3H,0D4H,0F7H,0CDH,05AH,0EDH,0C1H,0D1H,0CDH,0EFH,0EBH,078H,0CAH,019H,0F8H,0F2H,0E4H,0F7H
                db  0B7H,0CAH,07EH,0C6H,0B7H,0CAH,007H,0EAH,0D5H,0C5H,079H,0F6H,07FH,0CDH,05EH,0ECH,0F2H,001H,0F8H,0D5H,0C5H,0CDH,0F5H,0EDH,0C1H,0D1H,0F5H,0CDH,0B0H,0ECH,0E1H,07CH
                db  01FH,0E1H,022H,0A8H,003H,0E1H,022H,0A6H,003H,0DCH,0C0H,0F7H,0CCH,01EH,0ECH,0D5H,0C5H,0CDH,09CH,0EAH,0C1H,0D1H,0CDH,0DAH,0EAH,0CDH,043H,0ECH,001H,038H,081H,011H
                db  03BH,0AAH,0CDH,0DAH,0EAH,03AH,0A9H,003H,0FEH,088H,0D2H,0CBH,0EBH,0CDH,0F5H,0EDH,0C6H,080H,0C6H,002H,0DAH,0CBH,0EBH,0F5H,021H,08BH,0EAH,0CDH,094H,0E9H,0CDH,0D4H
                db  0EAH,0F1H,0C1H,0D1H,0F5H,0CDH,09DH,0E9H,0CDH,01EH,0ECH,021H,059H,0F8H,0CDH,089H,0F8H,011H,000H,000H,0C1H,04AH,0C3H,0DAH,0EAH,008H,040H,02EH,094H,074H,070H,04FH
                db  02EH,077H,06EH,002H,088H,07AH,0E6H,0A0H,02AH,07CH,050H,0AAH,0AAH,07EH,0FFH,0FFH,07FH,07FH,000H,000H,080H,081H,000H,000H,000H,081H,0CDH,043H,0ECH,011H,0F2H,0EEH
                db  0D5H,0E5H,0CDH,05EH,0ECH,0CDH,0DAH,0EAH,0E1H,0CDH,043H,0ECH,07EH,023H,0CDH,050H,0ECH,006H,0F1H,0C1H,0D1H,03DH,0C8H,0D5H,0C5H,0F5H,0E5H,0CDH,0DAH,0EAH,0E1H,0CDH
                db  061H,0ECH,0E5H,0CDH,0A0H,0E9H,0E1H,0C3H,092H,0F8H,0CDH,0EFH,0EBH,021H,05FH,000H,0FAH,00BH,0F9H,021H,080H,000H,0CDH,050H,0ECH,021H,05FH,000H,0C8H,086H,0E6H,007H
                db  006H,000H,077H,023H,087H,087H,04FH,009H,0CDH,061H,0ECH,0CDH,0DAH,0EAH,03AH,05EH,000H,03CH,0E6H,003H,006H,000H,0FEH,001H,088H,032H,05EH,000H,021H,00FH,0F9H,087H
                db  087H,04FH,009H,0CDH,094H,0E9H,0CDH,05EH,0ECH,07BH,059H,0EEH,04FH,04FH,036H,080H,02BH,046H,036H,080H,021H,05DH,000H,034H,07EH,0D6H,0ABH,0C2H,002H,0F9H,077H,00CH
                db  015H,01CH,0CDH,0F1H,0E9H,021H,080H,000H,0C3H,06AH,0ECH,077H,02BH,077H,02BH,077H,0C3H,0E6H,0F8H,068H,0B1H,046H,068H,099H,0E9H,092H,069H,010H,0D1H,075H,068H,021H
                db  069H,0F9H,0CDH,094H,0E9H,0CDH,043H,0ECH,001H,049H,083H,011H,0DBH,00FH,0CDH,053H,0ECH,0C1H,0D1H,0CDH,038H,0EBH,0CDH,043H,0ECH,0CDH,0F5H,0EDH,0C1H,0D1H,0CDH,09DH
                db  0E9H,021H,06DH,0F9H,0CDH,09AH,0E9H,0CDH,0EFH,0EBH,037H,0F2H,055H,0F9H,0CDH,091H,0E9H,0CDH,0EFH,0EBH,0B7H,0F5H,0F4H,01EH,0ECH,021H,06DH,0F9H,0CDH,094H,0E9H,0F1H
                db  0D4H,01EH,0ECH,021H,071H,0F9H,0C3H,07AH,0F8H,0DBH,00FH,049H,081H,000H,000H,000H,07FH,005H,0BAH,0D7H,01EH,086H,064H,026H,099H,087H,058H,034H,023H,087H,0E0H,05DH
                db  0A5H,086H,0DAH,00FH,049H,083H,0CDH,043H,0ECH,0CDH,025H,0F9H,0C1H,0E1H,0CDH,043H,0ECH,0EBH,0CDH,053H,0ECH,0CDH,01FH,0F9H,0C3H,036H,0EBH,0CDH,0EFH,0EBH,0FCH,0C0H
                db  0F7H,0FCH,01EH,0ECH,03AH,0A9H,003H,0FEH,081H,0DAH,0B8H,0F9H,001H,000H,081H,051H,059H,0CDH,038H,0EBH,021H,09AH,0E9H,0E5H,021H,0C2H,0F9H,0CDH,07AH,0F8H,021H,069H
                db  0F9H,0C9H,009H,04AH,0D7H,03BH,078H,002H,06EH,084H,07BH,0FEH,0C1H,02FH,07CH,074H,031H,09AH,07DH,084H,03DH,05AH,07DH,0C8H,07FH,091H,07EH,0E4H,0BBH,04CH,07EH,06CH
;               db  0AAH,0AAH,07FH,000H,000H,000H,081H,021H,04DH,000H,0CDH,005H,0FAH,0CDH,09BH,0C8H,0C3H,006H,0C7H,0DBH,0FFH,0E6H,0F0H,00FH,00FH,0FEH,03CH,0C8H,0FEH,038H,037H,0C2H
                db  0AAH,0AAH,07FH,000H,000H,000H,081H,021H,04DH,000H,0CDH,005H,0FAH,0CDH,09BH,0C8H,0C3H,006H,0C7H,03EH,000H,0E6H,0F0H,00FH,00FH,0FEH,03CH,0C8H,0FEH,038H,037H,0C2H
;                                                                                                                  ^^^^ ^^^^
                db  00CH,0FAH,021H,0FFH,03FH,04EH,02BH,07EH,0E6H,0F0H,00FH,00FH,0F5H,06FH,026H,000H,011H,071H,0FAH,019H,07EH,023H,056H,023H,046H,023H,05EH,067H,0F1H,0F5H,07CH,0DAH
                db  023H,0FAH,079H,032H,04DH,000H,0F1H,021H,08DH,0FAH,0E5H,00EH,0FFH,0FEH,010H,021H,000H,000H,022H,0AEH,000H,0CAH,047H,0FAH,0FEH,008H,0D0H,0C6H,011H,0F5H,03EH,003H
                db  0CDH,04CH,000H,0F1H,0C3H,04CH,000H,0AFH,0CDH,04CH,000H,0CDH,06AH,0FAH,0CDH,06AH,0FAH,02FH,00EH,001H,0CDH,06AH,0FAH,0E5H,02AH,04CH,000H,02EH,0DBH,022H,0AEH,000H
                db  0E1H,03EH,02CH,035H,0CDH,04CH,000H,035H,035H,035H,021H,04DH,000H,034H,0C3H,04CH,000H,010H,0CAH,001H,002H,010H,0CAH,001H,002H,000H,0C2H,001H,080H,006H,0C2H,001H
                db  080H,020H,0CAH,080H,080H,004H,0CAH,002H,001H,024H,0CAH,040H,040H,078H,032H,093H,000H,0EBH,022H,0A6H,000H,07CH,0E6H,008H,021H,0C6H,0FFH,0C2H,0A1H,0FAH,021H,0D6H
                db  001H,022H,094H,000H,03AH,04DH,000H,032H,091H,000H,0EEH,001H,032H,0A0H,000H,081H,032H,0A4H,000H,0EEH,001H,032H,0ACH,000H,0C9H,0C3H,021H,0FBH,0C3H,02AH,0FBH,0C3H
                db  0C2H,0FAH,0F1H,0F5H,0FEH,00DH,0CCH,02AH,0FBH,0FEH,00AH,0CAH,001H,0FBH,0FEH,009H,0CAH,0F2H,0FAH,0DAH,0FFH,0FAH,03AH,0C9H,000H,03CH,0E5H,021H,0CFH,000H,0BEH,0E1H
                db  0C2H,0E4H,0FAH,0AFH,032H,0C9H,000H,0DBH,002H,0E6H,011H,0CAH,0E7H,0FAH,0F1H,0D3H,003H,0C9H,03EH,020H,0CDH,0DCH,0CBH,03AH,0C9H,000H,0E6H,007H,0C2H,0F2H,0FAH,0F1H
                db  0C9H,03AH,0CAH,000H,0E6H,002H,0C2H,0FFH,0FAH,03AH,0C9H,000H,0E5H,067H,0CDH,02AH,0FBH,024H,025H,0CAH,01EH,0FBH,03EH,020H,0CDH,0DCH,0CBH,0C3H,012H,0FBH,0E1H,0F1H
                db  0C9H,0AFH,032H,0CAH,000H,03AH,0C9H,000H,0B7H,0C8H,03AH,0C9H,000H,0B7H,0C2H,036H,0FBH,03EH,00AH,0C3H,038H,0FBH,03EH,00DH,0F5H,0DBH,002H,0E6H,011H,0CAH,039H,0FBH
                db  0F1H,0D3H,003H,0AFH,032H,0C9H,000H,0C9H,0C3H,09DH,0FBH,0C3H,0A6H,0FBH,0C3H,051H,0FBH,0F1H,0F5H,0FEH,009H,0C2H,067H,0FBH,03EH,020H,0CDH,0DCH,0CBH,03AH,0C9H,000H
                db  0E6H,007H,0C2H,058H,0FBH,0F1H,0C9H,0FEH,00DH,0CCH,0A6H,0FBH,0FEH,00AH,0CAH,0DCH,0FBH,0DAH,065H,0FBH,03AH,0C9H,000H,03CH,0E5H,021H,0CFH,000H,0BEH,0E1H,0C2H,087H
                db  0FBH,03EH,001H,032H,0C8H,000H,0AFH,032H,0C9H,000H,0DBH,002H,0F6H,0F5H,03CH,0C2H,08AH,0FBH,0F1H,0FEH,060H,0FAH,09AH,0FBH,0E6H,0DFH,0D3H,003H,0C9H,0AFH,032H,0CAH
                db  000H,03AH,0C9H,000H,0B7H,0C8H,0DBH,002H,0F6H,0F5H,03CH,0C2H,0A6H,0FBH,03AH,0C9H,000H,0B7H,0C2H,0D0H,0FBH,03AH,0C8H,000H,0B7H,0CAH,0CAH,0FBH,0E5H,021H,038H,04AH
                db  02BH,07CH,0B5H,0C2H,0C0H,0FBH,0E1H,032H,0C8H,000H,03EH,002H,0D3H,002H,0AFH,0C9H,03EH,001H,0D3H,002H,032H,0C8H,000H,03DH,032H,0C9H,000H,0C9H,03AH,0CAH,000H,0E6H
                db  002H,0C2H,065H,0FBH,03AH,0C9H,000H,0E5H,067H,0CDH,0A6H,0FBH,024H,025H,0CAH,0F9H,0FBH,03EH,020H,0CDH,0DCH,0CBH,0C3H,0EDH,0FBH,0E1H,0F1H,0C9H,03EH,084H,032H,0CFH
                db  000H,03EH,070H,032H,0D0H,000H,021H,052H,0FCH,0CDH,0A8H,0DCH,0CDH,0B8H,0C8H,0DAH,006H,0FCH,0CDH,02CH,0CDH,0FEH,051H,0CAH,05EH,0FCH,0FEH,043H,0CAH,038H,0FCH,0FEH
                db  04FH,0C2H,006H,0FCH,011H,048H,0FBH,03EH,001H,0D3H,002H,03EH,050H,032H,0CFH,000H,03EH,038H,032H,0D0H,000H,0C3H,042H,0FCH,011H,0B9H,0FAH,03EH,011H,0D3H,003H,0AFH
                db  0D3H,002H,021H,084H,000H,00EH,009H,01AH,077H,023H,013H,00DH,0C2H,047H,0FCH,0C3H,083H,0FCH,04CH,049H,04EH,045H,050H,052H,049H,04EH,054H,045H,052H,000H,0AFH,0D3H
                db  030H,0D3H,032H,0D3H,034H,0D3H,036H,0D3H,035H,03EH,0FFH,0D3H,031H,0D3H,033H,0D3H,037H,03EH,004H,0D3H,030H,0D3H,032H,0D3H,034H,0D3H,036H,03EH,0FCH,0D3H,037H,03EH
                db  0FAH,0D3H,037H,0C9H,008H,000H,000H,080H,000H,080H,080H,089H,008H,080H,080H,080H,082H,08AH,08AH,082H,082H,08AH,082H,08AH,082H,082H,082H,082H,002H,082H,082H,002H
                db  082H,002H,082H,082H,002H,002H,082H,082H,00AH,002H,002H,003H,002H,002H,002H,002H,082H,082H,08AH,00AH,002H,002H,002H,002H,082H,08AH,082H,082H,002H,002H,002H,002H
                db  002H,08AH,08AH,002H,002H,082H,082H,002H,00AH,08AH,08AH,082H,002H,082H,082H,082H,082H,002H,08AH,082H,082H,082H,082H,082H,08AH,082H,00AH,082H,082H,082H,002H,002H
                db  082H,082H,082H,00AH,002H,082H,082H,00AH,088H,000H,080H,000H,000H,000H,000H,000H,008H,000H,008H,028H,000H,000H,008H,000H,08AH,08EH,002H,00EH,002H,002H,002H,006H
                db  037H,037H,03FH,03FH,03FH,037H,03FH,03FH,03FH,0B7H,03FH,0BFH,037H,037H,03FH,037H,0B7H,037H,037H,03FH,03FH,037H,01FH,037H,0B7H,03FH,0BFH,037H,037H,037H,03FH,03FH
                db  03FH,0BFH,0B7H,0B7H,037H,037H,037H,03FH,0BFH,0BFH,0B7H,0BFH,03FH,0B7H,037H,0BFH,0BFH,0B7H,0B7H,0B7H,037H,03FH,03FH,0B7H,0BFH,0B7H,0BFH,0B7H,036H,036H,0B7H,037H
                db  03FH,03FH,037H,037H,037H,037H,037H,097H,037H,037H,03FH,03FH,037H,03FH,03FH,03FH,03FH,0BFH,0BFH,03FH,037H,01FH,03FH,03FH,0B7H,03FH,03FH,0B7H,037H,03FH,03FH,03FH
                db  0B7H,0BFH,03FH,0B7H,03FH,03FH,037H,037H,0BFH,0BFH,0BFH,0BFH,037H,037H,037H,0BFH,0B7H,0B7H,0B7H,0BFH,037H,0B7H,037H,017H,0BFH,0B7H,0B7H,0B7H,037H,0B7H,037H,0B7H
                db  037H,03FH,0B7H,037H,03FH,03FH,037H,03FH,037H,037H,037H,03FH,037H,03FH,03FH,037H,03FH,03FH,0BFH,037H,03FH,037H,037H,037H,0B7H,03FH,03FH,03FH,037H,03FH,037H,037H
                db  037H,0BFH,0B7H,0B7H,037H,0BFH,037H,03FH,0B7H,0B7H,0B7H,0BFH,03FH,0B7H,03FH,0B7H,0BFH,0BFH,0B7H,0AFH,03FH,0B7H,03FH,0BFH,0BFH,0B7H,0BFH,0BFH,0BFH,0B7H,0B7H,0B7H
                db  0B7H,037H,037H,037H,037H,037H,03FH,037H,037H,0BFH,03FH,03FH,0BFH,037H,037H,037H,037H,03FH,037H,03FH,037H,03FH,037H,03FH,037H,0BFH,037H,0B7H,036H,037H,03FH,037H
                db  0BFH,0BFH,037H,0B7H,017H,03FH,037H,0B7H,0B7H,0B7H,0B7H,0B7H,026H,037H,0B6H,037H,0BFH,0BFH,0B7H,0BFH,03EH,0B7H,037H,0BFH,0B7H,0B7H,0BFH,0BFH,03FH,0B7H,03FH,037H
                db  00BH,002H,003H,002H,00AH,002H,002H,002H,082H,002H,082H,002H,002H,002H,082H,002H,080H,080H,080H,008H,000H,000H,080H,000H,08AH,08AH,003H,00AH,082H,082H,002H,00AH
                db  092H,002H,08AH,00AH,082H,002H,082H,002H,002H,08AH,002H,002H,002H,00AH,002H,00AH,003H,00AH,08BH,083H,00AH,00AH,002H,002H,00BH,00AH,002H,002H,002H,002H,002H,00AH
                db  082H,002H,082H,002H,082H,00AH,082H,002H,08BH,082H,08BH,00BH,082H,082H,092H,00AH,082H,002H,082H,012H,082H,00AH,012H,002H,082H,082H,08AH,00AH,082H,08AH,08AH,002H
                db  00AH,00AH,082H,003H,00AH,00AH,08AH,002H,002H,002H,002H,00BH,002H,002H,002H,002H,08AH,002H,002H,002H,002H,002H,00AH,002H,002H,003H,002H,023H,00AH,00AH,002H,002H
                db  088H,000H,080H,008H,000H,010H,080H,000H,000H,080H,088H,000H,000H,000H,088H,000H,002H,00AH,08AH,002H,002H,00AH,082H,002H,082H,08AH,00AH,002H,08AH,082H,002H,002H
                db  082H,002H,082H,082H,082H,002H,082H,002H,002H,003H,002H,002H,002H,00BH,002H,002H,083H,00BH,00BH,00AH,00AH,00AH,002H,002H,002H,00BH,00AH,022H,002H,002H,002H,00AH
                db  082H,082H,082H,012H,082H,00AH,082H,002H,082H,082H,082H,002H,082H,082H,002H,002H,08AH,08AH,002H,00AH,082H,082H,002H,002H,00AH,00AH,082H,013H,002H,083H,082H,002H
                db  00AH,003H,082H,03AH,002H,00AH,01AH,002H,001H,000H,001H,000H,000H,001H,000H,000H,008H,000H,008H,000H,000H,000H,000H,000H,00AH,007H,002H,02BH,002H,006H,00AH,003H
                db  021H,013H,0FFH,011H,000H,02CH,00EH,0EBH,07EH,012H,023H,013H,00DH,0C2H,008H,0FFH,0C3H,000H,02CH,0F3H,0AFH,0D3H,022H,02FH,0D3H,023H,03EH,02CH,0D3H,022H,03EH,003H
;               db  0D3H,010H,0DBH,0FFH,0E6H,010H,00FH,00FH,0C6H,010H,0D3H,010H,031H,079H,02DH,0AFH,0D3H,008H,0DBH,008H,0E6H,008H,0C2H,01CH,02CH,03EH,004H,0D3H,009H,0C3H,038H,02CH
                db  0D3H,010H,03EH,000H,0E6H,010H,00FH,00FH,0C6H,010H,0D3H,010H,031H,079H,02DH,0AFH,0D3H,008H,0DBH,008H,0E6H,008H,0C2H,01CH,02CH,03EH,004H,0D3H,009H,0C3H,038H,02CH
;                             ^^^^ ^^^^
                db  0DBH,008H,0E6H,002H,0C2H,02DH,02CH,03EH,002H,0D3H,009H,0DBH,008H,0E6H,040H,0C2H,02DH,02CH,011H,000H,000H,006H,000H,03EH,010H,0F5H,0D5H,0C5H,0D5H,011H,086H,080H
                db  021H,0EBH,02CH,0DBH,009H,01FH,0DAH,050H,02CH,0E6H,01FH,0B8H,0C2H,050H,02CH,0DBH,008H,0B7H,0FAH,05CH,02CH,0DBH,00AH,077H,023H,01DH,0CAH,072H,02CH,01DH,0DBH,00AH
                db  077H,023H,0C2H,05CH,02CH,0E1H,011H,0EEH,02CH,001H,080H,000H,01AH,077H,0BEH,0C2H,0CBH,02CH,080H,047H,013H,023H,00DH,0C2H,079H,02CH,01AH,0FEH,0FFH,0C2H,090H,02CH
                db  013H,01AH,0B8H,0C1H,0EBH,0C2H,0C2H,02CH,0F1H,0F1H,02AH,0ECH,02CH,0CDH,0E5H,02CH,0D2H,0BBH,02CH,004H,004H,078H,0FEH,020H,0DAH,044H,02CH,006H,001H,0CAH,044H,02CH
                db  0DBH,008H,0E6H,002H,0C2H,0ADH,02CH,03EH,001H,0D3H,009H,0C3H,042H,02CH,03EH,080H,0D3H,008H,0C3H,000H,000H,0D1H,0F1H,03DH,0C2H,046H,02CH,03EH,043H,001H,03EH,04DH
                db  0FBH,032H,000H,000H,022H,001H,000H,047H,03EH,080H,0D3H,008H,078H,0D3H,001H,0D3H,011H,0D3H,005H,0D3H,023H,0C3H,0DAH,02CH,07AH,0BCH,0C0H,07BH,0BDH,0C9H,000H,000H

            end
