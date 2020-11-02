; *******************************************************************  DV - Divide
; *** This software is copyright 2004 by Michael H Riley          ***
; *** You have permission to use, modify, copy, and distribute    ***
; *** this software so long as this copyright notice is retained. ***
; *** This software may not be used in commercial applications    ***
; *** without express written permission from the author.         ***
; *******************************************************************

; RA = instruction table
; RB = Basic data
; RC = TBC Proram counter
; RD = TBC Stack

include    bios.inc
include    kernel.inc

org:       equ     2000h

           org     8000h
           lbr     0ff00h
           db      'TBRUN',0
           dw      0a000h
           dw      endrom+0a000h-org
           dw      org
           dw      endrom-org
           dw      org
           db      0

           org     2000h
           br      start

include    date.inc

start:     mov     r2,07fffh           ; put stack at top of memory
           ghi     ra                  ; copy argument address to rf
           phi     rf
           glo     ra
           plo     rf
loop1:     lda     rf                  ; look for first less <= space
           smi     33
           bdf     loop1
           dec     rf                  ; backup to char
           ldi     0                   ; need proper termination
           str     rf
           ghi     ra                  ; back to beginning of name
           phi     rf
           glo     ra
           plo     rf
           ldi     high fildes         ; get file descriptor
           phi     rd
           ldi     low fildes
           plo     rd
           ldi     0                   ; flags for open
           plo     r7
           sep     scall               ; attempt to open file
           dw      o_open
           bnf     opened              ; jump if file was opened
           ldi     high errmsg         ; get error message
           phi     rf
           ldi     low errmsg
           plo     rf
           sep     scall               ; display it
           dw      o_msg
           lbr     o_wrmboot           ; and return to os
opened:    mov     rf,program          ; point to program buffer
           mov     rc,7f00h            ; set to read maximum amount
           sep     scall               ; read the header
           dw      o_read
           glo     rc                  ; need to find final address
           adi     program.0
           plo     rf
           ghi     rc
           adci    program.1
           phi     rf
           push    rf                  ; save it for now
           sep     scall               ; close the file
           dw      o_close

           mov     rc,program          ; point to loaded program
           mov     rd,07effh           ; set TBC stack
           mov     rb,07000h           ; basic data
           ldi     022h                ; location for end of memory
           plo     rb
           ldi     06fh                ; high byte
           str     rb
           inc     rb
           ldi     0ffh                ; low byte
           str     rb
           inc     rb
           pop     rf                  ; get free address
           ghi     rf                  ; and store it
           str     rb
           inc     rb
           glo     rf
           str     rb
           ldi     pstart.0            ; need address of program start
           plo     rb                  ; set into basic segment
           ldi     program.1           ; get program start
           str     rb                  ; and store
           inc     rb
           ldi     program.0
           str     rb
           dec     rb
           ldn     rc                  ; get first byte
           smi     0ffh                ; see if jump table present
           lbnz    mainlp              ; jump if not
           inc     rc                  ; move to first entry
jt1:       lda     rc                  ; get first byte of entry
           str     r2                  ; store it for or
           lda     rc                  ; get next byte
           or                          ; need to check for zero terminator
           lbz     jtdn                ; terminator found
           inc     rc                  ; move past address
           inc     rc
           lbr     jt1                 ; and keep looking for end
jtdn:      ghi     rc                  ; write new start offset
           str     rb
           inc     rb
           glo     rc
           str     rb

mainlp:    lda     rc                  ; get next command byte
           plo     re                  ; save it
           shl                         ; commands addresses are two bytes
           plo     rf                  ; store low offset
           ldi     0                   ; need high of offset
           shlc                        ; shift in the carry
           phi     rf                  ; rf now has offset
           glo     rf                  ; now add in command table address
           adi     cmdtab.0            ; add low of command table
           plo     rf                  ; store it here
           ghi     rf                  ; now high byte
           adci    cmdtab.1            ; of command table address
           phi     rf                  ; rf now points to entry
           mov     ra,jump+1           ; point to jump address
           lda     rf
           str     ra
           inc     ra
           lda     rf
           str     ra
           glo     re                  ; recover original command byte
jump:      lbr     0                   ; will be changed to command handler


op_lb:     lda     rc                  ; retrieve next program byte
           str     rd                  ; store onto stack
           dec     rd                  ; and decrement
           lbr     mainlp              ; back to main loop

op_ln:     lda     rc                  ; read high byte of number
           str     rd                  ; place on stack
           dec     rd 
           lda     rc                  ; get low byte of number
           str     rd                  ; place on stack
           dec     rd                  ; and decrement
           lbr     mainlp              ; back to main loop

op_sv:     inc     rd                  ; point to number on stack
           lda     rd                  ; get low byte
           plo     rf                  ; store here
           lda     rd                  ; get high byte
           phi     rf                  ; rf now has number to store
           ldn     rd                  ; get variable address
           plo     rb                  ; set into basic pointer
           ghi     rf                  ; store value into variable
           str     rb
           inc     rb
           glo     rf                  ; low byte
           str     rb
           lbr     mainlp              ; then back to main loop

op_fv:     inc     rd                  ; point to variable number
           ldn     rd                  ; get variable address
           plo     rb                  ; rb now points to variable data
           lda     rb                  ; retrieve msb
           str     rd                  ; place onto satck
           dec     rd
           ldn     rb                  ; retrieve it
           str     rd                  ; place onto stack
           dec     rd
           lbr     mainlp              ; then back to main loop

op_ad:     inc     rd                  ; retrieve first number from stack
           lda     rd
           plo     rf                  ; put it here
           lda     rd                  ; next get high byte
           phi     rf                  ; rf now has first number
           lda     rd                  ; get lsb of next number
           str     r2                  ; store for add
           glo     rf                  ; get first number lsb
           add                         ; and add
           plo     rf                  ; store here
           ldn     rd                  ; now get high byte of first number
           str     r2                  ; store for add
           ghi     rf                  ; get high byte of second number
           adc                         ; and add
           str     rd                  ; place answer on stack
           dec     rd
           glo     rf                  ; lsb of answer
           str     rd                  ; to stack
           dec     rd
           lbr     mainlp              ; back to main loop


op_mp:     inc     rd                  ; retrieve first number from stack
           lda     rd
           plo     rf                  ; put it here
           lda     rd                  ; next get high byte
           phi     rf                  ; rf now has first number
           lda     rd                  ; get lsb of next number
           plo     r7                  ; put it here
           ldn     rd
           phi     r7
           sep     scall               ; call multiply routine
           dw      mul16
           ghi     rf
           str     rd                  ; place answer on stack
           dec     rd
           glo     rf                  ; lsb of answer
           str     rd                  ; to stack
           dec     rd
           lbr     mainlp              ; back to main loop

op_dv:     push    rb                  ; save consumed registers
           push    rc
           inc     rd                  ; retrieve first number from stack
           lda     rd
           plo     r7                  ; put it here
           lda     rd                  ; next get high byte
           phi     r7                  ; rf now has first number
           lda     rd                  ; get lsb of next number
           plo     rb                  ; put it here
           ldn     rd
           phi     rb
           sep     scall               ; call multiply routine
           dw      div16
           ghi     rc
           str     rd                  ; place answer on stack
           dec     rd
           glo     rc                  ; lsb of answer
           str     rd                  ; to stack
           dec     rd
           pop     rc                  ; recover consumed registers
           pop     rb
           lbr     mainlp              ; back to main loop

op_an:     inc     rd                  ; retrieve first number from stack
           lda     rd
           plo     rf                  ; put it here
           lda     rd                  ; next get high byte
           phi     rf                  ; rf now has first number
           lda     rd                  ; get lsb of next number
           str     r2                  ; store for and
           glo     rf                  ; get first number lsb
           and                         ; and and
           plo     rf                  ; store here
           ldn     rd                  ; now get high byte of first number
           str     r2                  ; store for and
           ghi     rf                  ; get high byte of second number
           and                         ; and and
           str     rd                  ; place answer on stack
           dec     rd
           glo     rf                  ; lsb of answer
           str     rd                  ; to stack
           dec     rd
           lbr     mainlp              ; back to main loop

op_or:     inc     rd                  ; retrieve first number from stack
           lda     rd
           plo     rf                  ; put it here
           lda     rd                  ; next get high byte
           phi     rf                  ; rf now has first number
           lda     rd                  ; get lsb of next number
           str     r2                  ; store for or
           glo     rf                  ; get first number lsb
           or                          ; and or
           plo     rf                  ; store here
           ldn     rd                  ; now get high byte of first number
           str     r2                  ; store for or
           ghi     rf                  ; get high byte of second number
           or                          ; and or
           str     rd                  ; place answer on stack
           dec     rd
           glo     rf                  ; lsb of answer
           str     rd                  ; to stack
           dec     rd
           lbr     mainlp              ; back to main loop

op_xr:     inc     rd                  ; retrieve first number from stack
           lda     rd
           plo     rf                  ; put it here
           lda     rd                  ; next get high byte
           phi     rf                  ; rf now has first number
           lda     rd                  ; get lsb of next number
           str     r2                  ; store for xor
           glo     rf                  ; get first number lsb
           xor                         ; and xor
           plo     rf                  ; store here
           ldn     rd                  ; now get high byte of first number
           str     r2                  ; store for xor
           ghi     rf                  ; get high byte of second number
           xor                         ; and xor
           str     rd                  ; place answer on stack
           dec     rd
           glo     rf                  ; lsb of answer
           str     rd                  ; to stack
           dec     rd
           lbr     mainlp              ; back to main loop

op_su:     inc     rd                  ; retrieve first number from stack
           lda     rd
           plo     rf                  ; put it here
           lda     rd                  ; next get high byte
           phi     rf                  ; rf now has first number
           lda     rd                  ; get lsb of next number
           str     r2                  ; store for subtract
           glo     rf                  ; get first number lsb
           sd                          ; and subtract
           plo     rf                  ; store here
           ldn     rd                  ; now get high byte of first number
           str     r2                  ; store for add
           ghi     rf                  ; get high byte of second number
           sdb                         ; and subtract
           str     rd                  ; place answer on stack
           dec     rd
           glo     rf                  ; lsb of answer
           str     rd                  ; to stack
           dec     rd
           lbr     mainlp              ; back to main loop

op_pn:     inc     rd                  ; retrieve number from stack
           lda     rd                  ; lsb
           plo     rf                  ; store here for a moment
           ldn     rd
           phi     rf
           push    rd                  ; save rd
           mov     rd,rf               ; move number for call
           mov     rf,buffer           ; pointer to buffer
           sep     scall               ; call bios to convert number
           dw      f_intout            ; 
           ldi     0                   ; add terminator
           str     rf
           mov     rf,buffer           ; move back to beginning of buffer
           sep     scall               ; and write to terminal
           dw      f_msg
           pop     rd                  ; recover basic stack pointer
           lbr     mainlp              ; all done

op_nl:     sep     scall               ; print new line
           dw      f_inmsg
           db      10,13,0
           lbr     mainlp              ; then back to main loop

op_pt:     ldi     9                   ; tab character
           sep     scall               ; display it
           dw      f_type
           lbr     mainlp              ; then back to main

op_pc:     lda     rc                  ; get next program byte
           plo     rf                  ; save it
           ani     07fh                ; strip high bit
           sep     scall               ; display it
           dw      f_type
           glo     rf                  ; recover byte
           shl                         ; shift high bit to df
           lbnf    op_pc               ; jump if more to print
           lbr     mainlp              ; otherwise back to main loop

joffset:   ldi     pstart.0+1          ; offset to program start
           plo     rb
           glo     rc                  ; get low of pc
           sex     rb                  ; point x to program start
           add
           plo     rc                  ; put into pc
           dec     rb                  ; point to msb
           ghi     rc                  
           adc
           phi     rc                  ; rc now has proper program counter
           sex     r2                  ; put x back
           lbr     mainlp              ; back to main loop


op_j:      ani     07fh                ; strip high bit
           phi     rf                  ; save it
           lda     rc                  ; get next byte from program
           plo     rc                  ; put into low of pc
           ghi     rf                  ; get high byte
           phi     rc                  ; rc now has target of jump
           lbr     joffset             ; add in program offset

op_js:     smi     030h                ; strip code
           phi     rf                  ; save it
           lda     rc                  ; get next byte from program
           plo     rf                  ; put into low of pc
           ghi     rc                  ; save current pc
           str     rd
           dec     rd
           glo     rc
           str     rd
           dec     rd
           mov     rc,rf               ; now jump to subroutine
           lbr     joffset             ; add in program offset

op_rt:     inc     rd                  ; recover calling address
           lda     rd
           plo     rc
           ldn     rd
           phi     rc
           lbr     mainlp              ; and then continue execution

op_ne:     inc     rd                  ; recover number on stack
           lda     rd                  ; get low byte
           xri     0ffh                ; invert it
           plo     rf                  ; put it here
           ldn     rd                  ; get high byte
           xri     0ffh                ; invert it
           phi     rf                  ; store it
           inc     rf                  ; make 2s compliment
           ghi     rf                  ; put it back on stack
           str     rd
           dec     rd
           glo     rf
           str     rd
           dec     rd
           lbr     mainlp              ; back to main loop

op_cp:     inc     rd                  ; get second number off stack
           lda     rd                  ; lsb
           plo     rf
           lda     rd                  ; msb
           phi     rf
           lda     rd                  ; get comparison type
           plo     re                  ; save it
           lda     rd                  ; lsb of first number
           str     r2                  ; store for subtraction
           glo     rf                  ; lsb of second number
           sd                          ; perform subtraction
           plo     rf                  ; store result
           ldn     rd                  ; msb of first number
           str     r2                  ; store for subtraction
           ghi     rf                  ; msb of second number
           sdb                         ; subtract
           str     r2                  ; store for or
           shl                         ; shift sign bit into df
           glo     rf                  ; get low byte
           or                          ; combine with first
           plo     rf                  ; rf.0 now holds whether equal or not
           shlc                        ; shift df into d
           phi     rf                  ; rf.1 now holds df
           glo     re                  ; get comparison
           smi     1                   ; check for 1 <
           lbz     op_cp_lt            ; jump if so
           smi     1                   ; check for =
           lbz     op_cp_eq            ; jump if so
           smi     1                   ; check for <=
           lbz     op_cp_le
           smi     1                   ; check for >
           lbz     op_cp_gt
           smi     1                   ; check for <>
           lbz     op_cp_ne
op_cp_ge:  glo     rf                  ; see if equal
           lbz     op_cp_gd            ; skip next if so
           ghi     rf                  ; get df
           shr                         ; shift back into df
           lbnf    op_cp_gd            ; jump if positive
           lbr     mainlp              ; otherwise continue
op_cp_lt:  glo     rf                  ; need to see if equal
           lbz     mainlp              ; fails if they were
           ghi     rf                  ; get df
           shr                         ; shift for check
           lbdf    op_cp_gd            ; jump if true
           lbr     mainlp              ; otherwise continue
op_cp_eq:  glo     rf                  ; need to see if equal
           lbz     op_cp_gd            ; jump if so
           lbr     mainlp              ; otherwise continue
op_cp_le:  glo     rf                  ; see if equal
           lbz     op_cp_gd            ; skip next if so
           ghi     rf                  ; get df
           shr                         ; shift back into df
           lbdf    op_cp_gd            ; jump if negative
           lbr     mainlp              ; otherwise continue
op_cp_gt:  glo     rf                  ; need to see if equal
           lbz     mainlp              ; fails if they were
           ghi     rf                  ; get df
           shr                         ; shift for check
           lbnf    op_cp_gd            ; jump if greater
           lbr     mainlp              ; otherwise continue
op_cp_ne:  glo     rf                  ; need to see if equal
           lbnz    op_cp_gd            ; jump if not
           lbr     mainlp              ; otherwise continue
op_cp_gd:  inc     rc                  ; skip next 2 bytes
           inc     rc
           lbr     mainlp              ; and jump to main loop

op_pe:     inc     rd                  ; get address from stack
           lda     rd
           plo     rf
           ldn     rd
           phi     rf
           ldi     0                   ; high byte of peek'd value is zero
           str     rd
           dec     rd
           ldn     rf                  ; peek byte
           str     rd                  ; and put on stack
           dec     rd
           lbr     mainlp              ; back to main loop

op_po:     inc     rd                  ; retrieve value from stack
           lda     rd
           plo     rf
           lda     rd
           phi     rf
           lda     rd                  ; get poke address
           plo     r9
           ldn     rd
           phi     r9
           glo     rf                  ; get low byte of value
           str     r9                  ; and store it
           lbr     mainlp              ; then back to main loop

op_de:     inc     rd                  ; get address from stack
           lda     rd
           plo     rf
           ldn     rd
           phi     rf
           lda     rf                  ; read msb of byte
           str     rd                  ; and put on stack
           dec     rd
           ldn     rf                  ; peek low byte
           str     rd                  ; and put on stack
           dec     rd
           lbr     mainlp              ; back to main loop

op_do:     inc     rd                  ; retrieve value from stack
           lda     rd
           plo     rf
           lda     rd
           phi     rf
           lda     rd                  ; get poke address
           plo     r9
           ldn     rd
           phi     r9
           ghi     rf                  ; get high byte
           str     r9                  ; and poke it
           inc     r9                  ; point to lsb
           glo     rf                  ; get low byte of value
           str     r9                  ; and store it
           lbr     mainlp              ; then back to main loop

op_sp:     inc     rd                  ; just move stack 2 places
           inc     rd
           lbr     mainlp              ; and back to main loop

op_ds:     inc     rd                  ; get value on stack
           lda     rd
           plo     rf
           ldn     rd
           phi     rf                  ; rf now has value on stack
           dec     rd                  ; move stack back down
           dec     rd
           ghi     rf                  ; now push value again
           str     rd
           dec     rd
           glo     rf
           str     rd
           dec     rd
           lbr     mainlp              ; back to main loop

op_ws:     lbr     o_wrmboot           ; return to Elf/OS

op_us:     sep     scall               ; call ml subroutine
usr_addr:  dw      0
           ghi     rf                  ; msb of return value
           str     rd                  ; store on stack
           dec     rd
           glo     rf                  ; lsb
           str     rd
           dec     rd
           lbr     mainlp              ; back to main loop

op_ou:     inc     rd                  ; get value from stack
           lda     rd                  ; lsb
           str     r2                  ; store for out
           lda     rd                  ; msb
           lda     rd                  ; now get port
           ani     07h                 ; clear any odd bits
           adi     060h                ; convert to actual instruction
           plo     re                  ; save for a moment
           mov     rf,ioinst           ; point to instruction
           glo     re                  ; retrieve instructon
           str     rf                  ; and store it
ioinst:    db      0                   ; out instruction will be placed here
           dec     r2                  ; undo out increment
           lbr     mainlp              ; back to main loop

op_in:     inc     rd                  ; need to get port
           lda     rd                  ; now get port
           ani     07h                 ; clear any odd bits
           adi     068h                ; convert to actual instruction
           plo     re                  ; save for a moment
           mov     rf,inpinst          ; point to instruction
           glo     re                  ; retrieve instructon
           str     rf                  ; and store it
inpinst:   db      0                   ; out instruction will be placed here
           ldi     0                   ; msb of result is zero
           str     rd                  ; place on stack
           dec     rd
           ldn     r2                  ; get input byte
           str     rd                  ; and place on stack
           dec     rd
           lbr     mainlp              ; then back to main loop

op_fg:     inc     rd                  ; do not need value on stack
           inc     rd
           ldi     0                   ; start with zero
           bn1     op_fg1              ; jump if ef1 is zero
           ori     1                   ; signal ef1 set
op_fg1:    bn2     op_fg2              ; jump if ef2 is zero
           ori     2                   ; signal ef2 set
op_fg2:    bn3     op_fg3              ; jump if ef3 is zero
           ori     4                   ; signal ef3 set
op_fg3:    bn4     op_fg4              ; jump if ef4 is zero
           ori     8                   ; signal ef4 set
op_fg4:    plo     re                  ; save for a moment
           ldi     0                   ; msb of result is zero
           str     rd                  ; place on stack
           dec     rd
           glo     re                  ; get flags state
           str     rd                  ; and store onto stack
           dec     rd
           lbr     mainlp              ; back to main loop

op_gl:     sep     scall               ; display ?
           dw      f_inmsg
           db      '? ',0
           mov     rf,buffer           ; point to input buffer
           push    rc                  ; rc gets clobbeted by f_input
           sep     scall               ; get input from user
           dw      f_input
           pop     rc
           push    rd                  ; save basic stack
           mov     rf,buffer           ; point to input text
           sep     scall               ; Convert ascii to binary
           dw      atoi
           mov     rf,rd               ; move number
           pop     rd                  ; recover basic stack
           ghi     rf                  ; and push input number to it
           str     rd
           dec     rd
           glo     rf
           str     rd
           dec     rd
           sep     scall
           dw      f_inmsg
           db      10,13,0
           lbr     mainlp              ; back to main loop

op_pl:     inc     rd                  ; retrieve y value from stack
           lda     rd
           plo     rf
           lda     rd
           lda     rd                  ; get x value from stack
           phi     rf
           push    rd                  ; save basic stack
           mov     rd,rf               ; move coordinates
           sep     scall               ; position cursor
           dw      gotoxy
           pop     rd                  ; recover basic stack
           lbr     mainlp              ; then back to main loop

op_cl:     ldi     00ch                ; form feed
           sep     scall               ; display it
           dw      f_type
           lbr     mainlp              ; then back to main loop

op_sx:     ani     07h                 ; only 0-7 allowed
           shr                         ; 2 bytes per stack entry
           str     r2                  ; store for add
           inc     rd                  ; rd now points at tos
           glo     rd                  ; add in offset
           add
           plo     rf                  ; rf will point to second item
           ghi     rd
           adci    0                   ; propagate carry
           phi     rf                  ; rf now points to second entry
           ldn     rd                  ; get lsb from tos
           plo     re                  ; set aside
           ldn     rf                  ; get byte from stack
           str     rd                  ; put into tos
           glo     re                  ; get byte from os
           str     rf                  ; and store into entry
           inc     rd                  ; now point at msb
           inc     rf
           ldn     rd                  ; get lsb from tos
           plo     re                  ; set aside
           ldn     rf                  ; get byte from stack
           str     rd                  ; put into tos
           glo     re                  ; get byte from os
           str     rf                  ; and store into entry
           dec     rd                  ; put stack back where it belongs
           dec     rd
           lbr     mainlp              ; back to main loop

op_tj:     mov     r7,program+1        ; point to jump table
           inc     rd
           lda     rd                  ; get line number being jumped to
           plo     r8
           ldn     rd                  ; get msb
           phi     r8                  ; r8 now has line number
op_tjlp:   lda     r7                  ; get table line msb
           smi     0ffh                ; check for end of table
           lbnz    op_tj1              ; jump if not
           ldn     r7                  ; need to check 2nd byte
           smi     0ffh                ; check it
           lbnz    op_tj1              ; jump if not
           sep     scall               ; print error
           dw      f_inmsg
           db      'Invalid jump. Terminating',10,13,0
           lbr     o_wrmboot           ; return to Elf/OS
op_tj1:    ldn     r7                  ; get lsb
           dec     r7                  ; point to msb
           str     r2                  ; store for comparison
           glo     r8                  ; lsb of jump
           sm                          ; compare
           lbnz    op_tjno             ; jump if no match
           ldn     r7                  ; get msb
           str     r2                  ; store for comparison
           ghi     r8                  ; get high of jump address
           sm
           lbnz    op_tjno             ; jump if not it
           inc     r7                  ; correct line found, move to address
           inc     r7
           lda     r7                  ; get msb of jump
           phi     rc                  ; place into pc
           ldn     r7                  ; get lsb of jump
           plo     rc
           lbr     joffset             ; jump to add program offset
op_tjno:   inc     r7                  ; move to next entry
           inc     r7
           inc     r7
           inc     r7
           lbr     op_tjlp             ; and loop back to check
          
op_ts:     inc     rd                  ; get line number from stack
           lda     rd
           plo     r8
           ldn     rd
           phi     r8
           ghi     rc                  ; push current pc
           str     rd
           dec     rd
           glo     rc
           str     rd
           dec     rd
           mov     r7,program+1        ; point to jump table
           lbr     op_tjlp             ; and find address for jump

; *********************************************************
; ***** Takes value in D and makes 2 char ascii in RF *****
; *********************************************************
itoa:      plo     rf                ; save value
           ldi     0                 ; clear high byte
           phi     rf
           glo     rf                ; recover low
itoalp:    smi     10                ; see if greater than 10
           lbnf    itoadn            ; jump if not
           plo     rf                ; store new value
           ghi     rf                ; get high character
           adi     1                 ; add 1
           phi     rf                ; and put it back
           glo     rf                ; retrieve low character
           lbr     itoalp            ; and keep processing
itoadn:    glo     rf                ; get low character
           adi     030h              ; convert to ascii
           plo     rf                ; put it back
           ghi     rf                ; get high character
           adi     030h              ; convert to ascii
           phi     rf                ; put it back
           sep     sret              ; return to caller

; *********************************************
; ***** Send vt100 sequence to set cursor *****
; ***** RD.0 = y                          *****
; ***** RD.1 = x                          *****
; *********************************************
gotoxy:    ldi     27                ; escape character
           sep     scall             ; write it
           dw      f_type
           ldi     '['               ; square bracket
           sep     scall             ; write it
           dw      f_type
           glo     rd                ; get x
           sep     scall             ; convert to ascii
           dw      itoa
           ghi     rf                ; high character
           sep     scall             ; write it
           dw      f_type
           glo     rf                ; low character
           sep     scall             ; write it
           dw      f_type
           ldi     ';'               ; need separator
           sep     scall             ; write it
           dw      f_type
           ghi     rd                ; get y
           sep     scall             ; convert to ascii
           dw      itoa
           ghi     rf                ; high character
           sep     scall             ; write it
           dw      f_type
           glo     rf                ; low character
           sep     scall             ; write it
           dw      f_type
           ldi     'H'               ; need terminator for position
           sep     scall             ; write it
           dw      f_type
           sep     sret              ; return to caller

; *********************************************
; *** Function to multiply 2 16 bit numbers ***
; *** RF=R7*RF                              ***
; *********************************************
mul16:     ldi     0                   ; zero out total
           phi     r8
           plo     r8
           phi     r9
           plo     r9
           sex     r2                  ; make sure X points to stack
mulloop:   glo     r7                  ; get low of multiplier
           lbnz    mulcont             ; continue multiplying if nonzero
           ghi     r7                  ; check hi byte as well
           lbnz    mulcont
           ghi     r8                  ; transfer answer
           phi     rf
           glo     r8
           plo     rf
           sep     sret                ; return to caller
mulcont:   ghi     r7                  ; shift multiplier
           shr
           phi     r7
           glo     r7
           shrc
           plo     r7
           lbnf    mulcont2            ; loop if no addition needed
           glo     rf                  ; add 6 to 8
           str     r2
           glo     r8
           add
           plo     r8
           ghi     rf
           str     r2
           ghi     r8
           adc
           phi     r8
           glo     r9                  ; carry into high word
           adci    0
           plo     r9
           ghi     r9
           adci    0
           phi     r9
mulcont2:  glo     rf                  ; shift first number
           shl
           plo     rf
           ghi     rf
           shlc
           phi     rf
           lbr     mulloop             ; loop until done

; ************************************
; *** make both arguments positive ***
; *** Arg1 RB                      ***
; *** Arg2 R7                      ***
; *** Returns D=0 - signs same     ***
; ***         D=1 - signs difer    ***
; ************************************
mdnorm:    ghi     rb                  ; get high byte if divisor
           str     r2                  ; store for sign check
           ghi     r7                  ; get high byte of dividend
           xor                         ; compare
           shl                         ; shift into df
           ldi     0                   ; convert to 0 or 1
           shlc                        ; shift into D
           plo     re                  ; store into sign flag
           ghi     rb                  ; need to see if RB is negative
           shl                         ; shift high byte to df
           lbnf    mdnorm2             ; jump if not
           ghi     rb                  ; 2s compliment on RB
           xri     0ffh
           phi     rb
           glo     rb
           xri     0ffh
           plo     rb
           inc     rb
mdnorm2:   ghi     r7                  ; now check r7 for negative
           shl                         ; shift sign bit into df
           lbnf    mdnorm3             ; jump if not
           ghi     r7                  ; 2 compliment on R7
           xri     0ffh
           phi     r7
           glo     r7
           xri     0ffh
           plo     r7
           inc     r7
mdnorm3:   glo     re                  ; recover sign flag
           sep     sret                ; and return to caller
; *** RC = RB/R7
; *** RB = remainder
; *** uses R8 and R9
div16:     sep     scall               ; normalize numbers
           dw      mdnorm
           plo     re                  ; save sign comparison
           ldi     0                   ; clear answer
           phi     rc
           plo     rc
           phi     r8                  ; set additive
           plo     r8
           inc     r8
           glo     r7                  ; check for divide by 0
           lbnz    d16lp1
           ghi     r7
           lbnz    d16lp1
           ldi     0ffh                ; return 0ffffh as div/0 error
           phi     rc
           plo     rc
           sep     sret                ; return to caller
d16lp1:    ghi     r7                  ; get high byte from r7
           ani     128                 ; check high bit
           lbnz    divst               ; jump if set
           glo     r7                  ; lo byte of divisor
           shl                         ; multiply by 2
           plo     r7                  ; and put back
           ghi     r7                  ; get high byte of divisor
           shlc                        ; continue multiply by 2
           phi     r7                  ; and put back
           glo     r8                  ; multiply additive by 2
           shl
           plo     r8
           ghi     r8
           shlc
           phi     r8
           lbr     d16lp1              ; loop until high bit set in divisor
divst:     glo     r7                  ; get low of divisor
           lbnz    divgo               ; jump if still nonzero
           ghi     r7                  ; check hi byte too
           lbnz    divgo
           glo     re                  ; get sign flag
           shr                         ; move to df
           lbnf    divret              ; jump if signs were the same
           ghi     rc                  ; perform 2s compliment on answer
           xri     0ffh
           phi     rc
           glo     rc
           xri     0ffh
           plo     rc
           inc     rc
divret:    sep     sret                ; jump if done
divgo:     ghi     rb                  ; copy dividend
           phi     r9
           glo     rb
           plo     r9
           glo     r7                  ; get lo of divisor
           stxd                        ; place into memory
           irx                         ; point to memory
           glo     rb                  ; get low byte of dividend
           sm                          ; subtract
           plo     rb                  ; put back into r6
           ghi     r7                  ; get hi of divisor
           stxd                        ; place into memory
           irx                         ; point to byte
           ghi     rb                  ; get hi of dividend
           smb                         ; subtract
           phi     rb                  ; and put back
           lbdf    divyes              ; branch if no borrow happened
           ghi     r9                  ; recover copy
           phi     rb                  ; put back into dividend
           glo     r9
           plo     rb
           lbr     divno               ; jump to next iteration
divyes:    glo     r8                  ; get lo of additive
           stxd                        ; place in memory
           irx                         ; point to byte
           glo     rc                  ; get lo of answer
           add                         ; and add
           plo     rc                  ; put back
           ghi     r8                  ; get hi of additive
           stxd                        ; place into memory
           irx                         ; point to byte
           ghi     rc                  ; get hi byte of answer
           adc                         ; and continue addition
           phi     rc                  ; put back
divno:     ghi     r7                  ; get hi of divisor
           shr                         ; divide by 2
           phi     r7                  ; put back
           glo     r7                  ; get lo of divisor
           shrc                        ; continue divide by 2
           plo     r7
           ghi     r8                  ; get hi of divisor
           shr                         ; divide by 2
           phi     r8                  ; put back
           glo     r8                  ; get lo of divisor
           shrc                        ; continue divide by 2
           plo     r8
           lbr     divst               ; next iteration

; *******************
; *** Process RND ***
; *******************
op_rn:     ldi     16                  ; need to get 16 bits
rnd_lp:    stxd                        ; save count
           sep     scall               ; get random bit
           dw      fn_lfsr
           irx                         ; recover count
           ldx
           smi     1                   ; minus 1
           lbnz    rnd_lp              ; keep looping until all bits read
           mov     r7,lfsr
           lda     r7                  ; retrieve 16 bits wroth
           plo     rf
           lda     r7
           ani     07fh                ; no negative numbers
           phi     rf
           push    rb                  ; save consumed registers
           push    rc
           push    rf                  ; save random number
           inc     rd                  ; get range
           lda     rd
           plo     r7
           ldn     rd
           phi     r7
           push    r7                  ; save range
           mov     rb,rf               ; move random number here
           sep     scall               ; perform division
           dw      div16
           mov     rf,rc               ; move result to rf
           pop     r7                  ; get range
           sep     scall               ; and multiply
           dw      mul16               ; RF now even amount
           pop     r7                  ; get original random number
           glo     rf                  ; subtract rf from r7
           str     r2
           glo     r7
           sm
           plo     r7
           ghi     rf
           str     r2
           ghi     r7
           smb
           phi     r7
           ghi     r7                  ; put result on stack
           str     rd
           dec     rd
           glo     r7
           str     rd
           dec     rd
           pop     rc                  ; recover consumed registers
           pop     rb
           lbr     mainlp              ; back to main loop


; ********************************
; *** Get random bit from LFSR ***
; ********************************
fn_lfsr:   ldi     high lfsr           ; point to lfsr
           phi     r7
           ldi     low lfsr
           plo     r7
           inc     r7                  ; point to lsb
           inc     r7
           inc     r7
           ldn     r7                  ; retrieve it
           plo     re                  ; put into re  ( have bit 0)
           shr                         ; shift bit 1 into first position
           str     r2                  ; xor with previous value
           glo     re
           xor
           plo     re                  ; keep copy
           ldn     r2                  ; get value
           shr                         ; shift bit 2 into first position
           str     r2                  ; and combine
           glo     re
           xor
           plo     re
           ldn     r2                  ; now shift to bit 4
           shr
           shr
           str     r2                  ; and combine
           glo     re
           xor
           plo     re
           ldn     r2                  ; now shift to bit 6
           shr
           shr
           str     r2                  ; and combine
           glo     re
           xor
           plo     re
           dec     r7                  ; point to lfsr msb
           dec     r7
           dec     r7
           ldn     r7                  ; retrieve it
           shl                         ; shift high bit to low
           shlc
           str     r2                  ; combine with previous value
           glo     re
           xor
           xri     1                   ; combine with a final 1
           shr                         ; shift new bit into DF
           ldn     r7                  ; now shift the register
           shrc
           str     r7
           inc     r7                  ; now byte 1
           ldn     r7                  ; now shift the register
           shrc
           str     r7
           inc     r7                  ; now byte 2
           ldn     r7                  ; now shift the register
           shrc
           str     r7
           inc     r7                  ; now byte 3
           ldn     r7                  ; now shift the register
           shrc
           str     r7
           shr                         ; shift result bit into DF
           sep     sret                ; and return

atoi:      ldi     0                   ; set result to zero
           plo     rd
           phi     rd
           plo     re                  ; zero negative flag
           ldn     rf                  ; get byte from buffer
           smi     '-'                 ; see if negative
           lbnz    atoilp              ; jump if not
           inc     re                  ; set negative flag
           inc     rf                  ; and increment buffer
atoilp:    lda     rf                  ; get byte
           smi     030h                ; convert to binary
           lbnf    atoidn              ; jump if it was not 0-9
           smi     10                  ; check for upper range
           lbdf    atoidn              ; jump if above numerals
           adi     10                  ; but it back to normal
           stxd                        ; save it
           glo     rd                  ; multiply by 2
           shl
           plo     rd
           plo     r7                  ; put a copy here too
           ghi     rd
           shlc
           phi     rd
           phi     r7                  ; r7 now has x2
           glo     rd                  ; multiply by 4
           shl
           plo     rd
           ghi     rd
           shlc
           phi     rd
           glo     rd                  ; multiply by 8
           shl
           plo     rd
           ghi     rd
           shlc
           phi     rd
           glo     r7                  ; multiply by 10
           str     r2
           glo     rd
           add
           plo     rd
           ghi     r7
           str     r2
           ghi     rd
           adc
           phi     rd                  ; base number has now been multiplied by 10
           irx                         ; point back to new digit
           glo     rd                  ; and add to total
           add
           plo     rd
           ghi     rd
           adci    0                   ; propagate carry
           phi     rd
           lbr     atoilp              ; loop back for more digits
atoidn:    glo     re                  ; see if negative
           lbz     atoiret             ; jump if not
           glo     rd                  ; perform 2s compliment
           xri     0ffh
           plo     rd
           ghi     rd
           xri     0ffh
           phi     rd
           inc     rd
atoiret:   sep     sret                ; return to caller

cmdtab:    dw      op_sx               ; 00  SX 0
           dw      op_sx               ; 01  SX 1
           dw      op_sx               ; 02  SX 2
           dw      op_sx               ; 03  SX 3
           dw      op_sx               ; 04  SX 4
           dw      op_sx               ; 05  SX 5
           dw      op_sx               ; 06  SX 6
           dw      op_sx               ; 07  SX 7
           dw      mainlp              ; 08
           dw      op_lb               ; 09  LB - Byte to stack
           dw      op_ln               ; 0a  LN - Number to stack
           dw      op_ds               ; 0b  DS - Duplicate top 2 stack values
           dw      op_sp               ; 0c  SP - Pop 2 stack values
           dw      op_in               ; 0d  IN - INP
           dw      op_pe               ; 0e  PE - Peek
           dw      op_po               ; 0f  PO - Poke
           dw      op_ou               ; 10  OU - Out
           dw      op_fg               ; 11  FG - Flg
           dw      op_fv               ; 12  FV - Fetch variable
           dw      op_sv               ; 13  SV - Store variable
           dw      op_de               ; 14  DE - Dpeek
           dw      op_do               ; 15  DO - Dpoke
           dw      op_pl               ; 16  PL - Plot
           dw      op_ne               ; 17  NE - Negate
           dw      op_ad               ; 18  AD - Add
           dw      op_su               ; 19  SU - Subtract
           dw      op_mp               ; 1a  MP - Multiply
           dw      op_dv               ; 1b  DV - Divide
           dw      op_cp               ; 1c  CP - Compare
           dw      op_ts               ; 1d  TS - Table gosub
           dw      op_tj               ; 1e  TJ - Table jump
           dw      mainlp              ; 1f
           dw      op_pn               ; 20  PN - Print Number
           dw      mainlp              ; 21
           dw      op_pt               ; 22  PT - Print tab
           dw      op_nl               ; 23  NL - Print New Line
           dw      op_pc               ; 24  PC - Print literal string
           dw      op_an               ; 25  AN - And
           dw      op_or               ; 26  OR - Or
           dw      op_gl               ; 27  GL - Input line
           dw      op_xr               ; 28  XR - Xor
           dw      mainlp              ; 29
           dw      op_cl               ; 2a  CL - Cls
           dw      op_rn               ; 2b  RN - Rnd
           dw      mainlp              ; 2c
           dw      op_ws               ; 2d  WS - Return to system
           dw      op_us               ; 2e  US - USR call
           dw      op_rt               ; 2f  RT - Return from IL subroutine
           dw      op_js               ; 30
           dw      op_js               ; 31
           dw      op_js               ; 32
           dw      op_js               ; 33
           dw      op_js               ; 34
           dw      op_js               ; 35
           dw      op_js               ; 36
           dw      op_js               ; 37
           dw      op_js               ; 38
           dw      op_js               ; 39
           dw      op_js               ; 3a
           dw      op_js               ; 3b
           dw      op_js               ; 3c
           dw      op_js               ; 3d
           dw      op_js               ; 3e
           dw      op_js               ; 3f
           dw      op_js               ; 40
           dw      op_js               ; 41
           dw      op_js               ; 42
           dw      op_js               ; 43
           dw      op_js               ; 44
           dw      op_js               ; 45
           dw      op_js               ; 46
           dw      op_js               ; 47
           dw      op_js               ; 48
           dw      op_js               ; 49
           dw      op_js               ; 4a
           dw      op_js               ; 4b
           dw      op_js               ; 4c
           dw      op_js               ; 4d
           dw      op_js               ; 4e
           dw      op_js               ; 4f
           dw      op_js               ; 50
           dw      op_js               ; 51
           dw      op_js               ; 52
           dw      op_js               ; 53
           dw      op_js               ; 54
           dw      op_js               ; 55
           dw      op_js               ; 56
           dw      op_js               ; 57
           dw      op_js               ; 58
           dw      op_js               ; 59
           dw      op_js               ; 5a
           dw      op_js               ; 5b
           dw      op_js               ; 5c
           dw      op_js               ; 5d
           dw      op_js               ; 5e
           dw      op_js               ; 5f
           dw      op_js               ; 60
           dw      op_js               ; 61
           dw      op_js               ; 62
           dw      op_js               ; 63
           dw      op_js               ; 64
           dw      op_js               ; 65
           dw      op_js               ; 66
           dw      op_js               ; 67
           dw      op_js               ; 68
           dw      op_js               ; 69
           dw      op_js               ; 6a
           dw      op_js               ; 6b
           dw      op_js               ; 6c
           dw      op_js               ; 6d
           dw      op_js               ; 6e
           dw      op_js               ; 6f
           dw      op_js               ; 70
           dw      op_js               ; 71
           dw      op_js               ; 72
           dw      op_js               ; 73
           dw      op_js               ; 74
           dw      op_js               ; 75
           dw      op_js               ; 76
           dw      op_js               ; 77
           dw      op_js               ; 78
           dw      op_js               ; 79
           dw      op_js               ; 7a
           dw      op_js               ; 7b
           dw      op_js               ; 7c
           dw      op_js               ; 7d
           dw      op_js               ; 7e
           dw      op_js               ; 7f
           dw      op_j                ; 80
           dw      op_j                ; 81
           dw      op_j                ; 82
           dw      op_j                ; 83
           dw      op_j                ; 84
           dw      op_j                ; 85
           dw      op_j                ; 86
           dw      op_j                ; 87
           dw      op_j                ; 88
           dw      op_j                ; 89
           dw      op_j                ; 8a
           dw      op_j                ; 8b
           dw      op_j                ; 8c
           dw      op_j                ; 8d
           dw      op_j                ; 8e
           dw      op_j                ; 8f
           dw      op_j                ; 90
           dw      op_j                ; 91
           dw      op_j                ; 92
           dw      op_j                ; 93
           dw      op_j                ; 94
           dw      op_j                ; 95
           dw      op_j                ; 96
           dw      op_j                ; 97
           dw      op_j                ; 98
           dw      op_j                ; 99
           dw      op_j                ; 9a
           dw      op_j                ; 9b
           dw      op_j                ; 9c
           dw      op_j                ; 9d
           dw      op_j                ; 9e
           dw      op_j                ; 9f
           dw      op_j                ; a0
           dw      op_j                ; a1
           dw      op_j                ; a2
           dw      op_j                ; a3
           dw      op_j                ; a4
           dw      op_j                ; a5
           dw      op_j                ; a6
           dw      op_j                ; a7
           dw      op_j                ; a8
           dw      op_j                ; a9
           dw      op_j                ; aa
           dw      op_j                ; ab
           dw      op_j                ; ac
           dw      op_j                ; ad
           dw      op_j                ; ae
           dw      op_j                ; af
           dw      op_j                ; b0
           dw      op_j                ; b1
           dw      op_j                ; b2
           dw      op_j                ; b3
           dw      op_j                ; b4
           dw      op_j                ; b5
           dw      op_j                ; b6
           dw      op_j                ; b7
           dw      op_j                ; b8
           dw      op_j                ; b9
           dw      op_j                ; ba
           dw      op_j                ; bb
           dw      op_j                ; bc
           dw      op_j                ; bd
           dw      op_j                ; be
           dw      op_j                ; bf
           dw      op_j                ; c0
           dw      op_j                ; c1
           dw      op_j                ; c2
           dw      op_j                ; c3
           dw      op_j                ; c4
           dw      op_j                ; c5
           dw      op_j                ; c6
           dw      op_j                ; c7
           dw      op_j                ; c8
           dw      op_j                ; c9
           dw      op_j                ; ca
           dw      op_j                ; cb
           dw      op_j                ; cc
           dw      op_j                ; cd
           dw      op_j                ; ce
           dw      op_j                ; cf
           dw      op_j                ; d0
           dw      op_j                ; d1
           dw      op_j                ; d2
           dw      op_j                ; d3
           dw      op_j                ; d4
           dw      op_j                ; d5
           dw      op_j                ; d6
           dw      op_j                ; d7
           dw      op_j                ; d8
           dw      op_j                ; d9
           dw      op_j                ; da
           dw      op_j                ; db
           dw      op_j                ; dc
           dw      op_j                ; dd
           dw      op_j                ; de
           dw      op_j                ; df
           dw      op_j                ; e0
           dw      op_j                ; e1
           dw      op_j                ; e2
           dw      op_j                ; e3
           dw      op_j                ; e4
           dw      op_j                ; e5
           dw      op_j                ; e6
           dw      op_j                ; e7
           dw      op_j                ; e8
           dw      op_j                ; e9
           dw      op_j                ; ea
           dw      op_j                ; eb
           dw      op_j                ; ec
           dw      op_j                ; ed
           dw      op_j                ; ee
           dw      op_j                ; ef
           dw      op_j                ; f0
           dw      op_j                ; f1
           dw      op_j                ; f2
           dw      op_j                ; f3
           dw      op_j                ; f4
           dw      op_j                ; f5
           dw      op_j                ; f6
           dw      op_j                ; f7
           dw      op_j                ; f8
           dw      op_j                ; f9
           dw      op_j                ; fa
           dw      op_j                ; fb
           dw      op_j                ; fc
           dw      op_j                ; fd
           dw      op_j                ; fe
           dw      op_j                ; ff
errmsg:    db      'File not found',10,13,0
fildes:    db      0,0,0,0
           dw      dta
           db      0,0
           db      0
           db      0,0,0,0
           dw      0,0
           db      0,0,0,0

endrom:    equ     $

base:      equ     $
dta:       equ     $
buffer:    equ     base
program:   equ     base+512

           org     7000h
pstart:    equ     $
lfsr:      equ     $+026h

