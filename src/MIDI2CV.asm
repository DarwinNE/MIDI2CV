; *****************************************************************************
;               MIDI2CV.asm
;       A MIDI to continuous voltage converter using a PIC16F876
;       Davide Bucci, 2004-2016
;       Version 1.2
; *****************************************************************************
; This program implements a MIDI state machine which is able to recognize MIDI
; events sent to the processor. A voltage is output by means of PWM and a gate
; signal is activated by a NOTE ON event on the selected channel and
; deactivated by a NOTE OFF event. Footswitch pedal activates a second gate
; signal. The MIDI logic is implemented by means of a state machine. 
; State change occurs during the interrupt when the messages are received. The
; current channel is shown by means of two multiplexed seven segments LED
; displays. A minimalistic user interface is present with tho buttons. Both the
; buttons and the displays are multiplexed.


; License:
; --------
;
;    Copyright (C) 2004-2016  Davide Bucci
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.

        PROCESSOR 16F876

; disable the annoying warning
; "Register in operand not in bank 0.
; Ensure bank bits are correct."

        ERRORLEVEL -302

        #include "p16f876.inc"
        __CONFIG 0x3F7A         ; Disable watchdog, HS oscillator,
                        ; disable LT programming

; These registers are accessible in all pages
STORES  equ     0x70    ; Store status register
STOREW  equ     0x71    ; Store the w register
STOREP  equ     0x72    ; Store the PCLATH register

; These registers are in bank 0
CHANNEL equ     0x25    ; Channel number
TRNSM   equ     0x26    ; Transmission status (See TRN section)
RECBYTE equ     0x27    ; Received byte (used in interrupt)

ST      equ     0x28    ; State of the MIDI reception state machine

DISP0   equ     0x20    ; First display
DISP1   equ     0x21    ; Second display
ACTDISP equ     0x22    ; Actual display being refreshed
LEDST   equ     0x23    ; LED state

; Serial communication reception errors constants
TRN_RXF equ     0       ; Error: reception buffer full
TRN_FRM equ     1       ; Error: reception without a stop bit (frame error)

; These registers controls the buttons
BUTTONS equ     0x2A
OLD_BTN equ     0x2B
ACT_BTN equ     0x2C
TMP_1   equ     0x2D

; Button codes
SET_B   equ     0
CHNG_B  equ     1

; Delay registers
SDR     equ     0x30    ; Short Delay Register
LDR     equ     0x31    ; Long Delay Register

; Note being played 
NOTE_PL equ     0x32

; *****************************************************************************
;               Main Program
; *****************************************************************************
        org     0x0000
        goto    prg
        nop
        nop
        nop
        org     0x0004
        goto    interrupt

; Segment mapping routine (BCD to 7 segments, a bit like the CD4511)
segment
        addwf   PCL,f                   ;return into W the right set of segments
        retlw   0x3F                    ;to be turned on for each digit
        retlw   0x06
        retlw   0x5B
        retlw   0x4F
        retlw   0x66
        retlw   0x6D
        retlw   0x7C
        retlw   0x07
        retlw   0x7F
        retlw   0x67

; Main program
prg
        ; *************************************************************
        ; General configuration settings
        ; *************************************************************
        BANKSEL ADCON1
        movlw   0x06
        movwf   ADCON1                  ; Disable the ADC on port A
        clrf    INTCON

        BANKSEL TRISC
        bcf     TRISC, 2                ; PWM1
        bcf     TRISC, 3                ; mux 1
        bcf     TRISC, 4                ; mux 2
        bcf     TRISC, 5                ; mux 4
        bcf     TRISC, 0                ; Gate 1
        bcf     TRISC, 1                ; Gate 2
        BANKSEL PORTB
        ; *************************************************************
        ; UART settings for MIDI transmission
        ; *************************************************************
        BANKSEL TRNSM
        clrf    TRNSM                   ; Clear the transmission error register

        movlw   0x27                    ; Set transmission speed (31250 bauds
                                        ; for a 20MHz crystal)
        BANKSEL SPBRG
        movwf   SPBRG
        BANKSEL TXSTA

        bcf     TXSTA, TX9              ; 8 bits transmission
        bcf     TXSTA, SYNC             ; Asynchronous mode
        bsf     TXSTA, BRGH             ; High speed mode
        bsf     TXSTA, TXEN             ; Enable transmission

        BANKSEL PIE1
        bsf     PIE1, RCIE              ; Use interrupt driven reception
        BANKSEL RCSTA
        bsf     RCSTA, SPEN             ; Serial port enabled
        bcf     RCSTA, RX9              ; 8 bits reception
        bsf     RCSTA, CREN             ; Continous receive on
        bcf     RCSTA, ADDEN
        bcf     RCSTA, FERR
        bsf     INTCON, PEIE            ; Set USART interrupt enable
        ; *************************************************************
        ; PWM settings
        ; *************************************************************

        BANKSEL CCP1CON
        clrf    CCP1CON
        movlw   0x04                    ; 0b00000100 prescaler 1:1 tmr oN
        movwf   T2CON

        movlw   0x0C                    ; Configure PWM1
        movwf   CCP1CON
        movlw   0x00                    ; Do not use PWM2
        movwf   CCP2CON

        movlw   0xFE
        BANKSEL PR2
        movwf   PR2
        BANKSEL T2CON

        movlw   0x80
        movwf   CCPR1L
        movwf   CCPR2L

        BANKSEL CHANNEL

        movlw   0x00
        movwf   CHANNEL                 ; Set the default channel

        BANKSEL ST
        clrf    ST                      ; Reset the state machine

        ; *************************************************************
        ; Configuration of the display refresh interrupt
        ; *************************************************************
        BANKSEL TRISB
        clrf    TRISB                   ; Port B as output (display)
        bsf     TRISA, 0                ; Button sense line
        bcf     TRISA, 1                ; Display 0 ctrl
        bcf     TRISA, 2                ; Display 1 ctrl

        BANKSEL OPTION_REG
        bcf     OPTION_REG, T0CS        ; The clock drives the timer
        bcf     OPTION_REG, PSA         ; We use the prescaler
        bsf     OPTION_REG, PS2         ; 100, prescaler to 1:32
        bcf     OPTION_REG, PS1
        bcf     OPTION_REG, PS0

        BANKSEL INTCON
        bsf     INTCON, T0IE            ; Interrupt on TMR0 overflow
        bsf     INTCON, GIE             ; Set Global Interrupt enable

        ; *************************************************************
        ; End of setup! Main program loop starts soon.
        ; *************************************************************
        BANKSEL DISP0
        movlw   0x01
        movwf   DISP0
        movlw   0x02
        movwf   DISP1
        movlw   0xFF                    ; No note being played
        movwf   NOTE_PL

        ; In reality, the main loop does not do a lot. It mainly ensures that
        ; the numbers shown in the displays are correct. Moreover, it handles
        ; the buttons by means of a very crude check and debounce delay.

continue
        call    chnl2disp               ; Set up properly numbers <---------+
        call    wait_buttons            ; Check the button states           |
        btfsc   ACT_BTN, SET_B          ;                                   |
        bcf     LEDST,1                 ; Note on LED                       |
        incf    CHANNEL,f               ;                                   |
        movfw   CHANNEL                 ;                                   |
        xorlw   0x10                    ;                                   |
        btfsc   STATUS,Z                ;                                   |
        clrf    CHANNEL                 ;                                   |
        goto    continue                ; loop continuously  >--------------+

wait_buttons                            ; Wait until a button is pressed;
                                        ; return the button code in w and in
                                        ; OLD_BTN

        movlw   0x20                    ; Set the delay duration
        movwf   LDR
        call    longdelay
button_loop                             ; Main loop
        movfw   ACT_BTN                 ; Save the old state of the buttons
        movwf   OLD_BTN
        movfw   BUTTONS                 ; Save the new state of the buttons
        movwf   ACT_BTN
        xorwf   OLD_BTN,w               ; Verify if there was a change in the
        btfsc   STATUS, Z               ; button status
        goto    button_loop
        btfsc   ACT_BTN, SET_B          ; Test the set button
        retlw   SET_B
        btfsc   ACT_BTN, CHNG_B         ; Test the change button
        retlw   CHNG_B
        goto    wait_buttons
longdelay
        BANKSEL LDR
        call    shortdelay
        decfsz  LDR,f
        goto    longdelay
        return

shortdelay
        BANKSEL SDR
        decfsz  SDR,f
        goto    shortdelay
        return
chnl2disp
        movfw   CHANNEL
        addlw   0x01                    ; Channels are shown as 1-16
        addlw   -0x0A                   ; Test if it is >10
        btfss   STATUS, C
        goto    lt10
        movwf   DISP1
        movlw   0x01
        movwf   DISP0
        return
lt10
        addlw   0x0A
        movwf   DISP1
        movlw   0x00
        movwf   DISP0
        return

; *****************************************************************************
;               Interrupt routine. Most interesting stuff happens here.
; *****************************************************************************
interrupt
        movwf   STOREW                  ; Context save
        movfw   STATUS
        movwf   STORES
        movfw   PCLATH
        movwf   STOREP


charloop
        BANKSEL PIR1                    ; Loop until there is a
                                               ; character in the buffer
        btfss   PIR1, RCIF              ; Character received?
        goto    continueint             ; All characters processed

        ; *************************************************************
        ; Process MIDI messages
        ; *************************************************************

        BANKSEL RCREG
        movfw   RCREG                   ; Read character

        ; Recognize the start of a MIDI message is a pretty easy task
        ; if we note that it's the only byte in the transmission having
        ; the MSB set. The following bytes will be treated as part of
        ; the same message.
        ; In all MIDI message, the higher four bits in the first byte
        ; specif the message type and the four lower bits the channel.
        ; In our case, we simply read the channel to toggle between
        ; ports.

        BANKSEL RECBYTE                 ; Save the received byte
                                              ; (same bank as PORTC)
        movwf   RECBYTE
        movfw   ST                      ; MIDI state machine
        addwf   PCL,f                   ; Select the state
        goto    idle                    ; 0
        goto    note_on_k               ; 1
        goto    note_on_v               ; 2
        goto    note_off_k              ; 3
        goto    note_off_v              ; 4
        goto    control_c               ; 5
        goto    control_v               ; 6


idle
        btfss   RECBYTE, 7              ; Test the M.S. byte
        goto    skip                    ; Is not a MIDI message header

        ; This code is executed only for the MIDI message header.
        ; In this place we should recognise the channel
        movfw   RECBYTE
        andlw   0x0F                    ; Separate the channel number
        xorwf   CHANNEL,w               ; Test if it is "our" channel
        btfss   STATUS, Z
        goto    skip
        movfw   RECBYTE
        ; Now we determine the event. Only two event are detected here:
        ; Note on and Note off
        andlw   0xF0                    ; Determine the event
        xorlw   0x90
        btfss   STATUS,Z                ; note on
        goto    no_note_on
        movlw   0x01                    ; state 1: note_on_k
        movwf   ST
        goto    skip

no_note_on
        movfw   RECBYTE
        andlw   0xF0                    ; Determine the event
        xorlw   0x80
        btfss   STATUS,Z                ; note off
        goto    no_note_off
        movlw   0x03                    ; state 3: note_off_k
        movwf   ST
no_note_off
        movfw   RECBYTE
        andlw   0xF0                    ; Determine the event
        xorlw   0xB0
        btfss   STATUS,Z                ; control
        goto    no_control
        movlw   0x05                    ; state 5: control_on_k
        movwf   ST

no_control
        goto    skip


note_on_k
        BANKSEL LEDST
        bsf     LEDST,1                 ; Note on LED
        ;movfw  NOTE_PL
        ;xorlw  0xFF                    ; Is a note being played?
        ;btfss  SATUS,Z
        ;goto   noplaying
        BANKSEL PORTC                   ; if a note is being played
        bcf     PORTC, 0                ; gate 1 OFF
noplaying
        BANKSEL RECBYTE
        movfw   RECBYTE
        rlf     RECBYTE,f               ; 128 semitones!
        movwf   CCPR1L                  ; Set PWM
        movwf   CCPR2L
        movwf   NOTE_PL                 ; Store NOTE being played

        movlw   0x02                    ; state 2: note_on_v
        movwf   ST
        goto    skip
note_on_v
        movlw   0x00                    ; state 0: idle
        movwf   ST
        BANKSEL PORTC
        bsf     PORTC, 0                ; gate 1 On
        goto    skip

note_off_k
        movlw   0x04                    ; state 2: note_on_v
        movwf   ST
        BANKSEL RECBYTE
        movfw   RECBYTE                 ; read the key
        BANKSEL NOTE_PL                 ; note played
        xorwf   NOTE_PL,w
        btfsc   STATUS,Z
        goto    skip                    ; The correct note: continue
        movlw   0x00                    ; An old note: avoid
        movwf   ST
        goto    skip

note_off_v
        movlw   0x00                    ; next state 0: idle
        movwf   ST
        BANKSEL PORTC
        bcf     PORTC, 0                ; gate 1 OFF
        BANKSEL LEDST
        bcf     LEDST,1                 ; Note on LED
        goto    skip

control_c
        movlw   0x00                    ; next state 0: idle
        movwf   ST
        BANKSEL RECBYTE
        movfw   RECBYTE
        xorlw   0x40                    ; 0x40 is damper pedal control
        btfss   STATUS, Z
        goto    skip
        movlw   0x06                    ; if a pedal control is rec'd
        movwf   ST                      ; the next state will be
        goto    skip                    ; 6, control_v

control_v
        movlw   0x00                    ; next state 0: idle
        movwf   ST
        BANKSEL RECBYTE
        btfsc   RECBYTE,6
        goto    gate2_on
        BANKSEL PORTC
        bcf     PORTC, 1                ; gate 2 Off
        BANKSEL LEDST
        bcf     LEDST,0
        goto    skip
gate2_on
        BANKSEL PORTC                   ; gate 2 On
        bsf     PORTC, 1
        BANKSEL LEDST
        bsf     LEDST,0
        goto    skip


skip
        BANKSEL TXSTA
        btfsc   TXSTA, OERR             ; Test for a buffer overrun
        call    error_buffer
        btfsc   TXSTA, FERR             ; Test for a frame error
        call    error_frame
        goto    charloop                ; See if there is another byte
                                        ; The RCIF bit is cleared by
                                        ; hardware by reading the
                                        ; RCREG register
        ; *************************************************************
        ; Display refresh
        ; *************************************************************
continueint
        BANKSEL PIR1                    ; Test if the display need to
                                        ; be refreshed
        btfss   INTCON, T0IF            ; TMR0 overflow
        goto    exitinterrupt

        ;btfsc  ACTDISP,1               ; Decrease the display ON time
        ;goto   switchoff

        btfsc   ACTDISP,0               ; Display multiplex
        goto    refresh1

refresh0
        BANKSEL DISP0
        movfw   DISP0
        bsf     BUTTONS,SET_B
        btfsc   PORTA,0                 ; Test button 1
        bcf     BUTTONS,SET_B
        bsf     PORTA,1
        bcf     PORTA,2
        call    segment
        btfsc   LEDST,0
        iorlw   0x80
        movwf   PORTB
        goto    contrefresh

refresh1
        BANKSEL DISP1
        movfw   DISP1
        bsf     BUTTONS,CHNG_B
        btfsc   PORTA,0                 ; Test button 0
        bcf     BUTTONS,CHNG_B
        bcf     PORTA,1
        call    segment
        btfsc   LEDST,1
        iorlw   0x80
        movwf   PORTB
        bsf     PORTA,2
        goto    contrefresh

switchoff
        bcf     PORTA,1                 ; Switch off the two displays
        bcf     PORTA,2
contrefresh
        incf    ACTDISP,f               ; Change display next time
        bcf     INTCON, T0IF

exitinterrupt
        movfw   STOREP                  ; Context restore
        movwf   PCLATH
        movfw   STORES
        movwf   STATUS
        movfw   STOREW
        retfie                          ; Return from interrupt


        ; *************************************************************
        ; Serial transmission errors
        ; *************************************************************


error_buffer
        BANKSEL TRNSM                   ; Signal the error condition:
                                        ; buffer overrun
        bsf     TRNSM, TRN_RXF
        BANKSEL RCSTA
        bcf     RCSTA, CREN             ; Continous receive off:
                                        ; reset the USART
        bsf     RCSTA, CREN             ; Continous receive on:
                                        ; restart the normal way of
                                        ; life...
        return

error_frame
        BANKSEL TRNSM                   ; Signal the error condition:
                                        ; frame error
        bsf     TRNSM, TRN_FRM
        BANKSEL RCSTA
        bcf     RCSTA, CREN             ; Continous receive off:
                                        ; reset the USART
        bsf     RCSTA, CREN             ; Continous receive on:
                                        ; restart the normal way of life...
        return

        end
