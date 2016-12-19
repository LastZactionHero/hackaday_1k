;
; Hackaday1k.asm
;
; Created: 12/18/2016 9:40:35 PM
; Author : Zach
;

.include "M328PDEF.INC"

.equ PIN_DISPLAY_SCE = PD7
.equ PIN_DISPLAY_RST = PD6
.equ PIN_DISPLAY_DC  = PD5

.equ LCD_COMMAND = 0
.equ LCD_DATA = 1
.equ LCD_CONTRAST = 50

start:
	; Control Pins
	; DDRD
	ldi r16, (1 << PIN_DISPLAY_SCE) | (1 << PIN_DISPLAY_RST) | (1 << PIN_DISPLAY_DC);	
	out DDRD, r16
	; DDRB
	ldi r16, (1 << PB3) | (1 << PB5) | (1 << PB2) | (1 << PB1);
	out DDRB, r16
	; SPI Control Register
	ldi	R16, (1 << MSTR) | (1 << SPE)
	out	SPCR, r16
 
	; Reset the LCD to a known state
	sbi PORTD, PIN_DISPLAY_RST
	cbi PORTD, PIN_DISPLAY_RST
	sbi PORTD, PIN_DISPLAY_RST

	; LCD Setup
	; Tell LCD extended commands follow
	ldi r16, LCD_COMMAND
	ldi r17, 0x21
	rcall lcdwrite
	cbi PORTB, PB1

	; Set LCD Vop (Contrast)
	ldi r16, LCD_COMMAND
	ldi r17, 0xB0
	rcall lcdwrite

	; Set Temp coefficent
	ldi r16, LCD_COMMAND
	ldi r17, 0x04
	rcall lcdwrite

	; LCD bias mode 1:48 (try 0x13)
	ldi r16, LCD_COMMAND
	ldi r17, 0x14
	rcall lcdwrite

	; We must send 0x20 before modifying the display control mode
	ldi r16, LCD_COMMAND
	ldi r17, 0x20
	rcall lcdwrite

	; Set display control, normal mode.
	ldi r16, LCD_COMMAND
	ldi r17, 0x0C
	rcall lcdwrite

	; Tell LCD that extended commands follow
	ldi r16, LCD_COMMAND
	ldi r17, 0x21
	rcall lcdwrite

	; Set LCD Vop (Contrast): Try 0xB1(good @ 3.3V) or 0xBF if your display is too dark
	ldi r16, LCD_COMMAND
	ldi r17, 0x80 | LCD_CONTRAST
	rcall lcdwrite

	; Set display mode
	ldi r16, LCD_COMMAND
	ldi r17, 0x20
	rcall lcdwrite

	; Write some pixel data
	; Send 504 (0x01F8) bytes
	ldi r25, 0x01 ; high bit
	ldi r24, 0xF8 ; low bit
sample_data_write:
	; Send some sample data
	ldi r16, LCD_DATA
	ldi r17, 0xF0
	rcall lcdwrite

	; Subtract
	sbiw r25:r24,1 
	cpi r25, 0x00
	brne sample_data_write
	cpi r24, 0x00
	brne sample_data_write

	sbi PORTB, PB1
loop:
    inc r16
    rjmp loop


; r16 - write_type (LCD_COMMAND or LCD_DATA)
; r17 - data
lcdwrite: 
	ldi r18, LCD_DATA
	cp r16, r18
	breq lcdwrite_command_set_data ; Sending command data?
	; Sending pixel data: set D/C line low
	cbi PORTD, PIN_DISPLAY_DC
	rjmp lcdwrite_send_data
lcdwrite_command_set_data:
	sbi PORTD, PIN_DISPLAY_DC
lcdwrite_send_data:
	; Set chip select low
	cbi PORTD, PIN_DISPLAY_SCE

	; Send the data
	out		SPDR, r17

	; Wait for data to send
lcdwrite_wait:
	in		r16, SPSR
	sbrs	r16, SPIF
	rjmp	lcdwrite_wait

	; Set chip select high
	sbi PORTD, PIN_DISPLAY_SCE
	ret