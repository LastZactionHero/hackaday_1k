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
.equ LCD_WIDTH = 84
.equ LCD_HEIGHT = 48

.org $0000
.db 0x01, 0x7a, 0x00, 0x00


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
 
	cbi PORTB, PB1; backlight test - remove me

	; Reset the LCD to a known state
	cbi PORTD, PIN_DISPLAY_RST
	sbi PORTD, PIN_DISPLAY_RST

	; LCD Setup
	; Tell LCD extended commands follow
	ldi r16, LCD_COMMAND
	ldi r17, 0x21
	rcall lcdwrite

	; Might not be important, cutting for space...
	; Set LCD Vop (Contrast)
	; ldi r17, 0xB0
	; rcall lcdwrite

	; Might not be important, cutting for space...
	; Set Temp coefficent
	; ldi r17, 0x04
	; rcall lcdwrite

	; LCD bias mode 1:48 (try 0x13)
	ldi r17, 0x14
	rcall lcdwrite

	; We must send 0x20 before modifying the display control mode
	ldi r17, 0x20
	rcall lcdwrite

	; Set display control, normal mode.
	ldi r17, 0x0C
	rcall lcdwrite

	; Tell LCD that extended commands follow
	ldi r17, 0x21
	rcall lcdwrite

	; Set LCD Vop (Contrast): Try 0xB1(good @ 3.3V) or 0xBF if your display is too dark
	ldi r17, 0x80 | LCD_CONTRAST
	rcall lcdwrite

	; Set display mode
	ldi r17, 0x20
	rcall lcdwrite

	rcall display_buffer_clear

	; Set some pixels
	; Top left
	ldi r16, 0x00
	ldi r17, 0x00	
	rcall display_buffer_set_pixel

	; Middle
	ldi r16, 0x18
	ldi r17, 0x2A	
	rcall display_buffer_set_pixel

	; Bottom right
	ldi r16, 0x2F
	ldi r17, 0x53	
	rcall display_buffer_set_pixel

	rcall lcdwrite_display_buffer

	sbi PORTB, PB1
loop:
    rjmp loop

; Clear the display buffer
display_buffer_clear:
	ldi ZH, 0x01
	ldi ZL, 0x00
display_buffer_clear_store:
	st Z+, r0 ; r0 is probably 0....
	cpi ZH, 0x03 ; clear to 0x0300, instead of 0x02F8. Saves a few words...
	brne display_buffer_clear_store
	ret

; Set an individual pixel in the display buffer
; r16 - Y
; r17 - X
display_buffer_set_pixel:
	; Algorithm:
	; shift = y % 8;
	; display_map[x + (y/8)*LCD_WIDTH] |= 1<<shift;

	; divide Y by 8 (aka 3 right shifts)
	mov r19, r16
	asr r19 
	asr r19
	asr r19	

	; multiply Y by LCD_WIDTH
	ldi r20, LCD_WIDTH
	mul r19, r20
	mov ZH, r1
	mov ZL, r0

	; Add X
	adc ZL, r17	
	ldi r20, 0x01
	brcc display_buffer_set_pixel_add_offset
	add ZH, r20

	; Add display buffer memory offset, $0100
display_buffer_set_pixel_add_offset:
	add ZH, r20

	; Get the bit position within the display buffer byte
	; Y % 8 (aka bitmask with 0000 0111)
	ldi r19, 0x07
	and r16, r19

	; Build a mask from the bit position
	; Turn 2 into 0000 0100
	ldi r19, 0x01
	cpi r16, 0x00
	breq display_buffer_set_pixel_apply_mask
display_buffer_set_pixel_shift_mask:
	lsl r19
	dec r16
	cpi r16, 0x00
	brne display_buffer_set_pixel_shift_mask

	; Load existing value from memory
	; Apply the bitmask
	; Store back to memory
display_buffer_set_pixel_apply_mask:	 
	ld r20, Z
	or r20, r19	
	st Z, r20
	ret

; Write display buffer to the LCD
lcdwrite_display_buffer:
	; Set memory position at start of display buffer, $0100
	ldi ZH, 0x01
	ldi ZL, 0x00	

	; Loop through the buffer, sending each value to the LCD
lcdwrite_display_buffer_byte:
	ld r17, Z+
	ldi r16, LCD_DATA
	rcall lcdwrite

	; Stop if we're at the end, 84x48 + 0x0100 => 0x02F8
	cpi ZH, 0x02
	brne lcdwrite_display_buffer_byte
	cpi ZL, 0xF8
	brne lcdwrite_display_buffer_byte
	ret

; r16 - write_type (LCD_COMMAND or LCD_DATA)
; r17 - data
lcdwrite: 
	sbi PORTD, PIN_DISPLAY_DC ; Set D/C line high as a default (sending data) to save some space
	; But check if that's really the right choice...
	cpi r16, LCD_DATA
	breq lcdwrite_send_data ; Yup, we're sending data? We're already good.
	cbi PORTD, PIN_DISPLAY_DC ; Actually we're sending a command. Set the D/C line low

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