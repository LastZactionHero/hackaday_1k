;
; Hackaday1k.asm
;
; Created: 12/18/2016 9:40:35 PM
; Author : Zach Dicklin
;
; Application contains a small pixel font, and the ability to display a messsage to an LCD display with word wrapping.
; Built for an Atmega328p, using a Nokia 5110 displaly (https://www.sparkfun.com/products/10168)
;
;
; SRAM
; $0100 - $02FF ; Display Buffer
; $0300 - $033F ; Current Pixel Buffer
; $0340 - $03A0 ; Message Buffer

.include "M328PDEF.INC"

.equ PIN_DISPLAY_SCE = PD7
.equ PIN_DISPLAY_RST = PD6
.equ PIN_DISPLAY_DC  = PD5

.equ LCD_COMMAND = 0
.equ LCD_DATA = 1
.equ LCD_CONTRAST = 50
.equ LCD_WIDTH = 84
.equ LCD_HEIGHT = 48

.cseg

.org $0200

; The small pixel font, containing values from ! (0x21) to z (0x7A). Some punctuation is omitted.
; See Small Font Designer for more info: http://smallfont.develop.vodka
; Each character inclues:
; 1 byte  - width of character (e.g. 6 pixels)
; n bytes - the pixel data, drawing across to the width (x), then down a row (y)
; 0x00     - end of character
.db 0x01, 0x7a, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x03, 0xff, 0x0e, 0xff, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x04, 0x06, 0x99, 0x99, 0x60, 0x00, 0x03, 0x07, 0x92, 0x48, 0x00, 0x04, 0x06, 0x91, 0x68, 0xf0, 0x00, 0x04, 0x06, 0x92, 0x19, 0x60, 0x00, 0x04, 0x09, 0x99, 0xf1, 0x10, 0x00, 0x04, 0x0f, 0x8e, 0x19, 0x60, 0x00, 0x04, 0x06, 0x8e, 0x99, 0x60, 0x00, 0x04, 0x0f, 0x12, 0x44, 0x40, 0x00, 0x04, 0x06, 0x96, 0x99, 0x60, 0x00, 0x04, 0x06, 0x99, 0x71, 0x60, 0x00, 0x01, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x06, 0x91, 0x20, 0x20, 0x00, 0x00, 0x05, 0x01, 0x14, 0xaf, 0xc6, 0x20, 0x00, 0x05, 0x07, 0xa3, 0xe8, 0xc7, 0xc0, 0x00, 0x05, 0x03, 0xa3, 0x08, 0x45, 0xc0, 0x00, 0x05, 0x07, 0xa3, 0x18, 0xc7, 0xc0, 0x00, 0x04, 0x0f, 0x8e, 0x88, 0xf0, 0x00, 0x04, 0x0f, 0x8e, 0x88, 0x80, 0x00, 0x05, 0x03, 0xa3, 0x09, 0xc5, 0xc0, 0x00, 0x05, 0x04, 0x63, 0xf8, 0xc6, 0x20, 0x00, 0x01, 0x7e, 0x00, 0x03, 0x04, 0x93, 0x78, 0x00, 0x05, 0x04, 0x65, 0x4e, 0x4a, 0x20, 0x00, 0x04, 0x08, 0x88, 0x88, 0xf0, 0x00, 0x07, 0x01, 0x07, 0x1d, 0x5a, 0xb5, 0x64, 0x80, 0x00, 0x05, 0x04, 0x73, 0x59, 0xc6, 0x20, 0x00, 0x06, 0x01, 0xe8, 0x61, 0x86, 0x17, 0x80, 0x00, 0x04, 0x0f, 0x9f, 0x88, 0x80, 0x00, 0x06, 0x01, 0xc8, 0xa2, 0x8a, 0x67, 0x81, 0x00, 0x05, 0x07, 0xa3, 0x1f, 0x46, 0x20, 0x00, 0x05, 0x03, 0xa2, 0x83, 0x45, 0xc0, 0x00, 0x05, 0x07, 0xc8, 0x42, 0x10, 0x80, 0x00, 0x05, 0x04, 0x63, 0x18, 0xc5, 0xc0, 0x00, 0x05, 0x04, 0x63, 0x18, 0xa8, 0x80, 0x00, 0x07, 0x01, 0x26, 0xad, 0x5a, 0xa8, 0x91, 0xff, 0x00, 0x05, 0x04, 0x54, 0x42, 0x2a, 0x20, 0x00, 0x05, 0x04, 0x63, 0x17, 0x10, 0x80, 0x00, 0x05, 0x07, 0xc2, 0x66, 0x43, 0xe0, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x01, 0x02, 0x00, 0x04, 0xff, 0x71, 0x79, 0x70, 0x00, 0x04, 0x08, 0x8e, 0x99, 0xe0, 0x00, 0x04, 0xff, 0x69, 0x89, 0x60, 0x00, 0x04, 0x01, 0x17, 0x99, 0x70, 0x00, 0x04, 0xff, 0x69, 0xf8, 0x70, 0x00, 0x03, 0x0d, 0x74, 0x90, 0x00, 0x04, 0xff, 0x79, 0x71, 0x97, 0x00, 0x04, 0x08, 0x8e, 0x99, 0x90, 0x00, 0x01, 0x5e, 0x00, 0x02, 0x11, 0x56, 0x00, 0x04, 0x08, 0x9a, 0xca, 0x90, 0x00, 0x01, 0x7e, 0x00, 0x06, 0xff, 0x08, 0x3e, 0x96, 0x59, 0x40, 0x00, 0x03, 0xff, 0x0d, 0x68, 0x00, 0x04, 0xff, 0x06, 0x99, 0x60, 0x00, 0x04, 0xff, 0xe9, 0x9e, 0x88, 0x00, 0x04, 0xff, 0x07, 0x97, 0x11, 0x00, 0x03, 0x02, 0x79, 0x20, 0x00, 0x02, 0x03, 0x9c, 0x00, 0x03, 0x01, 0x74, 0x90, 0x00, 0x04, 0xff, 0x09, 0x99, 0x60, 0x00, 0x05, 0xff, 0x01, 0x18, 0xa8, 0x80, 0x00, 0x06, 0xff, 0xff, 0x21, 0xb5, 0x24, 0x80, 0x00, 0x04, 0xff, 0x09, 0x66, 0x90, 0x00, 0x04, 0xff, 0x09, 0x96, 0x26, 0x00, 0x04, 0xff, 0xf2, 0x48, 0xf0, 0x00, 0x00

; Sample message to display on program load
.db "Hello World!", 0x0A, "I am a string that uses word wrapping to extend to multiple lines.", 0x00

.org $0000

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

	; Prep the sample message and write it to the display buffer
	rcall init_message_buffer
	rcall add_line_breaks
	rcall text_write_string

	; Write the display buffer to the LCD
	rcall lcdwrite_display_buffer

	sbi PORTB, PB1 ; Turn on the backlight

	; All done
loop:
    rjmp loop

; Copy the preset message to ram
init_message_buffer:
	ldi ZH, 0x05 ; start of preloaded message in program
	ldi ZL, 0xD0

	ldi YH, 0x03 ; start of message buffer in SRAM
	ldi YL, 0x40

init_message_buffer_load_next_byte:
	; Store the byte
	lpm r16, Z+
	st Y+, r16

	; End of buffer?
	cpi r16, 0x00
	brne init_message_buffer_load_next_byte

	ret

; Preemptive word wrapping
; Replace spaces with line breaks in places where words will carry over to the next line.
add_line_breaks:
	ldi ZH, 0x03 ; start of message buffer
	ldi ZL, 0x40

	ldi r17, 0x02 ; current x position - HACK: starting at 2, there's a small math error somewhere...

add_line_breaks_load_next_byte:
	ld r16, Z+ ; load a byte of the message
	mov r22, r16
	; Check if at end of buffer
	cpi r16, 0x00
	breq add_line_breaks_end

	; Check if this is a newline
	cpi r16, 0x0A
	brne add_line_breaks_check_if_space
	ldi r17, 0x02 ; reset X to 0 - HACK: starting at 2, there's a small math error somewhere...
	rjmp add_line_breaks_load_next_byte

add_line_breaks_check_if_space:
	; Check if this is a space
	cpi r16, 0x20 ; space?
	brne add_line_breaks_get_length

	mov YH, ZH ; Save the position of this space for later replacement
	mov YL, ZL

	ldi r18, 0x04 ; add space width
	add r17, r18

	rjmp add_line_breaks_load_next_byte

add_line_breaks_get_length:
	; Get the length of the character
	push r22
	push r17
	push ZH
	push ZL
	push YH
	push YL
	rcall text_load_character
	pop YL
	pop YH
	pop ZL
	pop ZH
	pop r17
	pop r22

	; Add this character to the x position
	add r17, r16
	inc r17 ; space between letters

	; Compare the current width to the display
	ldi r18, LCD_WIDTH
	sub r18, r17
	brpl add_line_breaks_load_next_byte

	ldi r18, 0x0A
	st -Y, r18 ; Save a line break to the last space (todo: check if this position is valid)
	ldi r17, 0x02 ; reset X to 0 - HACK: starting at 2, there's a small math error somewhere...
	rjmp add_line_breaks_load_next_byte

add_line_breaks_end:
	ret

; Write string from the message buffer to the display buffer
text_write_string:
	ldi r18, 0x00 ; x position
	ldi r17, 0x00 ; y position

	ldi ZH, 0x03 ; starting message buffer in SRAM
	ldi ZL, 0x40

text_write_string_load_next_character:
	; Load the next character from the buffer
	ld r16, Z+
	cpi r16, 0x00
	breq text_write_string_end ; buffer finished?

	cpi r16, 0x0A ; newline?
	brne text_write_string_check_if_space_space
	ldi r19, 0x08 ; move Y down one line
	add r17, r19
	ldi r18, 0x00 ; move X to the start
	rjmp text_write_string_check_newline

text_write_string_check_if_space_space:
	cpi r16, 0x20 ; space?
	brne text_write_string_write_character
	ldi r19, 0x04
	add r18, r19
	rjmp text_write_string_check_newline

text_write_string_write_character:
	push ZH
	push ZL
	push r18
	push r17
	rcall text_write_character
	pop r17
	pop r18
	pop ZL
	pop ZH
	add r18, r16 ; increment x position
	inc r18

text_write_string_check_newline:
	rjmp text_write_string_load_next_character

text_write_string_end:
	ret

// Write an individual character to the display
// Inputs:
// r16 - ASCII character to write
// r17 - Starting Y Position
// r18 - Starting X Position
//
// Outputs:
// r16 - Character width
text_write_character:
	push r18 ; push starting X/Y positions onto the stack
	push r17

	rcall text_load_character
	mov r19, r16 ; character width

	; Restore starting x/y positions from the stack
	; pop r20 ; starting y position
	;pop r21 ; starting x position
	pop r16 ; y position
	pop r17 ; x position
	mov r20, r17 ; keep track of the starting X position
	mov r22, r16 ; keep track of the starting Y position

	ldi YH, 0x03  ; Buffer with uncompressed character pixel data
	clr YL

text_write_character_next_pixel:
	ld r18, Y+
	cpi r18, 0x01
	brne text_write_character_increment
	push r22
	push r20
	push r19
	push r18
	push r17
	push r16	
	rcall display_buffer_set_pixel
	pop r16
	pop r17
	pop r18
	pop r19
	pop r20
	pop r22

text_write_character_increment:
	inc r17		 ; increment X position
	; subtract from the starting position to determine how far in we are
	mov r21, r17 
	sub r21, r20
	cp r21, r19	 ; compare this distance to the character width to see if we're at the end
	brne text_write_character_next_pixel
	; At the end of the line
	mov r17, r20  ; move X back to the beginning of the character
	inc r16		  ; increment Y
	; subtract from the starting Y position to determine how far down we are
	mov r21, r16
	sub r21, r22
	cpi r21, 0x08 ; check if we're at the bottom of the character
	brne text_write_character_next_pixel

	mov r16, r19
	ret

; Load a character, unpack it, and save on/off bits into the character pixel buffer
; saves character bits to $0300
;
; Inputs:
; r16 - ASCII character to write
;
; Outputs:
; returns character width on r16
text_load_character:
	; load the width of the character
	ldi ZH, 0x04
	ldi ZL, 0x00

	; Loop to the position of the ASCII character
	; Subtract from the starting character
	; Then loop over program memory until looking for 0x00 until r16 is 0
	subi r16, 0x21 ; Starts at exclamation point, 0x21
	cpi r16, 0x00
	breq text_load_character_width

text_load_character_seek_next_byte:
	lpm r17, Z+ ; character width
	cpi r17, 0x00
	brne text_load_character_seek_next_byte

	subi r16, 0x01
	cpi r16, 0x00
	brne text_load_character_seek_next_byte

text_load_character_width:
	lpm r16, Z+ ; character width

	; relative position in SRAM
	ldi YH, 0x03
	ldi YL, 0x00

text_load_character_next_byte:
	ldi r17, 0x00 ; bit position in data byte
	lpm r18, Z+   ; data byte

	cpi r18, 0x00
	breq text_load_character_done

	cpi r18, 0xFF ; 0xFF symbolizes a null byte
				  ; Change it to 0x00
	brne text_load_character_next_bit
	ldi r18, 0x00

text_load_character_next_bit:
	inc r17

	ldi r20, 0x00

	lsl r18
	brcc text_load_character_store
	ldi r20, 0x01

text_load_character_store:
	st Y+, r20

	cpi r17, 0x08
	brne text_load_character_next_bit
	rjmp text_load_character_next_byte

text_load_character_done:
	ret


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
