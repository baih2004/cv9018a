;Display cv9018a 98x64 Pixel =784 bytes RAM von $0060-$0370
;Controller AT Mega8 L Intern 8 Mhz
;Display mit 8x 1 uF Keramik Kondensatoren
;PD0 = Chip select
;Pd1 = A0
;Pd2 = SCL (clock)
;Pd3 = Si Daten
;5 Tasten
;pd7 = beleuchtung
;pd5 taste1
;pd6 taste2
;pb0 taste3
;pb1 taste4
;pb2 taste5
;pc5 Lautsprecher Piezo
;pc4 Piezo

.include "m8def.inc"
.def temp1 = r16
.def temp2 = r17
.def temp3 = r18 
.def temp = r16
.org 0x000
	rjmp start
.org 0x004			;timer0 ovf atmega8
	rjmp timer2
.org 0x009
	rjmp timer0
start:

        ldi temp, LOW(RAMEND)             ; LOW-Byte der obersten RAM-Adresse
         out SPL, temp
         ldi temp, HIGH(RAMEND)            ; HIGH-Byte der obersten RAM-Adresse
         out SPH, temp

         ldi r16, 0xFF
         out DDRC, r16       ;Port c als ausgang schalten
	ldi r16,0b10011111
         out ddrd,r16
	ldi r16,0b11110000
	out ddrb,r16
	ldi r16,0
	out portc,r16
	ldi r16,0b00001111	;tasten 3-6 pull up
	out portb,r16
	ldi r16,0b01100000	;tasten 1 und 2 pull up
	out portd,r16
	sbi portd,0		;cs=1
	sbi portd,2		;clock=1 scl
	ldi r18,80
	rcall warten2
	rcall lcd_init
	ldi r18,200
	rcall warten2

;	rcall lcd_cls
	rcall text1
	rcall copy_ram_lcd
	ldi r18,200
	rcall warten2
	ldi r16,0

	ldi r17,0x09
	sts $038e,r17	;kontrast
	ldi r16,0
	sts $038f,r16	;frequenz
	sts $0390,r16	;ton länge 255=ende markierung
	sts $0391,r16	;ton aus=0 an=1
	sts $0392,r16	;lo zeiger lied
	sts $0393,r16	;hi zeiger lied
	sts $0394,r16	;zähler frequenz
	sts $0395,r16	;zähler 0-255
	;$0296 = wenn alarm dann zähler 0-60 dann geht alarm aus nach 1 minute
;ton zeiger einstellen, null als frequenz und länge laden und ton=1
;auf asynchron timer 2 umschalten
	ldi r16,0
	out timsk,r16	;irq aus
	ldi r16,8		;as2=1
	out assr,r16			;timer 2 mit externem quarz betreiben
	ldi r16,0
	out tcnt2,r16
	out ocr2,r16
	ldi r16,7
	out tccr2,r16	;prescaller	uhr
lop:
	in r16,assr
	andi r16,5
	cpi r16,5
	breq lop	;warten bis bit=0
	ldi r16,255
	out tifr,r16
;timer0 und timer2 init zeit
	ldi r16,2			;2=8
	out tccr0,r16	;prescaller 	ton

	ldi r16,0b00000001
	out timsk,r16	;interrupt an timer2 und timer0
	ldi r16,76
	out tcnt0,r16	;z"hler laden
    ldi r16,224
	out tcnt2,r16	;z"hler laden
;	sei
	
;ton spielen
	ldi r16,240
	sts $028f,r16
	ldi r16,10
	sts $0290,r16
	ldi r16,1
	sts $0291,r16
	ldi r18,20
	rcall warten2
	ldi r16,0
	sts $0291,r16	;ton aus
bild:
	rcall copy_bild2
	rcall copy_ram_lcd
	ldi r16,0
	ldi r17,0
;-------------------START---------------------
loop: 
loopa:
	ldi r26,0x60
	ldi r27,0x00
	push r16
	lds r16,$038e
	rcall dezimalb
	pop r16
;	rcall pixel
	rcall copy_ram_lcd
	inc r16
	cpi r16,98
	breq lap1 
	rjmp tasten_abfage
lap1:
	ldi r16,0
	inc r17
	cpi r17,64
	breq lap2
	rjmp tasten_abfage
lap2:
	ldi r17,0
	rjmp tasten_abfage

	ldi r26,0x60		;startposition uhrzeit
	ldi r27,0
;	lds r16,$028e
;	rcall dezimal
;		rcall copy_ram_lcd
;		rjmp tasten_abfage
	rcall uhrzeit1
	ldi r26,0xda		;startposition uhrzeit
	ldi r27,0x00
	lds r16,$028d		;alle alarme aus ?
	cpi r16,0
	breq loopa2
	lds r16,$0275		;alarm 0-6
	cpi r16,0
	breq loopa2
	ldi r16,255
	rcall zeichen		;alarm zeichen wenn an
loopa2:
	ldi r26,0xec		;startposition uhrzeit
	ldi r27,0x01
	;lds r16,$0297
	;rcall zeichen
	;rcall dezimal
	lds r16,$0274
;	rcall alarm_print	;alarm suchen tag heute
	rcall copy_ram_lcd
	lds r16,$0274
	inc r16
	cpi r16,7
	brne loop2
	ldi r16,0
loop2:
;	rcall alarm_print		;alarm suchen morgen
	rcall copy_ram_lcd
tasten_abfage:
	ldi r18,5 
	rcall warten2
	sbis PIND, 5
	rjmp taste5		;t1
	sbis PIND, 6
	rjmp taste3		;t2
	sbis PINb, 0
	rjmp taste4		;t3
	sbis PINb, 1
	rjmp taste2		;t4
	sbis PINb, 2
	rjmp taste1		;t5
	cbi portd,7		;led aus
	rjmp loop

taste1:
		rcall lcd_init
	rcall ton
	rjmp loop

taste2:
	rcall lcd_cls
	rcall text1
	rcall text2
	rcall text3
	rcall text4
	rcall copy_ram_lcd
	ldi r18,250
	rcall warten2
	rcall ton
	rcall taw
	rcall warten2
taste2b:
	sbic PINb, 1
	rjmp taste2b		;t4
;timer anzeigen
	rcall lcd_cls
	rcall text5
	rcall taw
	rcall warten2
taste2e:
	ldi r26,0xc2
	ldi r27,0x00
	lds r16,$0390
	rcall dezimalb
	lds r16,$0391
	rcall dezimalb
	lds r16,$0391
	inc r16
	sts $0391,r16
	cpi r16,0
	brne taste2d
	lds r16,$0390
	inc r16
	sts $0390,r16
taste2d:
	rcall copy_ram_lcd
	ldi r18,200
	;rcall warten2
	rcall ton
	;rcall taw
taste2c:
	sbic PINb, 1
	rjmp taste2e
	rcall taw
	rjmp bild

taw:
	sbic PINb, 1
	ret
	rjmp taw
taw2:
	sbic PINb, 0
	ret
	rjmp taw2

taste4:
	rcall copy_hase
	rcall copy_ram_lcd
	;rjmp menu_kuchentimer_1
	ldi r18,200
	rcall warten2
	rcall taw2
	ldi r18,100
	rcall warten2
taste4b:
	ldi r18,20
	rcall warten2
	rcall scroll
	rcall copy_ram_lcd
	sbic PINb, 0
	rjmp taste4b		;t3
	ldi r18,100
	rcall warten2
	rcall copy_pacman
	rcall copy_ram_lcd
taste4c:
	sbic PINb, 0
	rjmp taste4c		;t3
	ldi r18,100
	rcall warten2
	rjmp loop

;contrast -1
taste3:
	lds r16,$038e
	dec r16
	cpi r16,255
	breq taste5b
	sts $038e,r16
	rcall lcd_kontrast
	rjmp loop

;contrast +1
taste5:
	lds r16,$038e
	inc r16
	cpi r16,64
	breq taste5b
	sts $038e,r16
	rcall lcd_kontrast
	rjmp loop
taste5b:
;ton
	rcall ton
	rjmp loop

ton:
	ldi r17,0
ton2:
	sbi portc,5
	cbi portc,4
	rcall delay5
	cbi portc,5
	sbi portc,4
	rcall delay5
	inc r17
	cpi r17,0
	brne ton2
	cbi portc,5
	cbi portc,4
	ret


;kleine zeichen
uhrzeit:
	lds r16,$0273		;stunde
	rcall dezimal
	ldi r16,':'
	rcall zeichen
	lds r16,$0272		;minute
	rcall dezimal
	lds r16,$0274		;tag
	rcall tagsuchen		;tagnamen darstellen
	ret

;große zahlen
uhrzeit1:
	lds r16,$0273		;stunde
	ldi r16,12
	rcall dezimalb
	ldi r16,10
	rcall zeichenb		;doppelpunkt
	lds r16,$0272		;minute
	ldi r16,25
	rcall dezimalb
	ldi r16,10
;	rcall zeichenb		;doppelpunkt
	lds r16,$0271		;sekunde
;	rcall dezimalb
	ldi r26,0x24		;X position
	ldi r27,0x01	
	lds r16,$0274		;tag
	rcall tagsuchen		;tagnamen darstellen
	ret




text1:
	ldi r30,low(txt1*2)
	ldi r31,high(txt1*2)
	ldi r26,0x60		;X ziel adresse ram
	ldi r27,0x00
text1a:
	lpm r16,Z+
	cpi r16,0
	breq text1end
	rcall zeichen
	rjmp text1a
text1end:
	ret

text2:
	ldi r30,low(txt2*2)
	ldi r31,high(txt2*2)
	ldi r26,0xc2		;X ziel adresse ram
	ldi r27,0x00
text2a:
	lpm r16,Z+
	cpi r16,0
	breq text2end
	rcall zeichen
	rjmp text2a
text2end:
	ret
text3:
	ldi r30,low(txt3*2)
	ldi r31,high(txt3*2)
	ldi r26,0x24		;X ziel adresse ram
	ldi r27,0x01
text3a:
	lpm r16,Z+
	cpi r16,0
	breq text3end
	rcall zeichen
	rjmp text3a
text3end:
	ret

text4:
	ldi r30,low(txt4*2)
	ldi r31,high(txt4*2)
	ldi r26,0x86		;X ziel adresse ram
	ldi r27,0x01
text4a:
	lpm r16,Z+
	cpi r16,0
	breq text4end
	rcall zeichen
	rjmp text4a
text4end:
	ret

text5:
	ldi r30,low(txt5*2)
	ldi r31,high(txt5*2)
	ldi r26,0x60		;X ziel adresse ram
	ldi r27,0x00
text5a:
	lpm r16,Z+
	cpi r16,0
	breq text5end
	rcall zeichen
	rjmp text5a
text5end:
	ret

;r16=zeichen, RAM position= X, flash position=z
zeichen:
	push r16
	push r17
	push r30
	push r31
	ldi r30, low(zeichensatz*2)	;Z einstellen
	ldi r31, high(zeichensatz*2)
	ldi r17,8
	mul r16,r17
	add r30,r0
	adc r31,r1
	ldi r17,0
zeichen2:
	lpm r16,z+
	st x+,r16
	inc r17
	cpi r17,8
	brne zeichen2
	pop r31
	pop r30
	pop r17
	pop r16
	ret

;14 bytes in 2 zeilen ausgeben zahlen 14x16 bit
zeichenb:
	push r16
	push r17
	push r30
	push r31
	push r26
	push r27
	ldi r30, low(zahlen2*2)		;Z einstellen
	ldi r31, high(zahlen2*2)
	ldi r17,28
	mul r16,r17
	add r30,r0
	adc r31,r1		;adresse berechnet von der zahl
	ldi r17,0
zeichenb2:
	lpm r16,z+
	st x+,r16
	inc r17
	cpi r17,14
	brne zeichenb2
	clc
	ldi r17,84		;zeile-14 Zeichenbreite
	ldi r16,0
	add r26,r17		;x auf die untere zeile einstellen 132-14
	adc r27,r16
	ldi r17,0
zeichenb3:
	lpm r16,z+
	st x+,r16
	inc r17
	cpi r17,14
	brne zeichenb3
	pop r27
	pop r26
	ldi r16,0
	clc
	add r26,r17		;x auf die nächste position einstellen
	adc r27,r16
	pop r31
	pop r30
	pop r17
	pop r16
	ret

;pixel setzen r16=x r17=y
pixel:
	push r16
	push r17
	push r18
	push r19
	mov r18,r17
	andi r18,0b00000111	;rest
	lsr r17
	lsr r17
	lsr r17
	andi r17,0b00011111
	ldi r19,98			;122
	mul r17,r19
	ldi r17,0x60
	ldi r19,0
	add r0,r17	;$0060 addieren
	adc r1,r19
	add r0,r16
	adc r1,r19	;x addieren
;bit berechnen
	mov r30,r0
	mov r31,r1
	ldi r17,1
pixel2:
	cpi r18,0
	breq pixelende
	lsl r17
	dec r18
	rjmp pixel2			;geändert jmp
pixelende:
;r17=bit Z=adresse
	ld r16,z
	or r16,r17
	st z,r16
	pop r19
	pop r18
	pop r17
	pop r16
	ret

;löscht lcd ram ab $0060-$036f
lcd_cls:
	push r16
	ldi r30,0x60	;Z init $0060
	ldi r31,0
	ldi r16,0
lcd_clsb:
	st z+,r16
	cpi r30,0x70
	brne lcd_clsb
	cpi r31,0x03
	brne lcd_clsb
	pop r16
	ret

;kopiert ram zum LCD ab $0060-$026f
;verwendet z
copy_ram_lcd:
	push r16
	push r17
	ldi r30,0x60	;Z init $0060
	ldi r31,0
	ldi r16,0
	rcall lcd_page	;1 page
	rcall lcd_column
	rcall copy132
	ldi r16,1
	rcall lcd_page	;2 page
	ldi r16,0
	rcall lcd_column
	rcall copy132
	ldi r16,2
	rcall lcd_page	;3 page
	ldi r16,0
	rcall lcd_column
	rcall copy132
	ldi r16,3
	rcall lcd_page	;4 page
	ldi r16,0
	rcall lcd_column
	rcall copy132

	ldi r16,4
	rcall lcd_page	;5 page
	ldi r16,0
	rcall lcd_column
	rcall copy132
	ldi r16,5
	rcall lcd_page	;6 page
	ldi r16,0
	rcall lcd_column
	rcall copy132
	ldi r16,6
	rcall lcd_page	;7 page
	ldi r16,0
	rcall lcd_column
	rcall copy132
	ldi r16,7
	rcall lcd_page	;8 page
	ldi r16,0
	rcall lcd_column
	rcall copy132

	pop r17
	pop r16
	ret

copy132:
	ldi r17,0
	sbi portd,1		;a0=1
	nop
	nop
copy132b:
	ld r16,Z+
	rcall lcd_byte
	inc r17
	cpi r17,98
	brne copy132b
	ret

copy_bild1:
	
	push r16
	push r17
	push r30
	push r31
	ldi r30, low(bild_hk*2)	;Z einstellen
	ldi r31, high(bild_hk*2)
	ldi r26,0x60		;startposition x
	ldi r27,0x00
zeichen2bb:
	lpm r16,z+
	st x+,r16
	cpi r26,0x70
	brne zeichen2bb
	cpi r27,0x03
	brne zeichen2bb
	pop r31
	pop r30
	pop r17
	pop r16
	ret 

copy_bild2:
	
	push r16
	push r17
	push r30
	push r31
	ldi r30, low(kaffee*2)	;Z einstellen
	ldi r31, high(kaffee*2)
	ldi r26,0x60		;startposition x
	ldi r27,0x00
	rjmp zeichen2bb

copy_hase:
	push r16
	push r17
	push r30
	push r31
	ldi r30, low(hase*2)	;Z einstellen
	ldi r31, high(hase*2)
	ldi r26,0x60		;startposition x
	ldi r27,0x00
	rjmp zeichen2bb

copy_pacman:
	push r16
	push r17
	push r30
	push r31
	ldi r30, low(pacman*2)	;Z einstellen
	ldi r31, high(pacman*2)
	ldi r26,0x60		;startposition x
	ldi r27,0x00
	rjmp zeichen2bb

lcd_init:
	cbi portd,1		;a0=0
	cbi portd,0		;cs=0
	sbi portd,2		;clock=1
	ldi r18,10
	rcall warten2
;lcd Display on
	ldi r16,0xaf
	rcall lcd_byte
;display start adress
	ldi r16,0x40
	rcall lcd_byte
;page adresse
	ldi r16,0xb0
	rcall lcd_byte
;column adress hi bits
	ldi r16,0x10
	rcall lcd_byte
;column adress low bits
	ldi r16,0x00	
	rcall lcd_byte
;adc select
	ldi r16,0b10100000
	rcall lcd_byte
	ldi r18,10
	rcall warten2
;display normal / reverse
	ldi r16,0b10100110
	rcall lcd_byte
;display all points on / normal
	ldi r16,0b10100100
	rcall lcd_byte
;lcd bias set
	ldi r16,0b10100011
	rcall lcd_byte
	ldi r18,10
	rcall warten2
;common output mode select 6:00 oder 12:00 sicht
	ldi r16,0b11001000
	rcall lcd_byte
;power control set
	ldi r16,0b00101111		
	rcall lcd_byte
	ldi r18,10
	rcall warten2
;V0 voltage regulator internal resistor set
	ldi r16,0b00100100		;3 untere bits=wert
	rcall lcd_byte
	ldi r18,10
	rcall warten2
;booster ratio set	
	ldi r16,0b11111000
	rcall lcd_byte
	ldi r18,10
	rcall warten2
	ldi r16,0x00		;0=2x3x4x 1=5x 3=6x
	rcall lcd_byte
	ldi r18,10
	rcall warten2

;volume/kontrast
	ldi r16,0x81		;kontrast
	rcall lcd_byte
	ldi r18,10
	rcall warten2
	ldi r16,9		;kontrast
	rcall lcd_byte
;	rcall lcd_kontrast	
	ldi r18,10
	rcall warten2

	ret


lcd_kontrast:
	cbi portd,1		;a0=0
	ldi r16,0x81		;kontrast
	rcall lcd_byte
	lds r16,$038e		;kontrast
	rcall lcd_byte
	ret
	
;page nr einstellen r16=page	
lcd_page:
	push r16
	push r17
	cbi portd,1	;a0=0
	nop
	nop
	ldi r17,0b10110000
	;andi r16,0b00000011
	or r16,r17
	rcall lcd_byte
	pop r17
	pop r16
	ret

;Spalte nr einstellen r16=spalte
lcd_column:
	push r16
	push r17
	push r18
	mov r18,r16
	swap r16
	andi r16,0b00001111
	cbi portd,1	;a0=0
	nop
	nop
	ldi r17,0b00010000
	or r16,r17
	rcall lcd_byte
	mov r16,r18
	andi r16,0b00001111
	rcall lcd_byte
	pop r18
	pop r17
	pop r16
	ret

;byte senden r16=byte
lcd_byte:
	push r16
	push r17
	push r18
	mov r18,r16
	ldi r17,0
lcd_byte4:
	mov r16,r18
	andi r16,128
	cpi r16,0
	breq lcd_byte0
	sbi portd,3	;data=1
	cbi portd,2	;clock=0
	nop
	;rcall delay5
	sbi portd,2	;clock=1
	rjmp lcd_byte3

lcd_byte0:
	cbi portd,3	;data=0
	cbi portd,2	;clock=0
	nop
	;rcall delay5
	sbi portd,2	;clock=1
	rjmp lcd_byte3

lcd_byte3:
	lsl r18
	inc r17
	cpi r17,8
	brne lcd_byte4
	pop r18
	pop r17
	pop r16
	ret	

adresse:
	ret
byteout:
	ret

;--------------------------------------------------------------
warten2:			;r18 = länge 5= 1 Sec.
 				;Längere Pause für manche Befehle
	push r16
	push r17
	push r18
	                                 ;1 sec Pause (mit 1 Mhz)
wgloop3:   ldi  temp1, 200
WGLOOP0:   ldi  temp2, 60
WGLOOP1:   dec  temp2
           brne WGLOOP1
           dec  temp1
           brne WGLOOP0
	dec temp3
	brne wgloop3
	pop r18
	pop r17
	pop r16
           ret                          ;wieder zurück

delay5: 
	push r16 
	push r17
	push r18
	ldi temp3,1                               ;16 ms Pause (mit 1 Mhz)
wgloop3b:   ldi  temp1, 2
WGLOOP0b:   ldi  temp2, 5
WGLOOP1b:   dec  temp2
           brne WGLOOP1b
           dec  temp1
           brne WGLOOP0b
	dec temp3
	brne wgloop3b
	pop r18
	pop r17
	pop r16
           ret     

delay8: 			;warten für die schleife
	push r16 
	push r17
	push r18
	ldi temp3,1                               ;8 ms Pause (mit 1 Mhz)
wgloop3c:   ldi  temp1, 70	;$e0
WGLOOP0c:   ldi  temp2, 2
WGLOOP1c:   dec  temp2
           brne WGLOOP1c
           dec  temp1
           brne WGLOOP0c
	dec temp3
	brne wgloop3c
	pop r18
	pop r17
	pop r16
           ret            



eepromlesen:
	ldi r19,0		;adresse lcd
	ldi r18,0
	ldi r20,0
eeprc:	out EEARL, r18           ;Adresse setzen
	out EEARH, r20		
	sbi EECR, EERE           ;Lesevorgang aktivieren
        in r16, EEDR		 ;byte von adresport lesen
	cpi r16,0xff
	breq textende
	push r16
	mov r16,r19
	cpi r16,8
	breq adresse40
eeprb:	rcall adresse
	pop r16	
	rcall zeichen
	inc r18
	inc r19
	cpi r19,0x48
	brne eeprc
textende:
	ret
adresse40:
	ldi r16,0x40
	ldi r19,0x40
	rjmp eeprb

textzeigen:			;r18 r19 lo hi startposition r20 anzahl der zeichen r21 Adresse LCD
eeprc2:	out EEARL, r18           ;Adresse setzen
	out EEARH, r19		
	sbi EECR, EERE           ;Lesevorgang aktivieren
        in r16, EEDR		 ;byte von adresport lesen
	cpi r16,0xff
	breq textende2
	push r16
	mov r16,r21
	cpi r16,8
	breq adresse402
eeprb2:	rcall adresse
	pop r16	
	rcall zeichen
	dec r20
	inc r21
	inc r18
	cpi r18,0
	brne eeprc3
	inc r19
eeprc3:	cpi r20,0x00
	brne eeprc2
textende2:
	ret
adresse402:
	ldi r16,0x40
	ldi r21,0x40
	rjmp eeprb2

dezimal:
	push r17		;r16=zahl r17-19 ergebniss (r17=100) wird ins ram kopiert ab adresse X
	push r18
	push r19
	push r20
	ldi r17,0
	ldi r18,0
	ldi r19,0
	clc
dec100:
	inc r17
	subi r16,100
	brcc dec100
	ldi r20,100
	add r16,r20
	dec r17
	clc
dec10:
	inc r18
	subi r16,10
	brcc dec10
	ldi r20,10
	add r16,r20
	dec r18
	clc
	mov r19,r16
	ldi r16,'0'
	add r17,r16
	add r18,r16
	add r19,r16
	mov r16,r17
	;sbi portd,1
	;rcall zeichen		;zahl darstellen 3 stellig
	mov r16,r18
	rcall zeichen
	mov r16,r19
	rcall zeichen
	pop r20
	pop r19
	pop r18
	pop r17
	ret

;dezimal 1 stelle zeigen
dezimal1s:
	push r17		;r16=zahl r17-19 ergebniss (r17=100) wird ins ram kopiert ab adresse X
	push r18
	push r19
	push r20
	ldi r17,0
	ldi r18,0
	ldi r19,0
	clc
dec1s100:
	inc r17
	subi r16,100
	brcc dec1s100
	ldi r20,100
	add r16,r20
	dec r17
	clc
dec1s10:
	inc r18
	subi r16,10
	brcc dec1s10
	ldi r20,10
	add r16,r20
	dec r18
	clc
	mov r19,r16
	ldi r16,'0'
	add r17,r16
	add r18,r16
	add r19,r16
	mov r16,r17
	;sbi portd,1
	;rcall zeichen		;zahl darstellen 3 stellig
	;mov r16,r18
	;rcall zeichen
	mov r16,r19
	rcall zeichen
	pop r20
	pop r19
	pop r18
	pop r17
	ret

;dezimal aber größere zahlen 14 x 16 pixel
dezimalb:
	push r17		;r16=zahl r17-19 ergebniss (r17=100) wird ins ram kopiert ab adresse X
	push r18
	push r19
	push r20
	ldi r17,0
	ldi r18,0
	ldi r19,0
	clc
dec100b:
	inc r17
	subi r16,100
	brcc dec100b
	ldi r20,100
	add r16,r20
	dec r17
	clc
dec10b:
	inc r18
	subi r16,10
	brcc dec10b
	ldi r20,10
	add r16,r20
	dec r18
	clc
	mov r19,r16
	mov r16,r17
	;sbi portd,1
	rcall zeichenb		;zahl darstellen 3 stellig
	mov r16,r18
	rcall zeichenb
	mov r16,r19
	rcall zeichenb
	pop r20
	pop r19
	pop r18
	pop r17
	ret


;adresse von Programm einstellen
adresseeinstellen:
	push r16
	push r17
	push r18
	ldi r23,0x6f		;zeigt auf die Zeit
	ldi r24,0
	mov r16,r22
	
adr10:
	cpi r16,0		;wenn null dann fertig
	breq adr100
;nächstes 00 suchen
	dec r16
adr20:	rcall adrplus
	rcall adrplus
	out EEARL, r23           ;Adresse setzen
	out EEARH, r24		
	sbi EECR, EERE           ;Lesevorgang aktivieren
        in r17, EEDR		 ;byte von adresport lesen			
	cpi r17,0
	breq adr10
	rjmp adr20
adr100:
	rcall adrplus		;adresse auf wert 1 einstellen
	mov r25,r23		;zähler einstellen
	mov r26,r24
	rcall portinit
	pop r18
	pop r17
	pop r16
	ret

adrplus:
	ldi r18,1
	ldi r19,0
	clc
	adc r23,r18
	adc r24,r19
	ret	
adrminus:
	ldi r18,1
	ldi r19,0
	clc
	sbc r23,r18
	sbc r24,r19
	ret

testadresse:
	ldi r16,0
	rcall adresse
	mov r16,r24
	rcall dezimal
	mov r16,r23
	rcall dezimal
	ret

;initialisiert datenport wert und zeit wird eingestellt
portinit:
	rcall readeeprom
	out portd,r16
	rcall readeeprom
	mov r29,r16
	ret

;wert aus dem eeprom lesen und adresse incrementieren ergebnis in r16
readeeprom:
	out EEARL, r25           ;Adresse setzen
	out EEARH, r26		
	sbi EECR, EERE           ;Lesevorgang aktivieren
        in r16, EEDR		 ;byte von adresport lesen	
	ldi r18,1
	ldi r19,0
	clc
	adc r25,r18		;adresse +1
	adc r26,r19
	ret

;lädt 0-25 bytes in ram von eeprom, wenn das erste byte 255 ist dann wird nichts geladen
ee_load:
	ldi r25,0
	ldi r26,0
	rcall readeeprom
	cpi r16,255
	brne ee_load3
	ret
ee_load3:
	ldi r25,0
	ldi r28,0x75
	ldi r29,0x02		;y=adresse
ee_load2:
	rcall readeeprom
	st y+,r16		;daten	
	cpi r25,26
	brne ee_load2
	ret

schleife:		;wird dauernd aufgerufen
	cpi r29,0
	breq neuesladen
	dec r29
	rcall delay8
	ret
neuesladen:
	rcall readeeprom
	out portd,r16
	rcall readeeprom
	mov r29,r16
	cpi r16,0
	breq vonanfang
	ret		
vonanfang:
	mov r25,r23		;zähler einstellen
	mov r26,r24
	rjmp neuesladen

;*** IRQ ***
;ton
;zählt tondauer runter wenn=0 dann ton aus nächste note
;zählt dauer für frequenz wenn 0 dann wechsel am lautsprecher pin dann wieder von anfang
;
timer0:
	push r16
	in r16,sreg
	push r16
	push r17
	push r30
	push r31
	ldi r16,200
	out tcnt0,r16		;frequenz
	lds r16,$0291
	cpi r16,1
	breq tonan
	cbi portc,5		;ton aus
	cbi portc,4
	rjmp tim0ende
tonan:
	lds r16,$0295
	inc r16
	sts $0295,r16		;zählen 0-255 um etwas länger zu warten
	cpi r16,0
	brne frequenzzehlen
	lds r16,$0290		;dauer zählen
	cpi r16,0		;ton ende?
	breq tonende
	dec r16
	sts $0290,r16		;zähler-1 und speichern
	rjmp frequenzzehlen

tonende:
	rcall tonleiter
	rjmp tim0ende

	lds r30,$0292	;lo z
	lds r31,$0293	;hi z
	lpm r16,z+	;frequenz
	lpm r17,z+	;ton länge
	sts $0292,r30
	sts $0293,r31
	cpi r17,255
	breq tonausaus
	sts $028f,r16
	sts $0290,r17
	rjmp tim0ende
tonausaus:
	;ldi r16,0
	;sts $0291,r16		;ton stop
	rcall tontest		;lied von anfang spielen
tim0ende:

	pop r31
	pop r30
	pop r17
	pop r16
	out sreg,r16
	pop r16
	reti

frequenzzehlen:
	lds r16,$0294
	cpi r16,0
	breq tonwechsel
	dec r16
	sts $0294,r16
	rjmp tim0ende

tonwechsel:
;wechselt zustand pc5
	sbic pinc,5	
	rjmp ton_aus
	sbi portc,5
	cbi portc,4
tonwechsel2:
	lds r16,$028f
	ldi r17,255
	eor r16,r17
	sts $0294,r16
	rjmp tim0ende
ton_aus:
	cbi portc,5
	sbi portc,4
	rjmp tonwechsel2	

tonleiter:
	lds r16,$028f
	inc r16
	cpi r16,252
	brne tonleiter2
	ldi r16,200
tonleiter2:
	sts $028f,r16
	;sts $0294,r16
	ldi r16,2
	sts $0290,r16
	ret

;uhr
timer2:
	push r16
	in r16,sreg
	push r16
	push r17
	push r22
	push r20
	push r21
	push r19
	push r18
	push r26
	push r27
	ldi r16,224		;223 zu langsam
	out tcnt2,r16

	;rjmp secplus
tim0e:
	pop r27
	pop r26
	pop r18
	pop r19
	pop r21
	pop r20
	pop r22
	pop r17
	pop r16
	out sreg,r16
	pop r16
	reti

secplus:
	lds r16,$0297
	inc r16
	sts $0297,r16
	rcall testalarm		;zähler 0-60 addieren wenn alarm
;	rcall kuchentimer_irq	;zählen küchentimer wenn an
	ldi r16,0
	sts $0270,r16		;zähler 0-30 für sekunden
	lds r16,$0271
	inc r16
	cpi r16,60
	breq minplus
	sts $0271,r16
	rjmp tim0e
minplus:
	ldi r16,0
	sts $0271, r16		
	lds r16,$0272
	inc r16
	cpi r16,60
	breq stundenplus
	sts $0272,r16		;z"hler =0
;	rcall alarm_test_1	;auf alarm testen
	cpi r16,1
	brne minplus2
	rcall tontest
minplus2:
	rjmp tim0e
stundenplus:
;	rcall alarm_test	;auf alarm testen
	cpi r16,1
	brne stundenplus2
	rcall tontest
stundenplus2:
	ldi r16,0
	sts $0272, r16
	lds r16,$0273
	inc r16
	cpi r16,24
	breq tageplus
	sts $0273,r16		;z"hler =0
	rjmp tim0e
tageplus:
	ldi r16,0
	sts $0273,r16
	lds r16,$0274
	inc r16
	cpi r16,7
	breq tageplusb
	sts $0274,r16
	rjmp tim0e
tageplusb:
	ldi r16,0
	sts $0274,r16		;tag=0
	rjmp tim0e

;testen wenn alarm an dann zähler addieren bis 60
testalarm:
	lds r16,$0275
	cpi r16,0
	brne testalarm2
	ret
testalarm2:
	lds r16,$0296		;zähler addieren
	cpi r16,60
	breq testalarmend
	inc r16
	sts $0296,r16
testalarmend:
	ret



;Tagnamen darstellen
;r16=tagnr 0-6
tagsuchen:
	push r17
	push r16
	ldi r30,low(tage*2)	;Z quelle text lo
	ldi r31,high(tage*2)	;Z HI
	ldi r17,0
tagsuchen4:
	cpi r16,0
	breq tagsuchen3
tagsuchen2:
	lpm r17,z+		;lesen bis null kommt
	cpi r17,0
	brne tagsuchen2
	dec r16
	rjmp tagsuchen4
tagsuchen3:
	lpm r16,z+		;lesen bis null kommt
	cpi r16,0
	breq tagsuchen5
	rcall zeichen
	;inc r17
	;cpi r17,77
	;breq tagsuchen5
	rjmp tagsuchen3	
tagsuchen5:
	pop r16
	pop r17
	ret
; 			*** Menüs ***


EEPROM_write:
; Wait for completion of previous write
	sbic EECR,EEWE
	rjmp EEPROM_write
; Set up address (r18:r17) in address register
	out EEARH, r18
	out EEARL, r17
; Write data (r16) to data register
	out EEDR,r16
; Write logical one to EEMWE
	sbi EECR,EEMWE
; Start eeprom write by setting EEWE
	sbi EECR,EEWE
	ret

;stellt die erste melodie ein
tontest:
	ldi r16,low(tona*2)
	ldi r17,high(tona*2)
	sts $0292,r16	;lo zeiger auf lied
	sts $0293,r17	;hi zeiger
	ldi r16,200
	sts $028f,r16	;frequenz
	ldi r16,0
	sts $0290,r16	;ton dauer
	ldi r16,1
	sts $0291,r16	;ton an
	;rcall tonleiter
	ret

;zeile nach links scrollen das erste Byte kommt am ende hin 98 bytes verschieben
scroll:
	ldi r26,0x0e	;x von hier kopieren
	ldi r27,0x03
	ldi r28,0x0e	;y dort kopieren
	ldi r29,0x03
	ld r16,x+
	push r16		;erstes byte merken
	ldi r17,0		;zähler
scroll2:
	ld r16,x+
	st y+,r16
	inc r17
	cpi r17,97
	brne scroll2
	pop r16
	st y,r16
	ret


txt1:
.db "C64 BASIC V2",0
txt2:
.db "64K RAM SYS.",0
txt3:
.db " 38911 FREE ",0
txt4:
.db "READY.",0
txt5:
.db "Timer:",0
txtzeit:
.db "Stunden und Min. einstellen",0
txttag:
.db "Tag einstellen             ",0

tage:
.db " Montag    ",0
.db " Dienstag  ",0
.db " Mittwoch  ",0
.db " Donnerstag",0
.db " Freitag   ",0
.db " Samstag   ",0
.db " Sonntag   ",0

txtalarmnr:
.db "Alarm Nr.        einstellen",0
txtalarmnr2:
.db "Alarm Nr.        Stunde, Minute  einstellen",0
txtalarmnr3:
.db "Alarm Nr.        Tag, an/aus     einstellen",0
txtalarmnr4:
.db "Kontrast +-      Alarm an/aus    speichern ",0


;kompletter zeichensatz von 0-255 8x8 pixel, jedes zeichen von links nach rechts angeordnet
zeichensatz:
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 192 , 255 , 127 , 5 , 5 , 101 , 127 , 63 , 90 , 90 , 60 , 231 , 231 , 60 , 90 , 90 
.db  127 , 62 , 62 , 28 , 28 , 8 , 8 , 0 , 8 , 8 , 28 , 28 , 62 , 62 , 127 , 0 , 0 , 36 , 102 , 255 , 255 , 102 , 36 , 0 , 0 , 95 , 95 , 0 , 0 , 95 , 95 , 0 
.db  6 , 15 , 9 , 127 , 127 , 1 , 127 , 127 , 64 , 154 , 191 , 165 , 165 , 253 , 89 , 2 , 0 , 112 , 112 , 112 , 112 , 112 , 112 , 0 , 128 , 148 , 182 , 255 , 255 , 182 , 148 , 128 
.db  0 , 4 , 6 , 127 , 127 , 6 , 4 , 0 , 0 , 16 , 48 , 127 , 127 , 48 , 16 , 0 , 8 , 8 , 8 , 42 , 62 , 28 , 8 , 0 , 8 , 28 , 62 , 42 , 8 , 8 , 8 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 6 , 95 , 95 , 6 , 0 , 0 , 0 , 3 , 7 , 0 , 0 , 7 , 3 , 0 , 20 , 127 , 127 , 20 , 127 , 127 , 20 , 0 
.db  0 , 36 , 46 , 107 , 107 , 58 , 18 , 0 , 70 , 102 , 48 , 24 , 12 , 102 , 98 , 0 , 48 , 122 , 79 , 93 , 55 , 122 , 72 , 0 , 0 , 0 , 4 , 7 , 3 , 0 , 0 , 0 
.db  0 , 0 , 28 , 62 , 99 , 65 , 0 , 0 , 0 , 0 , 65 , 99 , 62 , 28 , 0 , 0 , 8 , 42 , 62 , 28 , 28 , 62 , 42 , 8 , 0 , 8 , 8 , 62 , 62 , 8 , 8 , 0 
.db  0 , 0 , 128 , 224 , 96 , 0 , 0 , 0 , 0 , 8 , 8 , 8 , 8 , 8 , 8 , 0 , 0 , 0 , 0 , 96 , 96 , 0 , 0 , 0 , 96 , 48 , 24 , 12 , 6 , 3 , 1 , 0 
.db  28 , 62 , 99 , 73 , 99 , 62 , 28 , 0 , 0 , 64 , 66 , 127 , 127 , 64 , 64 , 0 , 66 , 99 , 113 , 89 , 73 , 111 , 102 , 0 , 34 , 99 , 73 , 73 , 73 , 127 , 54 , 0 
.db  24 , 28 , 22 , 83 , 127 , 127 , 80 , 0 , 47 , 111 , 73 , 73 , 73 , 121 , 49 , 0 , 60 , 126 , 75 , 73 , 73 , 120 , 48 , 0 , 3 , 3 , 113 , 121 , 13 , 7 , 3 , 0 
.db  54 , 127 , 73 , 73 , 73 , 127 , 54 , 0 , 6 , 79 , 73 , 73 , 105 , 63 , 30 , 0 , 0 , 0 , 0 , 102 , 102 , 0 , 0 , 0 , 0 , 0 , 128 , 230 , 102 , 0 , 0 , 0 
.db  0 , 0 , 8 , 28 , 54 , 99 , 65 , 0 , 0 , 36 , 36 , 36 , 36 , 36 , 36 , 0 , 0 , 65 , 99 , 54 , 28 , 8 , 0 , 0 , 2 , 3 , 1 , 89 , 93 , 7 , 2 , 0 
.db  62 , 127 , 65 , 93 , 93 , 31 , 30 , 0 , 124 , 126 , 11 , 9 , 11 , 126 , 124 , 0 , 65 , 127 , 127 , 73 , 73 , 127 , 54 , 0 , 28 , 62 , 99 , 65 , 65 , 99 , 34 , 0 
.db  65 , 127 , 127 , 65 , 99 , 62 , 28 , 0 , 65 , 127 , 127 , 73 , 93 , 65 , 99 , 0 , 65 , 127 , 127 , 73 , 29 , 1 , 3 , 0 , 28 , 62 , 99 , 65 , 81 , 51 , 114 , 0 
.db  127 , 127 , 8 , 8 , 8 , 127 , 127 , 0 , 0 , 0 , 65 , 127 , 127 , 65 , 0 , 0 , 48 , 112 , 64 , 65 , 127 , 63 , 1 , 0 , 65 , 127 , 127 , 8 , 28 , 119 , 99 , 0 
.db  65 , 127 , 127 , 65 , 64 , 96 , 112 , 0 , 127 , 127 , 14 , 28 , 14 , 127 , 127 , 0 , 127 , 127 , 6 , 12 , 24 , 127 , 127 , 0 , 62 , 127 , 65 , 65 , 65 , 127 , 62 , 0 
.db  65 , 127 , 127 , 73 , 9 , 15 , 6 , 0 , 62 , 127 , 65 , 65 , 225 , 255 , 190 , 0 , 65 , 127 , 127 , 9 , 25 , 127 , 102 , 0 , 0 , 34 , 103 , 77 , 89 , 115 , 34 , 0 
.db  0 , 7 , 67 , 127 , 127 , 67 , 7 , 0 , 63 , 127 , 64 , 64 , 64 , 127 , 63 , 0 , 31 , 63 , 96 , 64 , 96 , 63 , 31 , 0 , 63 , 127 , 96 , 56 , 96 , 127 , 63 , 0 
.db  99 , 119 , 28 , 8 , 28 , 119 , 99 , 0 , 0 , 7 , 79 , 120 , 120 , 79 , 7 , 0 , 71 , 99 , 113 , 89 , 77 , 103 , 115 , 0 , 0 , 0 , 127 , 127 , 65 , 65 , 0 , 0 
.db  1 , 3 , 6 , 12 , 24 , 48 , 96 , 0 , 0 , 0 , 65 , 65 , 127 , 127 , 0 , 0 , 8 , 12 , 6 , 3 , 6 , 12 , 8 , 0 , 128 , 128 , 128 , 128 , 128 , 128 , 128 , 128 
.db  0 , 0 , 1 , 3 , 6 , 4 , 0 , 0 , 32 , 116 , 84 , 84 , 60 , 120 , 64 , 0 , 65 , 127 , 63 , 68 , 68 , 124 , 56 , 0 , 56 , 124 , 68 , 68 , 68 , 108 , 40 , 0 
.db  56 , 124 , 68 , 69 , 63 , 127 , 64 , 0 , 56 , 124 , 84 , 84 , 84 , 92 , 24 , 0 , 72 , 126 , 127 , 73 , 9 , 3 , 2 , 0 , 152 , 188 , 164 , 164 , 248 , 124 , 4 , 0 
.db  65 , 127 , 127 , 8 , 4 , 124 , 120 , 0 , 0 , 0 , 68 , 125 , 125 , 64 , 0 , 0 , 0 , 96 , 224 , 128 , 128 , 253 , 125 , 0 , 65 , 127 , 127 , 16 , 56 , 108 , 68 , 0 
.db  0 , 0 , 65 , 127 , 127 , 64 , 0 , 0 , 124 , 124 , 12 , 120 , 12 , 124 , 120 , 0 , 4 , 124 , 120 , 4 , 4 , 124 , 120 , 0 , 56 , 124 , 68 , 68 , 68 , 124 , 56 , 0 
.db  132 , 252 , 248 , 164 , 36 , 60 , 24 , 0 , 24 , 60 , 36 , 164 , 248 , 252 , 132 , 0 , 68 , 124 , 120 , 76 , 4 , 12 , 8 , 0 , 72 , 92 , 84 , 84 , 84 , 116 , 36 , 0 
.db  4 , 4 , 63 , 127 , 68 , 100 , 32 , 0 , 60 , 124 , 64 , 64 , 60 , 124 , 64 , 0 , 28 , 60 , 96 , 64 , 96 , 60 , 28 , 0 , 60 , 124 , 96 , 56 , 96 , 124 , 60 , 0 
.db  68 , 108 , 56 , 16 , 56 , 108 , 68 , 0 , 156 , 188 , 160 , 160 , 160 , 252 , 124 , 0 , 0 , 76 , 100 , 116 , 92 , 76 , 100 , 0 , 0 , 8 , 8 , 62 , 119 , 65 , 65 , 0 
.db  0 , 0 , 0 , 127 , 127 , 0 , 0 , 0 , 0 , 65 , 65 , 119 , 62 , 8 , 8 , 0 , 2 , 3 , 1 , 3 , 2 , 3 , 1 , 0 , 112 , 120 , 76 , 70 , 76 , 120 , 112 , 0 
.db  30 , 191 , 161 , 161 , 225 , 115 , 18 , 0 , 61 , 125 , 64 , 64 , 61 , 125 , 64 , 0 , 56 , 124 , 84 , 86 , 87 , 93 , 24 , 0 , 34 , 117 , 85 , 85 , 61 , 121 , 66 , 0 
.db  33 , 117 , 84 , 84 , 60 , 121 , 65 , 0 , 32 , 116 , 85 , 87 , 62 , 120 , 64 , 0 , 32 , 116 , 87 , 87 , 60 , 120 , 64 , 0 , 24 , 60 , 164 , 164 , 228 , 100 , 36 , 0 
.db  58 , 125 , 85 , 85 , 85 , 93 , 26 , 0 , 57 , 125 , 84 , 84 , 84 , 93 , 25 , 0 , 56 , 124 , 85 , 87 , 86 , 92 , 24 , 0 , 0 , 1 , 69 , 124 , 124 , 65 , 1 , 0 
.db  2 , 1 , 69 , 125 , 125 , 65 , 2 , 0 , 0 , 0 , 73 , 123 , 122 , 64 , 0 , 0 , 121 , 125 , 22 , 18 , 22 , 125 , 121 , 0 , 120 , 126 , 23 , 21 , 23 , 126 , 120 , 0 
.db  124 , 124 , 86 , 87 , 85 , 68 , 68 , 0 , 112 , 84 , 84 , 124 , 84 , 84 , 92 , 0 , 124 , 126 , 11 , 9 , 127 , 127 , 73 , 0 , 58 , 125 , 69 , 69 , 69 , 125 , 58 , 0 
.db  57 , 125 , 68 , 68 , 68 , 125 , 57 , 0 , 56 , 124 , 69 , 71 , 70 , 124 , 56 , 0 , 58 , 121 , 65 , 65 , 57 , 122 , 64 , 0 , 60 , 125 , 67 , 66 , 60 , 124 , 64 , 0 
.db  157 , 189 , 160 , 160 , 160 , 253 , 125 , 0 , 25 , 61 , 102 , 66 , 102 , 61 , 25 , 0 , 61 , 125 , 64 , 64 , 64 , 125 , 61 , 0 , 184 , 124 , 100 , 84 , 76 , 124 , 58 , 0 
.db  72 , 126 , 127 , 73 , 67 , 102 , 32 , 0 , 92 , 62 , 115 , 73 , 103 , 62 , 29 , 0 , 34 , 54 , 28 , 8 , 28 , 54 , 34 , 0 , 32 , 96 , 72 , 126 , 63 , 9 , 3 , 2 
.db  32 , 116 , 86 , 87 , 61 , 120 , 64 , 0 , 0 , 0 , 72 , 122 , 123 , 65 , 0 , 0 , 56 , 124 , 68 , 70 , 71 , 125 , 56 , 0 , 60 , 124 , 66 , 67 , 61 , 124 , 64 , 0 
.db  10 , 123 , 113 , 11 , 10 , 123 , 113 , 0 , 122 , 123 , 25 , 51 , 98 , 123 , 121 , 0 , 0 , 38 , 47 , 41 , 47 , 47 , 40 , 0 , 0 , 38 , 47 , 41 , 47 , 38 , 0 , 0 
.db  0 , 32 , 112 , 93 , 77 , 64 , 96 , 32 , 126 , 129 , 189 , 149 , 149 , 169 , 129 , 126 , 8 , 8 , 8 , 8 , 8 , 56 , 56 , 0 , 66 , 111 , 63 , 24 , 204 , 238 , 187 , 145 
.db  66 , 111 , 63 , 88 , 108 , 214 , 251 , 65 , 0 , 0 , 48 , 125 , 125 , 48 , 0 , 0 , 8 , 28 , 54 , 34 , 8 , 28 , 54 , 34 , 34 , 54 , 28 , 8 , 34 , 54 , 28 , 8 
.db  170 , 0 , 85 , 0 , 170 , 0 , 85 , 0 , 170 , 85 , 170 , 85 , 170 , 85 , 170 , 85 , 170 , 255 , 85 , 255 , 170 , 255 , 85 , 255 , 0 , 0 , 0 , 255 , 255 , 0 , 0 , 0 
.db  16 , 16 , 16 , 255 , 255 , 0 , 0 , 0 , 112 , 122 , 47 , 37 , 44 , 120 , 112 , 0 , 114 , 121 , 45 , 37 , 45 , 121 , 114 , 0 , 112 , 120 , 44 , 37 , 47 , 122 , 112 , 0 
.db  126 , 129 , 153 , 165 , 165 , 165 , 129 , 126 , 20 , 20 , 247 , 247 , 0 , 255 , 255 , 0 , 0 , 0 , 255 , 255 , 0 , 255 , 255 , 0 , 20 , 20 , 244 , 244 , 4 , 252 , 252 , 0 
.db  20 , 20 , 23 , 23 , 16 , 31 , 31 , 0 , 24 , 60 , 36 , 231 , 231 , 36 , 36 , 0 , 0 , 43 , 47 , 252 , 252 , 47 , 43 , 0 , 16 , 16 , 16 , 240 , 240 , 0 , 0 , 0 
.db  0 , 0 , 0 , 31 , 31 , 16 , 16 , 16 , 16 , 16 , 16 , 31 , 31 , 16 , 16 , 16 , 16 , 16 , 16 , 240 , 240 , 16 , 16 , 16 , 0 , 0 , 0 , 255 , 255 , 16 , 16 , 16 
.db  16 , 16 , 16 , 16 , 16 , 16 , 16 , 16 , 16 , 16 , 16 , 255 , 255 , 16 , 16 , 16 , 34 , 119 , 85 , 87 , 86 , 127 , 121 , 0 , 114 , 123 , 45 , 39 , 46 , 123 , 113 , 0 
.db  0 , 0 , 31 , 31 , 16 , 23 , 23 , 20 , 0 , 0 , 252 , 252 , 4 , 244 , 244 , 20 , 20 , 20 , 23 , 23 , 16 , 23 , 23 , 20 , 20 , 20 , 244 , 244 , 4 , 244 , 244 , 20 
.db  0 , 0 , 255 , 255 , 0 , 247 , 247 , 20 , 20 , 20 , 20 , 20 , 20 , 20 , 20 , 20 , 20 , 20 , 247 , 247 , 0 , 247 , 247 , 20 , 90 , 126 , 36 , 36 , 36 , 126 , 90 , 0 
.db  48 , 122 , 75 , 75 , 126 , 62 , 2 , 0 , 73 , 127 , 127 , 73 , 99 , 62 , 28 , 0 , 126 , 125 , 85 , 85 , 85 , 85 , 70 , 0 , 125 , 125 , 84 , 84 , 84 , 85 , 69 , 0 
.db  124 , 124 , 85 , 87 , 86 , 84 , 68 , 0 , 0 , 0 , 68 , 124 , 124 , 64 , 0 , 0 , 0 , 0 , 68 , 126 , 127 , 69 , 0 , 0 , 0 , 2 , 69 , 125 , 125 , 69 , 2 , 0 
.db  0 , 1 , 69 , 124 , 124 , 69 , 1 , 0 , 16 , 16 , 16 , 31 , 31 , 0 , 0 , 0 , 0 , 0 , 0 , 240 , 240 , 16 , 16 , 16 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 
.db  240 , 240 , 240 , 240 , 240 , 240 , 240 , 240 , 0 , 0 , 0 , 231 , 231 , 0 , 0 , 0 , 0 , 0 , 69 , 127 , 126 , 68 , 0 , 0 , 15 , 15 , 15 , 15 , 15 , 15 , 15 , 15 
.db  16 , 58 , 111 , 69 , 108 , 56 , 16 , 0 , 126 , 127 , 1 , 9 , 95 , 118 , 32 , 0 , 18 , 57 , 109 , 69 , 109 , 57 , 18 , 0 , 16 , 56 , 108 , 68 , 109 , 59 , 18 , 0 

;14x16 pixel zahlen, 14 bytes oben und 14 bytes unten
zahlen2:
.db  224 , 252 , 254 , 254 , 255 , 7 , 7 , 255 , 254 , 254 , 252 , 224 , 0 , 0 , 3 , 31 
.db  63 , 63 , 127 , 112 , 112 , 127 , 63 , 63 , 31 , 3 , 0 , 0 , 0 , 112 , 48 , 56 
.db  28 , 254 , 255 , 255 , 255 , 255 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 127 
.db  127 , 127 , 127 , 127 , 0 , 0 , 0 , 0 , 0 , 12 , 14 , 14 , 15 , 7 , 135 , 255 
.db  254 , 254 , 254 , 56 , 0 , 0 , 64 , 112 , 120 , 124 , 126 , 127 , 119 , 115 , 113 , 113 
.db  112 , 112 , 0 , 0 , 0 , 12 , 14 , 14 , 15 , 135 , 199 , 255 , 254 , 126 , 62 , 24 
.db  0 , 0 , 0 , 24 , 56 , 56 , 120 , 113 , 113 , 127 , 63 , 63 , 31 , 14 , 0 , 0 
.db  128 , 192 , 224 , 120 , 28 , 14 , 255 , 255 , 255 , 255 , 255 , 0 , 0 , 0 , 15 , 15 
.db  14 , 14 , 14 , 14 , 127 , 127 , 127 , 127 , 127 , 14 , 14 , 0 , 0 , 0 , 255 , 255 
.db  255 , 199 , 231 , 231 , 231 , 199 , 199 , 0 , 0 , 0 , 0 , 24 , 56 , 56 , 120 , 112 
.db  112 , 127 , 63 , 63 , 31 , 7 , 0 , 0 , 224 , 252 , 254 , 254 , 254 , 199 , 199 , 231 
.db  231 , 198 , 198 , 0 , 0 , 0 , 3 , 31 , 63 , 63 , 127 , 112 , 112 , 127 , 63 , 63 
.db  31 , 15 , 0 , 0 , 7 , 7 , 7 , 7 , 7 , 7 , 199 , 231 , 255 , 63 , 15 , 7 
.db  0 , 0 , 0 , 0 , 0 , 0 , 124 , 127 , 127 , 127 , 1 , 0 , 0 , 0 , 0 , 0 
.db  24 , 60 , 126 , 254 , 255 , 199 , 199 , 255 , 254 , 126 , 62 , 24 , 0 , 0 , 12 , 63 
.db  63 , 63 , 127 , 113 , 113 , 127 , 63 , 63 , 63 , 12 , 0 , 0 , 112 , 252 , 254 , 254 
.db  255 , 135 , 135 , 255 , 254 , 254 , 252 , 224 , 0 , 0 , 0 , 49 , 49 , 113 , 113 , 113 
.db  113 , 63 , 63 , 63 , 31 , 3 , 0 , 0 , 0 , 0 , 0 , 240 , 240 , 240 , 240 , 240 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 125 , 125 , 125 , 125 , 125 , 0 , 0 
.db  0 , 0 , 0 , 0
tona:
.db 1,25,10,25,20,25,30,25,40,25,50,25,60,25,70,25,80,25
.db 100,25,110,25,120,25,130,25,140,25,150,25,160,25,170,25,180,25,190,25
.db 200,25,210,25,220,25,230,25,240,25,250,50,255

;bild Horst kampa
bild_hk:
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 128 , 224 , 240 , 112 , 48 , 48 , 48 , 48 
.db  48 , 0 , 48 , 240 , 240 , 128 , 0 , 0 , 0 , 224 , 240 , 112 , 0 , 0 , 0 , 0 
.db  0 , 0 , 192 , 240 , 240 , 16 , 16 , 240 , 240 , 224 , 0 , 0 , 192 , 240 , 240 , 48 
.db  16 , 112 , 240 , 224 , 0 , 0 , 0 , 48 , 48 , 240 , 240 , 0 , 0 , 0 , 0 , 0 
.db  224 , 240 , 176 , 16 , 48 , 240 , 240 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 192 
.db  192 , 192 , 192 , 192 , 128 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 7 , 31 , 31 , 56 , 48 , 48 
.db  48 , 48 , 48 , 0 , 0 , 0 , 7 , 63 , 62 , 60 , 63 , 15 , 1 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 1 , 51 , 51 , 54 , 50 , 59 , 31 , 15 , 0 , 0 , 15 , 31 
.db  63 , 48 , 48 , 56 , 31 , 15 , 0 , 0 , 0 , 48 , 48 , 63 , 63 , 48 , 48 , 48 
.db  0 , 0 , 30 , 63 , 51 , 33 , 51 , 63 , 30 , 12 , 0 , 0 , 0 , 0 , 0 , 0 
.db  28 , 62 , 54 , 38 , 54 , 63 , 63 , 63 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 248 , 248 , 248 , 32 , 32 , 248 , 248 , 248 
.db  0 , 0 , 192 , 224 , 96 , 32 , 32 , 224 , 224 , 192 , 0 , 224 , 224 , 224 , 64 , 96 
.db  32 , 192 , 224 , 224 , 160 , 160 , 96 , 0 , 0 , 248 , 248 , 248 , 32 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 248 , 248 , 248 , 64 , 240 , 248 , 200 , 0 , 0 , 0 , 96 , 32 
.db  160 , 224 , 224 , 224 , 0 , 32 , 224 , 224 , 224 , 32 , 96 , 224 , 224 , 64 , 32 , 224 
.db  224 , 0 , 0 , 224 , 224 , 224 , 32 , 32 , 224 , 224 , 192 , 0 , 0 , 96 , 32 , 160 
.db  224 , 224 , 224 , 0 , 0 , 0 , 0 , 0 , 0 , 8 , 15 , 15 , 15 , 0 , 0 , 15 
.db  15 , 15 , 0 , 0 , 7 , 15 , 14 , 8 , 8 , 15 , 15 , 7 , 0 , 15 , 15 , 15 
.db  0 , 0 , 0 , 12 , 9 , 9 , 11 , 15 , 15 , 2 , 0 , 15 , 15 , 15 , 8 , 4 
.db  0 , 0 , 0 , 0 , 0 , 0 , 15 , 15 , 15 , 0 , 1 , 15 , 15 , 15 , 8 , 14 
.db  15 , 15 , 9 , 15 , 15 , 15 , 8 , 0 , 15 , 15 , 15 , 0 , 8 , 15 , 15 , 0 
.db  0 , 15 , 15 , 8 , 0 , 127 , 127 , 127 , 72 , 8 , 14 , 15 , 7 , 0 , 14 , 15 
.db  15 , 9 , 15 , 15 , 15 , 8 , 0 , 0 , 0 , 0 , 0 , 0 , 128 , 0 , 160 , 224 
.db  224 , 240 , 240 , 240 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 240 , 240 , 240 , 0 , 0 
.db  0 , 128 , 192 , 224 , 160 , 48 , 112 , 48 , 16 , 0 , 224 , 112 , 240 , 112 , 80 , 112 
.db  112 , 112 , 48 , 48 , 48 , 144 , 240 , 224 , 0 , 128 , 240 , 240 , 240 , 112 , 112 , 176 
.db  240 , 176 , 48 , 48 , 16 , 240 , 160 , 192 , 0 , 240 , 240 , 208 , 80 , 48 , 48 , 48 
.db  112 , 112 , 80 , 80 , 112 , 240 , 224 , 0 , 0 , 224 , 176 , 176 , 48 , 48 , 80 , 80 
.db  80 , 112 , 112 , 112 , 176 , 224 , 224 , 0 , 0 , 0 , 0 , 240 , 224 , 250 , 216 , 203 
.db  248 , 248 , 110 , 247 , 247 , 255 , 224 , 240 , 180 , 92 , 92 , 240 , 0 , 247 , 231 , 251 
.db  6 , 5 , 30 , 63 , 123 , 249 , 224 , 192 , 128 , 0 , 0 , 0 , 159 , 255 , 173 , 14 
.db  14 , 6 , 14 , 14 , 14 , 14 , 14 , 63 , 255 , 253 , 0 , 232 , 222 , 253 , 153 , 0 
.db  0 , 15 , 15 , 15 , 0 , 0 , 12 , 223 , 215 , 63 , 0 , 127 , 255 , 255 , 6 , 14 
.db  10 , 14 , 14 , 14 , 14 , 14 , 15 , 14 , 7 , 0 , 0 , 255 , 253 , 255 , 6 , 6 
.db  14 , 14 , 14 , 14 , 14 , 14 , 223 , 223 , 251 , 0 , 0 , 0 , 0 , 1 , 29 , 63 
.db  110 , 114 , 235 , 253 , 223 , 189 , 255 , 223 , 251 , 0 , 1 , 19 , 1 , 2 , 0 , 251 
.db  43 , 43 , 40 , 44 , 40 , 40 , 0 , 248 , 1 , 3 , 3 , 3 , 2 , 0 , 248 , 43 
.db  43 , 40 , 40 , 40 , 0 , 0 , 248 , 8 , 8 , 11 , 11 , 155 , 0 , 11 , 10 , 11 
.db  250 , 8 , 8 , 8 , 0 , 248 , 40 , 40 , 41 , 107 , 171 , 17 , 0 , 249 , 11 , 11 
.db  8 , 8 , 248 , 0 , 0 , 248 , 16 , 32 , 192 , 128 , 248 , 0 , 0 , 8 , 11 , 75 
.db  248 , 24 , 8 , 0 , 248 , 152 , 8 , 8 , 11 , 11 , 155 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 0 , 1 , 6 , 254 , 0 , 0 , 1 , 254 
.db  4 , 1 , 1 , 1 , 3 , 3 , 1 , 1 , 0 , 1 , 3 , 1 , 1 , 3 , 0 , 0 
.db  3 , 3 , 1 , 1 , 3 , 1 , 0 , 0 , 1 , 3 , 1 , 3 , 3 , 1 , 0 , 0 
.db  0 , 0 , 1 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 1 
.db  1 , 1 , 1 , 3 , 1 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 1 
.db  1 , 1 , 3 , 1 , 3 , 0 , 0 , 1 , 3 , 1 , 1 , 1 , 3 , 0 , 0 , 0

kaffee:
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  17 , 34 , 20 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  17 , 0 , 85 , 34 , 0 , 0 , 17 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 128 , 64 , 32 , 16 , 48 
.db  80 , 144 , 16 , 160 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 128 , 192 , 224 , 112 , 48 , 16 , 152 , 24 , 8 , 76 , 140 
.db  68 , 140 , 68 , 140 , 69 , 140 , 68 , 140 , 84 , 8 , 92 , 8 , 16 , 24 , 16 , 48 
.db  48 , 32 , 64 , 224 , 65 , 96 , 80 , 152 , 84 , 38 , 85 , 25 , 21 , 12 , 4 , 2 
.db  3 , 3 , 1 , 0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 160 , 244 , 14 , 71 , 227 , 213 , 226 , 245 , 251 , 213 , 254 , 119 , 255 
.db  93 , 255 , 117 , 251 , 85 , 255 , 87 , 255 , 87 , 255 , 119 , 191 , 85 , 255 , 119 , 255 
.db  221 , 254 , 253 , 234 , 245 , 249 , 117 , 231 , 7 , 14 , 116 , 160 , 0 , 0 , 1 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 128 , 80 , 32 
.db  16 , 48 , 80 , 64 , 64 , 187 , 255 , 248 , 113 , 227 , 199 , 143 , 23 , 31 , 29 , 63 
.db  55 , 59 , 119 , 127 , 119 , 127 , 93 , 127 , 119 , 127 , 127 , 127 , 119 , 127 , 117 , 127 
.db  117 , 63 , 119 , 63 , 23 , 31 , 93 , 143 , 71 , 33 , 64 , 224 , 85 , 2 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 4 , 11 , 21 , 32 
.db  81 , 128 , 69 , 0 , 17 , 0 , 21 , 161 , 65 , 111 , 117 , 115 , 197 , 139 , 85 , 3 
.db  21 , 42 , 87 , 42 , 84 , 46 , 84 , 60 , 84 , 172 , 85 , 184 , 84 , 172 , 84 , 52 
.db  84 , 168 , 84 , 2 , 84 , 234 , 85 , 235 , 85 , 168 , 81 , 112 , 93 , 143 , 85 , 8 
.db  16 , 32 , 112 , 96 , 192 , 128 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 
.db  16 , 32 , 85 , 0 , 81 , 131 , 197 , 134 , 85 , 131 , 197 , 128 , 17 , 162 , 215 , 239 
.db  87 , 174 , 92 , 184 , 81 , 176 , 116 , 232 , 117 , 248 , 85 , 232 , 85 , 233 , 85 , 120 
.db  117 , 56 , 85 , 58 , 85 , 62 , 21 , 14 , 23 , 7 , 69 , 3 , 17 , 0 , 21 , 0 
.db  85 , 128 , 85 , 234 , 85 , 56 , 17 , 131 , 87 , 126 , 84 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 3 , 7 , 6 , 5 , 12 , 29 , 25 
.db  17 , 27 , 17 , 51 , 21 , 35 , 53 , 35 , 117 , 35 , 117 , 35 , 117 , 32 , 117 , 32 
.db  100 , 32 , 117 , 32 , 116 , 32 , 117 , 32 , 116 , 34 , 53 , 48 , 117 , 50 , 49 , 50 
.db  21 , 25 , 17 , 24 , 29 , 12 , 29 , 14 , 7 , 3 , 1 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 

hase:
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 128 , 64 , 160 , 112 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 252 , 2 , 1 , 0 , 0 , 127 , 128 
.db  0 , 0 , 0 , 0 , 0 , 128 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 128 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 128 , 64 , 32 , 167 , 24 , 0 , 0 
.db  0 , 1 , 6 , 24 , 12 , 2 , 1 , 128 , 64 , 32 , 16 , 8 , 8 , 8 , 8 , 8 
.db  9 , 9 , 18 , 20 , 40 , 48 , 32 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 192 
.db  32 , 16 , 8 , 4 , 2 , 1 , 1 , 1 , 1 , 1 , 2 , 2 , 4 , 8 , 48 , 192 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 6 , 25 , 32 , 64 , 129 , 1 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 48 , 46 , 65 , 128 , 128 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  126 , 129 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 3 , 252 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 248 , 6 , 1 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 64 , 128 , 0 , 0 , 0 , 0 , 1 , 1 , 1 
.db  2 , 4 , 8 , 48 , 192 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 3 , 12 , 16 , 32 , 64 , 128 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  128 , 64 , 48 , 8 , 7 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 192 
.db  99 , 38 , 44 , 16 , 0 , 192 , 96 , 32 , 32 , 32 , 49 , 27 , 14 , 4 , 0 , 0 
.db  0 , 128 , 64 , 64 , 32 , 16 , 9 , 18 , 18 , 12 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 1 
.db  1 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 1 , 2 , 2 , 2 , 2 , 3 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 
.db  2 , 2 , 2 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 254 , 254 , 18 , 18 , 18 
.db  18 , 0 , 248 , 248 , 8 , 8 , 0 , 112 , 136 , 136 , 136 , 112 , 0 , 254 , 254 , 8 
.db  8 , 248 , 224 , 0 , 0 , 248 , 168 , 168 , 184 , 0 , 0 , 0 , 0 , 0 , 0 , 124 
.db  198 , 130 , 130 , 130 , 198 , 124 , 0 , 0 , 184 , 168 , 168 , 232 , 8 , 60 , 252 , 136 
.db  0 , 0 , 248 , 168 , 168 , 184 , 0 , 248 , 248 , 8 , 8 , 248 , 240 , 8 , 8 , 248 
.db  224 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
pacman:
.db  0 , 0 , 0 , 0 , 0 , 56 , 56 , 56 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 56 , 56 , 56 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 56 , 56 , 56 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 56 , 56 
.db  56 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 56 , 56 , 56 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 56 , 56 , 56 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 112 , 112 , 112 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 196 , 196 , 196 , 4 , 4 , 4 , 4 
.db  4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 196 , 196 , 196 , 4 , 4 , 4 , 4 , 4 
.db  4 , 132 , 132 , 132 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 
.db  132 , 132 , 132 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 132 
.db  132 , 132 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 132 , 132 , 132 , 4 
.db  4 , 4 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 248 
.db  8 , 8 , 8 , 0 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 33 , 33 , 33 , 32 , 32 
.db  32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 33 , 33 , 33 , 32 , 32 , 224 
.db  0 , 0 , 0 , 3 , 3 , 3 , 0 , 0 , 0 , 224 , 32 , 32 , 32 , 32 , 32 , 32 
.db  32 , 32 , 35 , 35 , 35 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 
.db  32 , 35 , 35 , 35 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 35 , 35 
.db  35 , 32 , 32 , 32 , 32 , 32 , 224 , 0 , 0 , 0 , 0 , 7 , 7 , 7 , 0 , 0 
.db  0 , 255 , 32 , 32 , 32 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 255 , 0 , 0 , 0 , 192 , 192 , 192 , 0 , 0 , 0 , 255 , 0 , 0 , 0 , 128 
.db  224 , 240 , 240 , 248 , 216 , 248 , 112 , 48 , 32 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 128 , 128 , 128 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 128 , 128 , 128 , 0 , 0 , 0 , 0 , 255 , 0 , 0 , 0 , 0 , 112 , 112 , 112 
.db  0 , 0 , 0 , 255 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 255 , 0 , 0 , 0 , 1 , 1 , 1 , 0 , 0 , 0 , 255 , 0 , 0 
.db  0 , 7 , 31 , 31 , 63 , 63 , 62 , 62 , 28 , 24 , 8 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 3 , 3 , 3 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 3 , 3 , 3 , 0 , 0 , 0 , 0 , 255 , 0 , 0 , 0 , 0 , 128 
.db  128 , 128 , 0 , 0 , 0 , 255 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 32 
.db  32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 , 160 , 160 , 224 , 224 , 224 
.db  160 , 160 , 32 , 32 , 32 , 63 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 127 
.db  64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 
.db  64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 
.db  64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 64 , 127 , 0 , 0 , 0 
.db  0 , 3 , 3 , 3 , 0 , 0 , 0 , 63 , 32 , 32 , 32 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 60 , 255 , 255 , 255 , 255 
.db  246 , 247 , 227 , 193 , 65 , 0 , 0 , 0 , 0 , 0 , 0 , 28 , 28 , 28 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 28 , 28 , 28 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 28 , 28 , 28 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 28 , 28 , 28 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 28 , 28 , 28 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 
.db  3 , 3 , 3 , 3 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 126 , 0 , 0 , 0 
.db  0 , 0 , 0 , 0 , 0 , 0 , 0 , 126 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 
.db  2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 
.db  2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 
.db  126 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 126 , 2 , 2 , 2 , 0 
