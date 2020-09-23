; Copyright (C) 2020 sg4e
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, version 3.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;----- Aliases/Labels ----------------------------------------------------------
; these are aliases for the Memory Mapped Registers we will use
INIDISP     = $2100     ; inital settings for screen
OBJSEL      = $2101     ; object size $ object data area designation
OAMADDL     = $2102     ; address for accessing OAM
OAMADDH     = $2103
OAMDATA     = $2104     ; data for OAM write
VMAINC      = $2115     ; VRAM address increment value designation
VMADDL      = $2116     ; address for VRAM read and write
VMADDH      = $2117
VMDATAL     = $2118     ; data for VRAM write
VMDATAH     = $2119     ; data for VRAM write
CGADD       = $2121     ; address for CGRAM read and write
CGDATA      = $2122     ; data for CGRAM write
TM          = $212c     ; main screen designation
NMITIMEN    = $4200     ; enable flag for v-blank
RDNMI       = $4210     ; read the NMI flag status
DMAP0       = $4300     ; DMA control register, channel 0
BBAD0       = $4301     ; DMA destination register, channel 0
A1T0L       = $4302     ; DMA source address register low, channel 0
A1T0H       = $4303     ; DMA source address register high, channel 0
A1T0B       = $4304     ; DMA source address register bank, channel 0
DAS0L       = $4305     ; DMA size register low, channel 0
DAS0H       = $4306     ; DMA size register high, channel 0
MDMAEN      = $420b     ; DMA enable register

DMAP1       = $4310     ; DMA control register, channel 1
BBAD1       = $4311     ; DMA destination register, channel 1
A1T1L       = $4312     ; DMA source address register low, channel 1
A1T1H       = $4313     ; DMA source address register high, channel 1
A1T1B       = $4314     ; DMA source address register bank, channel 1
DAS1L       = $4315     ; DMA size register low, channel 1

; my labels
OAMMIRROR   = $1800     ; leaving 1.5k for the stack
SPRITE_SIZE = 32
SPRITE_OAM_BYTE_SIZE = $4
HORIZONTAL_SPEED = $01
VERTICAL_SPEED = $01
SNES_HEIGHT = 224
SNES_WIDTH = 256
MAX_SPRITE_RIGHT = SNES_WIDTH - SPRITE_SIZE
MAX_SPRITE_DOWN = SNES_HEIGHT - SPRITE_SIZE

Joy1Raw     = $17FA     ; Holder of RAW joypad data from register (from last frame)
Joy1RawHigh = $17FB

;Joy1Press   = $17FC     ; Contains only pressed buttons (not held down)
;Joy1PressHigh   = $17FD

;Joy1Held    = $17FE     ; Contains only buttons that are Held
;Joy1HeldHigh    = $17FF

; sprites
SPRITE_KEKW = OAMMIRROR + $00

; input map
.DEFINE Button_A		$80
.DEFINE Button_X		$40
.DEFINE Button_L		$20
.DEFINE Button_R		$10
.DEFINE Button_B		$80
.DEFINE Button_Y		$40
.DEFINE Button_Select	$20
.DEFINE Button_Start	$10
.DEFINE Button_Up		$08
.DEFINE Button_Down	$04
.DEFINE Button_Left	$02
.DEFINE Button_Right	$01

;-------------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
;-------------------------------------------------------------------------------

;----- Includes ----------------------------------------------------------------
.segment "SPRITEDATA"
SpriteData: .incbin "KEKW-14.bin"
ColorData:  .incbin "KEKW-14.pal"
;-------------------------------------------------------------------------------

.segment "CODE"
.include "macros.inc"
;-------------------------------------------------------------------------------
;   This is the entry point of the demo
;-------------------------------------------------------------------------------
.proc   ResetHandler
        sei                     ; disable interrupts
        clc                     ; clear the carry flag
        xce                     ; switch the 65816 to native (16-bit mode)
        lda #$8f                ; force v-blanking
        sta INIDISP
        stz NMITIMEN            ; disable NMI

        ; set my sprite size
        lda #%10100000
        sta OBJSEL

        ; transfer VRAM data
        stz VMADDL              ; set the VRAM address to $0000
        stz VMADDH
        lda #$80
        ;sta VMAINC              ; increment VRAM address by 1 when writing to VMDATAH
        ldx #$00                ; set register X to zero, we will use X as a loop counter and offset
        ;rep #$10
        lda #%00000001          ; set DMA channel 0
        sta DMAP0
        lda #$18                ; set destination to VRAM
        sta BBAD0
        ;rep #$10
        ;.i16
        ldx #$00
        ldy #$00
        stz $00
        stz $01
        stz $02
        stz $03
        stz $4016 ; "otherwise the shift register gets stuck on the first bit, ie. all 16bit will be equal to the B-button state"
WriteVRAM:
        ;$00 is the sprite offset; $02 is the VRAM offset
        .a16
        rep #$20

        ;.byte $42, $00
        lda $02
        sta VMADDL
        lda #(SpriteData) ;#$81          ; get address of OAMRAM mirror
        clc
        adc $00
        ;.byte $42, $00
        sta A1T0L               ; set low and high byte of address
        ; calc next offsets
        lda $00
        clc
        adc #512/4
        sta $00
        clc
        lda $02
        adc #512/2  ; VMADDL is WORD-based, not byte-based
        sta $02
        ;sta A1T0H
        sep #$20
        .a8
        stz A1T0B               ; set bank to zero, since the mirror is in WRAM
        lda #$200/4            ; set the number of bytes to transfer
        sta DAS0L
        stz DAS0H

        lda #$01                ; start DMA transfer
        sta MDMAEN
        inx
        cpx #$4
        bcc WriteVRAM
        ;sep #$10
        ;.i8

        ; transfer CGRAM data
        lda #$80
        sta CGADD               ; set CGRAM address to $80
        ldx #$00                ; set X to zero, use it as loop counter and offset
CGRAMLoop:
        lda ColorData, X        ; get the color low byte
        sta CGDATA              ; store it in CGRAM
        inx                     ; increase counter/offset
        lda ColorData, X        ; get the color high byte
        sta CGDATA              ; store it in CGRAM
        inx                     ; increase counter/offset
        cpx #$20                ; check whether 32/$20 bytes were transfered
        bcc CGRAMLoop           ; if not, continue loop

        ;.byte $42, $00          ; debugger breakpoint

        ; set up OAM data
        ; OAM data for first sprite
        lda # (256/2 - 8)       ; horizontal position of first sprite
        sta SPRITE_KEKW + 0
        lda # (224/2 - 8)       ; vertical position of first sprite
        sta SPRITE_KEKW + 1
        lda #$00                ; name of first sprite
        sta SPRITE_KEKW + 2
        lda #$00                ; no flip, prio 0, palette 0
        sta SPRITE_KEKW + 3
        ; move all other sprite slots off screen
        SetIndex16
        ldx #(SPRITE_OAM_BYTE_SIZE + 1) ; y offset
        lda #(256 - 32)
InitOAM:
        sta OAMMIRROR, x
        inx
        inx
        inx
        inx
        cpx #512
        blt InitOAM
        SetIndex8

        jsr DMAOAM
        ; override DMA transfer amount for full transfer
        Set16
        lda #512
        sta DAS1L
        Set8
        lda #$02                ; enable the DMA channel 2 transfer
        sta MDMAEN

        ; make Objects visible
        lda #$10
        sta TM
        ; release forced blanking, set screen to full brightness
        lda #$0f
        sta INIDISP
        ; enable NMI, turn on automatic joypad polling
        lda #$81
        sta NMITIMEN

        jmp GameLoop            ; all initialization is done
.endproc

; writes the OAM copy in RAM to real OAM by DMA
.proc DMAOAM
        lda #$02
        sta DMAP1
        lda #$04
        sta BBAD1
        stz A1T1B
        Set16
        ; set start address of OAM to its beginning
        stz OAMADDL
        lda #(OAMMIRROR)
        sta A1T1L
        lda #(SPRITE_OAM_BYTE_SIZE)
        sta DAS1L
        Set8
        rts
.endproc

;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
; .smart ; keep track of registers widths
.proc   GameLoop
        wai                     ; wait for NMI / V-Blank

        ; here we would place all of the game logic
        ; and loop forever
ProcessInput:
        lda Joy1RawHigh
        and #(Button_Right)
        beq LeftInput
        lda SPRITE_KEKW
        ; check bounds
        cmp #MAX_SPRITE_RIGHT
        bge LeftInput
        ; move
        clc
        adc #(HORIZONTAL_SPEED)
        sta SPRITE_KEKW
        ; flip sprite for movement
        lda SPRITE_KEKW + 3
        ora #%01000000
        sta SPRITE_KEKW + 3
LeftInput:
        lda Joy1RawHigh
        and #(Button_Left)
        beq UpInput
        lda SPRITE_KEKW
        cmp #$01
        blt UpInput
        clc
        sbc #(HORIZONTAL_SPEED - 1)
        sta SPRITE_KEKW
        ; unflip sprite
        lda SPRITE_KEKW + 3
        and #%10111111
        sta SPRITE_KEKW + 3
UpInput:
        lda Joy1RawHigh
        and #(Button_Up)
        beq DownInput
        lda SPRITE_KEKW + 1
        cmp #$01
        blt DownInput
        clc
        sbc #(VERTICAL_SPEED - 1)
        sta SPRITE_KEKW + 1
DownInput:
        lda Joy1RawHigh
        and #(Button_Down)
        beq NoMoreInput
        lda SPRITE_KEKW + 1
        cmp #MAX_SPRITE_DOWN
        bge NoMoreInput
        clc
        adc #(VERTICAL_SPEED)
        sta SPRITE_KEKW + 1
NoMoreInput:
        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Will be called during V-Blank
;-------------------------------------------------------------------------------
.proc   VBlank
        lda RDNMI               ; read NMI status, acknowledge NMI
        jsr DMAOAM
        lda #$02                ; enable the DMA channel 2 transfer
        sta MDMAEN
Joypad:
        lda $4212           ; auto-read joypad status
        and #$01            ;
        bne Joypad          ; read is done when 0

        Set16               ; A/X/Y - 16 bit

        ; Player 1
        ldx Joy1Raw         ; load log of last frame's RAW read of $4218
                            ; the log will be 0 the first time read of course..
        lda $4218           ; Read current frame's RAW joypad data
        sta Joy1Raw         ; save it for next frame.. (last frame log is still in X)
        ;txa                 ; transfer last frame input from X -> A (it's still in X)
        ;eor Joy1Raw         ; Xor last frame input with current frame input
                            ; shows the changes in input
                            ; buttons just pressed or just released become set.
                            ; Held or unactive buttons are 0
        ;and Joy1Raw         ; AND changes to current frame's input.
                            ; this ends up leaving you with the only the buttons that
                            ; are pressed.. It's MAGIC!
        ;sta Joy1Press       ; Store just pressed buttons
        ;txa                 ; Transfer last frame input from X -> A again
        ;and Joy1Raw	        ; Find buttons that are still pressed (held)
        ;sta Joy1Held        ; by storing only buttons that are pressed both frames
        Set8
        rti
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Is not used in this program
;-------------------------------------------------------------------------------
.proc   IRQHandler
        ; code
        rti
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Interrupt and Reset vectors for the 65816 CPU
;-------------------------------------------------------------------------------
.segment "VECTOR"
; native mode   COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           VBlank,     $0000,      $0000

.word           $0000, $0000    ; four unused bytes

; emulation m.  COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           $0000,      ResetHandler, $0000
