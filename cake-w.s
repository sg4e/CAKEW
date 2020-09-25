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
STACK_POINTER_INIT = $1fff
OAMMIRROR   = $1800     ; leaving 1.5k for the stack
OAMSIZE     = 512 + 32
SPRITE_SIZE = 32
SPRITE_OAM_BYTE_SIZE = $4
UPPER_OAM   = OAMMIRROR + 512 ; OAM is 512 + 32
HORIZONTAL_SPEED = $01
VERTICAL_SPEED = $01
SNES_HEIGHT = 224
SNES_WIDTH = 256
MAX_SPRITE_RIGHT = SNES_WIDTH - SPRITE_SIZE
MAX_SPRITE_DOWN = SNES_HEIGHT - SPRITE_SIZE
MAX_FRIENDLY_PROJECTILES = 1 ; bugs with multiple projectiles to be fixed
OFFSCREEN_Y = (256 - 32)
FRIENDLY_PROJECTILE_SPEED = 2

Joy1Raw     = $17FA     ; Holder of RAW joypad data from register (from last frame)
Joy1RawHigh = $17FB

Joy1Press   = $17FC     ; Contains only pressed buttons (not held down)
Joy1PressHigh   = $17FD

;Joy1Held    = $17FE     ; Contains only buttons that are Held
;Joy1HeldHigh    = $17FF

; sprites
BULLET_SPRITE_VRAM_INDEX = 64
BULLET_PALETTE = %00000010
SPRITE_KEKW_OFFSET_IN_OAM = $00
SPRITE_KEKW = OAMMIRROR + SPRITE_KEKW_OFFSET_IN_OAM
FRIENDLY_PROJECTILES = OAMMIRROR + (4 * SPRITE_OAM_BYTE_SIZE)

; memory
NumberOfFriendlyProjectiles = $00 ; byte

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

.struct Sprite
        xx    .byte       ; limitation: ca65 requires field name to be longer than 1 character
        yy    .byte
        tile  .byte
        attr  .byte
.endstruct

;-------------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
;-------------------------------------------------------------------------------

;----- Includes ----------------------------------------------------------------
.segment "SPRITEDATA"
SpriteData: .incbin "KEKW-14.bin"
ColorData:  .incbin "KEKW-14.pal"
ShootSpriteData: .incbin "original sprites/shoot.bin"
ShootColorData: .incbin "original sprites/shoot.pal"
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
        lda #%01100000          ; 16x16, large 32x32
        sta OBJSEL

        stz $4016 ; "otherwise the shift register gets stuck on the first bit, ie. all 16bit will be equal to the B-button state"

        Set16
        ; initialize the stack pointer; for some reason it's auto-initialize too low at $01ff in bsnes
        lda #STACK_POINTER_INIT
        tcs
        Set8
        ; init all variables
        stz NumberOfFriendlyProjectiles

        ; transfer CGRAM data
        lda #$80
        sta CGADD               ; set CGRAM address to $80
        Set16
        lda #ColorData
        jsr LoadPalette
        .a8
        .i8
        lda #$90
        sta CGADD
        Set16
        lda #ShootColorData
        jsr LoadPalette
        .a8
        .i8

        ; set up OAM data
        ; init OAM as all $00 so no surprises
        SetIndex16
        ldx #$00
ClearOAM:
        stz OAMMIRROR, x
        inx
        cpx #OAMSIZE
        blt ClearOAM
        ; OAM data for first sprite
        lda # (256/2 - 8)       ; horizontal position of first sprite
        sta SPRITE_KEKW + 0
        lda # (224/2 - 8)       ; vertical position of first sprite
        sta SPRITE_KEKW + 1
        lda #$00                ; name of first sprite
        sta SPRITE_KEKW + 2
        lda #$00                ; no flip, prio 0, palette 0
        sta SPRITE_KEKW + 3
        lda #$02                ; sprite is large
        sta UPPER_OAM           ; zero offset
        ; move all other sprite slots off screen
        ldx #(SPRITE_OAM_BYTE_SIZE + 1) ; y offset
        lda #OFFSCREEN_Y
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
        lda #$02                ; enable the DMA channel 2 transfer
        sta MDMAEN

        ; load sprites into VRAM
        Set16
        lda #SpriteData
        pha
        lda #$0000
        pha
        Set8
        lda #$01
        pha
        jsr WriteSpriteToVRAM
        lda STACK_POINTER_INIT
        pla
        pla
        pla
        pla
        pla
        ; bullet sprite
        Set16
        lda #ShootSpriteData
        pha
        lda #$0800/2
        pha
        Set8
        lda #$00
        pha
        jsr WriteSpriteToVRAM
        pla
        pla
        pla
        pla
        pla

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

; assumes 16-bit, returns 8-bit SO TELL THE STUPID ASSEMBLER WITH .a8 .i8
; you need to set CGADD yourself
; acc: the address that contains the palette data
; cleanup: this subroutine will do it
.proc LoadPalette
        phd                     ; overwrite D with the palette-offset address
        tcd
        Set8
        ldx #$00
Loop:
        lda $00, X              ; get the color low byte
        sta CGDATA              ; store it in CGRAM
        inx                     ; increase counter/offset
        lda $00, X              ; get the color high byte
        sta CGDATA              ; store it in CGRAM
        inx                     ; increase counter/offset
        cpx #$20                ; check whether 32/$20 bytes were transfered
        bcc Loop                ; if not, continue loop
        pld                     ; recover D
        rts
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
        lda #OAMSIZE
        sta DAS1L
        Set8
        rts
.endproc

; assumes 8-bit, returns as 8-bit
; you need to clean up; clean up is 5 bytes
; stack args:
; S, 1 and S, 2: D (pushed inside this subroutine)
; S, 3 and S, 4: rts return address
; push 1st: sprite offset in rom  (S, 8)
; push 2nd: offset into VRAM      (S, 6)
; push 3rd: sprite size: 0 == 16px, 1 == 32px, single byte (S, 5)
; uses DMA channel 0
.proc WriteSpriteToVRAM
        SetDirect
        BigBoolean = $05
        VRAMOffset = $06
        SpriteOffset = $08
        SetIndex8
        ldy BigBoolean          ; stores boolean
        lda #%00000001          ; set DMA channel 0: VRAM type
        sta DMAP0
        lda #$18                ; set destination to VRAM
        sta BBAD0
        ldx #$02
        cpy #$01
        blt TwoRows
        ldx #$00                ; count 4 cycles for 32px, 2 for 16px
TwoRows:
        stz A1T0B               ; set bank to zero
WriteLoop:
        SetAcc16
        lda VRAMOffset
        sta VMADDL
        clc
        adc #512/2  ; VMADDL is WORD-based, not byte-based
        sta VRAMOffset
        lda SpriteOffset
        sta A1T0L               ; set low and high byte of address
        ; calc next offsets
        clc
        adc #64
        cpy #$01
        blt StoreSpriteOffset
        clc
        adc #64
StoreSpriteOffset:
        sta SpriteOffset
        lda #64             ; set the number of bytes to transfer
        cpy #$01
        blt SetData         ; it's a big 32px sprite
        lda #128
SetData:
        sta DAS0L               ; this gets reset after each transfer #justSNESthings
        SetAcc8
        lda #$01
        sta MDMAEN            ; start DMA transfer
        inx
        cpx #$4
        bcc WriteLoop
        pld
        rts
.endproc

;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
; .smart ; keep track of registers widths
.proc   GameLoop
        wai                     ; wait for NMI / V-Blank

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
        beq AInput
        lda SPRITE_KEKW + 1
        cmp #MAX_SPRITE_DOWN
        bge AInput
        clc
        adc #(VERTICAL_SPEED)
        sta SPRITE_KEKW + 1
AInput:
        lda Joy1Press
        and #Button_A
        beq NoMoreInput
        ; shoot
        lda NumberOfFriendlyProjectiles
        cmp #MAX_FRIENDLY_PROJECTILES
        bge NoMoreInput
        inc
        sta NumberOfFriendlyProjectiles
NoMoreInput:
        jsr MoveProjectiles
        jmp GameLoop
.endproc

.proc MoveProjectiles
        ldy #$00        ; use y as a loop counter
        ldx #Sprite::xx
        lda NumberOfFriendlyProjectiles
        pha
Loop:
        cpy NumberOfFriendlyProjectiles
        bge Cleanup
        lda FRIENDLY_PROJECTILES + Sprite::yy, x
        ; check if new projectile
        cmp #OFFSCREEN_Y
        bge CreateNewProj
        ; move offscreen and kill projectile at 0 y
        ; very important that these never go negative because offscreen == new projectile
        cmp #FRIENDLY_PROJECTILE_SPEED
        bge IsAlive
        ; is dead
        lda #OFFSCREEN_Y
        sta FRIENDLY_PROJECTILES + Sprite::yy, x
        lda $01, S
        clc
        sbc #$00
        sta $01, S
        ; this is bugged as soon as a proj dies; need better memory management -- or is it??!?!
        bra GoToNextSprite
CreateNewProj:
        lda SPRITE_KEKW + Sprite::xx
        clc
        adc #8 ; center x
        sta FRIENDLY_PROJECTILES + Sprite::xx, x
        lda SPRITE_KEKW + Sprite::yy
        clc
        sbc #16 ; move y out of sprite
        sta FRIENDLY_PROJECTILES + Sprite::yy, x
        ; set sprite to bullet
        lda #BULLET_SPRITE_VRAM_INDEX
        sta FRIENDLY_PROJECTILES + Sprite::tile, x
        lda #BULLET_PALETTE
        sta FRIENDLY_PROJECTILES + Sprite::attr, x
        bra GoToNextSprite
IsAlive:
        ; a still holds proj y
        clc
        sbc #(FRIENDLY_PROJECTILE_SPEED - 1)
        sta FRIENDLY_PROJECTILES + Sprite::yy, x
GoToNextSprite:
        .repeat .sizeof(Sprite)
        inx
        .endrepeat
        iny
        bra Loop
Cleanup:
        pla
        sta NumberOfFriendlyProjectiles
        rts
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Will be called during V-Blank
;-------------------------------------------------------------------------------
.proc   VBlank
        lda RDNMI               ; read NMI status, acknowledge NMI
        ; the CPU auto-pushes some registers on interrupt, but not a,x,y, so we have to do that
        ; who knows if the registers are in 8-bit or 16-bit; we'll have to maintain them ourselves
        pha
        phx
        phy
        php

        Set8
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
        txa                 ; transfer last frame input from X -> A (it's still in X)
        eor Joy1Raw         ; Xor last frame input with current frame input
                            ; shows the changes in input
                            ; buttons just pressed or just released become set.
                            ; Held or unactive buttons are 0
        and Joy1Raw         ; AND changes to current frame's input.
                            ; this ends up leaving you with the only the buttons that
                            ; are pressed.. It's MAGIC!
        sta Joy1Press       ; Store just pressed buttons
        ;txa                 ; Transfer last frame input from X -> A again
        ;and Joy1Raw	        ; Find buttons that are still pressed (held)
        ;sta Joy1Held        ; by storing only buttons that are pressed both frames

        plp
        ply
        plx
        pla
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
