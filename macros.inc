.macro Set16
      .a16
      .i16
      rep #$30
.endmacro

.macro Set8
      .a8
      .i8
      sep #$30
.endmacro

.macro SetIndex16
      .i16
      rep #$10
.endmacro

.macro SetIndex8
      .i8
      sep #$10
.endmacro

.macro bge arg
      bcs arg
.endmacro

.macro blt arg
      bcc arg
.endmacro

.macro debug
      .byte $42, $00
.endmacro
