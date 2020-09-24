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

.macro SetAcc16
      .a16
      rep #$20
.endmacro

.macro SetAcc8
      .a8
      sep #$20
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

.macro RunDMA arg
      lda arg
      sta MDMAEN
.endmacro

.macro SetDirect
      phd
      tsc
      tcd
.endmacro

.macro CleanDirect
      pld
.endmacro
