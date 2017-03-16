; Conway II
; Lee W. Fastenau
; thelbane@gmail.com
; Created 03/14/2017


;     .....012                                          * **.
;          3 4                           * ** **************.
;          567                  ****************************.
;                            * ***************************012
;                             * **************************3 4
;                            .............................567
;
;       1. Setup data indirect pointer per row, use same Y offset that's used to read/set screen char.
;       2. Data pointer points to first screen char (base row address + 1)
;       3. Backing pointer continues to update on every cell
;       4. 

                processor 6502
                incdir "include"
                include "apple2.asm"
                include "macros.asm"

; ------------------------------------
; Build Options
; ------------------------------------

NOISY           equ 1
CHARSET         equ 4                           ; 0 = Olde Skoole, 1 = Pixel, 2 = Inverse, 3 = Small O's, 4 = Experiment
TEST_PERF       equ 1

; ------------------------------------
; Constants
; ------------------------------------

textRow         equ ZPA0
textRowH        equ ZPA1

mainData        equ ZPC0
mainDataH       equ ZPC1

altData         equ ZPC2
altDataH        equ ZPC3

currentPage     equ ZPA2
temp            equ ZPA3

fieldWidth      equ 40
fieldHeight     equ 24

dataWidth       equ fieldWidth+2
dataHeight      equ fieldHeight+2

normalText      equ %10000000                   ; 'X | normalText
inverseText     equ %00111111                   ; 'X & inverseText

                if CHARSET == 0
charOn          equ '* | normalText
charOff         equ '. | normalText
                endif

                if CHARSET == 1
charOn          equ '  & inverseText
charOff         equ '  | normalText
                endif

                if CHARSET == 2
charOn          equ '  | normalText
charOff         equ ': & inverseText
                endif

                if CHARSET == 3
charOn          equ 'o | normalText
charOff         equ '  | normalText
                endif

                if CHARSET == 4
charOn          equ '* & inverseText
charOff         equ '  | normalText
                endif

topleft         equ %10000000                   ; Neighbor bit flags
top             equ %01000000                   ; All combinations are decoded in conwayRules table
topright        equ %00100000                   ; This is faster than counting neighbors each pass
left            equ %00010000
right           equ %00001000
bottomleft      equ %00000100
bottom          equ %00000010
bottomright     equ %00000001

n_topleft       equ bottomright                 ; Inverse neighbor relationships
n_top           equ bottom
n_topright      equ bottomleft
n_left          equ right
n_right         equ left
n_bottomleft    equ topright
n_bottom        equ top
n_bottomright   equ topleft

n_offset        equ dataWidth+1                 ; Alt data topleft offset from current cell

y_topleft       equ 0                           ; Alt data pointer offsets
y_top           equ 1
y_topright      equ 2
y_left          equ dataWidth
y_right         equ dataWidth+2
y_bottomleft    equ dataWidth*2
y_bottom        equ dataWidth*2+1
y_bottomright   equ dataWidth*2+2

; ------------------------------------
; Entry Point
; ------------------------------------
                seg program
                org $C00

start           subroutine
                lda #0
                sta currentPage                 ; Point main data segment to first block
                jsr OUTPORT                     ; PR#0 (Set output to 40-column text screen)
                jsr makeRules                   ; Create Conway rules table
                jsr initScreen                  ; Render initial cell layout
                jsr updateData                  ; Initialize backing data based on displayed cells
                if TEST_PERF
                jsr perfTest
                else
                jsr runLoop
                endif
                jmp EXITDOS

runLoop         subroutine
.loop           jsr iterate                     ; Modify and display next generation
                jmp .loop                       ; Until cows come home

perfTest        subroutine
                jsr RDKEY
.startTimer     
                lda #50
                sta .counter
.loop           jsr iterate
                dec .counter
                bne .loop
.endTimer
.break          jsr RDKEY
                echo "Breakpoint:", .break
                rts
.counter        ds.b 1
                echo "START TIMER BREAKPOINT:",.startTimer
                echo "END TIMER BREAKPOINT:",.endTimer

iterate         subroutine
                mac TURN_ON
                ldy #y_{1}
                lda (altData),y
                ora #n_{1}
                sta (altData),y
                endm
                lda #1
                eor currentPage
                sta currentPage
                jsr initUpdPtrs
                lda #fieldHeight-1
                sta .row
.rowLoop        jsr getRow
                lda #fieldWidth-1
                sta .column
.columnLoop     ldy .column                     ; get neighbor bit flags
                lda (mainData),y                ; at current data address
                tay
                lda conwayRules,y               ; convert bit flags to cell state character (or 0 for do nothing)
                beq .doNothing                  ; rule says do nothing, so update the neighbor data
                ldy #0 ; .column
.column         equ .-1
                sta (textRow),y                 ; set char based on rule
                bne .updateData
.doNothing      ldy .column
                lda (textRow),y
.updateData     cmp #charOn                     ; A = cell character
                bne .clearBit                   ; cell is disabled, so clear the topleft neighbor
                if NOISY
                bit CLICK
                endif
                ldy #y_topleft                  ; cell is enabled, so do the neighborly thing...
                lda #n_topleft
                sta (altData),y
                TURN_ON topleft
                if NOISY
                bit CLICK
                endif
                TURN_ON top
                TURN_ON topright
                TURN_ON left
                TURN_ON right
                TURN_ON bottomleft
                TURN_ON bottom
                TURN_ON bottomright
                jmp .continue
.clearBit       ldy #y_topleft
                lda #0
                sta (altData),y
.continue       ldy .column
                iny
                lda #0
                sta (mainData),y
                sec
                lda altData
                sbc #1
                sta altData
                lda altDataH
                sbc #0
                sta altDataH
.nextColumn     dec .column
                bmi .nextRow
                jmp .columnLoop
.nextRow        sec
                lda mainData
                sbc #dataWidth
                sta mainData
                lda mainDataH
                sbc #0
                sta mainDataH
                sec
                lda altData
                sbc #2
                sta altData
                lda altDataH
                sbc #0
                sta altDataH
                dec .row
                lda #0 ; .row
.row            equ .-1
                bmi .end
                jmp .rowLoop
.end            rts

updateData      subroutine
                jsr initUpdPtrs
                lda #fieldHeight-1
                sta .row
.rowLoop        jsr getRow
                lda #fieldWidth-1
                sta .column
.columnLoop     ldy #0 ; .column
.column         equ .-1
                lda (textRow),y
                cmp #charOff
                beq .nextColumn
                TURN_ON topleft
                TURN_ON top
                TURN_ON topright
                TURN_ON left
                TURN_ON right
                TURN_ON bottomleft
                TURN_ON bottom
                TURN_ON bottomright
.nextColumn     sec
                lda altData
                sbc #1
                sta altData
                lda altDataH
                sbc #0
                sta altDataH
                dec .column
                bpl .columnLoop
.nextRow        sec
                lda altData
                sbc #2
                sta altData
                lda altDataH
                sbc #0
                sta altDataH
                dec .row
                lda #0 ; .row
.row            equ .-1
                bpl .rowLoop
                rts

initUpdPtrs     subroutine                      ; Initializes the relevant data pointers
                lda currentPage
                bne .page1
.page0          lda <#datapg0_lastRow
                sta mainData
                lda >#datapg0_lastRow
                sta mainDataH
                lda <#datapg1_tln
                sta altData
                lda >#datapg1_tln
                sta altDataH
                jmp .continue
.page1          lda <#datapg1_lastRow
                sta mainData
                lda >#datapg1_lastRow
                sta mainDataH
                lda <#datapg0_tln
                sta altData
                lda >#datapg0_tln
                sta altDataH
.continue       rts

initScreen      subroutine
                lda <#initData
                sta mainData
                lda >#initData
                sta mainDataH
                lda #initDataLen-1              ; get data length
                sta .dataoffset                 ; save it
                lda #fieldHeight-1              ; load the field height
                sta .row                        ; save in row counter
.1              jsr getRow                      ; update textRow (A = row)
                lda #fieldWidth-1               ; load the field width (reset every new row)
                sta .column                     ; save in column counter
                ldy .dataoffset
                lda (mainData),y                ; get the current data byte
                sta .byte                       ; save it
                lda #8                          ; init the byte counter
                sta .bit                        ; save in bit counter
.2              ldy .column
                lda #0
.byte           equ .-1
                lsr
                sta .byte
                bcs .turnOn
.turnOff        lda #charOff
                bne .draw
.turnOn         lda #charOn
.draw           sta (textRow),y
                dec .bit
                bne .skipbit
                lda #8                          ; reset bit counter
                sta .bit                        ; decrease data byte reference
                sec
                dec .dataoffset
                ldy #0 ; .dataoffset
.dataoffset     equ .-1
                lda (mainData),y
                sta .byte
.skipbit        lda .column                     ; start to calculate init byte offset
                dec .column
                ldy #0 ; .column
.column         equ .-1
                bpl .2
                dec .row
                lda #0 ; .row
.row            equ .-1
                bpl .1
                rts
.bit            ds.b 1

; inputs:
; ?
; outputs:
; ?
setPoint        subroutine
                jsr getRow
                lda #charOn
                sta (textRow),y
                rts

; inputs:
; A = screen character code
; outputs:
; A = $FF, X = ?, Y = $FF
fillScreen      subroutine
                sta .char
                lda #fieldHeight-1
                sta .row
.1              jsr getRow
                ldy #fieldWidth-1
.2              lda #0 ; .char
.char           equ .-1
                sta (textRow),y
                dey
                bpl .2
                dec .row
                lda #0 ; .row
.row            equ .-1
                bpl .1
                rts

; inputs:
; A = row
; outputs:
; A = ?, X = A << 1
getRow          subroutine
                asl
                tax
                lda textRowsTable,x
                sta textRow
                lda textRowsTable+1,x
                sta textRowH
                rts

makeRules       subroutine                      ; Generate conway rules table
                lda #$ff
                sta .neighbors
.loop           jsr getRule
                ldx #0
.neighbors      equ .-1
                sta conwayRules,x
                dec .neighbors
                lda .neighbors
                bne .loop
                lda #charOff
                sta conwayRules
                rts

getRule         subroutine                      ; Returns #charOn, #charOff, or 0 (if no change)
                jsr countBits                   ; Translate bit pattern to number of neighbors
                cmp #2
                bcc .off                        ; Fewer than 2 neighbors, dies of loneliness
                cmp #3
                beq .on                         ; Exactly 3 neighbors, reproduces
                bcs .off                        ; More than 3 neighbors, dies of overpopulation
                lda #0                          ; Else (exactly 2 neighbors), no change
                rts
.off            lda #charOff
                rts
.on             lda #charOn
                rts

countBits       subroutine                      ; Compute Hamming Weight (number of enabled bits) in A
.f1             equ %01010101                   ; see: https://en.wikipedia.org/wiki/Hamming_weight
.f2             equ %00110011                   ; (Takes approx. 1/2 the time compared to lsr/adc loop.)
.f3             equ %00001111
                tax
                and #.f1
                sta .store1
                txa
                lsr
                and #.f1
                clc
                adc #0                          ; .store1
.store1         equ .-1
                tax
                and #.f2
                sta .store2
                txa
                lsr
                lsr
                and #.f2
                clc
                adc #0                          ; .store2
.store2         equ .-1
                tax
                and #.f3
                sta .store3
                txa
                lsr
                lsr
                lsr
                lsr
                and #.f3
                clc
                adc #0                          ; .store3
.store3         equ .-1
                rts

; ------------------------------------
; Utilities
; ------------------------------------

                include "utilities.asm"

; ------------------------------------
; Tables
; ------------------------------------
textRowsTable   subroutine
.pg             equ 1024
.y              set TXTPG0
                repeat 24
                dc.w .pg + (.y & %11111000) * 5 + ((.y & %00000111) << 7)
.y              set .y + 1
                repend
                LOG_REGION "textRowsTable", textRowsTable, 0

initData        dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%01000000,%00000000
                dc.b %00000000,%00000000,%00000001,%01000000,%00000000
                dc.b %00000000,%00000110,%00000110,%00000000,%00011000
                dc.b %00000000,%00001000,%10000110,%00000000,%00011000
                dc.b %01100000,%00010000,%01000110,%00000000,%00000000
                dc.b %01100000,%00010001,%01100001,%01000000,%00000000
                dc.b %00000000,%00010000,%01000000,%01000000,%00000000
                dc.b %00000000,%00001000,%10000000,%00000000,%00000000
                dc.b %00000000,%00000110,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000

initDataLen     equ .-initData

dataSeg         equ .
                seg.u conwayData                ; uninitialized data segment
                org dataSeg

                align 256
conwayRules     ds.b 256                        ; Reserved for rules table

datapg0         ds.b dataWidth * dataHeight     ; The start of data page 0 (padded)
datapg0_lastRow equ . - dataWidth - fieldWidth  ; The last non-padding cell of data page 0 (topleft neighbor of last cell)
datapg0_tln     equ . - [n_offset * 2]          ; Top left neighbor of last non-padding cell of page 0

datapg1         ds.b dataWidth * dataHeight     ; The start of data page 1 (padded)
datapg1_lastRow equ . - dataWidth - fieldWidth  ; The last non-padding cell of data page 1 (topleft neighbor of last cell)
datapg1_tln     equ . - [n_offset * 2]          ; Top left neighbor of last non-padding cell of page 1

                echo "conwayRules:", conwayRules