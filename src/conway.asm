; Conway II
; Lee W. Fastenau
; thelbane@gmail.com
; Created 03/14/2017

                processor 6502
                incdir "include"
                include "apple2.asm"
                include "macros.asm"

; ------------------------------------
; Build Options
; ------------------------------------

NOISY           equ 1                           ; 0 = Sound off, 1 = Sound on
CHARSET         equ 4                           ; 0 = Olde Skoole, 1 = Pixel, 2 = Inverse, 3 = Small O's, 4 = Enhanced
TEST_PERF       equ 0
INIT_PATTERN    equ 2                           ; 0 = Glider gun, 1 = "Random", 2 = Edge test

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
charOn          equ $ff ; | normalText
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

topRow          equ $FF ^ [ topleft | top | topright ]
bottomRow       equ $FF ^ [ bottomleft | bottom | bottomright ]
leftColumn      equ $FF ^ [ topleft | left | bottomleft ]
rightColumn     equ $FF ^ [ topright | right | bottomright ]

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

                mac TURN_ON
                ldy #y_{1}
                lda (altData),y
                ora #n_{1}
                sta (altData),y
                endm

iterate         subroutine
                jsr toggleDataPages
                jsr clearBorders
                lda #fieldHeight-1
                sta .row
.rowLoop        jsr getTextRow
                lda #fieldWidth-1
                sta .column
                lda #0
                ldy #y_top                      ; clean up stale data
                sta (altData),y
                ldy #y_topright
                sta (altData),y
.columnLoop     ldy .column                     ; get neighbor bit flags
                lda (mainData),y                ; at current data address
                tay
                lda conwayRules,y               ; convert bit flags to cell state character (or 0 for do nothing)
                beq .doNothing                  ; rule says do nothing, so update the neighbor data
                ldy #0 ; .column
.column         equ .-1
                sta (textRow),y                 ; set char based on rule
                bne .setBits
.doNothing      ldy .column
                lda (textRow),y
.setBits        cmp #charOn                     ; A = cell character
                bne .clearTopLeft               ; cell is disabled, so clear the topleft neighbor
                if NOISY
                bit CLICK
                endif
                ldy #y_topleft                  ; cell is enabled, so turn on corresponding neighbor bits
                lda #n_topleft                  ; top left neighbor is special since it contains stale data 
                sta (altData),y                 ; so we just set the whole byte instead of ORing the bit
                if NOISY
                bit CLICK                       ; (Pretend I'm not here... I just click the speaker)
                endif
                TURN_ON top                     
                TURN_ON topright
                TURN_ON left
                TURN_ON right
                TURN_ON bottomleft
                TURN_ON bottom
                TURN_ON bottomright
                jmp .continue
.clearTopLeft   ldy #y_topleft
                lda #0
                sta (altData),y
.continue       sec
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
                jsr toggleDataPages
                jsr clearBorders
                lda #fieldHeight-1
                sta .row
.rowLoop        jsr getTextRow
                lda #fieldWidth-1
                sta .column
                lda #0
                ldy #y_top                      ; clean up stale data
                sta (altData),y
                ldy #y_topright
                sta (altData),y
.columnLoop     ldy #0 ; .column
.column         equ .-1
                lda (textRow),y
                cmp #charOff
                beq .clearTopLeft
                ldy #y_topleft                  ; cell is enabled, so turn on corresponding neighbor bits
                lda #n_topleft                  ; top left neighbor is special since it contains stale data 
                sta (altData),y                 ; so we just set the whole byte instead of ORing the bit
                TURN_ON top
                TURN_ON topright
                TURN_ON left
                TURN_ON right
                TURN_ON bottomleft
                TURN_ON bottom
                TURN_ON bottomright
                jmp .nextColumn
.clearTopLeft   ldy #y_topleft
                lda #0
                sta (altData),y
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
                bmi .end
                jmp .rowLoop
.end            rts

toggleDataPages subroutine                      ; toggles the current data page and sets up the pointers
                lda #1
                eor currentPage
                sta currentPage
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

clearBorders    subroutine

                mac CLEAR_BORDERS
.first          set datapg{1} + dataWidth + 1
.last           set datapg{1}_end - [dataWidth * 2] + 1
.clear          set datapg{2}_end - [dataWidth * 2] + 1
                ldx #fieldWidth
.hloop          lda .first,x
                and #topRow
                sta .first,x
                lda .last,x
                and #bottomRow
                sta .last,x
                lda #0
                sta .clear,x
                dex
                bne .hloop
.firstAddr      set ZPB0
.lastAddr       set ZPB2
.first          set datapg{1}_end - [dataWidth * 2] + 1
.last           set .first + fieldWidth - 1
                lda <#.first
                sta <.firstAddr
                lda >#.first
                sta >.firstAddr
                lda <#.last
                sta <.lastAddr
                lda >#.last
                sta >.lastAddr
                ldy #0
                ldx #fieldHeight
.vloop          lda (.firstAddr),y
                and #leftColumn
                sta (.firstAddr),y
                lda (.lastAddr),y
                and #leftColumn
                sta (.lastAddr),y
                lda #dataWidth
                sec
                sbc <.firstAddr
                lda #0
                sbc >.firstAddr
                lda #dataWidth
                sec
                sbc <.lastAddr
                lda #0
                sbc >.lastAddr
                dex
                bne .vloop
                endm

                lda currentPage
                bne .page1
.page0          CLEAR_BORDERS 0,1
                rts
.page1          CLEAR_BORDERS 1,0
                rts

initScreen      subroutine
                lda <#initData
                sta mainData
                lda >#initData
                sta mainDataH
                lda #initDataLen-1              ; get data length
                sta .dataoffset                 ; save it
                lda #fieldHeight-1              ; load the field height
                sta .row                        ; save in row counter
.1              jsr getTextRow                  ; update textRow (A = row)
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
; A = row
; outputs:
; A = ?, X = A << 1, textRow = address of first character in row A
getTextRow      subroutine
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
.loop           jsr getRule                     ; get the on/off char based on hamming weight of .neighbors...
                ldx #0
.neighbors      equ .-1
                sta conwayRules,x               ; ...and store it.
                dec .neighbors
                lda .neighbors
                bne .loop
                lda #charOff                    ; index offset 0 special handling (0 bits = 0 neighbors = turn off)
                sta conwayRules
                rts

getRule         subroutine                      ; Returns #charOn, #charOff, or 0 (if no change)
                jsr countBits                   ; Translate bit pattern to number of neighbors
                cmp #2
                bcc .off                        ; Fewer than 2 neighbors, dies of loneliness
                cmp #3
                beq .on                         ; Exactly 3 neighbors, reproduces
                bcs .off                        ; More than 3 neighbors, dies of overpopulation
                lda #0                          ; Else exactly 2 neighbors, no change
                rts
.off            lda #charOff
                rts
.on             lda #charOn
                rts

countBits       subroutine                      ; Compute Hamming Weight (number of enabled bits) in Accumulator
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
textRowsTable   subroutine                      ; Lookup table for text page 0 row addresses
.pg             equ 1024
.y              set 0
                repeat 24
                dc.w .pg + (.y & %11111000) * 5 + ((.y & %00000111) << 7)
.y              set .y + 1
                repend
                LOG_REGION "textRowsTable", textRowsTable, 0

                if INIT_PATTERN == 0             ; Glider gun
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
                endif
                if INIT_PATTERN == 1        ; "Random"
initData        dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%01000000
                dc.b %00000000,%00000000,%00000000,%00000000,%10100000
                dc.b %00000000,%00000000,%00000000,%00000000,%10100000
                dc.b %00000000,%00000000,%00000000,%00000000,%01000000
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
                dc.b %00000000,%00100000,%00000000,%00000000,%00000000
                dc.b %00000000,%10110000,%00000000,%00000000,%00000000
                dc.b %00000000,%10100000,%00000000,%00000000,%00000000
                dc.b %00000000,%10000000,%00000000,%00000000,%00000000
                dc.b %00000010,%00000000,%00000000,%00000000,%00000000
                dc.b %00001010,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                endif
                if INIT_PATTERN == 2        ; Edge test
initData        dc.b %11000000,%00000000,%00011000,%00000000,%00000011
                dc.b %11000000,%00000000,%00100100,%00000000,%00000011
                dc.b %00000000,%00000000,%00011000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %11100000,%00000000,%00000000,%00000000,%00000111
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00011100,%00000000,%00111000,%00000000
                dc.b %00000000,%00010000,%11011011,%00001000,%00000000
                dc.b %00000000,%00001000,%11011011,%00010000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00001000,%11011011,%00010000,%00000000
                dc.b %00000000,%00010000,%11011011,%00001000,%00000000
                dc.b %00000000,%00011100,%00000000,%00111000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %11100000,%00000000,%00000000,%00000000,%00000111
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00000000,%00000000,%00000000
                dc.b %00000000,%00000000,%00011000,%00000000,%00000000
                dc.b %11000000,%00000000,%00100100,%00000000,%00000011
                dc.b %11000000,%00000000,%00011000,%00000000,%00000011
                endif
initDataLen     equ .-initData

dataSeg         equ .
                seg.u conwayData                ; uninitialized data segment
                org dataSeg

                align 256
conwayRules     ds.b 256                        ; character lookup table goes here (see makeRules subroutine)

datapg0         ds.b dataWidth * dataHeight     ; data page 0
datapg0_lastRow equ . - dataWidth - fieldWidth  ; first visible cell of the last row
datapg0_tln     equ . - [n_offset * 2]          ; topleft neighbor of the bottomright-most visible cell
datapg0_end     equ .

datapg1         ds.b dataWidth * dataHeight     ; data page 1
datapg1_lastRow equ . - dataWidth - fieldWidth  ; first visible cell of the last row
datapg1_tln     equ . - [n_offset * 2]          ; topleft neighbor of the bottomright-most visible cell
datapg1_end     equ .

                echo "conwayRules:", conwayRules