; Fast Conway
; Lee W. Fastenau
; info@leefastenau.com
; Created 06/29/2010

;
;  XXX
;    X
;   X
;
; 012
; 3X4 %01234567
; 567
;
; 1. Toggle X state toggles inverse relationship bit on surrounding 8 cells (8 instructions without bounds check if padded)
;    e.g., if toggled, then toggle neighbors (on alt. data page)
;
;    sec                                  ; 0  +2
;    lda currentAddress                   ; 2  +4
;    sbc #41                              ; 6  +2
;    sta neighborAddress                  ; 8  +4
;    lda currentAddress+1                 ; 12 +4
;    sbc #0                               ; 16 +2
;    sta neighborAddress+1                ; 18 +4  These 22 cycles could be reduced by tracking down topleft neighbor
;    ldy #0                               ; 0  +2
;    lda (neighborAddress),y=0            ; 2  +5/6
;    eor n_topleft                        ; 8  +2 
;    sta (neighborAddress),y=0            ; 10 +6
;    iny                                  ; 16 +2
;    lda (neighborAddress),y=1            ; 18 +5/6 Repeats 8 times... 16 cycles * 8 = 128 cycles to update one cell
;    eor n_top                            ;         PLUS 22 cycles for expensive neighbor tracking = 150 cycles
;    sta (neighborAddress),y=1
;    etc.
;
; 2. Lookup table returns on or off char based on conway rules
;    e.g., dataCell = %00000111 = $07, ldy dataCell, lda (charLookup),y, A = charOn
;
; 3. Screen data can update in real time (based on current data page)... will feel "wrong" because screen will reflect
;    both current and alt. data pages during refresh cycle. This is okay because the current data page is canon during
;    the refresh and completely drives the screen
;
; 4. Toggles are the most expensive thing that can happen in a cell. Sparse "on" cells should update very quickly, but
;    for a theorhetically busiest screen scenario, it will take:
;
;    150 cycles to toggle each cell
;    150 * 960cells = 144000 cycles @ 1.023 MHz = 0.14 seconds (But this is still optimistic... doesn't count data traversal or
;    screen data updates... 100 cycles per cell may be a generous estimate. Using 250 cycles, that still gives us an update
;    time of .23 seconds. Four frames per second update in an absolutely busy state.
;    Most "interesting" conway generations feel like they only update 1/20 - 1/10 of the screen, which gives us approx.
;    20 - 45 fps, which is quite respectable.
;

              processor 6502
              incdir "include"
              include "apple2.asm"
              include "macros.asm"

; ------------------------------------
; Build Options
; ------------------------------------

DEBUG         equ 1
USES_TXTPG0   equ 1
USES_TXTPG1   equ 0

; ------------------------------------
; Constants
; ------------------------------------

rowAddr       equ ZPA0
rowAddrH      equ ZPA1

dataAddr      equ ZPC0
dataAddrH     equ ZPC1

nbrAddr       equ ZPC2
nbrAddrH      equ ZPC3

fieldWidth    equ 40
fieldHeight   equ 24

dataWidth     equ fieldWidth+2
dataHeight    equ fieldHeight+2

charOff       equ " " & %00111111
charOn        equ " " | %10000000
noChange      equ 0

topleft       equ %10000000
top           equ %01000000
topright      equ %00100000
left          equ %00010000
right         equ %00001000
bottomleft    equ %00000100
bottom        equ %00000010
bottomright   equ %00000001

n_topleft     equ bottomright
n_top         equ bottom
n_topright    equ bottomleft
n_left        equ right
n_right       equ left
n_bottomleft  equ topright
n_bottom      equ top
n_bottomright equ topleft

n_offset      equ dataWidth+1

y_topleft     equ 0
y_top         equ 1
y_topright    equ 2
y_left        equ dataWidth
y_right       equ dataWidth+2
y_bottomleft  equ dataWidth*2
y_bottom      equ dataWidth*2+1
y_bottomright equ dataWidth*2+2

y_copyfrom    equ y_bottomright+1    ; Relative to current data pointer
y_copyto      equ dataWidth+2        ; Relative to current neighbor pointer

; ------------------------------------
; Entry Point
; ------------------------------------
              seg program
              org $C00

main          subroutine
              lda #0
              sta currentPage
              jsr initScreen
              jsr updateData
.1            jsr iterate       
              jmp .1
              rts
              LOG_REGION "main", main, 0

flipPage      subroutine
              lda #1
              eor currentPage
              sta currentPage
              rts 

              if DEBUG
showDebug     subroutine
              jsr initUpdPtrs
              lda #fieldHeight-1
              sta .row
.nextRow      jsr getRow
              lda #fieldWidth-1
              sta .column
.nextColumn   ldy #0 ; .column
.column       equ .-1
              ldy #0
              lda (dataAddr),y
              tay
              lda debugTable,y
              ldy .column
              sta (rowAddr),y

              dec dataAddr
              lda dataAddr
              cmp #$FF
              bne .noDataH
              dec dataAddrH
.noDataH

              dec .column
              bpl .nextColumn
              sec
              lda dataAddr
              sbc #2
              sta dataAddr
              lda dataAddrH
              sbc #0
              sta dataAddrH

              dec .row
              lda #0 ; .row
.row          equ .-1
              bpl .nextRow
              rts
              endif

iterate       subroutine

              mac TOGGLE
              ldy #y_{1}
              lda (nbrAddr),y
              eor #n_{1}
              sta (nbrAddr),y
              endm

              jsr flipPage
              jsr initUpdPtrs
              lda #fieldHeight-1
              sta .row
.rowLoop      jsr getRow
              lda #fieldWidth-1
              sta .column
.columnLoop   ldy #0
              lda (dataAddr),y
              tay
              lda conwayRules,y
              beq .continue               ; rule says do nothing
              ldy #0 ; .column
.column       equ .-1
              cmp (rowAddr),y
              beq .continue
              sta (rowAddr),y
              TOGGLE topleft
              TOGGLE top
              TOGGLE topright
              TOGGLE left
              TOGGLE right
              TOGGLE bottomleft
              TOGGLE bottom
              TOGGLE bottomright
.continue     ldy #y_copyfrom
              lda (nbrAddr),y
              ldy #y_copyto
              sta (dataAddr),y
              sec
              lda dataAddr
              sbc #1
              sta dataAddr
              lda dataAddrH
              sbc #0
              sta dataAddrH
              sec
              lda nbrAddr
              sbc #1
              sta nbrAddr
              lda nbrAddrH
              sbc #0
              sta nbrAddrH              
.nextColumn   dec .column
              bpl .columnLoop
.nextRow      sec
              lda dataAddr
              sbc #2
              sta dataAddr
              lda dataAddrH
              sbc #0
              sta dataAddrH
              sec
              lda nbrAddr
              sbc #2
              sta nbrAddr
              lda nbrAddrH
              sbc #0
              sta nbrAddrH
              dec .row
              lda #0 ; .row
.row          equ .-1
              bmi .end
              jmp .rowLoop
.end          rts

updateData    subroutine

              mac TURN_ON
              ldy #y_{1}
              lda (nbrAddr),y
              ora #n_{1}
              sta (nbrAddr),y
              endm

              jsr initUpdPtrs
              lda #fieldHeight-1
              sta .row
.rowLoop      jsr getRow
              lda #fieldWidth-1
              sta .column
.columnLoop   ldy #0 ; .column
.column       equ .-1
              lda (rowAddr),y
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
.nextColumn   sec
              lda nbrAddr
              sbc #1
              sta nbrAddr
              lda nbrAddrH
              sbc #0
              sta nbrAddrH
              dec .column
              bpl .columnLoop
.nextRow      sec
              lda nbrAddr
              sbc #2
              sta nbrAddr
              lda nbrAddrH
              sbc #0
              sta nbrAddrH
              dec .row
              lda #0 ; .row
.row          equ .-1
              bpl .rowLoop
              rts

initUpdPtrs   subroutine                  ; Initializes the relevant data pointers
              lda currentPage
              bne .page1
.page0        lda <#datapg0_last
              sta dataAddr
              lda >#datapg0_last
              sta dataAddrH
              lda <#datapg1_tln
              sta nbrAddr
              lda >#datapg1_tln
              sta nbrAddrH
              jmp .continue
.page1        lda <#datapg1_last
              sta dataAddr
              lda >#datapg1_last
              sta dataAddrH
              lda <#datapg0_tln
              sta nbrAddr
              lda >#datapg0_tln
              sta nbrAddrH
.continue     rts

initScreen    subroutine
              lda <#initData
              sta dataAddr
              lda >#initData
              sta dataAddrH
              lda #initDataLen-1          ; get data length
              sta .dataoffset             ; save it
              lda #fieldHeight-1          ; load the field height
              sta .row                    ; save in row counter
.1            jsr getRow                  ; update rowAddr (A = row)
              lda #fieldWidth-1           ; load the field width (reset every new row)
              sta .column                 ; save in column counter
              ldy .dataoffset
              lda (dataAddr),y            ; get the current data byte
              sta .byte                   ; save it
              lda #8                      ; init the byte counter
              sta .bit                    ; save in bit counter
.2            ldy .column
              lda #0
.byte         equ .-1
              lsr
              sta .byte
              bcs .turnOn
.turnOff      lda #charOff
              bne .draw
.turnOn       lda #charOn
.draw         sta (rowAddr),y
              dec .bit
              bne .skipbit
              lda #8                      ; reset bit counter
              sta .bit                    ; decrease data byte reference
              sec
              dec .dataoffset
              ldy #0 ; .dataoffset
.dataoffset   equ .-1
              lda (dataAddr),y
              sta .byte
.skipbit      lda .column                 ; start to calculate init byte offset
              dec .column
              ldy #0 ; .column
.column       equ .-1
              bpl .2
              dec .row
              lda #0 ; .row
.row          equ .-1
              bpl .1
              rts
.bit          ds.b

; inputs:
; ?
; outputs:
; ?
setPoint      subroutine
              jsr getRow
              lda #charOn
              sta (rowAddr),y
              rts

; inputs:
; A = screen character code
; outputs:
; A = $FF, X = ?, Y = $FF
fillScreen    subroutine
              sta .char
              lda #fieldHeight-1
              sta .row
.1            jsr getRow
              ldy #fieldWidth-1
.2            lda #0 ; .char
.char         equ .-1
              sta (rowAddr),y
              dey
              bpl .2
              dec .row
              lda #0 ; .row
.row          equ .-1
              bpl .1
              rts
              LOG_REGION "fillScreen", fillScreen, 0
; inputs:
; A = row
; outputs:
; A = ?, X = A << 1
getRow        subroutine
              asl
              tax
              lda tp0tbl,x
              sta rowAddr
              lda tp0tbl+1,x
              sta rowAddrH
              rts
              LOG_REGION "getRow", getRow, 0

; ------------------------------------
; Utilities
; ------------------------------------

              include "utilities.asm"

; ------------------------------------
; Tables
; ------------------------------------
              if USES_TXTPG0
tp0tbl        subroutine
.pg           equ 1024
.y            set TXTPG0
              repeat 24
              dc.w .pg + (.y & %11111000) * 5 + ((.y & %00000111) << 7)
.y            set .y + 1
              repend
              LOG_REGION "tp0tbl", tp0tbl, 0
              endif

              if USES_TXTPG1
              align 256
tp1tbl        subroutine
.pg           equ TXTPG1
.y            set 0
              repeat 50
              dc.w .pg + (.y & %11111000) * 5 + ((.y & %00000111) << 7)
.y            set .y + 1
              repend
              LOG_REGION "tp1tbl", tp1tbl, 1
              endif

              ifconst FAIL
initData      dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00100000,%00000000,%00000000,%00000000
              dc.b %00000000,%00010000,%00000000,%00000000,%00010000
              dc.b %00000000,%01110000,%00000000,%00000000,%00100000
              dc.b %00000000,%00000000,%00000000,%00000000,%00111000
              dc.b %00000000,%00000000,%00000000,%00001000,%00000000
              dc.b %00000000,%00000000,%00000000,%00001010,%00000000
              dc.b %00001000,%00000000,%00000000,%00001100,%00000000
              dc.b %00000100,%00000000,%00000000,%00000000,%00000000
              dc.b %00011100,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00011100,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00001000,%00000000,%00000000
              dc.b %00000000,%00000000,%10001000,%00000000,%00000000
              dc.b %00000000,%00000000,%10001000,%00000000,%00000000
              dc.b %00000000,%00000000,%10000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000001,%11000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              endif

initData      dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00000000,%00000000,%00000000
              dc.b %00000000,%00000000,%00010000,%00000000,%00000000
              dc.b %00000000,%00000000,%00010000,%00000000,%00000000
              dc.b %00000000,%00000000,%00010000,%00000000,%00000000
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

initDataLen   equ .-initData

currentPage   dc.b #0

;             var loggers = {
;               console: console.log,
;               html: (v) => {$("#console").html($("#console").html() + v + "\n")}
;             }
;             var log = loggers.html
;             
;             for(var i = 0; i<256; i++) {
;                           var bits = 0
;               for(var j = 0; j<8; j++) {
;                           bits += (Math.pow(2,j) & i) > 0 ? 1 : 0
;               }
;               var status = "noChange"
;               if (bits < 2 || bits > 3) status = "charOff"
;               if (bits == 3) status = "charOn"
;                           log("\tdc.b " + status)
;             }

conwayRules   dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b noChange
              dc.b charOff
              dc.b noChange
              dc.b noChange
              dc.b charOn
              dc.b charOff
              dc.b noChange
              dc.b noChange
              dc.b charOn
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b noChange
              dc.b noChange
              dc.b charOn
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b noChange
              dc.b noChange
              dc.b charOn
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b noChange
              dc.b noChange
              dc.b charOn
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b noChange
              dc.b noChange
              dc.b charOn
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b noChange
              dc.b charOn
              dc.b charOn
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOn
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff
              dc.b charOff

              if DEBUG
debugTable    dc.b "0" | %10000000
              dc.b "1" | %10000000
              dc.b "1" | %10000000
              dc.b "2" | %10000000
              dc.b "1" | %10000000
              dc.b "2" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "1" | %10000000
              dc.b "2" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "1" | %10000000
              dc.b "2" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "1" | %10000000
              dc.b "2" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "1" | %10000000
              dc.b "2" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "6" | %10000000
              dc.b "7" | %10000000
              dc.b "1" | %10000000
              dc.b "2" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "6" | %10000000
              dc.b "7" | %10000000
              dc.b "2" | %10000000
              dc.b "3" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "6" | %10000000
              dc.b "7" | %10000000
              dc.b "3" | %10000000
              dc.b "4" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "6" | %10000000
              dc.b "7" | %10000000
              dc.b "4" | %10000000
              dc.b "5" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "6" | %10000000
              dc.b "7" | %10000000
              dc.b "5" | %10000000
              dc.b "6" | %10000000
              dc.b "6" | %10000000
              dc.b "7" | %10000000
              dc.b "6" | %10000000
              dc.b "7" | %10000000
              dc.b "7" | %10000000
              dc.b "8" | %10000000
              endif

              echo "CALL",[showDebug]d,": REM SHOW DEBUG"
              echo "CALL",[flipPage]d,": REM FLIP PAGE"
              echo "CALL",[iterate]d,": REM ITERATE"
              echo "POKE",[currentPage]d,", 0 : REM SET PAGE"

dataSeg       equ .
              seg.u conwayData            ; uninitialized data segment
              org dataSeg
datapg0       ds.b dataWidth * dataHeight ; The start of data page 0 (padded)
datapg0_last  equ .-n_offset              ; The last non-padding cell of data page 0 (topleft neighbor of last cell)
datapg0_tln   equ datapg0_last - n_offset ; Top left neighbor of last non-padding cell of page 0
datapg0_end   equ .-1

datapg1       ds.b dataWidth * dataHeight ; The start of data page 1 (padded)
datapg1_last  equ .-n_offset              ; The last non-padding cell of data page 1 (topleft neighbor of last cell)
datapg1_tln   equ datapg1_last - n_offset ; Top left neighbor of last non-padding cell of page 1
datapg1_end   equ .-1