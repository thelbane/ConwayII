printline     subroutine
              tsx
              inx
              lda STACK,x
              sta ZPA0
              lda STACK+1,x
              sta ZPA1
.loop
              inc ZPA0
              bne .cont
              inc ZPA1
.cont       
              ldy #0
              lda (ZPA0),y
              beq .cleanup
              ora #%10000000
              jsr PUTCHAR
              bne .loop
.cleanup
              lda ZPA0
              sta STACK,x
.end
              lda ZPA1
              sta STACK+1,x
              rts

rand          subroutine
              jsr rand64k       ;Factors of 65535: 3 5 17 257
              jsr rand32k       ;Factors of 32767: 7 31 151 are independent and can be combined
              lda sr1+1         ;can be left out 
              eor sr2+1         ;if you dont use
              tay               ;y as suggested
              lda sr1           ;mix up lowbytes of SR1
              eor sr2           ;and SR2 to combine both 
              rts
 
;periode with 65535
;10+12+13+15
rand64k       subroutine
              lda sr1+1
              asl
              asl
              eor sr1+1
              asl
              eor sr1+1
              asl
              asl
              eor sr1+1
              asl
              rol sr1         ;shift this left, "random" bit comes from low
              rol sr1+1
              rts
 
;periode with 32767
;13+14
rand32k       subroutine
              lda sr2+1
              asl
              eor sr2+1
              asl
              asl
              ror sr2         ;shift this right, random bit comes from high - nicer when eor with sr1
              rol sr2+1
              rts
 
;feel free to set seeds as wished, if put in zeropage some speed-boost is 
;the result. For example sr1=$5c sr2=5e would fit
sr1           dc.w $a55a
sr2           dc.w $7653