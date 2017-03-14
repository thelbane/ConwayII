; ------------------------------------
; Zero Page Constants
; ------------------------------------
; Free zero page locacations according to
; http://home.swbell.net/rubywand/csa2pfaq.html#017

ZPA0          equ $06
ZPA1          equ $07
ZPA2          equ $08
ZPA3          equ $09

HTAB          equ $24
VTAB          equ $25
TXTROW        equ $28

ZPB0          equ $EB
ZPB1          equ $EC
ZPB2          equ $ED
ZPB3          equ $EE
ZPB4          equ $EF

ZPC0          equ $FA
ZPC1          equ $FB
ZPC2          equ $FC
ZPC3          equ $FD


STACK         equ $100


TXTPG0        equ $0400
TXTPG1        equ $0800
CLICK         equ $C030
HOME          equ $FC58
RDKEY         EQU $FD0C
COUT          equ $FDED

EXITDOS       equ $03D0