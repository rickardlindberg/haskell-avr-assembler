ADC    d,r   0001 11rd dddd rrrr
ADD    d,r   0000 11rd dddd rrrr
ADIW   d,k   1001 0110 kkdd kkkk
AND    d,r   0010 00rd dddd rrrr
ANDI   d,k   0111 kkkk dddd kkkk
ASR    d     1001 010d dddd 0101
BCLR   s     1001 0100 1sss 1000
BLD    d,b   1111 100d dddd 0bbb
BRBC   s,k   1111 01kk kkkk ksss
BRBS   s,k   1111 00kk kkkk ksss
BRCC   k     1111 01kk kkkk k000
BRCS   k     1111 00kk kkkk k000
BREAK        1001 0101 1001 1000
BREQ   k     1111 00kk kkkk k001
BRGE   k     1111 01kk kkkk k100
BRHC   k     1111 01kk kkkk k101
BRHS   k     1111 00kk kkkk k101
BRID   k     1111 01kk kkkk k111
BRIE   k     1111 00kk kkkk k111
BRLO   k     1111 00kk kkkk k000
BRLT   k     1111 00kk kkkk k100
BRMI   k     1111 00kk kkkk k010
BRNE   k     1111 01kk kkkk k001
BRPL   k     1111 01kk kkkk k010
BRSH   k     1111 01kk kkkk k000
BRTC   k     1111 01kk kkkk k110
BRTS   k     1111 00kk kkkk k110
BRVC   k     1111 01kk kkkk k011
BRVS   k     1111 00kk kkkk k011
BSET   s     1001 0100 0sss 1000
BST    d,b   1111 101d dddd 0bbb
CALL   k     1001 010k kkkk 111k kkkk kkkk kkkk kkkk
CBI    a,b   1001 1000 aaaa abbb
-- TODO: CBR
CLC          1001 0100 1000 1000
CLH          1001 0100 1101 1000
CLI          1001 0100 1111 1000
CLN          1001 0100 1010 1000
-- TODO: CLR
CLS          1001 0100 1100 1000
CLT          1001 0100 1110 1000
CLV          1001 0100 1011 1000
CLZ          1001 0100 1001 1000
COM    d     1001 010d dddd 0000
CP     d,r   0001 01rd dddd rrrr
CPC    d,r   0000 01rd dddd rrrr
CPI    d,k   0011 kkkk dddd kkkk
CPSE   d,r   0001 00rd dddd rrrr
DEC    d     1001 010d dddd 1010
DES    k     1001 0100 kkkk 1011
EICALL       1001 0101 0001 1001
EIJMP        1001 0100 0001 1001
-- TODO: ELPM
EOR    d,r   0010 01rd dddd rrrr
FMUL   d,r   0000 0011 0ddd 1rrr
FMULS  d,r   0000 0011 1ddd 0rrr
FMULSU d,r   0000 0011 1ddd 1rrr
ICALL        1001 0101 0000 1001
IJMP         1001 0100 0000 1001
IN     d,a   1011 0aad dddd aaaa
INC    d     1001 010d dddd 0011
JMP    k     1001 010k kkkk 110k kkkk kkkk kkkk kkkk
-- TODO: LAC
-- TODO: LAS
-- TODO: LAT
LDOne  d     1001 000d dddd 1100
LDTwo  d     1001 000d dddd 1101
LDThre d     1001 000d dddd 1110
-- TODO: LD (LDD)
-- TODO: LD (LDD)
LDI    d,k   1110 kkkk dddd kkkk
LDS    d,k   1001 000d dddd 0000 kkkk kkkk kkkk kkkk
-- TODO: LDS (16)
LPMA         1001 0101 1100 1000
LPMB   d     1001 000d dddd 0100
LPMC   d     1001 000d dddd 0101
