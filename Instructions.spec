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
