
!SLIDE 
.notes first slide

## Counting DNA Nucleotides inspirations
* Hacker's Delight, Henry S. Warren Jr.
* ForkJoin, Doug Lea

!SLIDE 
## ASCII values in binary
    87654321
    01000001 A
    01000011 C
    01000111 G
    01010100 T
## Mask off the bits 5, 3, 2
    simultaneous accumulation of T, G+T, C+G
    each accumulator is given 10 bits of 32
    ((b&0x10)<<16)+((b&0x04)<<8)+((b&0x02)>>>1)
        
!SLIDE

## ForkJoin
    Easy to read 
    Recursive
    Concurrent
    Work stealing
    Likes uneven workloads



