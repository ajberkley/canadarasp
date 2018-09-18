; create an array to be the colors each triplet is a color and the index of the triplet is the reference to that color
; multiply the array by 1.0 to turn it into float values
; then divide by 255 to put them in the required 0-1 range.
;; note these colors were chosen for 1) ability to tell color changes 2)symbolism of the colors red=hot and danger
;; orange not so much
;; pink and purple are perfect
;; light colors are light lift
;; matching the background is stable
;; greys are inversion   dark times for soaring pilots.

updcolors=(/ (/255,255,255/),\ ; white     0
    (/128,128,230/),\ ; purple/blue            1
    (/128,128,230/),\ ; purple/blue            2
    (/250,202,177/),\ ;light tan               3
    (/200,255,200/),\ ; light green            4
    (/120,255,120/),\ ; darker green           5
    (/21,255,21/),\ ; vibrant green            6
    (/255,150,255/),\ ;pale pink               7
    (/204,191,255/),\ ; pale purple  lavender  8
    (/220,180,0/),\  ; yellow/orange           9
    (/255,120,0/),\  ; dark orange             10
    (/255,61,61/),\  ; dark red                11
    (/230,230,230/),\ ; light grey             12
    (/204,204,204/),\ ; darker grey            13
    (/153,153,153/),\  ; even darker grey      14
    (/0,0,0/),\        ; black                 15
    (/255,  56,  255/),\ ;violet               16
    (/255,  150, 255/),\ ; paler violet        17
    (/255,  196, 255/),\ ; paler violet        18
    (/195,  255, 250/),\ ; pale turquose       19
    (/209,  204, 180/),\ ; grey/tan            20
    (/255,  248, 255/),\ ; paler violet        21
    (/70,  70, 70/),\     ; realy dark grey    22
    (/145,145,240/),\ ; purple/blue            23
    (/255,  255, 0/)/) * 1.0  ; bright yellow  24
    ; last index is 24 is yellow for contrasting fonts
updcolors=updcolors/255 ; but they must be in 0-1 range