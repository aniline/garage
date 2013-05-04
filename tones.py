#!python

import math, numpy, sys

def tone_with_sines(freq, time, rate):
    samples = int(rate * time)
    i = 0
    angles = map(lambda x: float(0), freq)
    cycle = math.radians(360)
    while i < samples:
        j = 0
        while j < len(freq):
            angles[j] = float(angles[j]) + ((cycle * freq[j])/rate)
            j = j + 1
        i = i + 1
        if (len(freq) > 0):
            val = int(128 * numpy.mean(map(math.sin, angles))) + 128
        else:
            val = 128
        sys.stdout.write(chr(val))

freq_base = range(1000, 5000, 100)
img = """
                                     XXXXXXXXXXXXXXXXXXXXXXXXXX           XXXXXXXXXXXXXXXXXXXXXXXXXXX 
                                     XXXXXXXXXXXXXXXXXXXXXXXXX              XXXXXXXXXXXXXXXXXXXXXXXXX
               ########              XXXXXXXXXXXXXXXXXXXXXXX            X     XXXXXXXXXXXXXXXXXXXXXXX
            ####  ## ####            XXXXXXXXXXXXXXXXXXXXXX                    XXXXXXXXXXXXXXXXXXXXXX
          ###     ##     ###         XXXXXXXXXXXXXXXXXXXXXX           X        XXXXXXXXXXXXXXXXXXXXXX
         ##       ##       ##        XXXXXXXXXXXXXXXXXXXXXX   XXX   XXXXXX     XXXXXXXXXXXXXXXXXXXXXX
        ##        ##        ##       XXXXXXXXXXXXXXXXXXXXXX  X XX   X    XX    XXXXXXXXXXXXXXXXXXXXXX
        ##       ####       ##       XXXXXXXXXXXXXXXXXXXXXX   X   X XX   X     XXXXXXXXXXXXXXXXXXXXXX
        ##     #######      ##       XXXXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXX    XXXXXXXXXXXXXXXXXXXXXX
         ##  ##   ##  ##   ##        XXXXXXXXXXXXXXXXXXXXXXX   XXXXXXXX XX      XXXXXXXXXXXXXXXXXXXXX
          ###     ##    ####         XXXXXXXXXXXXXXXXXXXXXXX   XXXXXXX  XX   XX  XXXXXXXXXXXXXXXXXXXX
            ###   ##   ###           XXXXXXXXXXXXXXXXXXXXXX  XX      XXXXXX       XXXXXXXXXXXXXXXXXXX
              ##########             XXXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXX       XXXXXXXXXXXXXXXXXX
                                     XXXXXXXXXXXXXXXXXXX   XXXXXXXXXXXXXXXXXX        XXXXXXXXXXXXXXXX
                                     XXXXXXXXXXXXXXXXXX    XXXXXXXXXXXXXXXXXXX   X     XXXXXXXXXXXXXX
                                     XXXXXXXXXXXXXXXXX     XXXXXXXXXXXXXXXXXXXX   X    XXXXXXXXXXXXXX
                                     XXXXXXXXXXXXXXXX    XXXXXXXXXXXXXXXXXXXXXX    X     XXXXXXXXXXXX
          ######      ######         XXXXXXXXXXXXXXX     XXXXXXXXXXXXXXXXXXXXXXX   XX    XXXXXXXXXXXX
        ########### ##########       XXXXXXXXXXXXXX     XXXXXXXXXXXXXXXXXXXXXXXX    X     XXXXXXXXXXX
       ########################      XXXXXXXXXXXXXX    XXXXXXXXXXXXXXXXXXXXXXXXX    X      XXXXXXXXXX
       ########################      XXXXXXXXXXXX      XXXXXXXXXXXXXXXXXXXXXXXXXX  X       XXXXXXXXXX
        ######################       XXXXXXXXXXXX    XXXXXXXXXXXXXXXXXXXXXXXXXXXX          XXXXXXXXXX
         ###################         XXXXXXXXXXXX XXXX XXXXXXXXXXXXXXXXXXXXXXXX           XXXXXXXXXXX
           ###############           XXXXXXXXXXX XXXXXX   XXXXXXXXXXXXXXXXXXXX XX       XXXXXXXXXXXXX
             ############            XXXXXXXX  XXXXXXXXX    XXXXXXXXXXXXXXXXXX XXXXXXXXXX XXXXXXXXXXX
                #######              XXXXXXXXXXXXXXXXXXXXX    XXXXXXXXXXXXXXXX XXXXXXXXXXXX XXXXXXXXX
                  ###                XXXXXX XXXXXXXXXXXXXXX    XXXXXXXXXXXXX X XXXXXXXXXXXXXX XXXXXXX
                   #                 XXXXX  XXXXXXXXXXXXXXXX XXXXXXXXXXXXXX  X XXXXXXXXXXXXXXXX XXXXX
                                     XXXXX  XXXXXXXXXXXXXXXXX               X  XXXXXXXXXXXX  XXXXXXXX
                                     XXXXX   XXXXXXXXXXXXXXXX  X            X  XXXXXXXX   XXXXXXXXXXX
                                     XXXXXXXXXXX      XXXXXX   XXXXXXXXXXXXXX   XXXX   XXXXXXXXXXXXXX
                                     XXXXXXXXXXXXXXXXXX      XXXXXXXXXXXXXXXXX       XXXXXXXXXXXXXXXX
"""

# tone_with_sines(freq, 3, 22050)
ptrn = img.split('\n')
n_rows = len(ptrn)
n_cols = max(map(len, ptrn))

i = 0
while (i<n_cols):
    r = 0
    freq = []
    while (r < n_rows):
        row = ptrn[r]
        if (i<len(row)):
            val = row[i]
        else:
            val = ' '
        if val != ' ':
            freq.append(freq_base[n_rows - r])
        r = r + 1
    tone_with_sines(freq, 0.07, 22050)
    i = i + 1
