#!python
#
# Playing around audio spectrum analyser on an android tablet, I tried making lines by
# whistling myself dizzy. Then thought of making a program for doing it. This is it.
#
# IF you have a grayscale 50x50 pixel raw bitmap (2500 bytes), pass it as argument to this
# script. It would spew out the audio in 44.1 Khz 16 bit little endian one channel PCM.
# Save it as raw and use aplay, play or sox. Example command to convert and save as
# WAV file is :
#
# python tones.py /tmp/img.raw | sox -r 44100 -t raw -u -c 1 -2 -- - foo.wav
#
# Its not fast enough to play back live (you'll encounter buffer underflows and gaps in audio)
#
import math, numpy, sys

cycle = math.radians(360)
n_rows = 50
n_cols = 50

# 50 Bands of 100Hz width each
freq_base = range(1000, 6000, 100)

# Images are have zero index at top, spectrum analyzer has lower freq at bottom
freq_base.reverse()

def tone_with_sines(freq, time, rate, vals, angles):
    samples = int(rate * time)
    buf = bytearray()
    if (len(freq) > 0):
        theta = map(lambda x: cycle * x / rate, freq)
        denom = 1.0/len(freq)
        for i in xrange(samples):
            angles = map(lambda x,y: x+y, angles, theta)
            val = int(32767 * numpy.mean(map(lambda x, y: y * x, map(math.sin, angles), vals))) + 32768
            buf = buf + chr(val & 0xff) + chr((val & 0xff00) >> 8)
        sys.stdout.write(buf)
    return angles

if len(sys.argv) > 1:
    f = open(sys.argv[1])
    img = f.read()
    f.close()
else:
    print "Pass a 50x50, 8 bit, grayscale, raw, image data file as argument."
    sys.exit(1)

# To avoid blips
angles = map(lambda x: float(0), freq_base)
i = 0
while (i<n_cols):
    r = 0
    freq = []
    vals = []
    while (r < n_rows):
        row = img[r*n_cols:(r*n_cols + n_cols)]
        if (i<len(row)):
            val = row[i]
        else:
            val = 0
        vals.append(ord(val)/255.0)
        r = r + 1
    angles = tone_with_sines(freq_base, 0.135, 44100, vals, angles)
    i = i + 1
