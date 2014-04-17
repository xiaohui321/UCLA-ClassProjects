#author: Xiaohui, Zhou
#ID:     104-014-248
import math
import struct

# PROBLEM 1

# parse the file named fname into a dictionary of the form 
# {'width': int, 'height' : int, 'max' : int, pixels : (int * int * int) list}
def parsePPM(fname):
    with open(fname,'r') as f:
        if f.readline() != "P6\n":
            return "Wrong file Type: Not a ppm file!"
        #read image properties
        w,h = [int(x) for x in f.readline().split()]
        [m] = [int(y) for y in f.readline().split()]
        l = []
        while True:
            chunk = f.read(3)
            if not chunk:
                break
            turple = struct.unpack('3B',chunk)
            l.append(turple)
        d = {}
        d['width']  = w
        d['height'] = h
        d['max']    = m
        d['pixels'] = l
    return d

# write the given ppm dictionary as a PPM image file named fname
# the function should not return anything
def unparsePPM(ppm, fname):
    w = ppm['width']
    h = ppm['height']
    m = ppm['max']
    l = ppm['pixels']
    with open(fname,'w') as f:
        f.write("P6\n")
        f.write(str(w) + " " + str(h) + "\n")
        f.write(str(m) + "\n")
        for (a,b,c) in l:
            t = struct.pack('3B',a,b,c)
            f.write(t)
    return

# PROBLEM 2
def negate(ppm):
    pixVal = ppm['pixels']
    maxVal = ppm['max']
    newPix = []
    for(a1,b1,c1)in pixVal:
        a2 = maxVal - a1
        b2 = maxVal - b1
        c2 = maxVal - c1
        newPix.append((a2,b2,c2))

    ppm['pixels'] = newPix
    return ppm


# PROBLEM 3
def mirrorImage(ppm):
    w = ppm['width']
    h = ppm['height']
    oldList = ppm['pixels']
    newList = []
    for i in range(0,h):
        SubList = []
        for t in oldList[i * w: (i+1)*w]:
            SubList = [t] + SubList
        newList = newList + SubList
    ppm['pixels'] = newList
    return ppm


# PROBLEM 4

# produce a greyscale version of the given ppm dictionary.
# the resulting dictionary should have the same format, 
# except it will only have a single value for each pixel, 
# rather than an RGB triple.
def greyscale(ppm):
    pixVal = ppm['pixels']
    newPix = []
    for(a,b,c)in pixVal:
        d = int(round(.299 * a + .587 * b  + .114 * c))
        newPix.append(d)
    ppm['pixels'] = newPix
    return ppm

# take a dictionary produced by the greyscale function and write it as a PGM image file named fname
# the function should not return anything
def unparsePGM(pgm, fname):
    w = pgm['width']
    h = pgm['height']
    m = pgm['max']
    l = pgm['pixels']
    with open(fname,'w') as f:
        f.write("P5\n")
        f.write(str(w) + " " + str(h) + "\n")
        f.write(str(m) + "\n")
        for a in l:
            t = struct.pack('B',a)
            f.write(t)
    return


# PROBLEM 5

# gaussian blur code adapted from:
# http://stackoverflow.com/questions/8204645/implementing-gaussian-blur-how-to-calculate-convolution-matrix-kernel
def gaussian(x, mu, sigma):
  return math.exp( -(((x-mu)/(sigma))**2)/2.0 )

def gaussianFilter(radius, sigma):
    # compute the actual kernel elements
    hkernel = [gaussian(x, radius, sigma) for x in range(2*radius+1)]
    vkernel = [x for x in hkernel]
    kernel2d = [[xh*xv for xh in hkernel] for xv in vkernel]

    # normalize the kernel elements
    kernelsum = sum([sum(row) for row in kernel2d])
    kernel2d = [[x/kernelsum for x in row] for row in kernel2d]
    return kernel2d

# blur a given ppm dictionary, returning a new dictionary  
# the blurring uses a gaussian filter produced by the above function
def gaussianBlur(ppm, radius, sigma):
    # obtain the filter
    gfilter = gaussianFilter(radius, sigma)
    width   = ppm['width']
    height  = ppm['height']
    oldList = ppm['pixels']
    newList = []
    for i in range(0,height):
        if ((i < radius) or ((height - i - 1) < radius)):
            for j in range(0,width):
                newList.append(oldList[i*width + j])
        else:
            for j in range(0,width):
                if ((j < radius) or ((width - j - 1) < radius)):
                    newList.append(oldList[i*width + j])
                else:
                    v1 = 0
                    v2 = 0
                    v3 = 0
                    for a in range(-radius,radius + 1):
                        for c in range(-radius,radius + 1):
                            (r,g,b)= oldList[(i+a)*width + j+c]
                            try:
                                v1 += r * gfilter[radius+a][radius+c]
                                v2 += g * gfilter[radius+a][radius+c]
                                v3 += b * gfilter[radius+a][radius+c]
                            except IndexError:
                                print "radius: " + str(radius)+ "a: " \
                                      + str(a) + "b: " + str(b)
                    newList.append((round(v1),round(v2),round(v3)))
    
    ppm['pixels'] = newList
    return ppm