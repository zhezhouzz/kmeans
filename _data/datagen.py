#!/usr/local/python

import numpy.random as nrand
import random as rand

def points_gen(num, scale_x, scale_y, loc_x, loc_y):
    datas = []
    for i in range(num):
        x = nrand.normal(loc_x, scale_x)
        y = nrand.normal(loc_y, scale_y)
        datas.append((x, y))
    return datas

def shuffle(l):
    rand.shuffle(l)

def toFile(file_name, l):
    f = open(file_name, "w")
    f.write('{}\n'.format(len(l)))
    for (x, y) in l:
        str = '{} {}\n'.format(x, y)
        f.write(str)
    f.close()

if __name__ == "__main__":
    datas1 = points_gen(100, 1.0, 2.0, 0.0, 0.0)
    datas2 = points_gen(100, 1.0, 2.0, 100.0, 100.0)
    datas = datas1 + datas2
    shuffle(datas)
    toFile("psudo100.txt", datas)
