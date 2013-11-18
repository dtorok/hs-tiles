import random

def randTile():
    shape = random.randint(0, 6)
    x = random.randint(0, 29)
    y = random.randint(0, 6)
    r = random.randint(0, 4)
    c = random.randint(0, 2)

    return (shape, x, y, r, c)

def randTiles(cnt):
    res = []
    for i in xrange(cnt):
        res.append(randTile())

    return res

print randTiles(1000)