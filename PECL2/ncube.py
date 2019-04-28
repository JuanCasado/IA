
import sys

def cuboid(x,y,z):
    t = {(n,m):' ' for n in range(3+x+z) for m in range(3+y+z)}
    xrow = ['+'] + ['%i' % (i % 10) for i in range(x)] + ['+']
    for i,ch in enumerate(xrow):
        t[(i,0)] = t[(i,1+y)] = t[(1+z+i,2+y+z)] = ch
    ycol = ['+'] + ['%i' % (j % 10) for j in range(y)] + ['+']
    for j,ch in enumerate(ycol):
        t[(0,j)] = t[(x+1,j)] = t[(2+x+z,1+z+j)] = ch
    zdepth = ['+'] + ['%i' % (k % 10) for k in range(z)] + ['+']
    for k,ch in enumerate(zdepth):
        t[(k,1+y+k)] = t[(1+x+k,1+y+k)] = t[(1+x+k,k)] = ch
    return '\n'.join(''.join(t[(n,m)] for n in range(3+x+z)).rstrip() for m in reversed(range(3+y+z)))

def format (str):
  return tuple(str.strip().replace('#','').replace('"','').replace("'",'').replace("%",'').replace('(', '').replace(')','').replace(',',' ').split(' '))

if __name__ == '__main__':
  print(cuboid(int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3])))