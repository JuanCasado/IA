
class cube:
	def __init__ (self, x, y, z):
		self.x = x
		self.y = y
		self.z = z


	def print ():
	    t = {(n,m):' ' for n in range(3+self.x+self.z) for m in range(3+self.y+self.z)}
	    xrow = ['+'] + ['-'  for i in range(self.x)] + ['+']
	    for i,ch in enumerate(xrow):
	        t[(i,0)] = t[(i,1+self.y)] = t[(1+self.z+i,2+self.y+self.z)] = ch
	    ycol = ['+'] + ['|' for j in range(y)] + ['+']
	    for j,ch in enumerate(ycol):
	        t[(0,j)] = t[(self.x+1,j)] = t[(2+self.x+self.z,1+self.z+j)] = ch
	    zdepth = ['+'] + ['/' for k in range(z)] + ['+']
	    for k,ch in enumerate(zdepth):
	        t[(k,1+self.y+k)] = t[(1+self.x+k,1+y+k)] = t[(1+self.x+k,k)] = ch
	    return '\n'.join(''.join(t[(n,m)] for n in range(3+self.x+self.z)).rstrip() for m in reversed(range(3+self.y+self.z)))

class node:
	def __init__ (cube, value, alpha, beta):
		self.cube = cube
		self.value = value;
		self.alpha = alpaha;
		self.beta = beta;

	def print():
		cube.print()

if __name__ == '__main__':
	node(cube(1,2,3), 3 ,3,3).print()

