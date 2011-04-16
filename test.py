import sys
import socket

reqs = 0
def run(t):
	global reqs
	s = socket.socket()
	s.connect(('localhost', 8000))
	f = s.makefile('rw', 0)
	if t=='hello':
		seq = 0
		while True:
			f.write('Hello %d\n'%seq)
			reqs += 1
			recved = f.readline()
			seq = int(recved.split()[1])
			sys.stdout.write(recved)
	elif t=='stat':
		while True:
			f.write('Stat\n')
			reqs += 1
			recved = f.readline()
			sys.stdout.write(recved)

if __name__ == '__main__':
	try:
		run(sys.argv[1])
	except:
		print 'requests', reqs
