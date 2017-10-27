with open('batsmen.txt', 'r') as f:
  bb = f.read().split('\n')
with open('batsmen-data.txt', 'w') as f:
  for b in bb:
    b = b.split(', ')
    f.write('{}, {}, {}, {}\n'.format(b[0], b[1].split(': ')[1], b[2], b[3].split(': ')[1]))
  