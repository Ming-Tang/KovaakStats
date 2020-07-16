from glob import glob
import sys
import os.path
import re
import traceback

# Path to Kovaak stats csv files
path = None
if len(sys.argv) == 2:
    path = sys.argv[2]
else:
    with open('kovaak_path.txt') as f:
        path = next(f).strip()

fn_pat = re.compile(r'^(.*) - Challenge - (.*?) Stats\.csv$')

with open('stats.csv', 'w') as outfile:
    print('Scenario, DateTime, Score, Accuracy, AvgTTK', file=outfile)
    for p in glob('{}/*.csv'.format(path)):
        try:
            _, fn = os.path.split(p)
            m = fn_pat.match(fn)
            if not m: continue
            name = m.group(1)
            date_part, time_part = m.group(2).split('-')
            date = '{}T{}'.format(date_part.replace('.', '-'), time_part.replace('.', ':'))
            
            score, acc, avg_ttk, weapons_part = None, None, None, None
            for line in open(p, 'r'):
                if line.strip() == '':
                    weapons_part = False
                elif line.startswith('Weapon,Shots,Hits,'):
                    weapons_part = True 
                elif weapons_part:
                    weapons_part = False
                    try:
                        parts = line.split(',')
                        shots, hits = parts[1], parts[2]
                        acc = int(hits) / int(shots)
                    except (IndexError, ArgumentError):
                        pass
                    
                elif line.startswith('Avg TTK:,'):
                    avg_ttk = line.replace('Avg TTK:,', '').strip()
                    
                elif line.startswith('Score:,'):
                    score = line.replace('Score:,', '').strip()
                    
            print('{}, {}, {}, {}, {}'.format(name, date, score, acc, avg_ttk),
                  file=outfile)
        except:
            traceback.format_exc()
    
