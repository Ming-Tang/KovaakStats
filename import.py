from glob import glob
import sys
import os.path
import re
import traceback

# Path to Kovaak stats csv files
path = None
if len(sys.argv) >= 2:
    path = sys.argv[1]
else:
    with open('kovaak_path.txt') as f:
        path = next(f).strip()
        
out_path = sys.argv[2] if len(sys.argv) >= 3 else 'stats.csv'

fn_pat = re.compile(r'^(.*) - Challenge - (.*?) Stats\.csv$')

fields = [
    'Kills',
    'Deaths', 
    'Fight Time',
    'Avg TTK',
    'Damage Done',
    'Damage Taken',
    'Distance Traveled',
    'Score',
    'Hash'
]

with open(out_path, 'w') as outfile:
    print('Scenario, DateTime, Accuracy, Shots, Hits, ' + ', '.join(f.replace(' ', '') for f in fields), file=outfile)
    for p in glob('{}/*.csv'.format(path)):
        try:
            _, fn = os.path.split(p)
            m = fn_pat.match(fn)
            if not m: continue
            name = m.group(1)
            date_part, time_part = m.group(2).split('-')
            date = '{}T{}'.format(date_part.replace('.', '-'), time_part.replace('.', ':'))
            
            data = ['NA' for f in fields]
            weapons_part, shots, hits, acc = None, 'NA', 'NA', 'NA'
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
                        acc = 0 if int(shots) == 0 else int(hits) / int(shots)
                    except (IndexError, TypeError):
                        pass
                    
                else:
                    for i, f in enumerate(fields):
                        pat = f+ ':,'
                        if line.startswith(pat):
                            data[i] = line[len(pat):].strip()
                            break
                    
            print('{}, {}, {}, {}, {}, {}'.format(name.replace(',', ';'), date, acc, shots, hits, ', '.join(map(str, data))), file=outfile)
        except:
            print(traceback.format_exc())
    
