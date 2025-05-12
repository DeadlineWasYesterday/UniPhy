#directory r6/tmr hardcoded

import os,sys
import numpy as np
import pandas as pd
import BioNick as bn

sys.setrecursionlimit(30000)

def mnpf(m,l):
    t1 = pd.DataFrame(bn.leaves(l))
    t1 = pd.merge(t1,m[['on','phylum','class','order','family','genus']], left_on=0, right_on='on', how = 'left')
    t,y,u = [],[],[]
    lv = 'family'
    r = t1.loc[t1[lv] == t1[lv].value_counts().index[-1], 'on'].iloc[0] #root taxon 
    t2 = fill_unc(t1) #fill unclassified        
    r = root_at(l,r)
    
    for on,f in t1[['on',lv]].values:
        r = r.replace(on+':',f+':')

    c = bn.nodes_w_all_descendants(r)[2]
    vcf = t2[lv].value_counts()    
    n = 0
    for i in set(list(zip(*bn.nodes_w_all_descendants(r)[1]))[0]):
        if (len(set(c['__'+str(i)])) == 1) and (len(c['__'+str(i)]) == vcf.loc[c['__'+str(i)][0]]):
            n+=1
            #y.append((sys.argv[1], c['__'+str(i)][0], len(c['__'+str(i)][0])))
            print(sys.argv[1], c['__'+str(i)][0], len(c['__'+str(i)]))
            #print(lv.capitalize(), c['__'+str(i)][0], 'is monophyletic.')
    return y

def root_at(tree,taxon):
    a,b = bn.recur_nw_pd(tree,len(bn.leaves(tree)),[])
    d = pd.DataFrame(b)
    i = d.loc[d[1] == taxon,0].iloc[0] #new root    
    tb = bn.encode_leaves(tree,b)
    return bn.recur_pd_nw('%016.10f' % i, bn.trail(bn.reasign(bn.swap_root(tb,i),tree)))

def fill_unc(df):
    for c in ['phylum','class','order','family','genus']:
        n=1
        for i,v in df.loc[:,c].items():
            if pd.isna(df.loc[i,c]):
                df.loc[i,c] = 'unc'+str(n).zfill(2)
                n+=1
    return df


with open('/data2/scratch/cat/r6/tmr/'+sys.argv[1],'r') as f:
    l = f.readline()[:-1] 

if sys.argv[1].split('_')[0][:2] == 'ar':
    m = pd.read_csv('/data2/scratch/cat/r7/arthropoda/m2c.tsv', sep = '\t')
elif sys.argv[1].split('_')[0][:2] == 'as':
    m = pd.read_csv('/data2/scratch/cat/r7/ascomycota/m2b.tsv', sep = '\t')
elif sys.argv[1].split('_')[0][:2] == 'ba':
    m = pd.read_csv('/data2/scratch/cat/r7/basidiomycota/m3b.tsv', sep = '\t')
elif sys.argv[1].split('_')[0][:2] == 'eu':
    m = pd.read_csv('/data2/scratch/cat/r7/eudicots/m4a.tsv', sep = '\t')
elif sys.argv[1].split('_')[0][:2] == 've':
    m = pd.read_csv('/data2/scratch/cat/r7/vertebrata/m3c.tsv', sep = '\t')
else:
    sys.exit()

y = mnpf(m,l)