import pandas as pd
import numpy as np
import os,sys
import shutil
from Bio import SeqIO

a1 = pd.read_csv(sys.argv[2], sep = '\t').drop_duplicates('on')

te = pd.read_csv(sys.argv[1]+'.bl', header = None)
te['genus'] = te[0].apply(lambda x: x.split('_')[0])
te['on'] = te[0].apply(lambda x: x.split('\t')[0])
tw = pd.merge(te[['on']], a1[['on','genus', 'phylum', 'class', 'order', 'family']], how = 'left')

c1,c2,c3,c4 = 0,0,0,0
t1,t2,t3,t4 = '','','',''
for i in tw['phylum'].dropna():
    if t1 != i:
        c1+=1
    t1 = i

u2 = list(tw['class'].dropna().unique())     
for i in tw['class'].dropna():
    if t2 != i:
        c2+=1
        if i in u2:
            u2.remove(i)
        else:
            print('class: ', i)
    t2 = i

u3 = list(tw['order'].dropna().unique())        
for i in tw['order'].dropna():
    if t3 != i:
        c3+=1
        if i in u3:
            u3.remove(i)
        else:
            print('order: ', i)
    t3 = i

u4 = list(tw['family'].dropna().unique())    
for i in tw['family'].dropna():
    if t4 != i:
        c4+=1
        if i in u4:
            u4.remove(i)
        else:
            print('family: ', i)
    t4 = i


    
c1 = c1 - tw['phylum'].dropna().nunique()
c2 = c2 - tw['class'].dropna().nunique()
c3 = c3 - tw['order'].dropna().nunique()
c4 = c4 - tw['family'].dropna().nunique()
print('Discordance: ', c1,c2,c3,c4)


tw.to_csv(sys.argv[1]+'.tw', index = 0, sep = '\t', header = None)