import os, sys
import numpy as np
import pandas as pd
import BioNick as bn

sys.setrecursionlimit(10000)
#pd.set_option('display.float_format', lambda x: '%.10f' % x)

#functions
def mnp(m,l):
    t,y,u = [],[],[]
    for lv in ['family', 'order','class']:
        l2 = l
        iter = 0
        while True:
            t1 = pd.DataFrame(bn.leaves(l2))
            t1 = pd.merge(t1,m[['on','phylum','class','order','family','genus']], left_on=0, right_on='on', how = 'left')
            
            y = []
            r = t1.loc[t1[lv] == t1[lv].value_counts().index[-1], 'on'].iloc[0] #root taxon 
            t2 = fill_unc(t1) #fill unclassified        
            r = root_at(l2,r)
            
            for on,f in t1[['on',lv]].values:
                r = r.replace(on+':',f+':')
    
            c = bn.nodes_w_all_descendants(r)[2]
            vcf = t2[lv].value_counts()    
            n = 0
            for i in set(list(zip(*bn.nodes_w_all_descendants(r)[1]))[0]):
                if (len(set(c['__'+str(i)])) == 1) and (len(c['__'+str(i)]) == vcf.loc[c['__'+str(i)][0]]):
                    n+=1
                    y.append((lv, c['__'+str(i)][0], len(c['__'+str(i)][0])))
                    #print(lv.capitalize(), c['__'+str(i)][0], 'is monophyletic.')
            
            #n2 = len(vcf.drop(t2[lv].value_counts().index[-1])[vcf.drop(t2[lv].value_counts().index[-1]) > 1])        
            n2 = len(vcf[vcf > 1])
            
            t.append((iter, lv, n, n2))

            #no more monophyletic families
            if len(y) == 0:
                break
                
            #remove monophyletic taxa and singleton taxa and save the tree
            t2.loc[t2[lv].isin(set(list(zip(*y))[1])), 'status'] = 'm'
            #t2.loc[t2[lv].isin(vcf.drop(t2[lv].value_counts().index[-1])[vcf.drop(t2[lv].value_counts().index[-1]) == 1].index), 'status'] = 's'
            t2.loc[t2[lv].isin(vcf[vcf == 1].index), 'status'] = 's'
            
            #fewer than 1 remaining taxa
            if len(t2[t2['status'] == 'n']['on'].to_list()) <= 1:
                break
    
            l2 = bn.extract_subtree(l2,t2[t2['status'] == 'n']['on'].to_list())
            iter+=1
            #print(iter, len(bn.leaves(l2)), len(y))
            #print(t)
    
    return t
    
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
    

#code

with open('/data2/scratch/cat/r6/tm3/'+sys.argv[1],'r') as f:
    l = f.readline().strip().replace(';','')

fi = sys.argv[1]
    
if fi.split('_')[0] == 'ar':
    m = pd.read_csv('/data2/scratch/cat/r7/arthropoda/m2c.tsv', sep = '\t')
elif fi.split('_')[0] == 'as':
    m = pd.read_csv('/data2/scratch/cat/r7/ascomycota/m2b.tsv', sep = '\t')
elif fi.split('_')[0] == 'ba':
    m = pd.read_csv('/data2/scratch/cat/r7/basidiomycota/m3b.tsv', sep = '\t')
elif fi.split('_')[0] == 'eu':
    m = pd.read_csv('/data2/scratch/cat/r7/eudicots/m4a.tsv', sep = '\t')
elif fi.split('_')[0] == 've':
    m = pd.read_csv('/data2/scratch/cat/r7/vertebrata/m3c.tsv', sep = '\t')

print(fi)
print(mnp(m,l))
