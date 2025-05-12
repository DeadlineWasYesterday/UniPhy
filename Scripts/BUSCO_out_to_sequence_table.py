import os,sys,warnings
import numpy as np
import pandas as pd
from Bio import SeqIO
from pandas.errors import SettingWithCopyWarning
warnings.simplefilter(action="ignore", category=SettingWithCopyWarning)


def protein_tsv(d1,df,lp,wp):
    df = df[(df['Status'] == 'Single') | (df['Status'] == 'Duplicated')]
    df['GG'] = df['Assembly'] + '__' + df['Best gene'] + '|' + df['Sequence'] + ':' + df['Gene Start'].astype(int).astype(str) + '-'+ df['Gene End'].astype(int).astype(str)

    #df['fi'] = df['Fraction'].astype(float) * df['Identity'].astype(float)
    #df = df.sort_values('fi', ascending = False).drop_duplicates('GG')
    df = df.drop_duplicates('GG')
    print('Data table shape: ', df.shape)

    t = []
    for i in df['Assembly'].unique():
        
        tdf = df[df['Assembly'] == i]
        
        vals = tdf['GG'].values
        
        if os.path.exists(lp+'{0}/{0}.mb/{1}_odb10/full_table.tsv'.format(i,lin)):
            for r in SeqIO.parse(lp+'{0}/{0}.mb/{1}_odb10/translated_protein.fasta'.format(i,lin), 'fasta'):
                
                if i + '__' + r.id in vals:
                    t.append((i + '__' + r.id, str(r.seq)))
        else:
            print('Error with {0}.'.format(i))

    dfa = pd.DataFrame(t, columns = ['GG', 's']).drop_duplicates('GG')
    #dfa.to_csv(wp, index = 0)

    print('Seq table shape: ', dfa.shape)

    pd.merge(df,dfa).to_csv(wp, index = 0)


d1 = pd.read_csv(sys.argv[1], sep ='\t')
df = pd.read_csv(sys.argv[2])
lp = sys.argv[3]
lin = sys.argv[4]
wp = sys.argv[5]

protein_tsv(d1,df,lp,wp)