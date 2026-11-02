"""Compare wedges_f90.txt (from test_rnodes_v2) with the Python references."""
import numpy as np, collections
from rnodes import rnodes, classify
from rnodes_v2 import rnodes_v2
P = np.load('profiles.npy'); nsw = P.shape[1]-1; n = nsw+1
rows = [l.split() for l in open('wedges_f90.txt')]
F = np.array([[float(v) for v in r[:10]] for r in rows]); LOK = np.array([r[10]=='T' for r in rows])
bad_old = bad_new = 0
for k, p in enumerate(P):
    o = rnodes(p)
    w = rnodes_v2(np.arange(1,n+1), p, nnode=n, ns2=nsw//2, bound_at_higher=True, npts=3)
    if not np.array_equal(o['xwedge'], F[k,1:4].astype(int)): bad_old += 1; print('old mismatch', k, o['xwedge'], F[k,1:4])
    pyw = w['xwedge'] if w['ok'] else np.zeros(3, int)
    if not np.array_equal(pyw, F[k,4:7].astype(int)) or w['ok'] != LOK[k] or (w['ok'] and abs(w['rms']-F[k,9]) > 1e-4):
        bad_new += 1; print('v2 mismatch', k, pyw, F[k,4:7], w.get('rms'), F[k,9])
print(f"{len(P)} profiles: old rnodes mismatches={bad_old}, rnodes_v2 mismatches={bad_new}")
print('old-wedge classes:', collections.Counter(classify(rnodes(p)['xwedge']) for p in P))
