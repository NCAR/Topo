"""Write profiles.txt for test_rms_wedge.  Either synthetic (default) or from a
Ridge_list via rdridgelist:   python make_profiles.py            -> 504 synthetic
                              python make_profiles.py LISTFILE NSW -> real rdg_profiles(:nsw+1)
"""
import sys, numpy as np
if len(sys.argv) >= 3:
    sys.path.insert(0, '.')
    from rdridgelist import read_ridgelist            # adjust to your reader's name
    L = read_ridgelist(sys.argv[1]); nsw = int(sys.argv[2])
    P = np.asarray(L.rdg_profiles)[:, :nsw+1]
else:
    nsw = 25; n = nsw+1; x = np.arange(n, dtype=float); rng = np.random.default_rng(1)
    g = lambda A, x0, w: A*np.exp(-((x-x0)/w)**2)
    P = [g(1500,24,4)+g(300,12,2.5), g(1500,1,4)+g(1500,24,4)+g(300,12,2.5), g(800,12,2.5)+np.where(x>18,900.,0.),
         np.array([-56.88,-44.72,-31.79,-19.91,-9.17,-14.90,-1.90,12.49,34.64,61.95,75.96,91.04,110.82,110.38,
                   108.44,101.81,92.94,103.74,123.47,130.13,122.91,127.45,142.43,150.16,172.37,191.10])]
    for k in range(500):
        p = np.zeros(n)
        for m in range(rng.integers(1,4)): p += g(rng.uniform(100,1500), rng.uniform(-3,n+2), rng.uniform(1.5,8))
        p += rng.normal(0, rng.uniform(0,40), n)
        if rng.random() < 0.1: p = np.linspace(0,500,n) + rng.normal(0,5,n)
        P.append(p)
    P = np.array(P)
with open('profiles.txt','w') as f:
    f.write(f"{len(P)} {nsw}\n")
    for p in P: f.write(" ".join(f"{v:.8f}" for v in p)+"\n")
np.save('profiles.npy', P); print(len(P), 'profiles, nsw =', nsw)
