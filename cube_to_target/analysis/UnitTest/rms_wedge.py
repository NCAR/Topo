"""
rms_wedge.py  --  nodes -> wedge, version 2 (5-point best-fit wedge)

    from rnodes import rnodes
    from rms_wedge import rms_wedge

    o  = rnodes(ridge)                      # existing node finder (port of Fortran RNODES)
    w  = rms_wedge(o['xnodes'], o['hnodes'], nnode=o['nnode'], ns2=len(ridge)//2)

    w['xwedge'], w['hwedge']     # npts=5 (default): [x_L, x_0, x_c, x_1, x_R]
                                 # npts=3          : [x_0, x_c, x_1]
    w['xwedge5'], w['xwedge3']   # both forms always present (and hwedge5 / hwedge3)
    w['xL'], w['xR']             # active-window limits
    w['rms']                     # rms of the best wedge
    w['ok']                      # False when no usable peak

Algorithm (Julio's outline, 2026-11-02)
  1) central peak: local maximum of hnodes closest to the window centre ns2
     (ties -> higher).                                   -> x_c, h_c
  2) active window: from x_c step outward one node at a time on each side
     (sides independent); keep going while the node is lower than h_c and
     not the edge.  The first node that is NOT lower than h_c, or the edge
     node, is the limit.                                 -> x_L, x_R
  3) best wedge: for every pair of interior nodes x_0 in (x_L, x_c) and
     x_1 in (x_c, x_R) build the 5-point polyline
         [x_L,h_L] [x_0,h_0] [x_c,h_c] [x_1,h_1] [x_R,h_R]
     interpolate it linearly onto the nodes in [x_L, x_R], compute the rms
     against hnodes there, and keep the pair with the smallest rms.
     A side with no interior node uses x_0 = x_L (or x_1 = x_R).

Interpretation switches (all keyword arguments)
  bound_at_higher : True  -> the limit IS the first node >= h_c (spec as written)
                    False -> the limit is the last node that is still < h_c
  fit_on          : 'nodes'   -> rms over the nodes in [x_L,x_R] (spec as written)
                    'profile' -> rms over every profile point in [x_L,x_R]
                                 (needs the ridge profile passed as `ridge`)
  strict_peak     : True  -> h_c must exceed BOTH neighbouring nodes
                    False -> h_c >= neighbours (lets a mesa top count)

Inputs may be the padded Fortran-style arrays from rnodes() (xnodes padded
with -1, hnodes with 0) together with nnode, or clean unpadded arrays.
Positions are 1-based throughout, consistent with rnodes.py and the Fortran.
"""
import numpy as np

__all__ = ["rms_wedge"]


def _central_peak(xn, hn, ns2, strict=True):
    """index (0-based into xn/hn) of the local max of hn closest to ns2, or -1."""
    n = len(xn)
    cands = []
    for i in range(1, n - 1):
        if strict:
            is_pk = hn[i] > hn[i - 1] and hn[i] > hn[i + 1]
        else:
            is_pk = hn[i] >= hn[i - 1] and hn[i] >= hn[i + 1] and (hn[i] > hn[i - 1] or hn[i] > hn[i + 1])
        if is_pk:
            cands.append(i)
    if not cands:
        return -1
    d = np.array([abs(xn[i] - ns2) for i in cands])
    best = [cands[k] for k in np.flatnonzero(d == d.min())]
    return max(best, key=lambda i: hn[i])        # tie on distance -> higher node


def _active_window(xn, hn, ic, bound_at_higher=True):
    """step outward from ic; return (iL, iR) as 0-based node indices."""
    n = len(xn)
    hc = hn[ic]
    # left
    i = ic
    while True:
        if i == 0:
            iL = 0; break
        j = i - 1
        if hn[j] < hc:
            i = j
            continue
        iL = j if bound_at_higher else i
        break
    # right
    i = ic
    while True:
        if i == n - 1:
            iR = n - 1; break
        j = i + 1
        if hn[j] < hc:
            i = j
            continue
        iR = j if bound_at_higher else i
        break
    # guarantee a window of at least one node either side when possible
    if iL == ic and ic > 0:
        iL = ic - 1
    if iR == ic and ic < n - 1:
        iR = ic + 1
    return iL, iR


def rms_wedge(xnodes, hnodes, nnode=None, ns2=None, ridge=None,
              bound_at_higher=True, fit_on='nodes', strict_peak=True,
              npts=5):
    """
    npts = 5 -> xwedge/hwedge are [x_L, x_0, x_c, x_1, x_R]
    npts = 3 -> xwedge/hwedge are [x_0, x_c, x_1]
    Both forms are always available as xwedge5/hwedge5 and xwedge3/hwedge3.
    """
    if npts not in (3, 5):
        raise ValueError('npts must be 3 or 5')
    xn_all = np.asarray(xnodes)
    hn_all = np.asarray(hnodes, dtype=float)
    if nnode is None:
        nnode = int((xn_all > 0).sum()) if (xn_all <= 0).any() else len(xn_all)
    xn = xn_all[:nnode].astype(int)
    hn = hn_all[:nnode]
    if ns2 is None:
        ns2 = (xn[-1] - 1) // 2                     # Fortran: nsw/2 with xn(nnode) = nsw+1

    empty = dict(ok=False, xwedge=np.zeros(npts, int), hwedge=np.zeros(npts),
                 xwedge5=np.zeros(5, int), hwedge5=np.zeros(5),
                 xwedge3=np.zeros(3, int), hwedge3=np.zeros(3),
                 xL=0, xR=0, xc=0, rms=np.nan, ic=-1, iL=-1, iR=-1, candidates=[])

    ic = _central_peak(xn, hn, ns2, strict=strict_peak)
    if ic < 0:
        return empty
    iL, iR = _active_window(xn, hn, ic, bound_at_higher)
    if iL == ic or iR == ic:
        return empty

    # evaluation abscissae / target heights for the rms
    if fit_on == 'profile':
        if ridge is None:
            raise ValueError("fit_on='profile' needs the ridge profile (1-based positions = index+1)")
        ridge = np.asarray(ridge, dtype=float)
        xs = np.arange(xn[iL], xn[iR] + 1)             # 1-based positions
        hs = ridge[xs - 1]
    else:
        xs = xn[iL:iR + 1]
        hs = hn[iL:iR + 1]

    left_choices  = list(range(iL + 1, ic)) or [iL]      # interior nodes, else the limit itself
    right_choices = list(range(ic + 1, iR)) or [iR]

    best = None
    cands = []
    for i0 in left_choices:
        for i1 in right_choices:
            idx = [iL, i0, ic, i1, iR]
            xw = xn[idx].astype(float)
            hw = hn[idx]
            # duplicate abscissae (when a side has no interior node) are fine for np.interp
            fit = np.interp(xs, xw, hw)
            rms = float(np.sqrt(np.mean((fit - hs) ** 2)))
            cands.append((rms, i0, i1))
            if best is None or rms < best[0]:
                best = (rms, i0, i1)

    rms, i0, i1 = best
    idx = [iL, i0, ic, i1, iR]
    xw5, hw5 = xn[idx].copy(), hn[idx].copy()
    xw3, hw3 = xn[[i0, ic, i1]].copy(), hn[[i0, ic, i1]].copy()
    return dict(ok=True,
                xwedge=(xw5 if npts == 5 else xw3), hwedge=(hw5 if npts == 5 else hw3),
                xwedge5=xw5, hwedge5=hw5, xwedge3=xw3, hwedge3=hw3,
                xL=int(xn[iL]), xR=int(xn[iR]), xc=int(xn[ic]), rms=rms,
                ic=ic, iL=iL, iR=iR, candidates=cands)


if __name__ == '__main__':
    import sys
    sys.path.insert(0, __file__.rsplit('/', 1)[0] or '.')
    from rnodes import rnodes, synthetic_profiles, classify

    profiles = synthetic_profiles()
    # the real profile from the 2026-10-31 exchange (nsw = 25)
    profiles['real: knob on a rising slope'] = np.array(
        [-56.88, -44.72, -31.79, -19.91, -9.17, -14.90, -1.90, 12.49, 34.64, 61.95,
         75.96, 91.04, 110.82, 110.38, 108.44, 101.81, 92.94, 103.74, 123.47, 130.13,
         122.91, 127.45, 142.43, 150.16, 172.37, 191.10])

    for name, ridge in profiles.items():
        o = rnodes(ridge)
        nsw = len(ridge) - 1
        w = rms_wedge(o['xnodes'], o['hnodes'], nnode=o['nnode'], ns2=nsw // 2)
        wp = rms_wedge(o['xnodes'], o['hnodes'], nnode=o['nnode'], ns2=nsw // 2,
                       fit_on='profile', ridge=ridge)
        print(f"{name}")
        print(f"   nodes      x={o['xnodes'][:o['nnode']]}  h={np.round(o['hnodes'][:o['nnode']])}")
        print(f"   old rnodes xwedge={o['xwedge']} ({classify(o['xwedge'])})")
        print(f"   v2 nodes   xwedge5={w['xwedge5']}  h={np.round(w['hwedge5'])}  rms={w['rms']:.1f}  "
              f"window=[{w['xL']},{w['xR']}]  xwedge3={w['xwedge3']}")
        print(f"   v2 profile xwedge5={wp['xwedge5']}  h={np.round(wp['hwedge5'])}  rms={wp['rms']:.1f}")
