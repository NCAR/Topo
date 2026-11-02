"""
rnodes.py  --  line-by-line Python port of subroutine RNODES (ridge_utils.F90)

    from rnodes import rnodes
    out = rnodes(ridge)                 # ridge: 1-D array of length nsw+1
    out['xnodes'], out['hnodes']        # kink positions (1-based, like Fortran) and heights
    out['xwedge'], out['hwedge']        # the 3-point wedge
    out['npeak'], out['nvail'], out['nnode'], out['dcenter'], out['inode_c'], out['tol']

Conventions
-----------
* Positions are returned 1-BASED so they compare directly with xwedge_list /
  xnodes_list written by the Fortran (Ridge_list.dat, rdridgelist).  Use
  xwedge-1 to index a numpy array.
* xnodes is padded with -1 and hnodes with 0.0 beyond nnode, exactly as in
  Fortran, because the Fortran computes tol from the FULL padded hnodes array
  (maxval/minval over nsw+1 entries).  That quirk is reproduced; see note.
* start_at_neighbour=True applies the proposed fix (outward searches start at
  inode_c-1 / inode_c+1 instead of at the central node), so the two
  behaviours can be compared on the same profiles.

Note on tol: Fortran sets tol = 0.25*(maxval(hnodes)-minval(hnodes)) with
hnodes(nnode+1:) = 0.  If all node heights are positive the minimum is the
padding zero, so tol = 0.25*max height, not 0.25*relief.  If the profile is
entirely negative (terr_dev can be), the maximum is the padding zero.
"""
import numpy as np

__all__ = ["rnodes"]


def rnodes(ridge, start_at_neighbour=False):
    ridge = np.asarray(ridge, dtype=float)
    n = ridge.size                      # = nsw+1
    nsw = n - 1
    ns2 = nsw // 2                      # Fortran integer division

    # 1-based helper: r(i) == ridge(i)
    r = lambda i: ridge[i - 1]

    picos = np.zeros(n + 1, int)        # index 1..n used; index 0 unused
    vails = np.zeros(n + 1, int)
    mesas = np.zeros(n + 1, int)

    for i in range(2, nsw + 1):         # do i=2,nsw
        ldh = r(i) - r(i - 1)
        rdh = r(i + 1) - r(i)
        if ldh > 0. and rdh < 0.:
            picos[i] = 1
        if ldh < 0. and rdh > 0.:
            vails[i] = 1
        if abs(ldh) > 0. and rdh == 0.:
            mesas[i] = 1
        if abs(rdh) > 0. and ldh == 0.:
            mesas[i] = 1

    if r(1) < r(2):
        vails[1] = 1
    if r(nsw + 1) < r(nsw):
        vails[nsw + 1] = 1

    kinks = picos + vails + mesas
    kinks[1] = 1
    kinks[nsw + 1] = 1

    xpico = [i for i in range(1, n + 1) if picos[i] == 1]
    xvail = [i for i in range(1, n + 1) if vails[i] == 1]
    xkink = [i for i in range(1, n + 1) if kinks[i] >= 1]
    npeak, nvail, nnode = len(xpico), len(xvail), len(xkink)

    # padded exactly like the Fortran arrays
    xnodes = np.full(n, -1, int)
    hnodes = np.zeros(n, float)
    xnodes[:nnode] = xkink
    hnodes[:nnode] = [r(x) for x in xkink]

    # distance of nearest peak to the nominal centre
    if npeak == 0:
        dcenter = -999
    else:
        dcenter = int(min(abs(x - ns2) for x in xpico))

    # highest peak closest to ns2
    if npeak > 1:
        xcntrpk = [x for x in xpico if abs(x - ns2) == dcenter]
    else:
        xcntrpk = [xpico[0] if npeak == 1 else -1]      # Fortran: xpico(1) = -1 when npeak==0
    if len(xcntrpk) == 2:
        if r(xcntrpk[1]) > r(xcntrpk[0]):
            xcntrpk[0] = xcntrpk[1]
    xc = xcntrpk[0]

    # ---- 3-point wedge --------------------------------------------------
    xwdg0 = xwdg1 = xwdg2 = -999
    tol = 0.25 * (hnodes.max() - hnodes.min())          # over the PADDED array (Fortran quirk)
    inode_c = 0
    for i in range(1, nnode + 1):                        # 1-based node index
        if xnodes[i - 1] == xc:
            inode_c = i

    hn = lambda i: hnodes[i - 1]                          # 1-based node height
    xn = lambda i: xnodes[i - 1]

    if inode_c > 1 and inode_c < nnode:
        xwdg1 = xn(inode_c)

        i0 = inode_c - 1 if start_at_neighbour else inode_c
        for i in range(i0, 1, -1):                        # do i=inode_c,2,-1
            ahq = hnodes[:i - 1].sum() / (i - 1)          # mean of nodes 1..i-1
            dhn = hn(i) - hn(i - 1)
            if hn(i) < ahq and dhn < tol:
                xwdg0 = xn(i)
                break
        if xwdg0 < 0:
            xwdg0 = 1

        i0 = inode_c + 1 if start_at_neighbour else inode_c
        for i in range(i0, nnode):                        # do i=inode_c,nnode-1
            ahq = hnodes[i:nnode].sum() / (nnode - i)     # mean of nodes i+1..nnode
            dhn = hn(i) - hn(i + 1)
            if hn(i) < ahq and dhn < tol:
                xwdg2 = xn(i)
                break
        if xwdg2 < 0:
            xwdg2 = nsw + 1

        xwedge = np.array([xwdg0, xwdg1, xwdg2], int)
        hwedge = np.array([r(xwdg0), r(xwdg1), r(xwdg2)], float)
    else:
        xwedge = np.zeros(3, int)
        hwedge = np.zeros(3, float)

    return dict(xnodes=xnodes, hnodes=hnodes, xwedge=xwedge, hwedge=hwedge,
                npeak=npeak, nvail=nvail, nnode=nnode, dcenter=dcenter,
                inode_c=inode_c, tol=tol, xpico=np.array(xpico, int))


def classify(xwedge):
    """'ok' | 'left' | 'right' | 'both' | 'zero'  for one xwedge triple."""
    a, b, c = xwedge
    if b == 0:
        return 'zero'
    if a == b and b == c:
        return 'both'
    if a == b:
        return 'left'
    if b == c:
        return 'right'
    return 'ok'


def synthetic_profiles(nsw=20):
    """Three test profiles (metres) on nsw+1 points; x = 0..nsw, centre at nsw/2."""
    x = np.arange(nsw + 1, dtype=float)
    g = lambda A, x0, w: A * np.exp(-((x - x0) / w) ** 2)
    return {
        # 300 m bump at the centre, 1500 m mountain with summit at the right edge
        'bump left of big mountain':
            g(1500, 19, 3) + g(300, 10, 2),
        # same bump with a 1500 m mountain on BOTH sides
        'bump between two big mountains':
            g(1500, 1, 3) + g(1500, 19, 3) + g(300, 10, 2),
        # control: 800 m ridge at the centre, flat 900 m plateau from x=15 on
        'ridge with higher plateau on right':
            g(800, 10, 2) + np.where(x > 14, 900.0, 0.0),
    }


if __name__ == '__main__':
    for name, ridge in synthetic_profiles().items():
        print(name)
        for fix in (False, True):
            o = rnodes(ridge, start_at_neighbour=fix)
            print(f"   fix={fix!s:5} xwedge={o['xwedge']} hwedge={np.round(o['hwedge'])} "
                  f"-> {classify(o['xwedge']):5s} (nodes {o['xnodes'][:o['nnode']]}, "
                  f"inode_c={o['inode_c']}, tol={o['tol']:.0f})")
