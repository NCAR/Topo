# Competition #1: how `paintridge2cube` builds the crest web
### `ridge_ana.F90`, function `paintridge2cube`

Julio Bacmeister / Claude, 23 November 2026, branch `jtbDevelBranch-fire`.

This documents the crest-painting competition — the step that turns 327,025
ridgelets in the Ridge_list into the sparse one-cell-wide crest web you see in
`mxdis` on the remap file. It is the single largest selector in the pipeline:
**62% of eligible ridgelets leave no trace in the output at all**, and it
happens before any wedge is laid down.

---

## 1. Where it sits

`remapridge2cube` calls it twelve times, once per field, all with the same
geometry and the same competition (ridge_ana.F90:1924-1970):

```
uniqid   mxdis   hwdth   clngth   anglx   aniso
mxvrx    mxvry   fallq   riseq    hwedge_list(2,:)
clngth (again, with crest_weight=.true.)
```

Each call paints one scalar per ridgelet onto the cube. Because every call runs
the identical competition, **all twelve maps share one skeleton**: they are
nonzero on exactly the same 501,957 cells. Measured: the nonzero set of `mxdis`
is bit-identical to `uniqid > 0`.

Everything downstream depends on this skeleton. `layer_profi` finds each
ridgelet's crest cells with `mxdisC >= 1.0 .and. uniqidC == k`, so the crest web
determines where wedges get stamped. `remapridge2tiles` counts crest cells per
target column for `CLNGT`. A ridgelet that wins no cell here is invisible to
every later stage.

---

## 2. The three arrays

| array | shape | role |
|---|---|---|
| `suba` -> `subr` -> `subdis` | `(-nsw:nsw, -nsw:nsw)` | the **brush**: what gets painted |
| `subblk0` -> `subblk` | same | the **claim**: what competes |
| `AXC`, `QC` | haloed cube, `(.., .., 6)` | the **canvas** and its quality record |

`AXC` is the output field. `QC` is the running record of the best claim seen at
each cell. Both start at zero.

The brush and the claim are **different shapes**, and that is the heart of the
mechanism.

---

## 3. The algorithm, step by step

Loop over `ipk = 1, npeaks` in ascending index order — which is seed order —
skipping any ridgelet with `mxdis(ipk) < 1.0`.

### 3.1 Build the brush: a one-pixel line

```fortran
ncl  = MIN( INT(clngth(ipk)/2), nsw )
suba(:,:) = 0.
suba( 0, -ncl:ncl ) = 1.             ! column 0, rows -ncl..ncl
rotangl = -anglx(ipk)
subr   = rotbyx( suba, 2*nsw+1, rotangl )
subdis = subr * axr(ipk)             ! the value being painted
```

Dimension 1 is across-ridge, dimension 2 is along-crest. So before rotation the
brush is a **single column of 2*ncl+1 cells** — one pixel wide, `clngth` long,
capped at the window. After rotation it lies along the ridge.

`subdis` is the brush scaled by whatever field this call is painting. Note
`axr` may be negative (`fallq <= 0`), which is fine: the competition never looks
at `subdis`.

### 3.2 Build the claim: a rectangle valued at `mxdis`

```fortran
nhw = MIN( INT(hwdth(ipk)/4), nsw/2 )
subblk0(:,:) = 0.
do jw = -nhw, nhw
   subblk0( jw, -ncl:ncl ) = 1.
end do
subblk0 = rotbyx( subblk0, 2*nsw+1, rotangl )
subblk  = subblk0 * mxdis(ipk)
```

The claim is a **rotated rectangle**: `2*ncl+1` long (same as the brush) and
`2*nhw+1` wide, where `nhw` is a **quarter** of the ridgelet's `hwdth`, capped
at `nsw/2`. Every cell of it holds the value `mxdis(ipk)`.

The quarter-width is a tuning choice with alternatives left in the source:

```fortran
!nhw = MIN( INT(hwdth(ipk)/1), nsw/2 )  ! _h0
!nhw = MIN( INT(hwdth(ipk)/2), nsw/2 )  ! _h1
 nhw = MIN( INT(hwdth(ipk)/4), nsw/2 )  ! _h2   <- live
!nhw = MIN( INT(hwdth(ipk)/8), nsw/2 )  ! _h3
```

### 3.3 Paint, where the claim beats the incumbent

```fortran
x0 = NINT( 1.*xspk(ipk) )
y0 = NINT( 1.*yspk(ipk) )
do jj = -NSWx_lim, NSWx_lim
do ii = -NSWx_lim, NSWx_lim
   ip = peaks(ipk)%ip
   if ( in halo bounds ) then
      if ( subblk(ii,jj) >= QC( x0+ii, y0+jj, ip ) )  AXC( x0+ii, y0+jj, ip ) = subdis(ii,jj)
      if ( subblk(ii,jj) >= QC( x0+ii, y0+jj, ip ) )  QC ( x0+ii, y0+jj, ip ) = subblk(ii,jj)
   end if
end do
end do
```

**The test is on the rectangle; the paint is the line.** A ridgelet whose
`mxdis` beats the incumbent takes the cell, writes its brush value there, and
raises `QC` to its own `mxdis`.

The reach is

```fortran
NSWx     = MAX( 4, NSW / RefFac(ipk) )
NSWx_lim = MAX( 1, MIN( nsw, INT( paint_reach_crest*NSWx ) ) )
```

with `paint_reach_crest = 1.0` by default.

---

## 4. The rotation: nearest-neighbour, with a sentinel

`rotbyx` is `rotby4` from `rot.F90` (aliased in `ridge_utils`). It is a
**backward map with nearest-neighbour sampling**:

```fortran
xp(i,j) = x(i)*cos(thetrad) - y(j)*sin(thetrad)
yp(i,j) = y(j)*cos(thetrad) + x(i)*sin(thetrad)
ir = NINT( xp(i,j) + n2 )
jr = NINT( yp(i,j) + n2 )
ar(i,j) = aa(ir,jr)
```

Two consequences worth knowing:

**No interpolation.** A rotated brush is exactly 0 or 1, never a fraction. That
is why the painted `mxdis` equals the Ridge_list `mxdis` to the last bit on
100.0000% of crest cells — if the rotation interpolated, diagonal crests would
carry fractions of their true height.

**Out-of-range maps to `-9999999.9`.** Corners of the output square sample
outside the input square and get the sentinel. In `paintridge2cube` this is
never cleaned, but it is self-protecting: `subblk` in those corners is
`-9999999.9 * mxdis`, hugely negative, so `subblk >= QC` always fails there and
corners can never win a cell. (Contrast `ANISO_ANA_3`, which does clean it:
`where( rot3 < -8000. ) rot3 = 0.`)

**Rasterisation.** A digital diagonal line touches fewer cells than its true
length — one cell per step axis-aligned, but sqrt(2) cells' worth of distance
per step at 45 degrees. This is why every length computed from a crest-cell
count is multiplied by `length_in_square(anglx)`, which is `1/cos` of the angle
from the nearest grid axis and runs 1.0 to 1.414.

---

## 5. Four consequences

### 5.1 A ridgelet suppresses over a band it never paints

The claim is `hwdth/4` wide on each side; the brush is one pixel. So a tall
ridgelet blocks every shorter crest whose line falls within that band, while
painting nothing there itself. A broad, tall ridge clears a corridor around
itself. This is the main reason 62% of ridgelets disappear — the seed field is
dense, and near-duplicate seeds along one physical ridge all fall inside the
winner's rectangle.

### 5.2 The winner erases

`AXC = subdis` is applied across the whole claim rectangle wherever the test
passes, and `subdis` is **zero off the winner's own line**. So when a taller
ridgelet's rectangle covers crest cells already painted by a shorter one, those
cells are overwritten with 0 — not merely outvoted, but erased. Painting is
order-dependent in this sense: a cell's final value depends on the sequence of
claims that passed over it.

### 5.3 Long crests are truncated to the window

`ncl = MIN( INT(clngth(ipk)/2), nsw )`. Any ridgelet with `clngth > 2*nsw`
(84 cells at `nsw=42`, about 280 km) has its painted crest cut to the window,
and both its brush and its claim shrink accordingly.

### 5.4 Ties go to the later index

The test is `>=`, not `>`. Equal-`mxdis` collisions are won by whichever
ridgelet is processed later, and the loop runs in peak-index order, which is
seed order. That tiebreak carries no physical meaning.

---

## 6. What it costs, measured

From `ne30pg3_Sco100_It02_Aw50_Wm1_Yn00_Px20_Gs05_Ws10_scale_pnt_v310_e02`:

| | ridgelets |
|---|---|
| in the Ridge_list | 327,025 |
| eligible (`mxdis >= 1`) | 319,107 |
| **appearing in the crest web** | **120,870** |
| winning zero cells — invisible | **198,237 (62.12%)** |
| surviving the later wedge layering | 96,470 |

Crest cells painted: **501,957**, which is 0.93% of the 54,000,000-cell cube.
For contrast the wedge footprint (`uniqwg > 0`) is 11,773,314 cells, 23x larger.

Survivors are also truncated. Comparing cells won against each ridgelet's own
`clngth`:

```
won / clngth :  median 0.000   p10 0.000   p90 0.703   >= 1 : 3.0%
clngth median 10.0 cells, cells won median 0.0 cells
```

The later layering is a much gentler filter by comparison: of the ridgelets
that won crest cells, `ncrest_vis / cells won` has median 0.947, and 24,400 are
buried entirely.

**So the crest competition is the dominant selector and the layering is a
refinement, not the other way round.**

---

## 7. The quality measure is the OLD height

`subblk = subblk0 * mxdis(ipk)` uses `mxdis` from the node analysis
(`ANISO_ANA_2`'s wedge), not `mxdis_eff`. It cannot be otherwise: this runs at
ridge_ana.F90:1928 and `layer_profi` is not called until :2004, taking `mxdisC`
as an input so it can find each ridgelet's crest cells.

So the entire selection — which 38% of ridgelets exist at all downstream — is
decided on the pre-layering wedge, using the height we now know is inflated for
75% of ridgelets by the lowest-foot re-fit. Nothing from this week's work
feeds back into it.

---

## 8. Disabled and optional paths

**The original reconciliation** is kept under `#if 0`. It selected on closeness
of the feature centre to its diagnosed peak rather than on height:

```fortran
dsq  = 1.0 - SQRT( (xs-xspk)**2 + (ys-yspk)**2 )/nsw
subq = sub1 * dsq
if ( QC(...) <= subq(ii,jj) ) ...
```

Also note it used `x0 = INT(xspk) + 1` where the live path uses
`x0 = NINT(xspk)` — a different cell for the same ridgelet.

**`all_pixels = .TRUE.`** bypasses the quality competition entirely and does a
plain max-wins on the painted value: `if (subdis >= AXC) AXC = subdis`. Not
used by any of the twelve calls.

**`crest_weight` / `crest_length`** change only what the brush carries
(`subdis = subr` versus `subr*axr`); the competition is identical. Only the
`clngth` call at :1958 passes `crest_weight=.true.`.

**`sub1` and `sub11`** are built at the top of the function — a disc of radius
`nsw` and an `nsw/2 x nsw/4` rectangle — but in the live path `sub1` is used
only inside the `#if 0` block and `sub11` only under `#ifdef ROTATEBRUSH`.
Neither affects the default build.

---

## 9. Things worth questioning

1. **The suppression width `hwdth/4` is a tuning constant** with no stated
   derivation, and the alternatives `/1`, `/2`, `/8`, `/16` are commented out
   beside it. It sets how aggressively the web is thinned.
2. **Selection happens on the wrong height** (section 7) — a ridgelet can lose
   its crest to a neighbour whose *wedge* would have been lower.
3. **Erasure is order-dependent** (5.2), and the order is seed index.
4. **Ties are arbitrary** (5.4).
5. **`thinout_list`** is a separate explicit thinning pass in the same module.
   How the two interact has not been examined; they may be doing overlapping
   work.
6. The `CLNGT` path inherits all of this indirectly, which is why it moved with
   every change to `anglx`, `xspk`, `yspk` this month.

---

## 10. Reproducing the numbers

`/glade/derecho/scratch/juliob/wedge_width_diag/` (scratch is purgeable):

- the crest-web counts: see `make_mxdis_eff_cube.py`, which loads `uniqid`,
  `uniqvis` and `mxdis` from the remap file and the Ridge_list alongside
- `mxdis_eff` painted on the crest web, derived without a rerun:
  `python make_mxdis_eff_cube.py <case>/output` writes
  `mxdis_eff_cube.npy` (visible crests) and `mxdis_eff_cube_allcrests.npy`
  (all crests, buried = 0), both `[panel, j, i]` float32

As of 23 Nov the code also writes this field directly as `mxdeffC`, a trailing
record on unit 911, readable as `mxdeff` via `rdremap.py`. That path is
compiled but **not yet verified against a run**.
