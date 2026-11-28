# Development log: adaptive windows to layered painting
### 13–22 November 2026, branch `jtbDevelBranch-fire`

Julio Bacmeister / Claude. Written 22 Nov 2026.

This covers the work from the Gaussian ridge-scale commit through the first
complete layered-painting implementation, and pairs each case run to the code
state that produced it. Companion documents in this directory:
`adaptive_window_handoff_memo.md` (the 16–19 Nov diagnosis) and
`mxdis_eff_inflation_report.{md,pdf}` (the height-inflation study, 21 Nov).

---

## 1. Where things stand

The ridge pipeline now paints wedges **layered widest-first** instead of
max-wins, and every per-ridgelet quantity that reaches `_tiles.nc` is measured
on the **same visible pixels at the same instant**:

| tiles field | source | what it measures |
|---|---|---|
| `MXDIS` | `mxdis_eff` | larger of the two visible flank drops |
| `RISEQ`/`FALLQ` | `riseq`/`fallq` | **still the old ANISO_ANA_2 wedge** (not yet moved) |
| `HWDTH` | `hwdth` | **still the old ANISO_ANA_2 wedge** (kept deliberately) |
| `HWDTH_EFF`, `_RISE`, `_FALL` | `hwdth_eff*` | visible footprint area / visible crest length |
| `CLNGT` | `ncrest_vis` | visible crest cells, x `length_in_square` |

Tagged `jtb-layered-painting-v1` at commit `02b6468`, pushed.

**The headline measurement of the week** is that the single largest gain came
from somewhere we were not looking: putting the ANISO_ANA_3 **refined node
list** into `nodos` improved that reconstruction by 13% *and* raised its
coverage by half, simultaneously. The wedge work improved `wedgo` by about 8%
against the pre-layering state.

---

## 2. Timeline, commits, tags and cases

| date | development | commit / tag | case run |
|---|---|---|---|
| 11-13 | flexible wedge method | `0efcb43` **tag `jtb-ridge-opts-v1`** | — |
| 11-14 | Gaussian ridge scale in index space | `07b487a` | `v207_e05` |
| 11-16 | adaptive window `c=5`, single pass | — | `v300_e01` |
| 11-17 | `--ridge_iter`; ANISO_ANA_2 window-resize fix | `f399054` | `v300_e02`, `v301_e01`, `v301_e02` |
| 11-17 | 5x5 log-smoothing of `gauss_scale` | (same) | `v302_e01`, `v302_e02`, `v302_e03` |
| 11-18 | `--ynudge` (shift-B damping) | `8726773` | `v303_e01`, `v303_e02` |
| 11-18 | ANISO_ANA_3 touch-up; `--profile_extend`; `--gauss_smooth`; tags always emitted | (same) | `v304_e01` |
| 11-19 | (analysis only: node/refine study) | — | `v304_e02` |
| 11-21 | cube-edge fix for ridge profiles | `b683b45` | — |
| 11-21 | lowest-foot wedge: `find_kinks`, `refine_nodes`, `find_feet`, `paint_wedge3` | `973c05f` | `v305_e01` |
| 11-21 | `layer_profi` layered painting; `uniqvisC` output | `8e2b3a9` **tag `jtb-adaptive-window-v1`** | `v306_e01` |
| 11-22 | Ridge_list additions; refined nodes into `nodos`; visible-flank heights | `aab1153` | `v307_e01` |
| 11-22 | `--wedge_search` (tag `_Ws##`) | (same) | `v308_e01` |
| 11-22 | `crestv` fix; `pshift_rdg` diagnostic | `02b6468` | `v309_e01` |
| 11-22 | `hwdth_eff*`; Danakil filter; `HWDTH_EFF*` | `02b6468` **tag `jtb-layered-painting-v1`** | `v310_e01` (running) |

Case names encode their settings, which is the point of the tagging convention:
`{ogrid}_Sco{scale}_It##_Aw##_Wm#_Yn##_Px##_Gs##_Ws##_{tag}`.

---

## 3. What each case tests, and what it showed

### The adaptive-window series (16–19 Nov)

**`v300_e01`** — first adaptive run, `c=5`, one pass. Showed the "ribbing":
wedge widths varying along a single ridge, clearest over Baja California.
Diagnosis: **noisy per-seed windows, not truncation**. Neighbour pairs sharing
a window differ in width by 1.0 cell; pairs with different windows by 5.0.
`sigwin_rdg` differs by a median 22% between adjacent seeds.

**`v300_e02`** — `ridge_iter=2`. Fixed the aggregate (neighbour `|sigwin diff|`
median 0.222 -> 0.000) but the visible ribbing survived, and only 29.9% of
ridges converged.

**`v301_e01`** — added the **ANISO_ANA_2 window-resize fix**: each pass sized
its window at `(icen,jcen)` but `ANISO_ANA` then shifted to `(ispk,jspk)` by a
median 3.6 cells, and `ANISO_ANA_2` — which builds the profile and fits the
wedge — ran there with the *old* window. Re-reading `gauss_scale_km(ispk,jspk)`
before `ANISO_ANA_2` fixed it. Best `wedgo` on the "all" metric at that point.

**`v301_e02`, `v302_*`, `v303_*`** — the `ridge_iter=3`, 5x5-smoothing and
`ynudge` scan. Findings: iterating past 2 loses coverage; the 5x5 smoothing
costs ~3.7 m on the metric; `ynudge=0` (shift B off) is the best of the three
`ynudge` settings and recovers coverage.

**`v304_e01/e02`** — ANISO_ANA_3 running, passive (wrote only
`rdg_profiles_x`). The extended profiles were the input to the node/foot study.

### The wedge and painting series (21–22 Nov)

**`v305_e01`** — Julio's lowest-foot wedge replacing the painted wedge shapes.
Coverage jumped (paint 24.80%) but shape got worse. `_tiles.nc` still wrote the
*node* `mxdis`, so the re-fitted wedge had not yet reached the product.

**`v306_e01`** — `layer_profi`, layered painting. First run where `MXDIS` comes
from the painting (`mxdis_eff`). 30,513 of 120,852 ridgelets fully buried
(25.2%); mean effective/original height 0.817. This is the run the
inflation report is based on.

**`v307_e01`** — first run with the Ridge_list additions, the refined nodes in
`nodos`, and the visible-flank `mxdis_eff`. Feet still unrestricted
(max \|foot offset\| 63). Not analysed in detail; superseded within hours.

**`v308_e01`** — adds `--wedge_search=1.0`. Foot offsets 100% within `nswx`;
median max-offset 26 -> 15 cells. Step-1 inflation ratio > 2 fell 8.94% ->
3.71%. **This run surfaced the `crestv` bug** (see section 6).

**`v309_e01`** — `ridge_iter=3`, plus the `crestv` fix and `pshift_rdg`.
Convergence improved sharply but the product did not (section 5).

**`v310_e01`** — `hwdth_eff*` and the Danakil filter. **Running at time of
writing; not yet analysed.**

---

## 4. The code changes in detail

### 4.1 Window sizing (`ridge_ana.F90`, `find_ridges`)

`nswx = MAX(4, MIN(nsw, NINT(cadapt*sgw/grid_length_scale)))` with
`cadapt = adapt_c(MIN(iter_ridge,nadapt))`. Verified exactly, 100.0000% of
319,634 ridgelets at `c=5.0`.

**`grid_length_scale = 3.3358477993 km`** at `ncube=3000` — it is
`(pi/2)/ncube * earth_radius` with **earth_radius = 6371.0**, not 6371.22.

**The `nsw=42` cap binds for 9.9% of ridgelets**, taking over at sigma >=
28.02 km. Ladder rungs 37/50/75 km all run into it, so the largest features are
NOT sized adaptively. This matters: it is why `--wedge_search` could not help
the Danakil cases, and why the Danakil filter was needed.

### 4.2 `ANISO_ANA_3` (touch-up pass)

Runs once per ridgelet after the `ridge_iter` loop, re-centred on the final
`(xspk,yspk)`, with the x reach widened to `profile_extend*nswx` while the
y-average stays over `nswx+1` rows. Capped at `PSW = NINT(1.5*nsw)` = 63.
CLI `--profile_extend` (default 2.0, `<=1` disables, tag `_Px##`).

It now also builds the **lowest-foot wedge**: `find_kinks` -> `refine_nodes` ->
`find_feet` -> `xwedge3_list`/`hwedge3_list`/`feet3_info`, and with
`paint_wedge3` rebuilds `hwedge_o`/`hwedge_x`, the shapes the painting uses.

Added 22 Nov: `refine_nodos` rebuilds `hnodes_o` from the refined node list, so
`nodos` sits on the same extended, refined profile as `wedgo`. `hnodes_x`
(the `NODES` field) is deliberately left on the old basis.

Added 22 Nov: `--wedge_search` and the Danakil filter (section 4.4).

### 4.3 `layer_profi` (layered painting)

Ridgelets painted **widest wedge first**. Pass A collects the ridgelet's own
highest value at every cell it touches (`kmax`), so its own neighbouring stamps
never count as "beneath" it; pass B compares with the surface painted before it.

`mxdis_eff` is now the **visible-flank** measure, to Julio's specification:

```
vis = (sv <= UNPHALF) .or. (v > sv)
if (vis) then
   surf = v ; owner = k
   lowvis(side) = MIN( lowvis(side), v )      ! lowest still-visible pixel
...
riseq_eff(k) = crestv - lowvis(1)             ! both POSITIVE
fallq_eff(k) = crestv - lowvis(2)
mxdis_eff(k) = MAX( riseq_eff(k), fallq_eff(k) )
crestv       = MAXVAL( hwedge_o( sL(k):sR(k), k ) )
```

**The side is tracked without any angle arithmetic.** `suba` is declared
`(-nsw:nsw,-nsw:nsw)` and its first index IS the signed profile offset, so a
third stamp holding that offset is rotated through the *same* `rotbyx` call as
the wedge; the label then travels with the value whatever the rotation does.
Validated: rise/fall win 49.57%/49.05% — no directional bias.

`hwdth_eff` (22 Nov) uses the same loop: visible footprint pixels each side,
crest column split evenly via its own `kside==0` count, divided by the **true**
visible crest length `ncv_paint * length_in_square(anglx)` — not the raw cell
count, which would under-report diagonal ridges by up to sqrt(2).

Note `riseq_eff`/`fallq_eff` are both **positive drops**, unlike `ridgescales`'
signed `fallq <= 0`.

### 4.4 `--wedge_search` and the Danakil filter

`--wedge_search` (default 1.0, tag `_Ws##`, `<=0` disables) limits the foot
search to `+/- NINT(wedge_search*nswx)` cells, carrying the window's two end
cells as nodes so a side with no node of its own still has somewhere to stop
(`reason = end`).

**Danakil filter** (module settings `danakil_span = 50.0`,
`danakil_cells = 20`; not CLI by choice): if the profile spans less than
`danakil_span` metres over `+/-danakil_cells` of the centre, the foot search is
capped at `danakil_cells` **regardless of `gauss_scale`**. Self-guaranteeing —
with both feet inside that band, the relief cannot exceed the span either.
Predicted to fire on 5,802 visible ridgelets (6.2%), of which 1,212 actually
change; total `MXDIS` effect 0.047%. An aesthetic fix, not an energetics one.

---

## 5. Results

### Reconstruction metric, `error[|error|>10].std()`, ne30pg3 Sco100

| run | `wedgo` all | painted | paint% | `nodos` all | painted | paint% |
|---|---|---|---|---|---|---|
| Sept `v202` baseline | 200.22 | 208.93 | 19.92 | 186.28 | 195.05 | 23.07 |
| `v300_e01` 1 pass | 182.25 | 178.92 | 17.80 | — | — | — |
| `v301_e01` +resize fix | 181.16 | 171.71 | 16.51 | — | — | — |
| `v301_e02` It03 | 183.36 | 171.38 | 15.20 | — | — | — |
| `v302_e01` +smoothing | 184.82 | 173.39 | 15.96 | — | — | — |
| `v303_e01` `Yn00` | 183.63 | 172.59 | 16.96 | — | — | — |
| `v306_e01` layered | 193.54 | 203.04 | 24.80 | 175.38 | 167.85 | 18.76 |
| **`v308_e01`** `Ws10` | **178.59** | 184.15 | 21.92 | 153.77 | 159.79 | 27.95 |
| `v309_e01` `It03` | 178.91 | 184.50 | 21.50 | **152.56** | 158.45 | 27.65 |

Read the two columns together: "all" counts an unpainted cell as `dev-0`, so it
rewards coverage; "painted" isolates shape. They disagree about which run is
best, which is why the eyeball comparison is the tiebreaker.

**The refined nodes in `nodos` are the single biggest win**: 175.38 -> 152.56
(-13%) with coverage 18.76 -> 27.65%. Improving metric and coverage together is
rare here — they have moved oppositely in almost every other comparison.
`nodos` now clearly beats `wedgo` (152.6 vs 178.9) with more coverage. The
refined polyline simply represents terrain better than a 3-point wedge.

### `--ridge_iter=3` converges ridgelets but buys nothing in the product

| | converged | cap, moving | panel edge | two-cycle |
|---|---|---|---|---|
| v308 `It02` | 46.56% | 48.31% | 0.27% | 2.60% |
| v309 `It03` | 65.95% | 25.01% | 0.29% | 6.48% |

`|pshift| > 1` fell 10.53% -> 5.82%; peak within one cell 96.75% -> 98.39%.
But `wedgo` got marginally **worse** (178.59 -> 178.91) and `nodos` marginally
better, with slightly less coverage in both. Two-cycles rose to 6.48% (~21k
ridgelets that oscillate and will never converge) and `max |pshift|` stays 21,
the profile-edge/sloping-terrain class that iterating cannot fix.
**Recommendation: do not spend more passes on this.**

### Height inflation

See `mxdis_eff_inflation_report.pdf` for the full study. Summary: the inflation
is created by the **lowest-foot wedge re-fit**, not by the layering.

| step | ratio | median | max | % > 1 |
|---|---|---|---|---|
| 1. wedge re-fit | relief3 / `mxdis` | 1.105 | 113.67 | 75.13 |
| 2. layering | `mxdis_eff` / relief3 | 0.875 | **1.00** | **0.00** |

Zero ridgelets exit `layer_profi` taller than their own lowest-foot wedge —
step 2 is a strict contraction. `--wedge_search` roughly halved step 1
(ratio > 2: 8.94% -> 3.71% -> 2.88% at It03).

---

## 6. Things I got wrong, and the corrections

Recorded because several cost a run or a wrong conclusion.

**Compared against the wrong baseline.** Used `Rw050_v202_rch1p0_e01` (ratio
0.5) instead of `v202_rch1p0_e01` (ratio 1). Julio caught it. The adaptive
scheme wins by 9.5% / 18% against its true baseline.

**Attributed the inflation to layered painting.** The report's first version was
titled for `mxdis_eff`. Julio read the figures correctly: the lowest-foot wedge
creates it and the layering only ever lowers. Report rewritten.

**`crestv = hwedge_o(PSW+1,k)`.** This is the wedge value at the frame
*centre*, not at its peak. The peak node is off-centre for 31% of ridgelets
(median understatement 20.7 m, p90 100 m, max 946 m; 35 cases had both feet on
one side of centre, where `hwedge_o(PSW+1)` reads 0 and one came out 4x too
high). Fixed to `MAXVAL( hwedge_o( sL(k):sR(k), k ) )`. **v308's `MXDIS` carries
this bug; v309 onward does not.**

**Claimed `rdg_profiles` and `rdg_profiles_x` were different objects.** They are
bit-identical where they overlap — 100% — once aligned. The alignment differs by
one cell with the **parity of `nswx`** (even -> shift 0, odd -> shift -1), and I
misread that as a real difference. Julio was right.

**Reader record order.** `pshift_rdg` is written right after `sigwin_rdg`, so
its `OPTIONAL_RECORDS` entry must sit there too. I placed it after the wedge3
entry and the reader rejected the file. **Rule: the reader's optional-record
order must match the write order exactly.**

**Used earth_radius = 6371.22.** The code uses 6371.0. This made my `nswx`
reconstruction miss on 186 borderline cases until corrected.

---

## 7. Diagnostics reference

### `Ridge_list_*.dat` optional records, in file order

All are trailing and size-identified, so older files still read. Requires the
matching `myPythonTools/TopoAna/rdridgelist.py`.

| record | shape | meaning |
|---|---|---|
| `lonc`, `latc` | (npeaks) | crest-point lon/lat, **radians** |
| `niter_rdg`, `istop_rdg` | (npeaks) | passes done; 1 converged, 2 cap reached still moving, 3 panel edge, 4 two-cycle |
| `xseed`, `yseed` | (npeaks) | original seed cell |
| `sigwin_rdg` | (npeaks) | raw **unscaled** Gaussian sigma, km, read after shift A but BEFORE shift B |
| `pshift_rdg` | (npeaks) | unapplied across-crest shift. 0 (or +/-0.5, odd `nswx`) = converged; `nswx`/2 = max on the profile edge |
| `xwedge3_list`, `hwedge3_list`, `feet3_info` | (3),(3),(5) | lowest-foot wedge: foot/peak/foot **offsets from the centre**, heights, and `m3` + reason L/R (1 min, 2 flat, 3 end) + blocked L/R |
| `ncrest_vis` | (npeaks) | visible crest cells; 0 = fully buried |
| `mxdis_eff` | (npeaks) | `MAX(riseq_eff, fallq_eff)` |
| `riseq_eff`, `fallq_eff` | (npeaks) | visible drop from crest, each side. **Both positive** |
| `hwdth_eff`, `_rise`, `_fall` | (npeaks) | visible width in **cube cells** (tiles value is this x `grid_length_scale`) |

### Reading `rdg_profiles_x`

```python
PSW = int(List.PSW)                        # 63; centre, 0-based
m3  = int(List.feet3_info[ipk, 0])         # half-width ANISO_ANA_3 wrote
off  = np.arange(-m3, m3 + 1)
dx   = (np.pi/2)/3000 * 6371.0             # 3.3358478 km
plt.plot(off*dx, List.rdg_profiles_x[ipk, PSW + off])
```

Using `NSWx_diag` as the half-width cuts the profile in half; using the full
127 points shows false zero tails beyond `+/-m3`.

### Matching Fortran indexing in Python

Fortran uses `NINT`. Use `int(np.floor(x + 0.5))` — **not** `int()` (truncates)
or `round()` (banker's rounding). `gauss_scale` is a **1-D** netCDF variable;
reshaped `(6,ncube,ncube)` it indexes as `[panel, j, i]`.

### Recovering the pending shift by hand

```python
n     = int(List.NSWx_diag[ipk])
xshft = np.argmax(List.rdg_profiles[ipk, 0:n+1]) - n/2.0
```
Units are cells along the across-ridge profile; the cube-space move is
`xshft*cos(anglx)` in i and `-xshft*sin(anglx)` in j.

---

## 8. Open questions

1. **`RISEQ`/`FALLQ` and `HWDTH` in the tiles still describe the ANISO_ANA_2
   wedge** while `MXDIS` and `CLNGT` describe the layered painting. The
   `_EFF` replacements exist for width; the identity
   `mxdis = max(riseq, -fallq)` that held exactly (100.000%) no longer holds in
   `_tiles.nc`. Decide whether to switch them, and mind the sign convention —
   those are CAM-facing fields.
2. **Two-cycle ridgelets (6.48%, ~21k)** oscillate between two centres and will
   never converge. No treatment yet.
3. **Profile-edge maxima** (`|pshift| = nswx/2`) are monotonic/sloping terrain
   with no interior peak. `ANISO_ANA` already flags them via `npks`; they are
   about half of the large pending shifts.
4. **The `nsw=42` cap** means the adaptive window is effectively off for sigma
   >= 28 km (9.9% of ridgelets). Scaling the foot search off `sigwin_rdg`
   instead of `nswx` would sidestep it.
5. **`hnodes_x` (`NODES`) is still on the old unrefined basis** while `hnodes_o`
   (`NODOS`) moved. The two are no longer comparable.
6. **Does `nodos` beating `wedgo` by 17% change the plan?** The refined polyline
   is a better reconstruction; whether that should feed the tiles is an open
   design question.
7. **GW_UnitTest** has not been run against any of this. The drag effect of the
   22–29% reduction measured back on 21 Nov is unverified.

---

## 9. Tooling

Scripts in `/glade/derecho/scratch/juliob/wedge_width_diag/` (**scratch is
purgeable — copy out anything that matters**):

| script | purpose |
|---|---|
| `recon_error.py` | the `dev - wedgo` / `dev - nodos` metric; takes case `output/` dirs as arguments |
| `recon_table.py` | the same across a list of cases |
| `wedge_width_diag.py` | the original ribbing diagnosis |
| `inflation_cases.py` | height-inflation scan by absolute band, with locations |
| `attribution.py` | splits the net height ratio into wedge-refit and layering steps |
| `draw_outlier.py UID` | ASCII profile with both wedges overlaid |
| `make_figs.py` | vector figures for the inflation report |
| `build_report_pdf.sh` | rebuilds `mxdis_eff_inflation_report.pdf` |
| `crest_check.py` | `hwedge3[1]` vs `hwedge[1]` comparison |
| `dig_diag.py`, `crest_dig.py` | where the painted surface sits below `dev` |
| `pending_shift.py` | pending shift A, by `istop_rdg` |
| `flat_then_drop.py` | the Danakil-class search |
| `v308_check.py`, `v309_conv.py` | per-run validation |
| `nswx_formula.py` | verifies the window formula against `sigwin_rdg` |

PDF toolchain on Casper: no LaTeX, so `pandoc -> groff -ms -Tpdf`, and
`groff -U` is required for `.PDFPIC` figure inclusion.

Backups of `ridge_ana.F90` through the 22 Nov edits are in the same folder as
`ridge_ana.F90.bak_nov22{,b,c,d,e,f,g}`, in chronological order.

---

## 10. What to check when `v310` lands

- `HWDTH_EFF_RISE` should be large where `riseq_eff` is large. If a ridgelet has
  a tall visible rise flank but near-zero rise width, the side bookkeeping in
  `layer_profi` is inconsistent between the height and width accumulators.
- The Danakil filter log line should report ~5,800 ridgelets.
- `uniqid 41028` should drop from `MXDIS` 157.06 m to under 1.4 m, and `42759`
  from 478.6 m to under 3.0 m.
- `HWDTH_EFF` well below `HWDTH` for ridgelets with a buried flank — expected,
  by design, but worth seeing the distribution.
