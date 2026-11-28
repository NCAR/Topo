# What the quantities in `_tiles.nc` actually are
### extensive, intensive-broadcast, and per-tile local

Julio Bacmeister / Claude, 27 November 2026, branch `jtbDevelBranch-fire`.
Measured on `ne30pg3_Sco100_It02_Aw50_Wm1_Yn00_Px20_Gs05_Ws10_scale_pnt_v310_e02`.

The per-tile fields in `_tiles.nc` are dimensioned `(nrdg, ncol)` and look
alike, but they behave in three quite different ways under aggregation. Mixing
them up is easy and the consequences are silent. This note sets out which is
which, with the identities verified against a real file.

**Index convention used throughout:** `i` is the target column index
(`ncol = 48600`), `ir` is the tile slot index (`nrdg = 46`). A slot is filled by
the importance sort, so the *same* slot holds *different* ridgelets in different
columns. The ridgelet occupying a slot is `UNIQID(ir,i)`.

---

## 1. The three categories

| kind | fields | how to aggregate to a ridgelet |
|---|---|---|
| **extensive** | `CLNGT`, `WGHTS` | **sum** over the slots holding that ridgelet |
| **intensive, broadcast** | `MXDIS`, `HWDTH`, `HWDTH_EFF`, `HWDTH_EFF_RISE`, `HWDTH_EFF_FALL`, `CLNGT_RDG`, `ANGLX`, `ANGLL`, `ANISO`, `ANIXY`, `RISEQ`, `FALLQ` | **take any one** -- they are identical in every slot |
| **per-tile local** | `latc`/`lonc`, `latw`/`lonw` | **weighted mean**, see section 4 |
| identity | `UNIQID` | = `ipk` in the Ridge_list, 0 = empty slot |

Separately, the `(ncol)` fields -- `PHIS`, `LANDFRAC`, `SGH`, `SGH30`,
`LANDM_COSLAT`, `area`, `lat`, `lon`, `ISOVAR`, `ISOWGT`, `GBXAR` -- are
properties of the column itself and have nothing to do with individual
ridgelets.

---

## 2. Extensive: `CLNGT` and `WGHTS`

### `CLNGT` -- in-column crest length, km

```fortran
clnin_tiles(i,ir) = LnObject(i,ir) * grid_length_scale &
                                   * length_in_square( anglx_tiles(i,ir) )
```

In words: **the true length, in km, of the portion of this ridgelet's VISIBLE
crest lying inside this column.** Three factors:

- `LnObject` -- the count of this ridgelet's crest cells falling in column `i`,
  taken from `uniqvisC` when layered painting is on (`lvis`), `uniqidC`
  otherwise;
- `grid_length_scale` = **3.3358477993 km** at `ncube=3000`, from
  `(pi/2)/ncube * earth_radius` with `earth_radius = 6371.0`;
- `length_in_square(anglx)` = `1/cos` of the angle from the nearest grid axis,
  1.0 to 1.414, correcting for a rasterised diagonal line touching fewer cells
  than its true length.

**Verified from the file alone.** Dividing out the two constants returns an
exact integer cell count for **99.98%** of tiles with `CLNGT > 0` (109,339 of
them), median 3 cells, max 35.

**It partitions exactly.** Summing `CLNGT` over the slots holding one ridgelet
reproduces `CLNGT_RDG` for **100.00%** of the 96,470 ridgelets in the file, to
1e-6. Totals: 1,522,369 km either way, nothing missing. So no ridgelet is
dropped from a column where it holds visible crest -- the 46-slot truncation
never bites on a crest-bearing tile, because `tkey = mxdis_eff * clnin` puts
`clnin = 0` slots at the bottom.

Mean slots per ridgelet: **2.01**.

### `WGHTS` -- area of the column covered by this ridgelet's wedge, m^2

```fortran
wghts_tiles(i,ir) = area_target(i) * WtObject(i,ir) / MAX(1,idcoun(i))
```

`WtObject` counts the cube cells of this ridgelet's wedge in the column,
`idcoun` counts all cube cells in it, so this is the column area times the
covered fraction. Because the layered painting gives every cell exactly one
owner (`uniqwgC`), the wedges **partition** the painted part of a column rather
than overlapping.

**Verified:** summing `WGHTS` over all slots in a column never exceeds the
column area -- maximum ratio exactly 1.0000, zero columns above it. (66.9% of
columns sum to zero, i.e. no ridge wedge touches them at all.)

---

## 3. Intensive, broadcast

These are properties of the ridgelet, written identically into every slot that
ridgelet occupies. There is nothing per-tile about them; the repetition is a
storage convention.

`CLNGT_RDG` is the clearest case: it is literally the broadcast of the sum in
section 2, `ncrest_vis * length_in_square * grid_length_scale`. It can be
recovered from `CLNGT` with a `max`-reduction rather than a lookup.

**A mismatch currently lives in this group.** `MXDIS` is `mxdis_eff`, the
visible-flank height from the layered painting, while `HWDTH`, `RISEQ` and
`FALLQ` in the same file are still the `ANISO_ANA_2` wedge. The identity
`max(RISEQ, -FALLQ) = MXDIS`, which held to **100.000%** before the layering
work, no longer holds in `_tiles.nc`. `HWDTH_EFF*` are the layered companions
to `HWDTH`, but see section 5.

---

## 4. Per-tile local: the two centroid pairs

These vary from slot to slot for one ridgelet -- so they are **not** broadcast
-- but they are also **not** additive. Summing centroids is meaningless.

### `latc` / `lonc` -- the crest centroid

```fortran
uniqidMap(i) = uniqvisC( idxmap(i,j) )            ! visible crests, as CLNGT
xpack = pack( xcoordMap, (uniqidMap == ThisRidge) )
xloid_tiles(j,ir) = sum(xpack) / npack            ! plain mean of cell coords
call CubedSphereRLLFromABP(alph, beta, pobj, lono, lato)
```

The **unweighted centroid of this ridgelet's visible crest cells inside the
column**, converted from cube `(alpha, beta, panel)` to lon/lat. Same
population `CLNGT` counts.

### `latw` / `lonw` -- the wedge centroid

```fortran
hpack = pack( MAX(wedgoMap,0._r8), (uniqwgMap == ThisRidge) )
xwoid_tiles(j,ir) = sum( xpack*hpack ) / sum(hpack)
```

A **different** population and a **different** weighting: packed on `uniqwgC`
(the wedge footprint, 23x larger than the crest web) and weighted by the painted
height `MAX(wedgo,0)`. So `latc`/`lonc` is where the crest sits; `latw`/`lonw`
is where the wedge's mass sits.

### Aggregating them

Because the per-column value is an unweighted mean over `npack` cells, and
`CLNGT` is proportional to that same `npack`, the whole-ridgelet crest centroid
is the `CLNGT`-weighted mean:

```
x_ridgelet = SUM_i CLNGT(i,ir)*xloid(i,ir) / SUM_i CLNGT(i,ir)
```

and the wedge centroid is the `WGHTS`-weighted mean of `xwoid` -- except that
`xwoid` is already height-weighted, so that combination is only approximate
unless the painted height is uniform.

**Two cautions.**

Do the averaging in cube `(alpha,beta)` space if you can, not in lon/lat. The
code converts the centroid of the cells to lon/lat, which is not the same as the
centroid of their lon/lat, and the difference grows near the panel corners and
across the dateline.

Each ridgelet lives on a single panel (`MyPanel`), so a lon/lat mean will not
straddle a panel seam, but a ridgelet near +/-180 longitude will still average
badly without wrapping.

---

## 5. Pending: `HWDTH_EFF*` is biased for short crests

`HWDTH_EFF`, `HWDTH_EFF_RISE` and `HWDTH_EFF_FALL` are in the file and are
intensive-broadcast like `HWDTH`, but **their definition is not yet right** and
they should not be used quantitatively.

They are an area over a length: visible footprint pixels on each side of the
crest, divided by the visible crest length,

```fortran
clen = REAL( ncv_paint, r8 ) * length_in_square( anglx(k) )
hwdth_eff_rise(k) = ( nvis(1) + 0.5*nvis0 ) / clen
```

The stamp laid at each crest cell is **three cells deep** along the crest
(`jw = -1, 0, 1`), so a crest of L cells paints a footprint about **L+2** long:

```
crest cells at along-crest positions 1,2,3
  stamp at 1 covers  0 1 2
  stamp at 2 covers    1 2 3
  stamp at 3 covers      2 3 4
  union              0 1 2 3 4      -> 5 positions, not 3
```

The numerator describes the L+2 object; the divisor `ncv_paint` is L. So the
width is too large by roughly `(L+2)/L`, and **the median `ncrest_vis` is only
3 cells**, so most ridgelets sit in the badly biased regime:

| `ncrest_vis` | n | median `hwdth_eff` / lowest-foot span | `(L+2)/L` |
|---|---|---|---|
| 1 | 25,222 | **1.719** | 3.00 |
| 2-3 | 29,813 | 1.287 | 2.00 |
| 4-7 | 27,389 | 1.095 | 1.40 |
| 8-15 | 11,896 | 1.039 | 1.20 |
| 16-31 | 2,043 | 1.017 | 1.11 |
| 32+ | 107 | 1.024 | 1.06 |

Converging to 1.0 for long crests, as it should. Overall median 1.197. (The
measured values sit below the naive `(L+2)/L` because rotation and the `lim`
clipping mean the stamp is not a full rectangle and adjacent stamps do not
extend the union by exactly two.)

This is why `HWDTH_EFF` came out about 2x `HWDTH` in the file -- median 95.5 km
against 46.7 km. Part of that is real (the lowest-foot wedge is genuinely wider
than the `ANISO_ANA_2` wedge) and part is this bias.

**Three ways to fix it, to be decided:**

1. **Centre row only.** Accumulate area from `jw == 0` alone, so numerator and
   denominator both describe a one-cell-deep crest line. No correction factor,
   no contiguity assumption, no extra storage; two-thirds less area sampled, so
   noisier for short crests, but *unbiased* rather than approximately corrected.
2. **Divide by `ncv_paint + 2`.** One token. Assumes the visible crest cells are
   contiguous along the crest; if a ridgelet is buried in the middle the true
   extent is `(last-first+1)+2` and this under-corrects.
3. **Measure the extent.** Carry a second offset stamp holding the along-crest
   coordinate (dimension 2 of `suba`, exactly as `subs` carries dimension 1 for
   the side), rotate it in the same `rotbyx` call, and take `max-min+1` over the
   visible footprint. Symmetric with how the side is already tracked, no
   contiguity assumption, at the cost of another per-cell coordinate.

A residual of about 2% at long crests remains in every case -- the honest
`2w+1` versus `2w` cell-counting offset.

---

## 6. Practical notes

**More than half the occupied slots carry zero crest.** 124,422 slots have
`UNIQID > 0` but `CLNGT = 0`, against 109,339 with `CLNGT > 0`. These are
columns a ridgelet's *wedge* reaches but its crest does not -- unsurprising,
since the wedge footprint is 23x the crest web. Anything averaged over slots is
dominated by them.

**`MXDIS` is `mxdis_eff`, not the node-analysis `mxdis`.** The override is
`if ( lvis ) mxdis_tiles(i,ir) = mxdis_eff(ThisRidge)` with
`lvis = layer_wedges .and. allocated(uniqvisC)`, and `layer_wedges` defaults
`.TRUE.`. The node `mxdis` survives only in the remap `.dat` crest map, which is
painted before `layer_profi` runs -- it matches the Ridge_list `mxdis` on
100.0000% of crest cells and `mxdis_eff` on 24.5%.

**`CLEXT` is written, but to `Ridge_tile_map.dat`, not the netCDF.** That file
(unit 911, reopened in `remapridge2tiles`) carries `idxmap`, `idcoun`,
`NumObjects`, `MyObject`, `WtObject`, `LnObject` and all the tile arrays
including `clext_tiles` and `clngt2`. It is a different file from the
`remap_*.dat` cube fields.

`CLEXT` measures a different thing from `CLNGT`:

```fortran
clext_tiles(j,ir) = sqrt( (maxval(xpack)-minval(xpack))**2 &
                        + (maxval(ypack)-minval(ypack))**2 )
```

the **diagonal of the bounding box** of the visible crest cells in the column --
straight-line end to end, in raw cube cells -- where `CLNGT` is the **path
length** along the rasterised crest in km. `CLEXT` needs no
`length_in_square`, because `sqrt(dx^2+dy^2)` already measures true separation;
`CLNGT` counts cells along a path, which is why it does. They agree for a
straight unbroken crest, `CLNGT > CLEXT` when the crest bends, and
`CLEXT > CLNGT` when burial leaves gaps, since the bounding box spans them.
That ratio is a usable fragmentation diagnostic, and is presumably why `clngt2`
accumulates `clext_tiles` over columns as an alternative whole-ridge length.
