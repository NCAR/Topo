# Where ridgelet heights get inflated: the lowest-foot wedge,
not the layered painting

Julio Bacmeister / Claude, 2026-11-21
Run analysed: `ne30pg3_Sco100_It02_Aw50_Wm1_Yn00_Px20_Gs05_scale_pnt_v306_e01`
(layered painting, `layer_profi`; 90,348 ridgelets present in `*_tiles.nc`)

---

## 1. Summary

Ridgelet heights in `_tiles.nc` are now written as `mxdis_eff`, produced by
`layer_profi`. A substantial minority of them are much larger than the node
analysis said: **38.7%** exceed `mxdis`, and the extreme instance is a 2.21 m
bump credited with 251 m of relief (113x).

**The layered painting is not the cause.** The inflation is created one step
earlier, when the ridgelet's wedge is re-fitted with the **lowest-foot**
definition (`xwedge3_list`/`hwedge3_list`, from `find_feet`) instead of the
`ANISO_ANA_2` wedge. Section 2 separates the two steps: the lowest-foot wedge
raises 75.1% of ridgelets and is responsible for all of the inflation, while the
layering can only ever lower a height and in aggregate removes more than twice
what the wedge step added.

What `layer_profi` did change is **visibility**: v305 painted with the
lowest-foot wedge but still wrote the node `mxdis` into the tiles file, so v306
is the first run in which the re-fitted wedge reaches the product. That is why
the problem surfaced now, and why it initially looked like a painting problem.

Within the wedge step, two distinct mechanisms are at work, and only one of them
is a bug:

1. **Foot escape across a flat run-out.** A crest with a shallow run-out has no
   local minimum nearby, so the foot search walks until it falls into whatever
   basin is next door and inherits that basin's depth. Rare, spectacular,
   and clearly wrong (uniqid 41028, section 3).
2. **Scale over-reach.** A genuine ridge between two genuine deep valleys. Both
   feet stop at real local minima. The node analysis says one thing, the foot
   search says roughly double. Nothing is malfunctioning -- the question is
   whether that relief belongs to this ridgelet or to a coarser one on the
   `gauss_scales` ladder. This is the large population (~6,400 ridgelets).

A third, smaller effect is noted in section 6: in 7.7% of ridgelets the
ANISO_ANA_2 wedge and the lowest-foot wedge do not even share a crest height.

---

## 2. Attribution: which step creates the inflation

Three heights exist for every ridgelet, in pipeline order:

| symbol | where it comes from | code |
|---|---|---|
| `mxdis` | the `ANISO_ANA_2` wedge, `max(hwedge) - min(hwedge)` | `ridgescales` |
| relief3 | the **lowest-foot** wedge, `crest - lower foot` | `find_feet` -> `hwedge3_list` |
| `mxdis_eff` | largest rise at the ridgelet's **visible** crest cells | `layer_profi` |

Ratios over the 90,348 ridgelets present in `_tiles.nc`:

| step | ratio | median | mean | max | % > 1 |
|---|---|---:|---:|---:|---:|
| **1. wedge re-fit** | relief3 / `mxdis` | 1.105 | 1.259 | **113.67** | **75.13** |
| **2. layering** | `mxdis_eff` / relief3 | 0.875 | 0.654 | **1.00** | **0.00** |
| net | `mxdis_eff` / `mxdis` | 0.929 | 0.817 | 113.67 | 37.99 |

Step 1 is `ANISO_ANA_2` wedge -> lowest-foot wedge; step 2 is lowest-foot wedge
-> `layer_profi`. The 99th percentiles are 2.87, 1.00 and 2.43 respectively.

**Zero ridgelets** come out of `layer_profi` taller than their own lowest-foot
wedge. That is structural, not luck: the datum is `MAX(surface_beneath,
own_lowest_foot)`, which is never below `own_lowest_foot`, and the rise is
measured only at crest cells the ridgelet still owns. Step 2 is a strict
contraction. It lowered 58.2% of ridgelets and left the remaining 41.8%
untouched.

In totals:

```
  sum(relief3   - mxdis)    =   +5,955,474 m    created by step 1
  sum(mxdis_eff - relief3)  =  -14,252,610 m    removed by step 2
  sum(mxdis_eff - mxdis)    =   -8,297,137 m    net
```

The three cases below all have step 2 = **1.00 exactly** -- the layering did
nothing to them at all:

| uniqid | `mxdis` | relief3 | `mxdis_eff` | step 1 | step 2 |
|---|---:|---:|---:|---:|---:|
| 41028 | 2.21 | 250.99 | 250.99 | 113.67 | 1.00 |
| 194354 | 103.17 | 228.32 | 228.32 | 2.21 | 1.00 |
| 214662 | 528.26 | 1190.43 | 1190.43 | 2.25 | 1.00 |

So every figure in sections 3-5 is a picture of **step 1**. The red wedge is the
object that needs fixing; the layering merely reports its relief faithfully.

---

## 3. Case 1 — the 113x outlier: Afar / Danakil Depression, Ethiopia

**uniqid 41028**, panel 1, centre (2900.3, 2175.6), **41.8 E 15.5 N**,
`nswx = 42`, `m3 = 63` (extended profile +/- 210 km).

```
      295 |
      272 |#
      250 | #
      227 |
      205 |
      182 |
      159 |
      137 |
      114 |
       92 |  #
       69 |
       47 |
       24 |   #                                                               ####
        1 |                                        ###########################################################################    ##  ####
      -21 |    #                                ###                  ---                                                            ##
      -44 |                                   ##                 ----
      -66 |     #                           ##               ----
      -89 |                                #             ----
     -112 |      #        ##              #           ---
     -134 |              #              ##        ----
     -157 |       ##        #          #      ----
     -179 |             #    #       ##   ----
     -202 |         ####            # ----
     -225 |                   ##  ##--
     -247 |                     ##-
     -270 |
          +-------------------------------------------------------------------------------------------------------------------------------
                                L                                       l C                    r                       R

  #  terrain (terr_dev)     =  ANISO_ANA_2 wedge     -  lowest-foot wedge
  L,R = lowest-foot feet    l,r = ANISO_ANA_2 feet   C = crest
  one column = one cube cell = 3.34 km
```

|                    | left foot | crest | right foot | relief |
|--------------------|----------:|------:|-----------:|-------:|
| ANISO_ANA_2 wedge  |      0.00 |  2.21 |       0.00 |   2.21 |
| lowest-foot wedge  |   -248.79 |  2.21 |      -0.71 | 250.99 |

`reason L/R = min/min`, `blocked L/R = 1/1`, clearance index **0.012**.

The geography is real and that is the point. This is the **Danakil Depression**
on the Ethiopia-Eritrea border: the -249 m trench is the depression floor
(genuinely below sea level) and the 274 m feature at the far left edge of the
profile is the Ethiopian escarpment. The feature being analysed is a 2.21 m
speck on the flat floor. `ANISO_ANA_2` got it exactly right — feet at 0.00 m
both sides, 23 cells wide. Its wedge is all but invisible at this vertical
scale: a 2.21 m triangle against a 545 m range.

Looking left from the crest the ground is flat, so there is no local minimum to
stop on. The search walked **42 cells / 140 km** before falling into the deeper
part of the same depression. Both sides report `blocked = 1` because literally
anything nearby is taller than a 2.21 m peak.

`layer_profi` then paints the 251 m wedge; nothing buries it (it sits in a
hole, so no taller wedge overlaps its crest cell) and `mxdis_eff = 251 m` for a
2 m hill.

---

## 4. Case 2 — the ordinary version: Bighorn Basin, Wyoming

**uniqid 194354**, panel 4, centre (1006, 2999), **105.0 W 44.1 N**,
`nswx = 14`, `m3 = 28`. `mxdis` 103.2 m -> `mxdis_eff` 228.3 m, **ratio 2.21**.

```
        0 |
      -10 |     ####                   ##
      -20 |                          -= -
      -30 |    #    #               ### =#
      -40 |#  #      ###          -#     =-
      -50 |  #          #        ##        --
      -60 |              #     -#         #  -
      -70 | #                 -#           =  -
      -80 |               # ###            #   -
      -90 |                #                #   -
     -100 |                                  #   --
     -109 |                                        -
     -119 |                                   ##    -
     -129 |                                     #    -
     -139 |                                      #    -
     -149 |                                       #    -
     -159 |                                        #    --         #
     -169 |                                         #    #-
     -179 |                                          #    #-
     -189 |                                             #  ####
     -199 |                                           #      - #
     -209 |                                            #      - #
     -219 |                                                    --
     -229 |                                                      #
     -239 |                                                       #
     -249 |
          +---------------------------------------------------------
                           L    l      C      r                   R
```

|                    | left foot | crest  | right foot | relief |
|--------------------|----------:|-------:|-----------:|-------:|
| ANISO_ANA_2 wedge  |    -60.74 | -11.48 |    -114.66 | 103.17 |
| lowest-foot wedge  |    -88.55 | -11.48 |    -239.81 | 228.32 |

Same crest. The left foot moved only modestly (-60.7 -> -88.6). **The entire
extra 125 m is the right foot**, continuing down the basin flank another ~20
cells (65 km) to the basin floor at -240 m. `reason = min/min` — it did stop at a
genuine local minimum. Clearance index 0.338.

There is no cliff, no trench, and nothing obviously broken. The ground simply
keeps descending gently. This is the representative case, and it is the one
that matters, because there are thousands like it.

---

## 5. Case 3 — the regime-changing band: Eastern Alps

**uniqid 214662**, panel 6, centre (1898, 78.5), **13.2 E 47.0 N**,
`nswx = 19`, `m3 = 38`. `mxdis` 528.3 m -> `mxdis_eff` 1190.4 m, **ratio 2.25**.

```
     1177 |
     1108 |                       #
     1039 |
      970 |                      # #
      902 |                         #
      833 |
      764 |                     #    #
      695 |                                      #
      626 |                    #        #         --
      557 |       #                   ## #      -   -
      488 |                 ###           #   #   #  --
      420 |      #                             -       -
      351 |        #       #                       #  # -
      282 |               #                  #- #        --#
      213 |          ##  #                 # =      #     #-#
      144 |         #   #                            # #    -#
       75 |     #      #                     -          #     -
        6 |# ##                                #         #     -
      -62 | #                               #                 ##--                     #
     -131 |    #                                                  ##                 ##
     -200 |                                                     ## -#              ##
     -269 |                                                          ##         ###
     -338 |                                                           -#      ##
     -407 |                                                            --  ###
     -476 |                                                             ###
     -544 |
          +-----------------------------------------------------------------------------
                                            l  r C                       R
```

|                    | left foot | crest  | right foot | relief  |
|--------------------|----------:|-------:|-----------:|--------:|
| ANISO_ANA_2 wedge  |    -48.52 | 479.75 |      12.52 |  528.26 |
| lowest-foot wedge  |    -48.52 | 709.70 |    -480.73 | 1190.43 |

Left foot identical (-5 cells, -48.5 m), so the two left-foot markers coincide.
The right foot went from +12.5 m to -480.7 m, 24 cells / 80 km out, down into
the Alpine foreland. Clearance index 0.637.

Note this case **also changed its crest** (479.75 -> 709.70), so the foot is not
the whole story here. See section 6.

### The band: mxdis 450-550 m -> eff 900-1200 m (67 ridgelets)

Feet are given as *offset in cells / height in m*; `clr` is the clearance index
of section 8.1.

| uniqid | mxdis | eff | ratio | location | left foot | right foot | clr |
|---|---:|---:|---:|---|---|---|---:|
| 214662 | 528 | 1190 | 2.25 | E Alps, 13.2E 47.0N | -5 / -49 | +24 / -481 | 0.637 |
| 115546 | 455 | 1160 | 2.55 | NE Tibet, 99.7E 35.5N | -34 / -698 | +38 / -144 | 0.523 |
| 125491 | 540 | 1156 | 2.14 | Altai, 96.3E 41.8N | -59 / -682 | +58 / -743 | 0.947 |
| 124781 | 522 | 1133 | 2.17 | Altai, 96.3E 41.8N | -58 / -639 | +58 / -735 | 0.916 |
| 113756 | 546 | 1123 | 2.05 | Ladakh, 79.1E 34.4N | -28 / -376 | +27 / -906 | 0.528 |
| 266706 | 538 | 1120 | 2.08 | Altai, 88.2E 48.7N | -42 / -218 | +61 / -561 | 0.693 |

### Largest absolute gains anywhere

| uniqid | mxdis | eff | ratio | location |
|---|---:|---:|---:|---|
| 161800 | 1565 | 3540 | 2.26 | Peruvian Andes, 77.5W 9.2S |
| 118297 | 1606 | 3254 | 2.03 | Karakoram, 74.5E 36.7N |
| 116402 | 1517 | 2967 | 1.96 | Ladakh, 76.8E 35.5N |
| 126249 | 1414 | 2895 | 2.05 | Tian Shan, 70.5E 41.3N |
| 117156 | 1153 | 2665 | 2.31 | Karakoram, 76.2E 36.1N |
| 105267 |  886 | 2366 | 2.67 | E Tibet, 96.3E 29.8N |

1.5-1.6 km ridges being credited with ~3 km of relief.

---

## 6. Secondary finding: the two wedges do not always share a crest

Comparing `hwedge3_list(:,2)` with `hwedge_list(:,2)` over all 319,149
ridgelets with `mxdis > 0`:

```
  identical (|d| < 0.01) : 92.33 %
  median 0.000   mean 2.078   p95 1.55   max 1207.66
```

So in **7.7%** of ridgelets the lowest-foot wedge takes a different crest height
from the one `ANISO_ANA_2` fitted, by up to 1208 m. uniqid 214662 is one of
them (479.75 vs 709.70, the latter being the extended profile's value at
centre). uniqid 161800 differs the other way (`rdg_profiles_x` at centre is
1979.25 while both wedges carry 2274.01), so the crest is not simply
`rdg_profiles_x(PSW)` either.

Worth running down separately — in some of the large-ratio cases this is a
bigger contributor than the foot walk.

---

## 7. How large is the affected population

| selection | n | % | sum(eff - mxdis) |
|---|---:|---:|---:|
| all ridgelets in tiles | 90,348 | 100.00 | **-8,297,137 m** |
| ratio > 1.5 | 6,389 | 7.07 | +1,109,942 m |
| ratio > 2 | 1,851 | 2.05 | +353,279 m |
| ratio > 2, eff > 500 m | 326 | 0.36 | +187,675 m |
| ratio > 2, eff > 1000 m | 107 | 0.12 | +92,627 m |

Distribution of `mxdis_eff / mxdis` over the 90,348: median 0.929, mean 0.817,
**37.66% greater than 1**, max 113.67.

For reference, `layer_profi` reports 30,513 ridgelets fully buried (25.2% of
120,852 painted), mean effective/original height 0.817.

**The regime argument applies to the last two rows.** 107 ridgelets roughly
double their height *and* land above a kilometre, which is where `Nh/U` crosses
from flow-over to blocked. Small in count, but these are the ones that change
what the gravity-wave scheme does rather than merely how much.

---

## 8. Diagnosis and candidate fixes

### 8.1 A clearance index separates case 1 but not cases 2 and 3

Define clearance = `(crest - higher_foot) / (crest - lower_foot)`. A ridgelet
genuinely sitting in a saddle clears its higher foot by a decent fraction of its
claimed relief.

- uniqid 41028: **0.012** — cleanly flagged.
- the 2x population: **0.3 - 0.95** — not flagged at all.
- uniqid 125491: **0.947**, feet at -682 and -743, both genuine local minima: a
  real ridge between two deep Altai valleys, with no search pathology whatever.

So a clearance threshold would catch the foot-escape class and leave the large
population untouched. Useful but narrow.

### 8.2 Re-referencing the datum to the pre-painted surface (Julio's proposal)

Instead of the lowest foot, reference each ridgelet's zero to the surface
elevation already present at its location. Assessment:

**Apply it to the wedge, not to the painting datum.** This is the one change
section 2 forces. `layer_profi` already uses `rise = v - MAX(sv, lowft(k))`, so
it is tempting to read the proposal as "drop `lowft` from that `MAX`". That
would correct the *number* in `_tiles.nc` and nothing else. The painted value
`v` is the lowest-foot wedge's own absolute height (`ridge_ana.F90:3321`, via
`sL`/`sR`/`lowft` set from `xwedge3_list`/`hwedge3_list` at `:3257-3260`), so
the surface `wedgoC` that you eyeball against `dev` would still carry a wedge
ramping 140 km down into the Danakil. The re-referencing has to happen where
the feet are chosen, in `find_feet`, upstream of both the painting and the
height.

**The failure also occurs in the branch where `sv` does not exist.** 41028 is in
`sv <= UNPHALF` territory — nothing painted beneath — so even the datum version
falls back to `lowft`. With painted coverage around 25%, most cells are in that
branch, so `sv` alone leaves three quarters of the domain with no reference.

**The gap-filler should be the smoothed terrain, not `sv`.** The Gaussian-
smoothed terrain on the existing scale ladder is defined everywhere and already
computed. Referencing a ridgelet to `terr_smooth(sigma)` with sigma a rung or
two above its own scale gives "the environment this bump sits on" with no
search, no walking, and no sensitivity to how far away the next basin is. For
41028 the smoothed field over that flat ground is ~0 m and the bump correctly
becomes a 2 m bump.

**That version is also order-independent; `sv` alone is not.** With the painted
surface as datum, two identical bumps get different heights depending on whether
a wide ridgelet happened to be laid down nearby first, which the widest-first
sort decides. A smoothed-terrain datum has no such dependence.

**Self-consistency is the binding constraint, and fixing the wedge satisfies it
for free.** Whatever sets the heights must also set the shape that gets painted.
Change the foot definition and `mxdis_eff`, `wedgoC`, `hwdth` and `clngt` all
move together, because they are all read off the same wedge. Change only the
painting datum and they separate: the number in the tiles file stops matching
the picture on the cube.

**Drop the "smaller ridgelets" qualifier.** A size threshold is another knob and
another discontinuity. The scale-relative statement covers it with no special
case: every ridgelet references the surface defined by scales above it, and the
widest ridgelet references the existing `terr_dev` zero. Small ridgelets get the
desired behaviour automatically because there is more above them.

**Two things it would not fix.**

- *Width*, if the re-referencing is applied only to the height. `xwedge3` for
  41028 is `[-42, 0, 45]`, 87 cells, against `ANISO_ANA_2`'s 23. The runaway
  foot corrupts `hwdth`/`clngt` as much as the height. Changing where the feet
  are placed fixes both at once; changing only the datum fixes neither.
- *Burial.* `v > sv` already decides what is buried; the 25.2% fully-buried
  population is unaffected.

**Physical argument in favour.** For blocked drag the relevant height is what
the flow sees above the surrounding surface, not the depth of the deepest hole
within 140 km. The foot search answers the second question; the proposal answers
the first.

---

## 9. Reproduction

All in `/glade/derecho/scratch/juliob/wedge_width_diag/` (scratch is purgeable —
copy out if it matters):

| script | what it does |
|---|---|
| `inflation_cases.py` | scans tiles vs Ridge_list, bands by absolute height, prints location/feet/clearance and the population table |
| `draw_outlier.py UID` | renders one ridgelet's extended profile with both wedges overlaid (the plots in sections 3-5) |
| `crest_check.py` | the `hwedge3[1]` vs `hwedge[1]` comparison in section 6 |
| `attribution.py` | splits the net ratio into the two steps of section 2 |
| `show_outlier.py` | original ratio ranking across the v305/v306 tiles pair |

Inputs: `output/Ridge_list_*.dat` (read with `myPythonTools/TopoAna/rdridgelist.py`,
`profiles=True`) and `output/*_tiles.nc` (`UNIQID`, `MXDIS`). Note `lonc`/`latc`
in the Ridge_list are **radians**.

---

## 10. Open questions

1. Is the 7.7% crest disagreement (section 6) a bug or intended? In some
   large-ratio cases it dominates the foot walk.
2. If the datum moves to a smoothed-terrain field, which rung of the ladder —
   the ridgelet's own sigma, one above, two above?
3. How is the reconstruction kept consistent with the new datum where painted
   coverage is thin?
4. What stops the foot search on a flat run-out that has no local minimum?
   This is the single decision behind both the inflated heights and the
   inflated widths.
5. `layer_profi` falls back to the `ANISO_ANA_2` wedge when
   `xwedge3_list(3,k) <= xwedge3_list(1,k)` (`ridge_ana.F90:3257`). How many
   ridgelets take that path, and are they better or worse behaved?
6. Worth checking the 107 regime-changing ridgelets against GW_UnitTest to see
   whether the drag actually flips character there.
