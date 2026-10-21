# cube_to_target

Smooths cubed-sphere topography, detects sub-grid ridges for the orographic
gravity-wave-drag scheme, and remaps to the target grid. This is where nearly
all active work happens.

Repo-wide rules (do not reformat, `cases/` is generated, regression testing)
are in the top-level `CLAUDE.md`. This file covers conventions specific to
this directory.

## Key files

| File | Role |
|---|---|
| `cube_to_target.F90` | Main program, command-line parsing, netCDF writers |
| `ridge_ana.F90` | Ridge detection and remapping; the four unformatted writers |
| `ridge_angles_mod.F90` | Cubed-sphere to lat-lon crest angles; reference style for new modules |
| `create_topo.py` | Driver: builds the command line, manages case directories |
| `rdtilemap.py` | Reads `Ridge_tile_map.dat`; oracle for the netCDF migration |

## Build and run

Building happens inside a *case directory*, not in the source tree.
`create_topo.py` creates `../cases/<case>/`, copies `Makefile`,
`machine_settings.make` and **all `*.F90` files** into it, then builds and
runs there.

```
module load gcc; gmake -f Makefile clean; gmake -f Makefile
```

Consequences worth remembering:

- A new `.F90` module is copied automatically by the glob, but **you must add
  it to the `Makefile`** with the right dependency order, or the build breaks
  only inside the case directory and not obviously.
- Editing sources in the case directory is editing a copy. Real changes go in
  the source tree.
- Requires netCDF and the NCAR module environment. Build on Casper/Derecho,
  not on a laptop.

## Fortran conventions

- Real kind is `r8` from `shr_kind_mod` (`use shr_kind_mod, only: r8 => shr_kind_r8`).
  Some modules alias it again as `rpx` or use `dbl_kind` from `reconstruct`.
- **Write literals as `1.0_r8`, `0.01_r8`.** Existing code is full of bare
  `0.01` and `180.` in `r8` expressions. Don't go fix them all, but don't add more.
- New code gets `implicit none` and explicit `intent` on every dummy argument.
- Prefer an optional `ierr` argument over `stop`. Existing `stop` calls are
  fine to leave in place.
- `ridge_angles_mod.F90` is the reference for the style new modules should
  follow: explicit arguments, no hidden module state, documented conventions.

### Known hazards in this codebase

These have all caused real bugs. Watch for them.

- **`pack()` into unallocated arrays.** `remapridge2tiles` assigns `pack(...)`
  results to unallocated allocatables (`IdxPack`, `xpack`, `hpack`), relying on
  F2003 realloc-on-assignment. gfortran does this by default; Intel needs
  `-assume realloc_lhs`.
- **Two sentinel conventions coexist: `-9000.` and `-9999.`** `latlonangles`
  writes `-9000`, the tile arrays use `-9999`. Test defensively (e.g. `> -900.`)
  rather than assuming one.
- **Count/fill loop mismatches.** In `find_local_maxes`, `npeaks` is counted
  over all six panels while the fill loop populates `peaks`. Narrowing one loop
  and not the other leaves uninitialized `peaks` entries with `ip = -99`, which
  segfaults later in `find_ridges`. If you touch a panel loop, drive count and
  fill from shared bounds.
- **`terr_max` is never zeroed** — it is safe only because the copy loop covers
  all six panels completely.
- **Array index order.** Tile arrays are `(ntarget, maxtiles)`; `idxmap` is
  `(nalloc, ntarget)`. Loop with the target index innermost for the former.
- **Progress writes inside hot loops.** `write(*,900,advance='no')` fires once
  per exchange-grid cell — millions of formatted writes. Don't add more.

## netCDF conventions

Use the **F77 interface**, matching `cube_to_target.F90`:

```fortran
#include <netcdf.inc>
status = nf_def_var(foutid, 'MXDIS', NF_DOUBLE, 2, nid, mxdisid)
if (status .ne. NF_NOERR) call handle_err(status)
```

- Do **not** introduce the modern `use netcdf` F90 module. One idiom per repo.
- Every status goes through `handle_err`.
- `nf_create` with `NF_64BIT_DATA`; define all dimensions and variables, then
  `nf_enddef`, then write.
- Give every variable `long_name`, `units`, `missing_value` and `_FillValue`
  (`1.d36`), as the existing writers do.
- Output goes under `output/`, path from the `-q` argument.

## Current work: converting unformatted writes to netCDF

`ridge_ana.F90` has four `form="UNFORMATTED"` writers to convert:

| Routine | Unit | File |
|---|---|---|
| `find_ridges` | 31 | `Ridge_list_nc*_Nsw*_Co*_Fi*_<date>.dat` (~40 records) |
| `remapridge2cube` | 911 | `remap_nc*_Nsw*_Nrs*_Co*_Fi*_vX_<date>.dat` |
| `remapridge2target` | 911 | `grid_remap_nc*_<grid>.dat` |
| `remapridge2tiles` | 911 | `output/Ridge_tile_map.dat` |

**Do `remapridge2tiles` first.** It is self-contained, and `rdtilemap.py`
already reads the binary format, so it provides a ready-made oracle.

Rules for this migration:

1. **Write both formats side by side** behind a flag until verified. Do not
   delete a binary writer in the same change that adds its netCDF replacement.
2. **Verify numerically.** For each converted file, the netCDF must match the
   binary array-for-array. Use `rdtilemap.py` for the tile map.
3. **Make types explicit and check them against the declarations.** The point
   of this exercise is partly to kill ambiguities like `clngt2`, whose
   precision could not be determined from the reader alone.
4. `idxmap(nalloc, ntarget)` can exceed 2 GB and is mostly `-1` fill.
   Ask before choosing CDF-5 (`NF_64BIT_DATA`, large vars, no compression)
   versus netCDF-4 (compression, big win here).
5. `peaks%i` and `peaks%j` are derived-type components written directly.
   Extract to plain arrays first.

## Driver: create_topo.py

Builds the `cube_to_target` command line and manages case directories.

- Options resolve CLI → YAML (`create_topo.yaml`) → default.
- Use `is not None`, not truthiness, for numeric options — `0.0` is falsy.
- **`ridge_window_ratio` must stay in sync in three places:** the `_Rw###`
  filename tag in `cube_to_target.F90`, `ridge_window_tag()` in
  `create_topo.py`, and the case-directory name. The Python uses
  `floor(x+0.5)` rather than `round()` to match Fortran `NINT` on exact
  halves. If you change one, change all three and re-check the rounding.