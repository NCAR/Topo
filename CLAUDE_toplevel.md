# NCAR Topo

Software for generating CAM/CESM topography files. Upstream:
https://github.com/NCAR/Topo

## Layout

| Directory | Purpose |
|---|---|
| `bin_to_cube/` | Builds the intermediate cubed-sphere elevation file (`ncube3000`) from raw source data. Runs rarely. |
| `cube_to_target/` | **Main working area.** Smoothing, ridge detection, remapping to the target grid. See `cube_to_target/CLAUDE.md`. |
| `make_rll_grid_descriptor_file/` | Generates SCRIP grid descriptor files for lat-lon target grids. |
| `cases/` | **Generated output. Never edit.** See below. |
| `regression-test-data/` | Reference data for verifying that changes do not alter results. |

Nearly all active work is in `cube_to_target/`.

## `cases/` is generated — never edit anything under it

`cube_to_target/create_topo.py` creates `cases/<ogrid>_Sco<scale>[_Rw###]_<tag>/`,
**copies every `*.F90` file into it**, then compiles and runs there.

This means `cases/` contains many stale copies of the real sources.

- **Never edit a file under `cases/`.** Edits there are silently overwritten on
  the next run and never reach the real source.
- When searching the tree, exclude `cases/`. A grep for a subroutine name will
  otherwise return a dozen stale duplicates, and the newest copy is not
  necessarily the current source.
- Read files under `cases/` only to inspect build logs or run output.

## Working style

This is legacy scientific Fortran, actively used to produce production topo
files. Correctness and reproducibility of numerical output matter more than
cleanliness.

- **Do not reformat, restyle, or "modernize" code you were not asked to
  change.** Mixed indentation, `#if 0` blocks, commented-out experiments and
  unused variables are everywhere. Leave them. A diff should contain only what
  was requested.
- **Do not delete commented-out code.** Several blocks are tuning alternatives
  someone still switches between.
- **Preserve bit-for-bit output unless a change is explicitly about changing
  results.** If a refactor could alter numerical output, say so before doing it.
- Ask before changing a subroutine's argument list. Several routines
  communicate through module-level state, so the signature is not the whole
  interface.
- Check changes against `regression-test-data/` before calling them complete.

## Environment

Builds and runs on NCAR HPC (Casper/Derecho); requires the NCAR module
environment and netCDF. It will not build on a laptop.