#!/usr/bin/env python
import os
import sys
import shutil
import subprocess
import glob
import argparse
import yaml

def parse_arguments():
    parser = argparse.ArgumentParser(description="Generate topo cases.")
    parser.add_argument("--ogrid", type=str, help="Output grid, e.g., ne30pg3")
    parser.add_argument("--smoothing_scale", type=int, help="Smoothing scale")
    parser.add_argument("--tag", type=str, help="Tag for case identification")
    parser.add_argument("--config", type=str, default="create_topo.yaml", help="Path to YAML configuration file (default: create_topo.yaml)")
    parser.add_argument("--clean_case", action="store_true", help="Delete existing case directory if it exists")
    return parser.parse_args()

def load_config(config_path):
    with open(config_path, 'r') as file:
        return yaml.safe_load(file)

def clean_case_directory(case_dir):
    if os.path.exists(case_dir):
        print(f"Cleaning existing case directory: {case_dir}")
        shutil.rmtree(case_dir)

def main():

    # Parse command-line arguments
    args = parse_arguments()

    # Load settings from YAML if the file exists
    config = {}
    try:
        config = load_config(args.config)
    except FileNotFoundError:
        print(f"Configuration file '{args.config}' not found. Using defaults where applicable.")

    # Override YAML settings with explicit command-line arguments
    ogrid = args.ogrid or config.get("ogrid")
    smoothing_scale = args.smoothing_scale or config.get("smoothing_scale")
    tag = args.tag or config.get("tag")
    #clean_case = args.clean_case or config.get("clean_case")
    clean_case = args.clean_case if args.clean_case else config.get("clean_case", False)

    # Ensure required arguments are set
    if not ogrid or not smoothing_scale or not tag:
        raise ValueError("Missing required arguments: ogrid, smoothing-scale, tag")

    print(f"Ogrid: {ogrid}")
    print(f"Smoothing Scale: {smoothing_scale}")
    print(f"Tag: {tag}")

    case = f"{ogrid}_Sco{smoothing_scale}_{tag}"
    print(case)
    case_dir = os.path.join("..", "cases", case)

    # Clean the case directory if the flag is set
    if clean_case==True :
        clean_case_directory(case_dir)

    # Create directories
    os.makedirs(os.path.join(case_dir, "output", "raw"), exist_ok=True)
    os.makedirs(os.path.join(case_dir, "output", "clean"), exist_ok=True)

    # Copy files
    for file in ["Makefile", "machine_settings.make"]:
        shutil.copy(file, case_dir)
    shutil.copy(__file__, os.path.join(case_dir, "create_topo.py"))
    
    # Copy all .F90 files to the target directory
    f90_files = glob.glob("*.F90")  # This will match all .F90 files in the current directory
    for f90_file in f90_files:
        shutil.copy(f90_file, case_dir)

    # Move to case directory
    os.chdir(case_dir)

    
    # Load gcc and compile
    # subprocess.run(["module", "load", "gcc"], shell=True)
    #command = "module load gcc"

    command=f"module load gcc; gmake -f Makefile clean; gmake -f Makefile"
    result = subprocess.run(command, shell=True, executable="/bin/tcsh", capture_output=True, text=True)
    print(result.stdout)
    print(result.stderr)
    print("Return code:", result.returncode)

    #subprocess.run(["make", "-f", "Makefile", "clean"], shell=True)
    #subprocess.run(["make", "-f", "Makefile"], shell=True)

    # Define grid descriptor
    scrip = None
    scrip_gll = None
    yfac = "1"

    ogrid_to_scrip = {
        "geos_fv_c48": "PE48x288-CF.nc4",
        "geos_fv_c90": "PE90x540-CF.nc4",
        "geos_fv_c180": "PE180x1080-CF.nc4",
        "geos_fv_c360": "PE360x2160-CF.nc4",
        "geos_fv_c720": "PE720x4320-CF.nc4",
        "geos_fv_c1440": "PE1440x8640-CF.nc4",
        "fv0.9x1.25": "fv0.9x1.25.nc",
        "scam": "fv0.9x1.25.nc",
        "ne120pg3": "ne120pg3_scrip_170628.nc",
        "ne240pg3": "ne240pg3_scrip_170628.nc",
        "ne30pg3": "ne30pg3.nc",
        "Arctic": "ne0ARCTICne30x4_np4_SCRIP.nc",
        "POLARRES": "POLARRES_ne30x4_np4_SCRIP.nc",
        "HMA": "HMACUBIT_ne30x16_np4_SCRIP.nc"
    }

    if ogrid in ogrid_to_scrip:
        scrip = f"/glade/work/juliob/GridFiles/Scrip/{ogrid_to_scrip[ogrid]}"
        if ogrid == "ne120pg3":
            scrip_gll = "/glade/work/juliob/GridFiles/Scrip/ne120np4_pentagons_100310.nc"
        elif ogrid == "ne240pg3":
            scrip_gll = "/glade/work/juliob/GridFiles/Scrip/ne240np4_091227_pentagons.nc"
        elif ogrid == "ne30pg3":
            scrip_gll = "/glade/work/juliob/GridFiles/Scrip/ne30np4_091226_pentagons.nc"
        elif ogrid in ["Arctic", "POLARRES"]:
            yfac = "4"

    cstopo = "/glade/work/juliob/Topo/CubeData/gmted2010_modis_bedmachine-ncube3000-220518.nc"

    cog = f"Co{int(smoothing_scale):03d}"
    fig = "Fi000"
    smtopo = f"topo_smooth_gmted2010_bedmachine_nc3000_{cog}.nc"

    print("Here you are")
    print(f"Smoothing scale: {cog}")
    print(f"Fine radius: {fig}")
    print(f"Smooth topo file: {smtopo}")

    # Run cube_to_target
    if (scrip_gll==None):
        command = [
            "./cube_to_target",
            f"--grid_descriptor_file={scrip}",
            f"--intermediate_cs_name={cstopo}",
            f"--output_grid={ogrid}",
            f"--smoothing_scale={smoothing_scale}",
            f"--fine_radius=0",
            "-u", "juliob@ucar.edu",
            "-q", "output/",
            "-z"
        ]
    else:
        command = [
            "./cube_to_target",
            f"--grid_descriptor_file={scrip}",
            f"--grid_descriptor_file_gll={scrip_gll}",
            f"--intermediate_cs_name={cstopo}",
            f"--output_grid={ogrid}",
            f"--smoothing_scale={smoothing_scale}",
            f"--fine_radius=0",
            "-u", "juliob@ucar.edu",
            "-q", "output/",
            "-z"
        ]

        
    subprocess.run(command, check=True)

if __name__ == "__main__":
    main()
