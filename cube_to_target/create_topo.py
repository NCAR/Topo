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

def gridInfo(grid):
    # Default values
    scrip = None
    scrip_gll = None
    yfac = None

    base_path = "/glade/work/juliob/GridFiles/Scrip/"

    # Conditional logic for all grids
    if grid == "geos_fv_c48":
        scrip = f"{base_path}PE48x288-CF.nc4"
    elif grid == "geos_fv_c90":
        scrip = f"{base_path}PE90x540-CF.nc4"
    elif grid == "geos_fv_c180":
        scrip = f"{base_path}PE180x1080-CF.nc4"
    elif grid == "geos_fv_c360":
        scrip = f"{base_path}PE360x2160-CF.nc4"
    elif grid == "geos_fv_c720":
        scrip = f"{base_path}PE720x4320-CF.nc4"
    elif grid == "geos_fv_c1440":
        scrip = f"{base_path}PE1440x8640-CF.nc4"
    elif grid == "fv0.9x1.25" or grid == "scam" or grid == "fv1x1":
        scrip = f"{base_path}fv0.9x1.25.nc"
    elif grid == "ne120pg3":
        scrip = f"{base_path}ne120pg3_scrip_170628.nc"
        scrip_gll = f"{base_path}ne120np4_pentagons_100310.nc"
    elif grid == "ne240pg3":
        scrip = f"{base_path}ne240pg3_scrip_170628.nc"
        scrip_gll = f"{base_path}ne240np4_091227_pentagons.nc"
    elif grid == "ne30pg3":
        scrip = f"{base_path}ne30pg3.nc"
        scrip_gll = f"{base_path}ne30np4_091226_pentagons.nc"
    elif grid == "Arctic":
        scrip = f"{base_path}ne0ARCTICne30x4_np4_SCRIP.nc"
        yfac = "4"
    elif grid == "POLARRES":
        scrip = f"{base_path}POLARRES_ne30x4_np4_SCRIP.nc"
        yfac = "4"
    elif grid == "HMA":
        scrip = f"{base_path}HMACUBIT_ne30x16_np4_SCRIP.nc"
    else:
        raise ValueError(f"Unrecognized grid: {grid}")

    # Return results as a dictionary
    Res = {
        "scrip": scrip,
        "scrip_gll": scrip_gll,
        "yfac": yfac
                    }

    return Res

def create_command(ogrid=None, cstopo=None, smoothing_scale=None, scrip=None, scrip_gll=None, yfac=None , development_diags=False ):
    """
    Creates a command for subprocess.run, in this list form:
    
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
    """
    #----------------------------------------------------
    # Some of these 'optional' arguments are not really
    # optional - ogrid, cstopo, scrip and smoothing_scale.
    # Sorry.
    #-----------------------------------------------------
    command = [  "./cube_to_target" ]
    #------------------------------------------------------------
    # We are to rely on Python's 'truthy' vs 'falsy' idea below,
    # to decide whether an input is 'provided'. If this doesn't 
    # work can always go to 'if var is not None:'
    #------------------------------------------------------------
    if scrip: 
        command.append(f"--grid_descriptor_file={scrip}")
    else:
        raise ValueError("scrip file is required but was not provided.")
    if scrip_gll: 
        command.append(f"--grid_descriptor_file_gll={scrip_gll}")
    if cstopo: 
        command.append(f"--intermediate_cs_name={cstopo}")
    else:
        raise ValueError("intermediate cubed sphere topo file is required but was not provided.")
    if ogrid: 
        command.append(f"--output_grid={ogrid}")
    else:
        raise ValueError("destination grid is required but was not provided.")
    if smoothing_scale: 
        command.append(f"--smoothing_scale={smoothing_scale}")
    else:
        raise ValueError("smoothing scale is required but was not provided.")

    command.append(f"--fine_radius=0")

    if yfac: 
        command.append(f"--rrfac_max={yfac}")

    command.append(f"-u juliob@ucar.edu")
    command.append(f"-q output")

    if development_diags==True: 
        command.append(f"-z")

    print("created command line", flush=True)
    print( command, flush=True )
    
    return command

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

    
    #----------------------------------------------------------
    # Load gcc and compile.  Note these command are glommed 
    # together because otherwise gcc's settting of FC=gfortran
    # is not passed to the make commands ...
    #----------------------------------------------------------
    command=f"module load gcc; gmake -f Makefile clean; gmake -f Makefile"
    result = subprocess.run(command, shell=True, executable="/bin/tcsh", capture_output=True, text=True)
    print(result.stdout)
    print(result.stderr)
    print("Return code:", result.returncode)

    #------------------------------------------------------------------------------------
    # Get information about destination grid 'ogrid' 
    #------------------------------------------------------------------------------------
    grid_info = gridInfo( ogrid )
    scrip = grid_info['scrip']
    scrip_gll = grid_info['scrip_gll']
    yfac = grid_info['yfac']
    
    #------------------------------------------------------------------------------------
    # Specify cubed-sphere 'intermediate' topography. Currently, code is configured 
    # to work with a 3000x3000x6 grid, which corresponds to a grid size of around 3km.
    # Clearly, this is a problem at k-scale resolutions.
    #------------------------------------------------------------------------------------
    cstopo = "/glade/work/juliob/Topo/CubeData/gmted2010_modis_bedmachine-ncube3000-220518.nc"

    cog = f"Co{int(smoothing_scale):03d}"
    smtopo = f"topo_smooth_gmted2010_bedmachine_nc3000_{cog}.nc"

    print("Here you are")
    print(f"Smoothing scale: {cog}")
    print(f"Smooth topo file: {smtopo}")

    # Create command line that runs cube_to_target
    command = create_command(ogrid=ogrid, 
                             cstopo=cstopo, 
                             smoothing_scale=smoothing_scale, 
                             scrip=scrip, 
                             scrip_gll=scrip_gll, 
                             yfac=yfac, 
                             development_diags=True )
        
    subprocess.run(command, check=True)

if __name__ == "__main__":
    main()
