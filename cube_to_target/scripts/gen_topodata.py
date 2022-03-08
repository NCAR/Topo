#!/usr/bin/env python3

import sys, os, shutil
import xml.etree.ElementTree as ET
import logging
import argparse, textwrap
import subprocess
from datetime import datetime

logger = logging.getLogger(__name__)

def get_parser():
    """
    Get parser object for this script.
    """
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )

    parser.print_usage = parser.print_help

    parser.add_argument(
        "--res",
        help="""
            model resolution [default: %(default)s]
            To see available supported resolutions, simply invoke this command
            with a --res unknown opion
            """,
        action="store",
        dest="res",
        required=True
    )
    parser.add_argument(
        "--inputdata-dir",
        help="""
            /path/of/root/of/input/data',
            [default: %(default)s]
            """,
        action="store",
        dest="input_path",
        default="/glade/p/cesm/cseg/inputdata/",
    )
    parser.add_argument(
        "--author",
        help="""
            author name and Email
            """,
        action="store",
        dest="author",
        required=True
    )

    return parser

def main ():

    args = get_parser().parse_args()

    res    = args.res
    author = args.author
    input_path = args.input_path
    #
    # tree points to https://github.com/ESCOMP/CAM check out
    #
    tree = ET.parse('/glade/work/mvertens/cam6_3_050.gentopo/ccs_config/component_grids_nuopc.xml')# temporary
#    tree = ET.parse('../../ccs_config/component_grids_nuopc.xml')
    root = tree.getroot()
    print("root xxx = "+str(root))
    model_mesh = ""
    for child1 in root:  # this is domain tag
        for name, value in child1.attrib.items():
            if value == res:
                for child2 in child1:
                    if child2.tag == 'mesh':
                        model_mesh = child2.text
                        model_mesh = os.path.join(input_path, model_mesh.strip('$DIN_LOC_ROOT/'))

    if len(model_mesh) == 0:
        print (f"ERROR: input res {res} is invalid")
        valid_grids = []
        for child1 in root:  # this is domain tag
            for name, value in child1.attrib.items():
                valid_grids.append(value)
        print (f"valid grid values are {valid_grids}")
        sys.exit()

    print (f"topo mesh file is {model_mesh}")

    tree = ET.parse('./gen_topodata.xml')
    root = tree.getroot()
    smoothing_radius = -999
    for child1 in root:  # this is domain tag
        for name, value in child1.attrib.items():
            if value == res:
                for child2 in child1:
                    if child2.tag == 'smoothing_radius':
                        smoothing_radius = child2.text

    print (f"smoothing radius is {smoothing_radius}")

#    intermediate_cubed_sphere_data = str(input_path)+""
#    intermediate_cubed_sphere_data = "/glade/p/cgd/amp/pel/topo/cubedata/gmted2010_bedmachine-ncube3000.nc"
    intermediate_cubed_sphere_data = "/glade/p/cgd/amp/pel/topo/cubedata/gmted2010_bedmachine-ncube0540.nc"
    # get smoothing radius - parse gen_topodata.xml
    # now call something - binary for topo 
    arguments = " --coarse_radius="+str(smoothing_radius)
    arguments = arguments+" --grid_descriptor_file="+"'"+model_mesh+"'"
    arguments = arguments+" --intermediate_cs_name="+"'"+intermediate_cubed_sphere_data+"'"
    arguments = arguments+" --output_grid="+res
    arguments = arguments+" -u '"+author+"'"
    arguments = arguments+" -p -r"
    print(arguments)
#    cmd = '../cube_to_target'.split()
#    subprocess.call(["../cube_to_target",arguments])
    cmd = ['../cube_to_target','--coarse_radius',smoothing_radius,'''--grid_descriptor_file=''',model_mesh]
    print(cmd)
    subprocess.call(cmd,shell=True)


if __name__ == "__main__":
    main()
