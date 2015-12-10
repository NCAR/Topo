from netCDF4 import Dataset
import numpy
from astropy.io import ascii
import sys
from mpl_toolkits.basemap import Basemap, addcyclic, shiftgrid
#
#input file
#
print("NetCDF file is "+sys.argv[1])
print("Metadata ASCII file is "+sys.argv[2])
file = Dataset(sys.argv[1], "r+", format="NETCDF4")
f    = open(sys.argv[2], 'r')
metadata = ascii.read(f,format="basic",delimiter="|")
iline=0

print(" ")
print("Adding the following global attributes to "+sys.argv[1])
print("=====================")
print(" ")
print("data_summary           = "+metadata[0][1])
file.data_summary             =   metadata[0][1]
print("data_creator           = "+metadata[1][1])
file.data_creator             =   metadata[1][1] 
print("cesm_contact           = "+metadata[2][1])
file.cesm_contact             =   metadata[2][1]
print("creation_data          = "+metadata[3][1])
file.creation_date            =   metadata[3][1]
print("update_date            = "+metadata[4][1])
file.update_date              =   metadata[4][1]
print("history                = "+metadata[5][1])
file.history                  =   metadata[5][1]
print("data_script            = "+metadata[6][1])
file.data_script              =   metadata[6][1]
print("data_description       = "+metadata[7][1])
file.data_description         =   metadata[7][1]
print("data_source_url        = "+metadata[8][1])
file.data_source_url          =   metadata[8][1]
print("data_reference         = "+metadata[9][1])
file.data_reference           =   metadata[9][1]
print("data_doi               = "+metadata[10][1])
file.data_doi                 =   metadata[10][1]
print("climoe_years           = "+metadata[11][1])
file.climo_years              =   metadata[11][1]
print("data_mods              = "+metadata[12][1])
file.data_mods                =   metadata[12][1]

f.close()
file.close()
