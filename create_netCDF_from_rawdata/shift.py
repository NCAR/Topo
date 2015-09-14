from netCDF4 import Dataset
import numpy
import sys
from mpl_toolkits.basemap import Basemap, addcyclic, shiftgrid
#
#input file
#
modis     = Dataset(sys.argv[1], "r", format="NETCDF4")
#output file
dsout = Dataset(sys.argv[2], "w", format="NETCDF4")

#Copy dimensions
for dname, the_dim in modis.dimensions.iteritems():
    print dname, len(the_dim)
    if dname=='lon': nlon=len(the_dim)
    if dname=='lat': nlat=len(the_dim)
    dsout.createDimension(dname, len(the_dim) if not the_dim.isunlimited() else None)

dx=360.0/nlon
print 'read/write lats'
varin = modis.variables['lat']
outVar = dsout.createVariable('lat', varin.datatype, varin.dimensions)
outVar.setncatts({k: varin.getncattr(k) for k in varin.ncattrs()})
outVar.setncatts({'units': u"degrees north"})
outVar[:] = varin[:]
print 'read lons'
lons        = modis.variables['lon']
outVar_lons = dsout.createVariable('lon', lons.datatype, lons.dimensions)
outVar_lons.setncatts({k: lons.getncattr(k) for k in lons.ncattrs()})
print 'read landfract, shift data and write data'
varin = modis.variables['landfract']
outVar = dsout.createVariable('landfract', varin.datatype, varin.dimensions)
outVar.setncatts({k: varin.getncattr(k) for k in varin.ncattrs()})
outVar[:], lons_out = shiftgrid(dx/2.0, varin[:], lons[:], start=True)
modis_landfrac=outVar[:]
print 'write shifted lons'
outVar_lons[:] = lons_out[:]
outVar_lons.setncatts({'units': u"degrees east"})
print 'read htopo, shift data and write data'
varin = modis.variables['htopo']
outVar = dsout.createVariable('htopo', varin.datatype, varin.dimensions)
outVar.setncatts({k: varin.getncattr(k) for k in varin.ncattrs()})
outVar[:], lons_out = shiftgrid(dx/2.0, varin[:], lons[:], start=True)
modis_h=outVar[:]
dsout.close()
