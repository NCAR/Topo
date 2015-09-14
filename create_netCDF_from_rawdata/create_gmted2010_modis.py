from netCDF4 import Dataset
import numpy
#from mpl_toolkits.basemap import Basemap, addcyclic, shiftgrid
import sys
#
#user settings
#
debug=False
print ' '
print 'Merge GMTED2010 elevation data with MODIS derived land fraction'
print '==============================================================='
print ' '
#input files
#modis     = Dataset("modis/landwater.nc")
#gmted2010 = Dataset("gmted2010/mea.nc")
modis=Dataset(sys.argv[1])
gmted2010=Dataset(sys.argv[2])
if debug:
    gtopo30   = Dataset("gtopo30/src/gtopo30-rawdata.nc")
#output file
#dsout = Dataset("gmted2010_elevation_and_landfrac_modis.nc", "w", format="NETCDF4")
dsout = Dataset(sys.argv[3], "w", format="NETCDF4")

#Copy dimensions
print 'Copy dimensions'
for dname, the_dim in gmted2010.dimensions.iteritems():
    if dname=='lon': nlon=len(the_dim)
    if dname=='lat': nlat=len(the_dim)
    dsout.createDimension(dname, len(the_dim) if not the_dim.isunlimited() else None)
dx=360.0/nlon
print 'Write dimensions (lat/lon)'
varin = gmted2010.variables['lat']
outVar = dsout.createVariable('lat', varin.datatype, varin.dimensions)
outVar.setncatts({k: varin.getncattr(k) for k in varin.ncattrs()})
outVar[:] = varin[:]
lons = varin[:]

varin = gmted2010.variables['lon']
outVar = dsout.createVariable('lon', varin.datatype, varin.dimensions)
outVar.setncatts({k: varin.getncattr(k) for k in varin.ncattrs()})
outVar[:] = varin[:]

#
# LANDFRAC data from MODIS
#
print ' ' 
print 'Read MODIS data'
print ' '
varin = modis.variables['Band1']
a=numpy.array(varin)
landfrac = dsout.createVariable('landfract','u1',('lat','lon',))
landfrac.units='fraction (0-1)'
landfrac.long_name='30-second land fraction from MODIS'
#landfrac.setncatts({k: varin.getncattr(k) for k in varin.ncattrs()})
print 'Modification of MODIS data:'
print 'shallow ocean (0) points remain 0'
print 'set shoreline (2) points to 1'
a=numpy.where(a==2,1,a)
print 'set shallow inland water (3) to 0'
a=numpy.where(a==3,0,a)
print 'set ephemeral water (4) to 1'
a=numpy.where(a==4,1,a)
print 'set deep inland water (5), moderate ocean (6), deep ocean (7), fill (237) points to 0'
a=numpy.where(a>4,0,a)
landfrac[:] = a
#
# ELEVATION data from GMTED2010
#
print ' '
print 'Read/write elevation data from GMTED2010'
print ' '
varin = gmted2010.variables['Band1']
h = dsout.createVariable('htopo','i4',('lat','lon',))
h.units='meter'
h.long_name='GMTED2010 30-arc second (~1km) elevation'
h[:] = varin[:]

if debug:
  print 'we are debugging'
  lon_gtopo30 = gtopo30.variables['lon']
  print 'lon_gtopo30[0]',lon_gtopo30[0]
  print 'lon_gtopo30[1]',lon_gtopo30[1]

  varin = gtopo30.variables['landfract']
  tmp, lons_out = shiftgrid(180., varin[:], lon_gtopo30[:], start=True)
  gtopo30_landfrac=numpy.array(tmp)
  diff=numpy.where(gtopo30_landfrac<>a,10,0)

  landfrac_diff = dsout.createVariable('LANDFRAC_DIFF','i1',('lat','lon',))
  landfrac_diff[:] = diff

  varin = gtopo30.variables['htopo']
  tmp, lons_out = shiftgrid(180.004027778, varin[:], lon_gtopo30[:], start=True)
  gtopo30_h=numpy.array(tmp)
  diff=h[:]-tmp
  print 'max gmted2010-gtopo30: ',numpy.max(diff)
  print 'min gmted2010-gtopo30: ',numpy.min(diff)
 
  h_diff = dsout.createVariable('h_DIFF','int',('lat','lon',))
  h_diff[:] = diff
dsout.close()
