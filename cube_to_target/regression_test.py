import subprocess

#####################
def run(Co=12,ncube=540,Fi=0,ogrid='ne30pg3',tag='dummy'):
     #####################
     

    smoothing_scale = 112.

    ncube_sph_smooth_coarse = 10 #floor( 0.5+ 60.0*(smoothing_scale/100.0)/(3000.0/float(ncube))) )
    ncube_sph_smooth_coarse = int(0.5+ 60.0*(smoothing_scale/100.0)  * float(ncube)/3000.0 )


    print(ncube_sph_smooth_coarse)
    print( int(11.5) )
    print( int(10.999) )

    smoo = ( Co/60.)*100.*(3000./float(ncube))
    print( smoo )

    smoo=float(int(smoo))
    coo = int(0.5+ 60.0*(smoo/100.0)  * float(ncube)/3000.0 )
    print( smoo, coo )

