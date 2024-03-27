import iris 
import iris.plot as iplt
import iris.quickplot as qplt

from mpl_toolkits.basemap import Basemap, maskoceans, shiftgrid 

import numpy as np
import matplotlib.pyplot as plt

import scipy
from scipy import stats
import pandas as pd

################## Functions ###############################################################################


###################################################### LOAD DATA #############################################
#Load high resolution dataset  (ERA5 reanalysis) 
D1=iris.load_cube('MSLP_ERA5_1950_2014_mean_30km.nc')
print (D1)
#Load lower resolution dataset (UKESM model outputs)
D2=iris.load_cube('MSLP_UKESM_1950_2014_mean_100km.nc')
print (D2)

#let's have a look at the latitude coordinate
print (D1.coords('latitude'))
print (D2.coords('latitude'))


#Example of how to produce a map using Basemap

fig=plt.figure()
ax=plt.gca()
 
bmap=Basemap(projection= 'gall', llcrnrlat= -90,  urcrnrlat= 90, llcrnrlon=0,  urcrnrlon= 360, resolution='l')
lon= D1.coord('longitude').points     
lat= D1.coord('latitude').points
x,y=bmap(*np.meshgrid(lon,lat))
contours=bmap.contourf(x,y, D1.data, levels=20, cmap='jet')
 
bmap.drawcoastlines()
plt.colorbar()
plt.title(' ')

plt.show()








