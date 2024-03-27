import iris 
import iris.plot as iplt
import iris.quickplot as qplt

from mpl_toolkits.basemap import Basemap, maskoceans, shiftgrid 

import numpy as np
import matplotlib.pyplot as plt

import scipy
from scipy import stats
import pandas as pd

####################### Regridding Function #####################################

def regrid(input_dataset, target_grid):

 output_dataset=input_dataset.regrid(target_grid, iris.analysis.Linear())
 
 return(output_dataset)

####################### Interpolating Function #####################################

def interpolate(data_in, sites):
 
 data_out=iris.cube.CubeList()
 for site in sites:
  data=data_in.interpolate(site, iris.analysis.Nearest())
  print (site)
  print (data.data) 
  
  data_out.append(data) 
 
 return(data_out)

################### Plotting Function ############################################

def plot_map(data_in, cmap, levels, title):

 fig=plt.figure(figsize=(10,10))
 ax=plt.gca()
 
 bmap=Basemap(projection= 'gall', llcrnrlat= -90,  urcrnrlat= 90, llcrnrlon=0,  urcrnrlon= 360, resolution='l')
 lon= data_in.coord('longitude').points     
 lat= data_in.coord('latitude').points
 x,y=bmap(*np.meshgrid(lon,lat))
 contours=bmap.contourf(x,y, data_in.data, levels, cmap=cmap)
 
 bmap.drawcoastlines()
 plt.colorbar()
 plt.title(title)

 return()

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

#Call the re-gridding function to regrid the high-res dataset into the low-res one
D2_rgr=regrid(D2, D1)
#print the result
print ('###########  Regridded Dataset ##############')
print (D2_rgr)

#plot the original and regridded datasets for a visual inspection of the result
plot_map(D1, cmap='RdBu', levels=50, title='Original High Res Dataset')
plot_map(D2, cmap='RdBu', levels=50, title='Original Low Res Dataset ')
plot_map(D2_rgr, cmap='RdBu', levels=50, title='Regridded Dataset to High Res')
anomaly=D2_rgr-D1
plot_map(D2_rgr-D1, cmap='jet', levels=50, title='UKESM - ERA5 anomaly ')
plt.show()

#Call interpoalting function to extract values at single locations 
site_1=[('latitude', 41.9),('longitude', 12.5)]    #Rome
site_2=[('latitude', 37.8),('longitude', 122.4)]   #S.Francisco
site_3=[('latitude', -16.5),('longitude', 68.12)]  #La Paz
site_4=[('latitude', -33.86),('longitude', 151.20)] #Sidney 

sites=[site_1, site_2, site_3, site_4]
data_out=interpolate(anomaly, sites)
#print (data_out)








