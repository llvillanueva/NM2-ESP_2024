import iris 
import iris.plot as iplt
import iris.quickplot as qplt

from mpl_toolkits.basemap import Basemap, maskoceans, shiftgrid 

import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import ticker

import scipy
from scipy import stats
import pandas as pd
from numpy.fft import *
from scipy.ndimage import gaussian_filter

############################################### Extract geographical area #################################################################################################

def extract_area(data_in, where):

 if where=='NA':
  lat_min=0
  lat_max=60
  lon_min=280
  lon_max=360

 if where=='NP':
  lat_min=20
  lat_max=70
  lon_min=120
  lon_max=250  
  
 if where=='SO':
  lat_min=-70
  lat_max=-50
  lon_min=0
  lon_max=360   
  
 #Define a geographical constraint based on the input coordinates 
 R=iris.Constraint(latitude=lambda  lat: lat_min <= lat <= lat_max, longitude= lambda lon: lon_min <= lon <= lon_max )  
 #Extract area 
 data_out=data_in.extract(R) 
 
 #'''
 #Plot selected area for visual inspection
 bmap=Basemap(projection= 'gall', llcrnrlat= lat_min,  urcrnrlat= lat_max, llcrnrlon= lon_min,  urcrnrlon= lon_max, resolution='l')
 lon= data_out.coord('longitude').points     
 lat= data_out.coord('latitude').points
 x,y=bmap(*np.meshgrid(lon,lat))
 contours=bmap.contourf(x,y, data_out[0,:,:].data, levels=80, cmap=matplotlib.cm.RdYlGn)
 bmap.drawcoastlines()
 #plt.show()
 #'''
 
 return(data_out)

#################################### function to compute annual means from monthly data ########################################################################################################
#working with iris cubes 
def compute_annual_nc(data):

 #compute annual means: 
 months=data.shape[0]
 cubes=iris.cube.CubeList()
 for i in range (12, months+1, 12):
  st=i-12
  cubes.append(data[st:i].collapsed('time', iris.analysis.MEAN))

 data_ann=cubes.merge_cube()
 #print data_ann
 return(data_ann)
 

########## Compute area-weighted mean of input data-set. Return annual mean ##################

def area_weighted(data_in):

 #call function to compute annuals mean from monthly means
 data_in=compute_annual_nc(data_in) 
 
 #On a lat,long grid the grid-spacing reduces near the poles, we need to use area weights in our spatial mean to take into account
 #the irregularity of the grid. To compute the area-weighted spatial mean we get the area of each cell using cartography.area_weights.  
 #This uses the 'cell_area' attribute to calculate the area of each grid-box. 
 data_in.coord('latitude').guess_bounds()
 data_in.coord('longitude').guess_bounds()
 cell_area = iris.analysis.cartography.area_weights(data_in)

 data_out= data_in.collapsed(['latitude', 'longitude'],
                                 iris.analysis.MEAN,
                                  weights=cell_area)
 
 return(data_out) 
 
######## Plotting function for time-series ####################################################

def plot_tseries(data_in, label, linestyle, color, xlabel, ylabel, title, date):

 fig=plt.figure(tight_layout=True)
 ax=plt.gca()

 time=np.arange(0, len(data_in) ,1)
 if date: 
  date= pd.date_range(start='1990-01-01', periods=len(time), freq='A')
  plt.plot(date, data_in, c=color, linestyle=linestyle, linewidth=3, label=label)
 else:
  plt.plot(time, data_in, c=color, linestyle=linestyle, linewidth=3, label=label) 

 plt.xlabel(xlabel, fontsize=14)
 plt.ylabel(ylabel, fontsize=14)
 plt.title(title, fontsize=16)
 plt.legend()

 return()
 
 
###############################  Plot 1D PWS  ################################################################### 

def Plot_S_1D(S, nk, title, period):
 
  fig=plt.figure(figsize=(10,5))
  ax=plt.gca()

  
  if period:
   ax.stem(1/nk, S, linefmt='orange', markerfmt='o',label=' ') 
   ax.set_xlabel("Period (year)")
  else: 
   ax.stem(nk, S, linefmt='blue', markerfmt='o', label=' ')
   ax.set_xlabel('Frequency  (year$^{-1}$)')
  

  ax.set_ylabel('PWSD')
  ax.set_title(title)
  
  return() 


###################################################### LOAD DATA and CALL FUNCTIONS #############################################

#Load Netcdf dataset
SST=iris.load_cube('PI_CMIP6_HadGEM3_100yrs.nc')
print (SST)

#Extract Ocean Basin
where='SO'   #NA= North Atlantic, NP= North Pacific , SO= Southern Ocean 
SST=extract_area(SST, where=where) 

#Compute area-weighted annual time series 
SST=area_weighted(SST)
print (SST)

#Plot SST time-series 
plot_tseries(SST.data, label='PI Control Run', color='black', linestyle='-', xlabel='Years', ylabel='SST (degC)', title='100 years of SST variability, {where}'.format(where=where), date=False)

#Detrend the time-series 
SST=my_function_to_detrend(SST)

#Plot the detrended time-series
plot_tseries(SST.data, label='PI Control Run', color='black', linestyle='-', xlabel='Years', ylabel='SST (degC)', title='100 years of SST variability detrended', date=False)

#Call functions to compute 1D Power Spectrum (PWS)
S, nk=my_function_to_compute_FFT_1D(SST.data) 

#Function to plot PWS. It takes as arguments the PWS (S), the frequencies (nk), the plot title, 
#and a logical switch: period=False for frequencies on x-axis, period=True for period on x-axis. 
Plot_S_1D(S, nk, title='PWS {where}'.format(where=where), period=True)

plt.show()



