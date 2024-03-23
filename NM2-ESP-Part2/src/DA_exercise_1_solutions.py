import iris 
import iris.plot as iplt
import iris.quickplot as qplt

from mpl_toolkits.basemap import Basemap, maskoceans, shiftgrid 

import numpy as np
import matplotlib.pyplot as plt

import scipy
from scipy import stats

import pandas as pd



###### Define Fucntion to compute mean of input dataset ############
#working with ASCII files 
def compute_mean(data_in):

 #mean=np.sum(data_in)/len(data_in)
 #print ('Method 1'),(mean)
 
 i_sum=0
 for i in range (0,len(data_in)):
  i_sum=data_in[i]+i_sum
  
 mean=i_sum/len(data_in) 
 print ('MEAN my_function:', mean)
 
 mean=np.average(data_in)
 #mean=np.nanmean(data_in)
 print ('MEAN np.average:', mean)
 
 return(mean)

#working with NetCDF files  
def compute_mean_nc(data_in):
 
 i_sum=0
 for i in range (0,len(data_in.data)):
  i_sum=data_in[i].data+i_sum
  
 mean=i_sum/len(data_in.data) 
 print (mean)
 
 mean=data_in.collapsed('time', iris.analysis.MEAN)
 print ('MEAN Iris:', mean.data)
 
 return(mean)
 
 
#working with ASCII files  
def compute_stdev(data_in, mean):
 
 i_sum=0
 for i in range (0,len(data_in)):
  error=(data_in[i]-mean)**2.
  i_sum=error+i_sum
  
 stdev=(i_sum/(len(data_in)-1))**0.5
 print ('STDEV my_function:', stdev)
 
 stdev=np.std(data_in)
 #stdev=np.nanstd(data_in)
 print ('STDEV np.std:', stdev)
 
 return(stdev)

#working with NetCDF files  
def compute_stdev_nc(data_in):

 
 stdev=data_in.collapsed('time', iris.analysis.STD_DEV)
 print ('STDEV Iris:', stdev.data)
 
 return(stdev) 

######## Plotting function for time-series ############
def plot_tseries(data_in, mean, stdev, label, linestyle, color, title):

 fig=plt.figure(tight_layout=True)
 ax=plt.gca()

 months=len(data_in)
 times=np.arange(0,months,1)
 date= pd.date_range(start='1990-01-01', periods=len(times), freq='MS')
 plt.plot(date, data_in, c=color, linestyle=linestyle, linewidth=3, label=label)
 plt.axhline(y=mean, color='grey', linestyle='--',linewidth=3)
 #plt.fill_between(times, mean, mean+(2*stdev) , alpha=0.5, facecolor='red')
 #plt.fill_between(times, mean, mean-(2*stdev) , alpha=0.5, facecolor='gray')

  
 #plt.xlabel('Time(months)', fontsize=14)
 plt.ylabel('K', fontsize=14)
 #plt.xlim(0,408)
 plt.ylim(270,305)
 plt.title(title, fontsize=16)
 plt.legend()

 return()
 


############### Load datasets #############
#Load txt file and call functions
data_in=np.loadtxt('ERA5_2m_SAT_TS_1990_2023.txt', usecols=2)
#data_in=np.loadtxt('ERA5_2m_SAT_TS_1990_2023_nan.txt', usecols=2)
#print the first 15 lines of file
print (data_in[:15])

mean=compute_mean(data_in) 
stdev=compute_stdev(data_in, mean)


#if NaNs are present, mask them in input array
'''data_masked = np.ma.masked_invalid(data_in)
mean=compute_mean(data_masked)
stdev=compute_stdev(data_masked, mean)'''

plot_tseries(data_in, mean, stdev, label='ERA5', color='black', linestyle='-', title='2m air Temperature @ Trieste 1990-2023')
plt.show()


#use iris to load netcdf file 
'''
data_in=iris.load_cube('ERA5_2m_SAT_TS_1990_2023.nc')
print (data_in) 
mean=compute_mean_nc(data_in)
stdev=compute_stdev_nc(data_in)
'''



