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
 print ('MEAN my_function:', mean)
 
 mean=data_in.collapsed('time', iris.analysis.MEAN)
 print ('MEAN Iris:', mean.data)
 
 return(mean)
 
 
def compute_stdev_nc(data_in):

 stdev=data_in.collapsed('time', iris.analysis.STD_DEV)
 print ('STDEV Iris:', stdev.data)
 
 return(stdev) 
 

############### Load datasets and call functions #############
#Load txt file and call functions
data_in=np.loadtxt('ERA5_2m_SAT_TS_1990_2023.txt', usecols=2)
#print the first 15 lines of file
print (data_in[:15])

mean=compute_mean(data_in) 



#use iris to load netcdf file 
#'''
data_in=iris.load_cube('ERA5_2m_SAT_TS_1990_2023.nc')
print (data_in) 
mean=compute_mean_nc(data_in)
stdev=compute_stdev_nc(data_in)
#'''


