import iris 
import iris.plot as iplt
import iris.quickplot as qplt

from mpl_toolkits.basemap import Basemap, maskoceans, shiftgrid 

import numpy as np
import matplotlib.pyplot as plt

import scipy
from scipy import stats

import pandas as pd



#################################### function to compute annual means from monthly data ########################################################################################################

def compute_annual(data):

 #compute annual means: 
 months=len(data)
 years=[]
 for i in range (12, months+1, 12):
  st=i-12
  years.append(np.average(data[st:i]))

 #print (len(years))
 return(years)


###### Define Fucntion to compute mean of input dataset ############
#working with ASCII files 
def compute_mean(data_in):
 
 i_sum=0
 for i in range (0,len(data_in)):
  i_sum=data_in[i]+i_sum
  
 mean=i_sum/len(data_in) 
 #print ('MEAN my_function:', mean)
 
 #mean=np.average(data_in)
 #mean=np.nanmean(data_in)
 #print ('MEAN np.average:', mean)
 
 return(mean)

 
#working with ASCII files  
def compute_stdev(data_in, mean):
 
 i_sum=0
 for i in range (0,len(data_in)):
  error=(data_in[i]-mean)**2.
  i_sum=error+i_sum
  
 stdev=(i_sum/(len(data_in)-1))**0.5
 #print ('STDEV my_function:', stdev)
 
 #stdev=np.std(data_in)
 #stdev=np.nanstd(data_in)
 #print ('STDEV np.std:', stdev)
 
 return(stdev)

 
######## Compute slope and t-statistics  for input dataset ##########
def t_student(data_in):

 #our 'x' is time, let's define the x axis accordingly 
 time=np.arange(0,len(data_in),1)
 #compute time mean
 mean_t=compute_mean(time)
 #compute data_in mean
 mean_data=compute_mean(data_in)
 
 #compute Cod_xy
 Cod_xy=..
 
 #Now compute Dev_x
 Dev_x=..
 
 #call a function to compute the error variance (s_err^2)
 df=len(data_in*2)-2 #define degrees of freedom as (n_x+n_y-2)
 err_var=my_function(...)
 
 #Compute now the slope (b1)
 b1=...
 #and the standard deviation of the residual of the slope (s_b1) -also known as standard error 
 s_b1=...
 
 #Finally, compue the t-statistics: t=b1-beta_1/s_b1. Note: beta_1=0 because of our null hypothesis 
 t_score=b1/s_b1
 
 
 print ('SLOPE my_function' , b1)
 print ('STD_ERR SLOPE my_function' , s_b1)
 print ('t-score my_function' , t_score)
 print ('       ')

 return(b1, s_b1, t_score)
 
######## Plotting function for time-series ############
def plot_tseries(data_in, mean, stdev, label, linestyle, color, title):

 fig=plt.figure(tight_layout=True)
 ax=plt.gca()

 months=len(data_in)
 times=np.arange(0,months,1)
 date= pd.date_range(start='1990-01-01', periods=len(times), freq='M')  #for monthly means 
 #date= pd.date_range(start='1990-01-01', periods=len(times), freq='A') #for annual means
 plt.plot(date, data_in, c=color, linestyle=linestyle, linewidth=3, label=label)
 plt.axhline(y=mean, color='grey', linestyle='--',linewidth=3)
 
 slope, intercept, r_value, p_value, std_err=stats.linregress(times,  data_in)
 line_fit=slope*times +intercept
 plt.plot(date, line_fit, c='red', linestyle=':',linewidth=2)
 #Print p values
 print ('P_VALUE', p_value)
 print ('SLOPE', slope)
 print ('STD_ERR', std_err)
 print ('     ')

  
 plt.ylabel('K', fontsize=14)
 plt.ylim(270,305)
 plt.title(title, fontsize=16)
 plt.legend()

 return()
 


############### Load datasets #############
#Load txt file and call functions
data_in=np.loadtxt('ERA5_2m_SAT_TS_1990_2023.txt', usecols=2)

mean=compute_mean(data_in) 
stdev=compute_stdev(data_in, mean)

#Plot data with regression line and print out 'slope, std_err, p_value'
plot_tseries(data_in, mean, stdev, label='ERA5', color='black', linestyle='-', title='2m air Temperature @ Trieste 1990-2023')

#call function to perform t-test 
slope, std_err, t_score=t_student(data_in)
plt.show()


#Compute annual means 
data_in=compute_annual(data_in)
mean=compute_mean(data_in) 
stdev=compute_stdev(data_in, mean)
plot_tseries(data_in, mean, stdev, label='ERA5', color='black', linestyle='-', title='Annual 2m air Temperature @ Trieste 1990-2023')

slope, std_err, t_score=t_student(data_in)
plt.show()