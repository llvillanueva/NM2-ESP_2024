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

 
def compute_err_var(y, x, mean_y, mean_x, Dev_x, df):
  
 #To compute the error variance we use the formula: s_err^2= Dev_y*(1-r^2)/df  (see lectrure notes for expansion)
 
 #1. Compute Dev_y
 i_sum=0
 for i in range (0,len(y)):
  error_y=(y[i]-mean_y)**2.
  i_sum=error_y+i_sum
 Dev_y=i_sum
 
 #2. Compute remaining term needed for the calcualtions
 i_sum=0
 for i in range (0,len(x)): #note x and y have same length
  error_x=(x[i]-mean_x)**2.
  error_y=(y[i]-mean_y)**2.
  error_xy=error_x*error_y
  i_sum=error_xy+i_sum
 error_xy_tot=i_sum 

 
 #Finally, compute s_err^2
 err_var=1/df*(Dev_y-(error_xy_tot/Dev_x))
 
 return(err_var) 
 
######## Compute slope and t-statistics  for input dataset ##########
def t_student(data_in):

 #our 'x' is time, let's define the x axis accordingly 
 time=np.arange(0,len(data_in),1)
 #compute time mean
 mean_t=compute_mean(time)
 #compute data_in mean
 mean_data=compute_mean(data_in)
 
 #compute Cod_xy
 i_sum=0
 for i in range (0,len(data_in)):
  error_x=time[i]-mean_t
  error_y=data_in[i]-mean_data
  error=error_x*error_y
  i_sum=error+i_sum
 Cod_xy=i_sum
 
 #Now compute Dev_x
 i_sum=0
 for i in range (0,len(time)):
  error_x=(time[i]-mean_t)**2.
  i_sum=error_x+i_sum
 Dev_x=i_sum
 
 #call a function to compute the error variance (s_err^2)
 df=len(data_in*2)-2 #define degrees of freedom as (n_x+n_y-2)
 err_var=compute_err_var(data_in, time, mean_data, mean_t, Dev_x, df)
 
 #Compute now the slope (b1)
 b1=Cod_xy/Dev_x
 #and the standard deviation of the residual of the slope (s_b1) -also known as standard error 
 s_b1=(err_var/Dev_x)**0.5
 
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
 date= pd.date_range(start='1990-01-01', periods=len(times), freq='M')
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

  
 #plt.xlabel('Time(months)', fontsize=14)
 plt.ylabel('K', fontsize=14)
 #plt.xlim(0,408)
 plt.ylim(270,305)
 plt.title(title, fontsize=16)
 plt.legend()

 return()
 
def plot_tseries_annual(data_in, mean, stdev, label, linestyle, color, title):

 fig=plt.figure(tight_layout=True)
 ax=plt.gca()

 months=len(data_in)
 times=np.arange(0,months,1)
 date= pd.date_range(start='1990-01-01', periods=len(times), freq='A')
 plt.plot(date, data_in, c=color, linestyle=linestyle, linewidth=3, label=label)
 plt.axhline(y=mean, color='grey', linestyle='--',linewidth=3)
 plt.fill_between(date, mean,  np.ma.masked_where(data_in <= mean, data_in) , alpha=0.5, facecolor='red')
 plt.fill_between(date, mean,  np.ma.masked_where(data_in >= mean, data_in), alpha=0.5, facecolor='gray')
 
 slope, intercept, r_value, p_value, std_err=stats.linregress(times,  data_in)
 line_fit=slope*times +intercept
 plt.plot(date, line_fit, c='black', linestyle=':',linewidth=2)
 print ('P_VALUE', p_value)
 print ('SLOPE', slope)
 print ('STD_ERR', std_err)
 print ('     ')

  
 plt.ylabel('K', fontsize=14)
 plt.ylim(284,288)
 plt.title(title, fontsize=16)
 plt.legend()

 return()
 
def plot_tseries_annual_nc(data_in, mean, stdev, label, linestyle, color, title):

 fig=plt.figure(tight_layout=True)
 ax=plt.gca()

 months=data_in.shape[0]
 times=np.arange(0,months,1)
 date= pd.date_range(start='1990-01-01', periods=len(times), freq='A')
 plt.plot(date, data_in.data, c=color, linestyle=linestyle, linewidth=3, label=label)
 plt.axhline(y=mean.data, color='grey', linestyle='--',linewidth=3)
 plt.fill_between(date, mean.data,  np.ma.masked_where(data_in.data <= mean.data, data_in.data) , alpha=0.5, facecolor='red')
 plt.fill_between(date, mean.data,  np.ma.masked_where(data_in.data >= mean.data, data_in.data), alpha=0.5, facecolor='gray')
 
 slope, intercept, r_value, p_value, std_err=stats.linregress(times,  data_in.data)
 line_fit=slope*times +intercept
 plt.plot(date, line_fit, c='black', linestyle=':',linewidth=2)
 print ('P_VALUE', p_value)
 print ('  ')

  
 plt.ylabel('K', fontsize=14)
 plt.ylim(284,288)
 plt.title(title, fontsize=16)
 plt.legend()

 return()

############### Load datasets #############
#Load txt file and call functions
data_in=np.loadtxt('ERA5_2m_SAT_TS_1990_2023.txt', usecols=2)

mean=compute_mean(data_in) 
stdev=compute_stdev(data_in, mean)

plot_tseries(data_in, mean, stdev, label='ERA5', color='black', linestyle='-', title='2m air Temperature @ Trieste 1990-2023')

slope, std_err, t_score=t_student(data_in)

#Compute annual means 
data_in=compute_annual(data_in)
mean=compute_mean(data_in) 
stdev=compute_stdev(data_in, mean)

plot_tseries_annual(data_in, mean, stdev, label='ERA5', color='black', linestyle='-', title='Annual 2m air Temperature @ Trieste 1990-2023')

slope, std_err, t_score=t_student(data_in)

plt.show()
