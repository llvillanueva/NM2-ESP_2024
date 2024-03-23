import numpy as np
import matplotlib.pyplot as plt
import scipy
import iris
import pandas as pd
import matplotlib.gridspec as gridspec

from mpl_toolkits.basemap import Basemap

def compute_mean(data_in):
    """
    compute for the mean
    """
    i_sum = 0
    for i in range (0,len(data_in)):
        if np.isnan(data_in[i]):
            continue
        i_sum += data_in[i]
    mean = i_sum/(len(data_in)-np.isnan(data_in).sum())
    return mean

def compute_std(data_in, mean):
    """
    compute for std
    """
    i_std = 0
    for i in range (0, len(data_in)):
        if np.isnan(data_in[i]):
            continue
        i_std += (data_in[i] - mean)**2 
    std = np.sqrt(i_std/(len(data_in)-np.isnan(data_in).sum()-1))
    return std

def plot_tseries(data_in, mean, stdev, label, linestyle, color, title):
    times=np.arange(0,len(data_in),1)
    slope, intercept, r_value, p_value, std_err = scipy.stats.linregress(times, data_in)
    line_fit= slope*times + intercept
    #plt.plot(data_in)
    plt.title(title)
    plt.xlabel('Year')
    plt.ylabel('Temperature (K)')
    plt.axhline(y=mean, c ='black', linestyle = ':', label=r'$\mu$')
    plt.plot(data_in, label=label)
    plt.plot(line_fit, c=color, linestyle=linestyle,linewidth=2, label = 'regression line')
    plt.legend(loc='upper right', bbox_to_anchor=(1.25, 1))
    #Print p values and etc.
    print ('P_VALUE', p_value)
    print ('SLOPE', slope)
    print ('STD_ERR', std_err)
    print (' ')

def compute_codxy(data_x, data_y, mean_x, mean_y):
    """
    compute for the Cod_xy
    """
    i_cod = 0
    for i in range (0,len(data_y)):
        if np.isnan(data_y[i]):
            continue
        i_cod += (data_y[i] - mean_y)*(data_x[i] - mean_x)
    cod_xy = i_cod
    return cod_xy

def compute_devx(data_in, mean):
    """
    compute for the Cod_xy
    """
    i_dev = 0
    for i in range (0,len(data_in)):
        if np.isnan(data_in[i]):
            continue
        i_dev += (data_in[i] - mean)**2
    dev_x = i_dev
    return dev_x

def compute_errvar(data_x, data_y, mean_x, mean_y):
    s2_err = 0
    t1 = 0
    n1 = 0
    d1 = 0
    for i in range (0,len(data_y)):
        if np.isnan(data_y[i]):
            continue
        t1 += (data_y[i] - mean_y)**2 
        n1 += (((data_y[i] - mean_y)**2)*((data_y[i] - mean_y)**2))
        d1 += (data_x[i] - mean_x)**2
    s2_err = t1 - n1/d1
    s2_err = s2_err/((len(data_y)-np.isnan(data_y).sum())-2)
    return s2_err

def compute_tcrit(df, alpha=0.05):
    """
    Find the critical t-value from the t-distribution table.
    """
    critical_value = scipy.stats.t.ppf(1 - alpha/2, df)
    return critical_value

def compute_annual(data_in):
    """
    Compute annual means from monthly means.
    """
    # Reshape the data to 12 months per year
    data_reshaped = data_in.reshape(-1, 12)
    # Compute the mean along the months axis
    annual_means = np.mean(data_reshaped, axis=1)
    return annual_means

def t_student(data_in):
    #our 'x' is time, let's define the x axis accordingly
    time=np.arange(0,len(data_in),1)
    #compute time mean
    mean_t=compute_mean(time)
    #compute data_in mean
    mean_data=compute_mean(data_in)
    #compute Cod_xy
    Cod_xy=compute_codxy(time, data_in, mean_t, mean_data)
    #Now compute Dev_x
    Dev_x= compute_devx(time, mean_t)
    #call a function to compute the error variance (s_err^2)
    df=len(data_in*2)-2 #define degrees of freedom as (n_x+n_y-2)
    err_var=compute_errvar(time, data_in, mean_t, mean_data)
    #Compute now the slope (b1)
    b1= Cod_xy/Dev_x
    #and the standard deviation of the residual of the slope (s_b1) -also known as standard error
    s_b1=np.sqrt(err_var/Dev_x)
    #Finally, compue the t-statistics: t=b1-beta_1/s_b1. Note: beta_1=0 because of our null hypothesis
    t_score=b1/s_b1
    t_crit = compute_tcrit(df)
    p_value = scipy.stats.t.sf(np.abs(t_score), df) * 2
    print ('SLOPE my_function:' , b1)
    print ('STD_ERR SLOPE my_function:' , s_b1)
    print ('t-score:' , t_score)
    print ('critical t-value:', t_crit)
    print("p-value:", p_value)
    return (b1, s_b1, t_score, p_value, t_crit)

def regrid(data_in, grid, binary=True):
    if binary:
        data_gridded=data_in.regrid(grid, iris.analysis.Nearest())
    else:
        data_gridded=data_in.regrid(grid, iris.analysis.Linear())
    #print (data_in, data_in_coarse)
    
    #let's check the regridding result
    '''
    plt.figure()
    qplt.contourf(data_in[0], 25)
    plt.figure()
    qplt.contourf(data_coarse[0], 25)
    plt.show()
    '''
    return data_gridded

def create_map(D1, title):
    fig=plt.figure()
    ax=plt.gca()

    bmap= Basemap(projection= 'gall', llcrnrlat= -90, urcrnrlat= 90, llcrnrlon=0, urcrnrlon= 360, resolution='l')
    
    lon= D1.coord('longitude').points
    lat= D1.coord('latitude').points
    
    x,y=bmap(*np.meshgrid(lon,lat))
    
    contours=bmap.contourf(x,y, D1.data, levels=20, cmap='jet')
    bmap.drawcoastlines()
    
    plt.colorbar()
    plt.title(title)
    plt.show()

def interpolationM(data_in, lat, lon):
    data_inter = {}
    data_inter['latitude'] = lat
    data_inter['longitude'] = lon
    data_inter['Sea Level Pressure (hPa) - Linear'] = []
    data_inter['Sea Level Pressure (hPa) - Nearest Neighbor'] = []
    for i in range(len(lat)): #21=number of sites
        site = [('latitude', lat[i] ), ('longitude', lon[i])]    
        data_inter['Sea Level Pressure (hPa) - Linear'].append(data_in.interpolate(site, iris.analysis.Linear()).data)
        data_inter['Sea Level Pressure (hPa) - Nearest Neighbor'].append(data_in.interpolate(site, iris.analysis.Nearest()).data)
    data_inter = pd.DataFrame(data_inter)
    return data_inter


def create_double_map(D1, D2, title1, title2):
    fig = plt.figure(figsize=(12, 6))

    # Define a grid layout with 1 row and 3 columns (2 for maps, 1 for colorbar)
    gs = gridspec.GridSpec(1, 3, width_ratios=[1, 1, 0.05])

    # Plot the first map
    ax1 = plt.subplot(gs[0])
    bmap1 = Basemap(projection='gall', llcrnrlat=-90, urcrnrlat=90, llcrnrlon=0, urcrnrlon=360, resolution='l', ax=ax1)
    lon1 = D1.coord('longitude').points
    lat1 = D1.coord('latitude').points
    x1, y1 = bmap1(*np.meshgrid(lon1, lat1))
    contours1 = bmap1.contourf(x1, y1, D1.data, levels=20, cmap='jet')
    bmap1.drawcoastlines()
    ax1.set_title(title1)

    # Plot the second map
    ax2 = plt.subplot(gs[1])
    bmap2 = Basemap(projection='gall', llcrnrlat=-90, urcrnrlat=90, llcrnrlon=0, urcrnrlon=360, resolution='l', ax=ax2)
    lon2 = D2.coord('longitude').points
    lat2 = D2.coord('latitude').points
    x2, y2 = bmap2(*np.meshgrid(lon2, lat2))
    contours2 = bmap2.contourf(x2, y2, D2.data, levels=20, cmap='jet')
    bmap2.drawcoastlines()
    ax2.set_title(title2)

    # Add a shared colorbar
    cbar_ax = plt.subplot(gs[2])
    cbar = fig.colorbar(contours2, cax=cbar_ax, orientation='vertical')

    plt.tight_layout()
    plt.show()