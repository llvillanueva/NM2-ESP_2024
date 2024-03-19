import numpy as np
import matplotlib.pyplot as plt
import scipy

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