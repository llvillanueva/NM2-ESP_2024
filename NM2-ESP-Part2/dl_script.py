from bs4 import BeautifulSoup
import os
import requests

# Adapt path to your own computer :
path_data = '/home/llvillanueva/ESP_2nd_Term/NM2-ESP_2024/NM2-ESP-Part2/data/' 
path_prog = '/home/llvillanueva/ESP_2nd_Term/NM2-ESP_2024/NM2-ESP-Part2/src/' 
webpage = 'http://clima-dods.ictp.it/Users/mguarino/Data_Analysis_2023_2024/python/'

try:
    r = requests.get(webpage)
    data = r.text
    soup = BeautifulSoup(data, 'html.parser')
    
    for link in soup.find_all('a'):
        href = link.get('href')
        if href.endswith('.nc') or href.endswith('.txt'):   
            name = href.split('/')[-1]  # Extract PDF name from the URL            
            if not os.path.exists(os.path.join(path_data, name)):  # Check if PDF already exists            
                try:
                    os.system(f'wget {webpage}{href} -P {path_data}')
                    os.system(f'chmod a+xw {path_data}{href}')
                except Exception as e:
                    print(f'An error occurred: {str(e)}')
            else:
                print(f"Skipping download, '{name}' already exists.")
        elif href.endswith('.py'):
            name = href.split('/')[-1]  # Extract PDF name from the URL  
            if not os.path.exists(os.path.join(path_prog, name)):  # Check if PDF already exists          
                try:
                    os.system(f'wget {webpage}{href} -P {path_prog}')
                    os.system(f'chmod a+xw {path_prog}{href}')
                except Exception as e:
                    print(f'An error occurred: {str(e)}')      
                else:
                    print(f"Skipping download, '{name}' already exists.")  
except Exception as e:
    print(f'An error occurred: {str(e)}')
