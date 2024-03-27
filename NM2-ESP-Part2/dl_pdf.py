from bs4 import BeautifulSoup
import os
import requests

path_lec = '/home/llvillanueva/ESP_2nd_Term/NM2-ESP_2024/NM2-ESP-Part2/'
webpage = 'http://clima-dods.ictp.it/Users/mguarino/Data_Analysis_2023_2024/'

try:
    r = requests.get(webpage)
    data = r.text
    soup = BeautifulSoup(data, 'html.parser')
    
    for link in soup.find_all('a'):
        href = link.get('href')
        if href.endswith('.pdf'):   
            pdf_name = href.split('/')[-1]  # Extract PDF name from the URL
            if not os.path.exists(os.path.join(path_lec, pdf_name)):  # Check if PDF already exists
                try:
                    os.system(f'wget {webpage}{href} -P {path_lec}')
                    os.system(f'chmod a+xw {os.path.join(path_lec, pdf_name)}')
                except Exception as e:
                    print(f'An error occurred: {str(e)}')
            else:
                print(f"Skipping download, '{pdf_name}' already exists.")
except Exception as e:
    print(f'An error occurred: {str(e)}')

