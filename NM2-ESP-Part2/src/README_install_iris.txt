1) Upload the ‘jeditor.py’ file provided 
2) File -> New -> Terminal
3) Type :
conda create -y -n iris
source activate iris
conda install -p $HOME/.conda/envs/iris -c conda-forge conda ipykernel iris
python -m ipykernel install --user --name=iris python3 jeditor.py


#To install another packet (for example, Basemap):

source activate iris
conda install -p $HOME/.conda/envs/iris -c conda-forge basemap
