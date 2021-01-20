mkdir xengagement && cd xengagement
c:/users/aelhabr/appdata/local/programs/python/python37/python -m venv venv
venv\scripts\activate.bat
# MUST BE BACKSLASHES WHEN ACTIVATING
python -m pip install --upgrade pip
python -m pip install dash==1.13.3 pandas==1.0.5