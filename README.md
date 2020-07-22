# Kovaak Stats

# Data import

Find your Kovaak folder by clicking "Local Files" -> "Browse Local Files" for Kovaak on Steam.
Navigate to `FPSAimTrainer/stats`. You should see `.csv` files within the folder.

Copy the entire path to Kovaak stats into `kovaak_path.txt` and save it.

To re-run data import, repeat steps 3 and 4 below.

# Setup and run

1. Install R, RStudio and Python

   - https://cran.r-project.org/
   - https://www.rstudio.org/
   - https://www.python.org/

2. Open `KovaakStats.Rproj` with RStudio

3. Enter the full path to `FPSAimTrainer/stats` and save it into `kovaak_path.txt` in the project folder

4. Open `import.py`

5. Click "Source Script" (Ctrl+Shift+S)

6. Open `KovaakStats/app.R`

7. Install missing R packages

8. Click "Run App"
