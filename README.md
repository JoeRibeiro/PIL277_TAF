Repository for pil.27.7 stock assessment (sardine)

## How to run assessment

* After cloning this repository, please open pil277TAF.Rproj in rstudio and   ▢-> source   the bootstrap.r script.
* Note it is NOT set up to run in base R, and requires that you are running it from Rstudio! No guarantees that it will work in R gui.
* If the correct packages are not already installed, bootstrap.r will prompt you to install them, then you will need to source bootstrap.r a second time with the packages installed.

The full running order is:
 - bootstrap.r
 - data.r
 - model.r
 - output.r
 - report.r

Data for this assessment are held in this github repository in the folder "pil277TAF/bootstrap/initial/data/".

This folder should only be read once as part of the bootstrap procedure, where the origins of each piece of data are outlined in a .bib metadata file, and are then saved in the root data folder "pil277TAF\data".

Files data.r and all onward scripts should not read from bootstrap/initial/data/, as these files lack any metadata.

This is the standard TAF process.