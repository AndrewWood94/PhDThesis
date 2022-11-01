This repo provides the methods required to recreate the dataset of walking and hiking tracks in the walking speeds paper.

- [Preparation](#preparation)
    - [OS Terrain 5 DTM](#os-terrain-5-dtm)
    - [Conda Environment](#conda-environment)
    - [Config file](#config-file)
- [Replication](#replication)
    - [GPS tracks](#gps-tracks)
    - [Importing files](#importing-files)
    - [Tagging breakpoints](#tagging-breakpoints)
    - [Merging files](#merging-files)
    - [Terrain Calculation](#terrain-calculation)
    - [Data combination & filtering](#data-combination--filtering)
    
For replication of the methods used in the paper, the full guide should be followed, 
however to simply recreate the dataset of filtered walking tracks you should follow the guide to recreate the Hikr data, 
then jump straight to the [Terrain Calculation](#terrain-calculation) using the unzipped OSM.csv files in the data folder as inputs,
after completing the [Preparation](#preparation). (Hikr data cannot be provided directly due to copyright).
 
Note: If replicating the methods in full, be aware that the ```find_breaks``` script takes a very long time to run. 
(~1 week on 2018 MacBook Air, 1.6 GHz Dual-Core Intel Core i5, 8 GB 2133 MHz LPDDR3)


## Preparation

### OS Terrain 5 DTM

Due to licensing requirements, the Digital Terrain Map containing elevation data required for calculating both walking and hill slope angles 
is not available for public download and must be accessed separately. It is available under an Educational License from Digimap and can be 
accessed using the [Bulk Download Request form](https://goo.gl/FxyyFs),
with the following criteria:

Data Collection : Digimap: Ordnance Survey  
Dataset : OS Terrain 5 DTM  
Area : Scotland coverage  
Format : ASC

This should result in receiving data for the region covered by the following 100 km National Grid tiles:

                HP
             HT HU
       HW HX HY HZ
    NA NB NC ND   
    NF NG NH NJ NK
    NL NM NN NO
       NR NS NT NU
       NW NX NY 

Note: the NY tile may not contain all constituent 5km tiles, as the majority of the NY region covers England. 
The following NY tiles are sufficient to include all data for Scotland, however having more files will not cause issues.

    09 19 29 39 49 59 69
    08 18 28 38 48 58 
    07 17 27 37 47 
    06 16 26 36 
        
### Conda Environment

Create a conda environment and install python, QGIS, R, click and pandas.
If QGIS or R are already present on the system these can be left out

```Bash
conda create -n [environment_name] python=3.8
conda activate [environment_name]
conda install -c conda-forge qgis=3.18.2
conda install -c conda-forge r=4.0
conda install -c conda-forge click=7.1.2
conda install -c conda-forge pandas=1.2.4
```

If recreating data from scratch, scrapy is also required:
```Bash
conda install -c conda-forge scrapy=2.4.1
```

Navigate to the downloaded repository and install this package locally:

```Bash
cd /where/the/package/lives/WalkingSpeedsPaper
pip install .

#alternatively: pip install /where/the/package/lives/WalkingSpeedsPaper
```

### Config file

The file config.yaml contained within this repository is used as the configuration file
for reading and importing the data, and should be edited to point to the correct file locations.
Descriptions of each variable, and the scripts in which they are used are shown below. 

- **filetype** :  
should be set to either *osm* or *hikr* depending on which data type is being read  
```run_gpx_importer```
```prepare_data.R```
- **data**
  - **os_grid_folder** :  
  Path to folder containing OS National grid files (data/os-grids)
    ```
    .
    ├── os-grids
        └── 10km_grid_region.shp
        └── 100km_grid_region.shp
         ⋮
    ```
    ```run_gpx_importer```
  - **GPS_files**
    - **hikr**
      - **website** :  
      Address of Hikr results to parse for GPS data  
      ```scrape```  
      - **folder** : 
      Path to location of json file containing hikr data links  
      ```scrape```
      ```run_gpx_importer``` 
      - **name** :  
      Filename of json file containing hikr data links  
      ```scrape```
      ```run_gpx_importer``` 
    - **osm**
      - **folder** :  
      Path to location of OpenStreetMap tracks to be read  
      ```run_gpx_importer``` 
  - **terrain**
    - **DTM_folder** :  
    Path to top level folder containing DTM files e.g  
        ```
        .
        ├── DTM_folder
            └── hp
                └── HP40NE.asc
                └── HP40NE.prj
                └── HP40NE.gml
                └── Metadata_HP40NE.xml
                 ⋮
            └── ht
             ⋮
        ```
        ```get_terrain```
    - **DTM_resolution** :  
    Resolution of DTM data (should be set to 5)  
    ```get_terrain```  
  - **processed_hikr_filepath** :  
  If ```type``` = osm, give filepath of processed hikr data to use for filtering tracks in ```prepare_data.R```  
  ```prepare_data.R```
- **conditions**
  - **data_filter** :  
  Whether to run initial filter on OSM data to remove files which are not in scope 
  (not required but speeds up processing), default is True, should be set to False if using pre-filtered dataset  
  ```run_gpx_importer```
  - **in_scope_folder** :  
  Location to copy & save in-scope .gpx files  
  ```run_gpx_importer```
- **output** 
  - **gpkg_folder** :  
  Location to save .gpkg output files  
  ```run_gpx_importer```
  ```find_breaks```
  ```merge_tracks```
  - **name_root** :  
  Name stem for .gpkg files, indexes are automatically added for each new track & segment, e.g [name_root]0_001   
  ```run_gpx_importer```
  - **merged_folder** :  
  Location to save merged .gpkg and .csv files  
  ```merge_tracks```
  ```get_terrain```
  ```prepare_data.R``` 
  - **merged_name** :  
  File name to save merged .gpkg and .csv files  
  ```merge_tracks```
  ```get_terrain```
  ```prepare_data.R```
  - **processed_folder** :  
  Destination folder for processed & filtered output files  
  ```prepare_data.R```
  - **optional**
    - **valid_breaks_filename** :  
    Filename for dataset with only valid breaks (>30 seconds) tagged  
    ```prepare_data.R```
    - **combined_50_filename** :  
    Filename for dataset with data combined into 50m segments   
    ```prepare_data.R```
    - **processed_breaks_filename** :  
    Filename for processed & filtered dataset with breaks tagged  
    ```prepare_data.R```
  - **processed_filename** :  
  Filename for processed & filtered dataset with breaks/non-walking sections removed  
  ```prepare_data.R```

## Replication

### GPS tracks:

The list of Hikr tracks used can be found in hikr_filepaths.json file in the data folder.
If desired, this file can be reproduced by running the ```scrape``` script with 
[website] = https://www.hikr.org/region518/ped/?gps=1 in the config file.

```Bash
scrape -c config.yaml

#windows: python scripts/scrape -c config.yaml
```

The OpenStreetMap (OSM) tracks used are saved in the scotland-osm-tracks.zip file which should be unzipped. This is a copy of 
the planet.osm gpx file list for Scotland which is available [here](http://zverik.openstreetmap.ru/gps/files/extracts/europe/great_britain),
reduced to only include 'Identifiable' or 'Trackable' tracks, i.e. those which contain timestamps in the .gpx file.

### Importing Files:

The code to import the files is a modified version of the [gpx_segment_importer
QGIS plugin.](https://github.com/SGroe/gpx-segment-importer)  
The ```run_gpx_importer``` script will the .json file ([filetype] = hikr), or folder ([filetype] = osm), and import gpx files as .gpkg files.
These files are saved to the config [output][gpkg_folder].

If running with [filetype] = osm and [data_filter] = True, this will also save all of the valid gpx files to [in_scope_folder], which can be used as
the input location in future for faster processing.

```Bash
run_gpx_importer -c config.yaml

#windows: python scripts/run_gpx_importer -c config.yaml
```

It is important to change the output file location or name between runs, as previous tracks will be overwritten.
It is recommended that separate output folder are used for Hikr and OSM data, as they will need to be in separate folders
at the Merge Files stage.

### Tagging Breakpoints:

The ```find_breaks``` script will add an 'OnBreak' attribute to each gpkg file, using the methods outlined in the paper.  
This script will also delete files which are clearly not walking tracks (median speed > 10km/h), 
or don't contain enough data to be useful (distance < 250m or duration < 2.5 minutes)

```Bash
find_breaks -c config.yaml

#windows: python scripts/find_breaks -c config.yaml
```

### Merging files:

The ```merge``` script takes all of the files in [output][gpkg_folder] 
and combines them into a single file, [output][merged_filename], saved in [output][merged_folder].  
It is important to merge Hikr and OSM files separately, as the merged Hikr tracks are used to filter non-walking tracks out 
of the OSM dataset later on.

```Bash
merge_tracks -c config.yaml

#windows: python scripts/merge_tracks -c config.yaml
```

### Terrain Calculation

The ```get_terrain``` script will calculate the elevation and slope values for each line segment from the OS terrain data.  
It takes the [merged_name] .csv file in [merged_folder] and adds the following attributes:
- a_OS height
- b_OS height
- OS height_diff
- a_OS slope
- b_OS slope  

(a_ is the value at the start of the line segment, b_ is the value at the end)

```Bash
get_terrain -c config.yaml

#windows: python scripts/get_terrain -c config.yaml
```

### Data combination & filtering

The ```prepare_data.R``` script reads the [merged_filename] .csv and 
filters/combines the data for use in modelling. 
This script must first be run with [filetype] = hikr. The [processed_filename] output of this 
should then be set as the the [processed_hikr_filepath]  input when running with [filetype] = osm, so that 
the OSM data can be filtered to remove any non-walking tracks or segments.

```bash
prepare_data.R config.yaml

#windows: Rscript scripts/prepare_data.R config.yaml
```

The outputs of these can be merged together in R to produce the dataset used for modelling walking speeds.

```R
#R script 

> combined_dataset = rbind(hikr_output_file, osm_output_file)
```
