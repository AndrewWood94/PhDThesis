This repo provides the methods required to recreate the dataset of walking and hiking tracks in the walking speeds paper.

- [Preparation](#preparation)
    - [OS Terrain 5 DTM](#os-terrain-5-dtm)
    - [Conda Environment](#conda-environment)
    - [Config file](#config-file)
- [Replication](#replication)
    - [GPS tracks](#gps-tracks)
    - [Importing files](#importing-files)
    - [Merging files](#merging-files)
    
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
Area : UK coverage  
Format : ASC

### Conda Environment

A conda environment can be created using the environment.yaml to set up the system 

```Bash
conda create -file=environment.yaml
```
Navigate to the downloaded repository and install this package locally:

```Bash
cd /where/the/package/lives/WalkingSpeedsPaper
pip install .

#alternatively: pip install /where/the/package/lives/WalkingSpeedsPaper
```

### Config file

The file config(Hikr/OSM).yaml contained within this repository is used as the configuration file
for reading and importing the data, and should be edited to point to the correct file locations.
Note there are two config files in the repo for ease of processing - keeping filepaths distinct for each filetype - the variable names in each are identical


- **data**:
  - **GPS_files**:
    - **hikr_website**: #site for web scraper to find gps tracks
    - **folder**: #destination to store scraped GPS files, and where GPS files are read from in processing
  - **terrain**:
    - **terrain_folder**: #path to folder containing Terrain files - Subfolder structure within this is organised in the terrain_classifier.py file within the gps_reader_pkg.
    - **DTM_resolution**: #resolution of the Lidar maps
  - **filetype**:  #filetype should be either Hikr or OSM
  - **listfile_folder**: #folder containing lists of files for multi-processing. For large scale processing, the tracks in the GPS folder are split into separate lists, such that each job receives a fixed subset of the tracks
  - **processed_hikr_filepath**: #If type = OSM, give location of fully processed (merged) hikr data .csv to use in PrepareData script
  - **R_script**: #path to the Prepare_data.R file
- **output**:
  - **imported_gpx_folder**: #location to place successfully imported .gpx files
  - **out_of_scope_folder**: #location to move out-of-scope .gpx files
  - **CSV_folder**: #Location to save processed files

  The following are used in the prepare_R script:
  - **valid_breaks_folder**: #Location to save output files with only valid breaks (>30 seconds) tagged (OPTIONAL)
  - **combined_50_folder**: #Location to save output files with data combined into 50m segments (OPTIONAL)
  - **processed_breaks_folder**:  #Location to save output files processed & filtered dataset with breaks tagged (OPTIONAL)
  - **processed_folder**: #destination folder for fully processed output files 
  - **short_segments_folder**: #Location to save output files with data combined into shorter segments (OPTIONAL)
    
  - **merged_folder**: #Location to save merged processed files for use in analysis
  - **merged_filename**: #Filename for merged processed files for use in analysis

## Replication

This should be completed first with [filetype] = "hikr", as the Hikr data is used to filter OSM data

### GPS tracks:

The list of Hikr tracks used can be be reproduced by running the ```scrape``` script with 
[website] = https://www.hikr.org/region516/ped/?gps=1 in the config file. The GPX tracks will be saved in  
config['data']['GPS_files']['Hikr']['folder']

```Bash
scrape -c config.yaml
```

The OpenStreetMap (OSM) tracks are available [here](http://zverik.openstreetmap.ru/gps/files/extracts/europe/great_britain.tar.xz),

### Importing Files:

The code to import the files is a modified version of the [gpx_segment_importer
QGIS plugin.](https://github.com/SGroe/gpx-segment-importer)  
The ```import_tracks``` script will read the folder, and import gpx files as csv files. The script can be changed to include terrain calculation within this step. The 'check scope' parameter can be used to perform a quick scan through the files to identify and exclude tracks which extend beyond the scope of the region being checked

The output from this is saved in the [output][csv_folder]
Processed files are moved to the [output][imported_gpx_folder] and invalid /out-of-scope files are moved to [out_of_scope_folder], for faster processing in future.

```Bash
import_tracks -c config.yaml
```
### Terrain calculation:

Terrain calculation can be done as part of the track import step, or separately, using the get_terrain_details script. This uses the ```terrain_classifier.py``` to calculate elevation & slope values, terrain obstruction values, and terrain type for each point in the gps track.

The ```terrain_classifier.py``` is set up to calculate UK national grid tile names and find the corresponding file within a fixed file structure. This will need to be modified for working with different regions or data types. An example of remapping data to this structure is in the [remapping](https://github.com/AndrewWood94/PhDThesis/blob/main/Remapping/OSMToNationalGrid.py) file, where OSM road data is split into 10km national grid squares.

The terrain classifier is also used to find the out-of-scope tracks; elevation data was acquired for the whole uk region, so any point returning a null elevation value was outside the scope of the project.

### Break calculation:

Breaks are calculated as part of the ```import_tracks``` script but can be recalculated using ```rerun_breaks```. This uses the ```break_finder.py``` script to process breaks as described in the paper. (This is the biggest slowdown in the processing, as each neighbours for each datapoint are searched for iteratively)

### Valid breaks & Combining data

The ```runR``` script will prepare the data for analysis. Broadly it takes each imported CSV file, identifies & tags outlier points as breaks, incorporates a minimum 30 second threshold to tag 'valid' breaks, and merges data points into segments at least 50m long for analysis. Output files from various steps along this process can be saved for error_checking by including the optional filenames in the config.yaml file. The final output files are saved in the [output][processed_folder] 

### Merging files:

The ```merge_tracks``` script takes all of the files in [output][processed_folder] 
and combines them into a single file, [output][merged_filename], saved in [output][merged_folder]. (this sometimes needs to be broken up into chunks for large numbers of files, as the qgis:mergevectorlayers function cannot handle too many files are once
