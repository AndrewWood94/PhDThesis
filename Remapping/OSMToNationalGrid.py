import os
import math
from gps_reader_pkg.folder_methods import create_folder
from gps_reader_pkg.qgis_app_init import QgisSetup
from gps_reader_pkg.vector_file_writer import VectorFileWriter
from pathlib import Path
from qgis.core import QgsVectorLayer, QgsCoordinateTransform, QgsCoordinateReferenceSystem



def main():

    qgs = QgisSetup()
    if not qgs:
        return False

    from qgis import processing
    from processing.core.Processing import Processing

    Processing.initialize()

    gridpath10 = '/Volumes/LaCie/LaCie/LaCie Rugged USB-C/UK Data/OSM Paths/gb-grids_4126495/10km_grid_region.shp'
    grid10=QgsVectorLayer(gridpath10, 'grid10', 'ogr')

    OSM_scotland_path = "/Volumes/LaCie/LaCie/LaCie Rugged USB-C/UK Data/OSM Paths/scotland-latest-free.shp/gis_osm_roads_free_1.shp"
    OSM_wales_path = "/Volumes/LaCie/LaCie/LaCie Rugged USB-C/UK Data/OSM Paths/wales-latest-free.shp/gis_osm_roads_free_1.shp"
    OSM_england_path = "/Volumes/LaCie/LaCie/LaCie Rugged USB-C/UK Data/OSM Paths/england-latest-free.shp/gis_osm_roads_free_1.shp"
    OSM_scotland_layer=QgsVectorLayer(OSM_scotland_path, 'OSM_line', 'ogr')
    OSM_wales_layer=QgsVectorLayer(OSM_wales_path, 'OSM_line', 'ogr')
    OSM_england_layer=QgsVectorLayer(OSM_england_path, 'OSM_line', 'ogr')

    fileWriter = VectorFileWriter(None)
    fileWriter.options.attributes = [2]

    tf=QgsCoordinateTransform()
    ref=QgsCoordinateReferenceSystem()
    tf.setSourceCrs(ref.fromEpsgId(4326))
    tf.setDestinationCrs(ref.fromEpsgId(27700))
    fileWriter.options.ct=tf
    prevpath = ""

    skip = True
    for feature in grid10.getFeatures():
        if feature['TILE_NAME'] == "TQ67":
            skip = False
        if skip:
            continue
        box = feature.geometry().boundingBox()
        fileWriter.options.filterExtent = box
        path = "/Volumes/LaCie/LaCie/LaCie Rugged USB-C/UK Data/OSM path/" + feature['TILE_NAME'][0:2]
        if path != prevpath:
            create_folder(Path(path))
            prevpath = path
        fileWriter.output_directory = path
        writer_s = fileWriter.writeGPKG(OSM_scotland_layer, overwrite=True, name=(feature['TILE_NAME'] + '_s'))
        writer_w = fileWriter.writeGPKG(OSM_wales_layer, overwrite=True, name=(feature['TILE_NAME'] + '_w'))
        writer_e = fileWriter.writeGPKG(OSM_england_layer, overwrite=True, name=(feature['TILE_NAME'] + '_e'))

        layerlist = []

        layerlist.append(writer_s + '|layername=' + feature['TILE_NAME'] + '_s')
        layerlist.append(writer_w + '|layername=' + feature['TILE_NAME'] + '_w')
        layerlist.append(writer_e + '|layername=' + feature['TILE_NAME'] + '_e')

        parameter_dictionary = {'CRS': QgsCoordinateReferenceSystem('EPSG:27700'),
                                'LAYERS': layerlist,
                                'OUTPUT': path + '/' + feature['TILE_NAME'] + '.gpkg'}

        processing.run("qgis:mergevectorlayers", parameter_dictionary)

    qgs.exit()

if __name__ == "__main__":
    main()