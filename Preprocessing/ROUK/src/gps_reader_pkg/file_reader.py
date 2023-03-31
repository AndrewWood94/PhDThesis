"""
/***************************************************************************
 This file was taken and adapted from:

 GpxSegmentImporter
                                 A QGIS plugin
 This plugin imports an GPX file and creates short line segments between track points
                              -------------------
        begin                : 2017-12-01
        git sha              : $Format:%H$
        copyright            : (C) 2018 by Simon Gröchenig @ Salzburg Research
        email                : simon.groechenig@salzburgresearch.at


        MODIFIED:
        begin                : 2019-11-04
        email                : andrew.wood@ed.ac.uk
 ***************************************************************************/
/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
"""

"""
Read a gpx file and save it as a gpkg file, with point attributes calculated
"""

from xml.etree import ElementTree
from qgis.core import (QgsCoordinateTransform, QgsPoint, QgsCoordinateReferenceSystem, QgsPointXY)
from gps_reader_pkg.datatype_definition import (DataTypeDefinition, DataTypes)
from gps_reader_pkg.gpx_feature_builder import GpxFeatureBuilder
from gps_reader_pkg.geom_tools import GeomTools
from gps_reader_pkg.terrain_classifier import TerrainClassifier


class GpxFileReader:
    """
    Class to read gpx files and assemble vector layers
    """

    def __init__(self, output_directory=None, name=None, overwrite=False, terrain_path=None, track_crs=4326,
                 terrain_crs=27700):
        """
        Setup file reader with required file locations

        :param output_directory: location to save output files
        :param name: filename for output files
        :param overwrite: overwrite existing filenames
        :param terrain_path: folder containing terrain DTM files
        :param track_crs: coordinate system of gps file
        :param terrain_crs: coordinate system of terrain files
        """

        self.namespace = None
        self.vector_layer_builder = None
        self.name = name
        self.error_message = ''
        self.crs = QgsCoordinateReferenceSystem('EPSG:4326')
        self.output_directory = output_directory
        self.overwrite = overwrite
        self.segment_number = 0
        if terrain_path is not None:
            self.terrain_classifier = TerrainClassifier(terrain_path)

        self.tf = QgsCoordinateTransform()
        self.tf.setSourceCrs(QgsCoordinateReferenceSystem().fromEpsgId(track_crs))
        self.tf.setDestinationCrs(QgsCoordinateReferenceSystem().fromEpsgId(terrain_crs))
        self.segmentList = []
        self.attribute_definitions = list()

    def reset(self, trackname):
        """
        Setup empty vector layer, with attributes included
        :param trackname: Full nanme of vecot layer (including Track/Segment number)
        :return:
        """
        self.vector_layer_builder = GpxFeatureBuilder(trackname, self.attribute_definitions, self.crs)

    def import_gpx_file(self, file_path,
                        calculate_motion_attributes=False,
                        get_elevation_slope=False,
                        get_obstruction=False,
                        get_terrain_type=False,
                        road_radius=50,
                        min_duration=1, min_distance=0, write_file=True):

        """
        :param road_radius:
        :param get_terrain_type:
        :param get_elevation_slope:
        :param get_obstruction:
        :param file_path: gpx file to import
        :param calculate_motion_attributes: Calculate speed, distance, elevation change
        :param min_duration: Minimum time between saved point
        :param min_distance: Minimum distance between saved points
        :param write_file: Save output as gpkg?
        :return:
        """

        self.segment_number = 0
        self.segmentList = []

        tree = ElementTree.parse(file_path)
        root = tree.getroot()

        # https://stackoverflow.com/questions/1953761/accessing-xmlns-attribute-with-python-elementree
        if root.tag[0] == "{":
            uri, ignore, tag = root.tag[1:].partition("}")
            self.namespace = {'gpx': uri}

        self.error_message = ''

        # Setup attributes for first segment
        self.attribute_definitions = list()
        self.attribute_definitions.append(DataTypeDefinition('track_name', DataTypes.String, True))
        self.attribute_definitions.append(DataTypeDefinition('segment_no', DataTypes.Double, True))
        self.attribute_definitions.append(DataTypeDefinition('time', DataTypes.String, True))
        if calculate_motion_attributes:
            self.attribute_definitions.append(DataTypeDefinition('distance', DataTypes.Double, True))
            self.attribute_definitions.append(DataTypeDefinition('duration', DataTypes.Double, True))
            self.attribute_definitions.append(DataTypeDefinition('speed', DataTypes.Double, True))
        if get_elevation_slope:
            self.attribute_definitions.append(DataTypeDefinition('hill_slope', DataTypes.Double, True))
            self.attribute_definitions.append(DataTypeDefinition('elevation', DataTypes.Double, True))
            self.attribute_definitions.append(DataTypeDefinition('elevation_change', DataTypes.Double, True))
        if get_obstruction:
            self.attribute_definitions.append(DataTypeDefinition('obstruction', DataTypes.Double, True))

        self.vector_layer_builder = GpxFeatureBuilder(self.name, self.attribute_definitions, self.crs)

        for track in root.findall('gpx:trk', self.namespace):
            for track_segment in track.findall('gpx:trkseg', self.namespace):
                feature = PointFeature()
                halfway_point = None
                in_scope = True
                self.segment_number += 1

                trackname = self.name + '_' + str(self.segment_number / 1000)[2:]
                self.reset(trackname)

                for track_point in track_segment.findall('gpx:trkpt', self.namespace):

                    try:
                        time = DataTypes.create_date(track_point.find('gpx:time', self.namespace).text)
                    except:
                        break

                    if feature.start_point is None:
                        if time is None:
                            continue
                        else:
                            feature.start_time = time

                        feature.start_point = QgsPoint(
                            float(track_point.get('lon')),
                            float(track_point.get('lat')),
                            None
                        )
                        halfway_point = feature.start_point

                        projected_point = self.tf.transform(QgsPointXY(feature.start_point))
                        height, slope, obstruction, terrain = self.terrain_classifier.get_terrain_data(projected_point,
                                                                                                       get_elevation_slope,
                                                                                                       get_obstruction,
                                                                                                       get_terrain_type,
                                                                                                       road_radius)
                        if slope is None and get_elevation_slope:
                            in_scope = False
                            break
                        feature.height = height
                        feature.slope = slope
                        feature.obstruction = obstruction
                        feature.terrain = terrain

                    else:

                        if time is not None:
                            feature.end_time = time
                            feature.duration = GeomTools.calculate_duration(feature.start_time, feature.end_time)

                        feature.end_point = QgsPoint(
                            float(track_point.get('lon')),
                            float(track_point.get('lat')),
                            None
                        )

                        if feature.duration >= min_duration:

                            feature.distance = feature.distance + GeomTools.distance(halfway_point, feature.end_point,
                                                                                     self.crs)
                            halfway_point = feature.end_point

                            if feature.distance >= min_distance:

                                feature = self.save_attributes(feature,
                                                               get_elevation_slope,
                                                               get_obstruction,
                                                               get_terrain_type,
                                                               road_radius,
                                                               calculate_motion_attributes)
                                if not feature:
                                    in_scope = False
                                    break

                # Don't skip segment/track numbers if some are not valid
                if not in_scope:
                    self.segment_number -= 1
                    continue

                if feature.duration > 0:

                    feature.distance = feature.distance + GeomTools.distance(halfway_point, feature.end_point,
                                                                             self.crs)
                    feature = self.save_attributes(feature,
                                                   get_elevation_slope,
                                                   get_obstruction,
                                                   get_terrain_type,
                                                   road_radius,
                                                   calculate_motion_attributes)
                    if not feature:
                        self.segment_number -= 1
                        continue

                savedSegment = self.save_layer(write_file)
                if savedSegment is False:
                    self.segment_number -= 1
                else:
                    self.segmentList.append(savedSegment)

        if self.segment_number == 0:
            self.error_message = 'No valid segments'
            return False

        return self.segmentList

    def save_attributes(self,
                        feature,
                        get_elevation_slope,
                        get_obstruction,
                        get_terrain_type,
                        road_radius,
                        calculate_motion):
        """
        Calculate feature attributes and add to vector layer
        :return:
        """

        attributes = {}

        projected_point = self.tf.transform(QgsPointXY(feature.end_point))

        height, slope, obstruction, terrain = self.terrain_classifier.get_terrain_data(projected_point,
                                                                                       get_elevation_slope,
                                                                                       get_obstruction,
                                                                                       get_terrain_type,
                                                                                       road_radius)
        if slope is None and get_elevation_slope:
            self.error_message = 'out of scope'
            return False

        if get_elevation_slope:
            attributes['elevation'] = feature.height
            attributes['elevation_change'] = height - feature.height
            attributes['hill_slope'] = feature.slope

        if get_obstruction:
            attributes['obstruction'] = feature.obstruction

        if get_terrain_type:
            feature.terrain.update(terrain)
            new_terrains = []
            for terrain_type in feature.terrain.keys():
                definition = DataTypeDefinition(terrain_type, DataTypes.Integer, True)
                if definition not in self.attribute_definitions:
                    self.attribute_definitions.append(definition)
                    new_terrains.append(definition)
                attributes[terrain_type] = True
            if len(new_terrains) > 0:
                self.vector_layer_builder.add_new_attributes(new_terrains)

        attributes['time'] = str(feature.start_time)

        if calculate_motion:
            attributes['distance'] = feature.distance
            attributes['duration'] = feature.duration
            attributes['speed'] = float((feature.distance / 1000) / (feature.duration / 3600))

        attributes['track_name'] = self.name
        attributes['segment_no'] = self.segment_number / 1000

        self.vector_layer_builder.add_feature([feature.start_point, feature.end_point], attributes)

        feature = PointFeature(feature.end_point, feature.end_time, height, slope, obstruction, terrain)
        return feature

    def save_layer(self, write_file):
        """
        Commit & save layer. If write_file = False, layer is committed but not saved
        :param write_file: Boolean to check file write
        :return: vector_layer: QgsVectorLayer
        """

        vector_layer = self.vector_layer_builder.save_layer(self.output_directory, self.overwrite, write_file)
        if vector_layer is False:
            self.error_message = "No features"
            return False
        if self.vector_layer_builder.error_message != '':
            self.error_message = self.vector_layer_builder.error_message
            print(self.error_message)
        return vector_layer


class PointFeature:

    def __init__(self, start_point=None, start_time=None, height=None, slope=None, obstruction=None, terrain=None):
        self.start_point = start_point
        self.start_time = start_time
        self.height = height
        self.slope = slope
        self.obstruction = obstruction
        self.terrain = terrain
        self.end_point = None
        self.end_time = None
        self.duration = 0
        self.distance = 0
