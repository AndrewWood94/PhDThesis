"""
Read a gpx file and save it as a gpkg file, with point attributes calculated
"""

from xml.etree import ElementTree
from qgis.core import (QgsCoordinateTransform, QgsPoint, QgsCoordinateReferenceSystem, QgsVectorLayer, QgsPointXY)
from gps_reader_pkg.datatype_definition import (DataTypeDefinition, DataTypes)
from gps_reader_pkg.gpx_feature_builder import GpxFeatureBuilder
from gps_reader_pkg.geom_tools import GeomTools
from gps_reader_pkg.terrain_classifier import TerrainClassifier


class GpxFileReader:
    """
    Class to read gpx files and assemble vector layers
    """

    def __init__(self, output_directory, name=None, overwrite=False, terrain_path=None, scope_path=None, track_crs=4326,
                 terrain_crs=27700):
        """
        Setup file reader with required file locations

        :param output_directory: location to save vector layer gpkg files
        :param name: filename format for vector layer gpkg files
        :param overwrite: overwrite existing filenames
        :param terrain_path: folder containing terrain DTM files
        :param scope_path: folder containing OS national grid squares to check scope of track
        :param track_crs: coordinate system of gps file
        :param terrain_crs: coordinate system of terrain files
        """

        self.namespace = None
        self.name = name
        self.error_message = ''
        self.equal_coordinates = 0
        self.crs = QgsCoordinateReferenceSystem('EPSG:4326')
        self.output_directory = output_directory
        self.overwrite = overwrite
        self.call_count = -1
        if scope_path is not None:
            scope_file_zoom = scope_path + '/10km_grid_region.shp'
            scope_file = scope_path + '/100km_grid_region.shp'
            self.scope_layer_zoom = QgsVectorLayer(scope_file_zoom, 'scope', 'ogr')
            self.scope_layer = QgsVectorLayer(scope_file, 'scope', 'ogr')
        if terrain_path is not None:
            self.terrain_classifier = TerrainClassifier(terrain_path)

        self.tf = QgsCoordinateTransform()
        self.tf.setSourceCrs(QgsCoordinateReferenceSystem().fromEpsgId(track_crs))
        self.tf.setDestinationCrs(QgsCoordinateReferenceSystem().fromEpsgId(terrain_crs))

    def reset(self, trackname):
        """
        Setup empty vector layer, with attributes included
        :param trackname: Full nanme of vecot layer (including Track/Segment number)
        :return:
        """
        self.vector_layer_builder = GpxFeatureBuilder(trackname, self.attribute_definitions, self.crs)

    def import_gpx_file(self, file_path, attribute_select="Last",
                        calculate_motion_attributes=False, terrain_attributes=False, check_scope=False,
                        min_duration=1, min_distance=0, file_as_string=False, write_file = True):
        """

        :param file_path: gpx file to import
        :param attribute_select: save the start, end or both point attributes to line segment
        :param calculate_motion_attributes: Calculate speed, distance, elevation change
        :param terrain_attributes: Calculate terrain elevation from OS data
        :param check_scope: Check whether all points lie in given region
        :param min_duration: Minimum time between saved point
        :param min_distance: Minimum distance between saved points
        :param file_as_string: Input file type; xml string or .gpx file
        :param write_file: Save output as gpkg?
        :return:
        """

        def save_attributes():
            """
            Calculate feature attributes and add to vector layer
            :return:
            """

            nonlocal elevation_b, height, slope

            elevation_a = elevation_b
            elevation_b_element = track_point.find('gpx:ele', self.namespace)
            elevation_b = float(elevation_b_element.text) if (elevation_b_element is not None) else None

            projected_point = self.tf.transform(QgsPointXY(new_point))

            #Check feature is within scope
            if check_scope:
                in_scope = False
                # Loop over 100km boxes to find feature
                for feat in self.scope_layer.getFeatures():
                    box = feat.geometry()
                    if box.contains(projected_point):
                        # Loop over 10km boxes to find feature
                        for zoom_feat in self.scope_layer_zoom.getFeatures(box.boundingBox()):
                            zoom_box = zoom_feat.geometry()
                            if zoom_box.contains(projected_point):
                                in_scope = True
                                break
                        break

                if not in_scope:
                    return False

            attributes = {}
            if attribute_select == 'First':
                attributes['time'] = str(time_a)
                attributes['ele'] = elevation_a
            elif attribute_select == 'Last':
                attributes['time'] = str(time_b)
                attributes['ele'] = elevation_b
            elif attribute_select == 'Both':
                attributes['a_time'] = str(time_a)
                attributes['b_time'] = str(time_b)
                attributes['a_ele'] = elevation_a
                attributes['b_ele'] = elevation_b

            if calculate_motion_attributes:
                attributes['distance'] = distance

                if time_a is not None and time_b is not None:
                    attributes['duration'] = duration
                    attributes['speed'] = float((distance / 1000) / (duration / 3600))

                if elevation_a is not None and elevation_b is not None:
                    attributes['elevation_diff'] = elevation_b - elevation_a

            if terrain_attributes:
                prev_height = height
                prev_slope = slope

                height, slope = self.terrain_classifier.get_data(projected_point)

                if attribute_select == 'First':
                    attributes['OS height'] = prev_height
                    attributes['OS slope'] = prev_slope
                elif attribute_select == 'Last':
                    attributes['OS height'] = height
                    attributes['OS slope'] = slope
                elif attribute_select == 'Both':
                    attributes['a_OS height'] = prev_height
                    attributes['b_OS height'] = height
                    attributes['a_OS slope'] = prev_slope
                    attributes['b_OS slope'] = slope

                attributes['OS height_diff'] = height - prev_height

            attributes['Track No'] = self.call_count
            attributes['Segment No'] = self.call_count + (self.segment_number / 1000)
            self.vector_layer_builder.add_feature([previous_point, new_point], attributes)
            return True

        self.call_count = self.call_count + 1
        self.segment_number = 0

        if file_as_string:
            root = ElementTree.fromstring(file_path)
        else:
            tree = ElementTree.parse(file_path)
            root = tree.getroot()

        # https://stackoverflow.com/questions/1953761/accessing-xmlns-attribute-with-python-elementree
        if root.tag[0] == "{":
            uri, ignore, tag = root.tag[1:].partition("}")
            self.namespace = {'gpx': uri}

        self.error_message = ''

        #Setup attributes for first segment
        if self.call_count == 0:

            self.attribute_definitions = list()
            self.attribute_definitions.append(DataTypeDefinition('Track No', DataTypes.Integer, True))
            self.attribute_definitions.append(DataTypeDefinition('Segment No', DataTypes.Double, True))
            if attribute_select == 'Both':
                self.attribute_definitions.append(DataTypeDefinition('time', DataTypes.String, True, 'a_'))
                self.attribute_definitions.append(DataTypeDefinition('time', DataTypes.String, True, 'b_'))
                self.attribute_definitions.append(DataTypeDefinition('ele', DataTypes.Double, True, 'a_'))
                self.attribute_definitions.append(DataTypeDefinition('ele', DataTypes.Double, True, 'b_'))
            else:
                self.attribute_definitions.append(DataTypeDefinition('time', DataTypes.String, True))
                self.attribute_definitions.append(DataTypeDefinition('ele', DataTypes.Double, True))
            if calculate_motion_attributes:
                self.attribute_definitions.append(DataTypeDefinition('distance', DataTypes.Double, True))
                self.attribute_definitions.append(DataTypeDefinition('duration', DataTypes.Double, True))
                self.attribute_definitions.append(DataTypeDefinition('speed', DataTypes.Double, True))
                self.attribute_definitions.append(DataTypeDefinition('elevation_diff', DataTypes.Double, True))

            if terrain_attributes:
                if attribute_select == 'Both':
                    self.attribute_definitions.append(DataTypeDefinition('OS height', DataTypes.Double, True, 'a_'))
                    self.attribute_definitions.append(DataTypeDefinition('OS height', DataTypes.Double, True, 'b_'))
                    self.attribute_definitions.append(DataTypeDefinition('OS height_diff', DataTypes.Double, True))
                    self.attribute_definitions.append(DataTypeDefinition('OS slope', DataTypes.Double, True, 'a_'))
                    self.attribute_definitions.append(DataTypeDefinition('OS slope', DataTypes.Double, True, 'b_'))
                else:
                    self.attribute_definitions.append(DataTypeDefinition('OS height', DataTypes.Double, True))
                    self.attribute_definitions.append(DataTypeDefinition('OS slope', DataTypes.Double, True))
                    self.attribute_definitions.append(DataTypeDefinition('OS height_diff', DataTypes.Double, True))

            self.vector_layer_builder = GpxFeatureBuilder(self.name, self.attribute_definitions, self.crs)

        for track in root.findall('gpx:trk', self.namespace):
            for track_segment in track.findall('gpx:trkseg', self.namespace):
                in_scope = True
                prev_track_point = None
                distance = 0
                duration = 0
                self.segment_number += 1

                trackname = self.name + str(self.call_count) + '_' + str(self.segment_number / 1000)[2:]
                self.reset(trackname)

                for track_point in track_segment.findall('gpx:trkpt', self.namespace):
                    if prev_track_point is None:
                        try:
                            time_a = DataTypes.create_date(track_point.find('gpx:time', self.namespace).text)
                        except:
                            break

                        elevation_b_element = track_point.find('gpx:ele', self.namespace)
                        elevation_b = float(elevation_b_element.text) if (elevation_b_element is not None) else None

                        new_point = QgsPoint(
                            float(track_point.get('lon')),
                            float(track_point.get('lat')),
                            None
                        )
                        halfway_point = new_point
                        projected_point = self.tf.transform(QgsPointXY(new_point))

                        if check_scope:
                            in_scope = False
                            for feat in self.scope_layer.getFeatures():
                                box = feat.geometry()
                                if box.contains(projected_point):
                                    for zoom_feat in self.scope_layer_zoom.getFeatures(box.boundingBox()):
                                        zoom_box = zoom_feat.geometry()
                                        if zoom_box.contains(projected_point):
                                            in_scope = True
                                            break
                                    break

                            if not in_scope:
                                break

                        if terrain_attributes:
                            height, slope = self.terrain_classifier.get_data(projected_point)

                        prev_track_point = track_point
                        previous_point = new_point

                    else:
                        try:
                            time_b = DataTypes.create_date(track_point.find('gpx:time', self.namespace).text)
                        except:
                            break

                        if time_a is None:
                            time_a = time_b
                        if time_b is not None:
                            duration = GeomTools.calculate_duration(time_a, time_b)

                        new_point = QgsPoint(
                            float(track_point.get('lon')),
                            float(track_point.get('lat')),
                            None
                        )

                        distance = distance + GeomTools.distance(halfway_point, new_point, self.crs)
                        halfway_point = new_point

                        if duration >= min_duration and distance >= min_distance:

                            save = save_attributes()
                            if not save:
                                in_scope = False
                                break
                            prev_track_point = track_point
                            previous_point = new_point
                            time_a = time_b
                            distance = 0
                            duration = 0

                #Don't skip segment/track numbers if some are not valid
                if not in_scope:
                    self.segment_number -= 1
                    continue

                if duration > 0:
                    save = save_attributes()
                    if not save:
                        self.segment_number -= 1
                        continue

                features = self.save_layer(write_file)
                if features == "None":
                    self.segment_number -= 1

        if self.segment_number == 0:
            self.call_count -= 1
            return ('out of scope')

    def save_layer(self, write_file):
        """
        Commit & save layer. If write_file = False, layer is committed but not saved as gpkg
        :param write_file: Boolean to confirm file save
        :return: vector_layer: QgsVectorLayer
        """

        vector_layer = self.vector_layer_builder.save_layer(self.output_directory, self.overwrite, write_file)
        if vector_layer.featureCount() == 0:
            return "None"
        if self.vector_layer_builder.error_message != '':
            self.error_message = self.vector_layer_builder.error_message
            print(self.error_message)
        return vector_layer
