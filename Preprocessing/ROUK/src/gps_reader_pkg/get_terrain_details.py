from qgis.core import (QgsCoordinateReferenceSystem, QgsCoordinateTransform, QgsPointXY)
from gps_reader_pkg.terrain_classifier import TerrainClassifier
import pandas as pd
import math


def get_terrain_details(file, terrain_folder, road_radius=50):

    terrain_classifier = TerrainClassifier(terrain_folder)

    tf = QgsCoordinateTransform()
    ref = QgsCoordinateReferenceSystem()
    tf.setSourceCrs(ref.fromEpsgId(4326))
    tf.setDestinationCrs(ref.fromEpsgId(27700))

    with open(file) as csv_file:
        csv_input = pd.read_csv(csv_file)
        if 'obstruction' not in csv_input.columns:
            csv_input['obstruction'] = ""
        point_b = 0

        for index, WKT in enumerate(csv_input['WKT']):

            coords = WKT.split('(')[1][:-1].split(',')
            point_a = coords[0].split(' ')

            if point_a == point_b:
                start_obstruction = end_obstruction
                terrain_a = terrain_b
            else:
                projected_point = tf.transform(QgsPointXY(float(point_a[0]), float(point_a[1])))
                elevation, slope, start_obstruction, terrain_a = terrain_classifier.get_terrain_data(projected_point,
                                                                                                     road_radius=road_radius,
                                                                                                     get_elevation_slope=False,
                                                                                                     get_obstruction=True,
                                                                                                     get_terrain_type=True,
                                                                                                     )

            point_b = coords[1].split(' ')
            projected_point = tf.transform(QgsPointXY(float(point_b[0]), float(point_b[1])))
            elevation, slope, end_obstruction, terrain_b = terrain_classifier.get_terrain_data(projected_point,
                                                                                               road_radius=road_radius,
                                                                                               get_elevation_slope=False,
                                                                                               get_obstruction=True,
                                                                                               get_terrain_type=True,
                                                                                               )

            if not math.isnan(start_obstruction):
                csv_input.loc[index, 'obstruction'] = start_obstruction
            terrain_a.update(terrain_b)
            for terrain_type in terrain_a.keys():
                csv_input.loc[index, terrain_type] = 1

        csv_input.to_csv(file, index=False)
    return True
