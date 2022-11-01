from qgis.core import QgsPointXY, QgsRectangle, QgsRasterLayer
import math
import os


def get_tile_name(x, y):
    """
    Find which 5km National grid square contains the given point
    :param x: Easting
    :param y: Northing
    :return: tile_name: 5x5 km nation grid tile
    """

    # Ensure grid boundaries are correct
    if y % 5000 == 0:
        y = y - 1

    # Get first letter
    if (y > 1000000):
        first_char = "H"
        y -= (1000000)
    elif (y > 500000):
        first_char = "N"
        y -= 500000
    else:
        if (x >= 500000):
            first_char = "T"
            x -= 500000
        else:
            first_char = "S"

    # Get  second letter
    col_offset = math.floor(x / 100000)
    row_offset = math.floor(y / 100000)
    val = 86 + col_offset - (row_offset * 5)
    if (val < 74):
        val = val - 1  # correct for missing 'i' for values below 'j'
    second_char = chr(val)

    # Get 10k*10k square
    x = x % 100000
    y = y % 100000
    col_name = math.floor(x / 10000)
    row_name = math.floor(y / 10000)

    # Get 5k*5k quadrant
    x = x % 10000
    y = y % 10000
    vertical_quadrant = "N" if y > 5000 else "S"
    horizontal_quadrant = "E" if x >= 5000 else "W"

    tile_name = first_char + second_char + str(col_name) + str(row_name) + vertical_quadrant + horizontal_quadrant
    return tile_name


class TerrainClassifier:
    """
    Class to calculate OS terrain information
    """

    def __init__(self, file_path, resolution=5):
        self.data_file_path = file_path
        self.elev_layer = None
        self.elev_boundaries = QgsRectangle(0, 0, 0, 0)
        self.resolution = resolution

    def get_slope_coordinates(self, x, y):
        """
        Return coordinates required to calculate slope using quadratic surface method
        i.e. given point +-resolution distance in compass directions
        :param x: x coordinate of point to calculate slope at
        :param y: y coordinate of point to calculate slope at
        :return: coordinate_dict: dictionary of ['N','S','E','W'] & coordinates at each point
        """
        coordinate_dict = {}
        coordinate_dict['N'] = QgsPointXY(x, y + self.resolution)
        coordinate_dict['S'] = QgsPointXY(x, y - self.resolution)
        coordinate_dict['E'] = QgsPointXY(x + self.resolution, y)
        coordinate_dict['W'] = QgsPointXY(x - self.resolution, y)
        return coordinate_dict

    def calculate_slope(self, elevation_array, degrees=True):
        """
        Calculate slope using quadratic surface method
        :param elevation_array: dictionary containing elevations at 4 compass points about calculation point
        :param degrees: degrees or radians
        :return: slope angle
        """
        g = (elevation_array['E'] - elevation_array['W']) / (2 * self.resolution)
        h = (elevation_array['N'] - elevation_array['S']) / (2 * self.resolution)
        s = math.sqrt(g ** 2 + h ** 2) * 100
        if degrees:
            s = math.atan(s / 100) * 180 / math.pi
        return s

    def get_data(self, trackpoint):
        """
        Calculate elevation value and hill slope for a given point using OS data
        :param trackpoint:
        :return: elevation_value
        :return: slope_value
        """
        x = trackpoint[0]
        y = trackpoint[1]


        """Elevation/Slope Classification"""

        tile_name = get_tile_name(x, y)
        point = QgsPointXY(x, y)

        if not self.elev_boundaries.contains(point):
            elev_path = self.data_file_path + "/" + tile_name[0:2].lower() + "/" + tile_name + ".asc"
            if os.path.isfile(elev_path):
                self.elev_layer = QgsRasterLayer(elev_path)
                self.elev_boundaries = self.elev_layer.extent()
            else:
                self.elev_boundaries = QgsRectangle(0, 0, 0, 0)
                self.elev_layer = None

        if self.elev_layer is not None:
            elevation_value = self.elev_layer.dataProvider().sample(point, 1)[0]
        else:
            elevation_value = float('NaN')

        slope_coordinates = self.get_slope_coordinates(x, y)
       
        elevation_array = {}
        for key in slope_coordinates:
            if self.elev_boundaries.contains(slope_coordinates[key]):
                elevation_array[key] = self.elev_layer.dataProvider().sample(slope_coordinates[key], 1)[0]
            else:
                dummy_tile_name = get_tile_name(slope_coordinates[key][0], slope_coordinates[key][1])
                dummy_elev_path = self.data_file_path + "/" + dummy_tile_name[
                                                              0:2].lower() + "/" + dummy_tile_name + ".asc"
                if os.path.isfile(dummy_elev_path):
                    dummy_elev_layer = QgsRasterLayer(dummy_elev_path)
                    elevation_array[key] = dummy_elev_layer.dataProvider().sample(slope_coordinates[key], 1)[0]
                else:
                    elevation_array[key] = float('NaN')
        slope_value = self.calculate_slope(elevation_array)

        return elevation_value, slope_value
