from qgis.core import QgsPointXY, QgsRectangle, QgsRasterLayer
import math
import os
import geopandas as gpd


def get_tile_name(x, y):
    """
    Find which 5km National grid square contains the given point
    :param x: Easting
    :param y: Northing
    :return: tile_name: 5x5 km nation grid tile
    """
    # Check within national grid boundaries
    if y < 0 or y > 1500000 or x < 0 or x > 1000000:
        return "XXXXXX", "XXXXXX"

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
    col_name = str(math.floor(x / 10000))
    row_name = str(math.floor(y / 10000))

    # Get 5k*5k quadrant
    x = x % 10000
    y = y % 10000

    vertical_quadrant = "N" if y > 5000 else "S"
    horizontal_quadrant = "E" if x >= 5000 else "W"

    short_name = first_char + second_char + col_name + row_name + vertical_quadrant + horizontal_quadrant

    col_name = col_name + str(math.floor(x / 1000))
    row_name = row_name + str(math.floor(y / 1000))

    long_name = first_char + second_char + col_name + row_name

    return short_name, long_name


class TerrainClassifier:
    """
    Class to calculate OS terrain information
    """

    def __init__(self, file_path, resolution=5):
        self.OS_DTM_file_path = file_path + '/terrain-5-dtm/'
        self.resolution = resolution
        self.elev_layer = None
        self.elev_boundaries = QgsRectangle(0, 0, 0, 0)
        self.lidar_DTM_path = file_path + '/Lidar/dtm/'
        self.lidar_DSM_path = file_path + '/Lidar/dsm/'
        self.lidar_DTM_boundaries = QgsRectangle(0, 0, 0, 0)
        self.lidar_DSM_boundaries = QgsRectangle(0, 0, 0, 0)
        self.OS_terrain_path = file_path + '/OS_Terrain_Type/'
        self.OSM_road_path = file_path + '/Roads/'

    def get_elevation_slope(self, point, tile_name):
        """
        Calculate elevation value and hill slope for a given point using OS data
        :param point:
        :return: elevation_value
        :return: slope_value
        """
        elevation_value = None
        slope_value = None

        """Elevation/Slope Classification"""

        if not self.elev_boundaries.contains(point):
            elev_path = self.OS_DTM_file_path + "/" + tile_name[0:2].lower() + "/" + tile_name + ".asc"
            if os.path.isfile(elev_path):
                self.elev_layer = QgsRasterLayer(elev_path)
                self.elev_boundaries = self.elev_layer.extent()
            else:
                self.elev_boundaries = QgsRectangle(0, 0, 0, 0)
                self.elev_layer = None
                return elevation_value, slope_value

        elevation_value = self.elev_layer.dataProvider().sample(point, 1)[0]

        slope_coordinates = self.get_slope_coordinates(point)

        elevation_array = {}
        for key in slope_coordinates:
            if self.elev_boundaries.contains(slope_coordinates[key]):
                elevation_array[key] = self.elev_layer.dataProvider().sample(slope_coordinates[key], 1)[0]
            else:
                dummy_tile_name = get_tile_name(slope_coordinates[key][0], slope_coordinates[key][1])[0]
                dummy_elev_path = self.OS_DTM_file_path + "/" + dummy_tile_name[
                                                                0:2].lower() + "/" + dummy_tile_name + ".asc"
                if os.path.isfile(dummy_elev_path):
                    dummy_elev_layer = QgsRasterLayer(dummy_elev_path)
                    elevation_array[key] = dummy_elev_layer.dataProvider().sample(slope_coordinates[key], 1)[0]
                else:
                    elevation_array[key] = float('NaN')
        slope_value = self.calculate_slope(elevation_array)

        return elevation_value, slope_value

    def get_slope_coordinates(self, point):
        """
        Return coordinates required to calculate slope using quadratic surface method
        i.e. given point +-resolution distance in compass directions
        :param x: x coordinate of point to calculate slope at
        :param y: y coordinate of point to calculate slope at
        :return: coordinate_dict: dictionary of ['N','S','E','W'] & coordinates at each point
        """
        x = point.x()
        y = point.y()
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

    def get_obstruction_height(self, point, tile_name):

        obstruction = math.nan
        if not self.lidar_DTM_boundaries.contains(point):
            lidar_DTM_path = self.lidar_DTM_path + "/" + tile_name[0:2].lower() + "/" + tile_name.lower() + "_dtm_2m.asc"
            if os.path.isfile(lidar_DTM_path):
                self.DTM_layer = QgsRasterLayer(lidar_DTM_path)
                self.lidar_DTM_boundaries = self.DTM_layer.extent()
            else:
                self.lidar_DTM_boundaries = QgsRectangle(0, 0, 0, 0)
                self.DTM_layer = None
                return obstruction

        if not self.lidar_DSM_boundaries.contains(point):
            lidar_DSM_path = self.lidar_DSM_path + "/" + tile_name[
                                                         0:2].lower() + "/" + tile_name.lower() + "_dsm_2m.asc"
            if os.path.isfile(lidar_DSM_path):
                self.DSM_layer = QgsRasterLayer(lidar_DSM_path)
                self.lidar_DSM_boundaries = self.DTM_layer.extent()
            else:
                self.lidar_DSM_boundaries = QgsRectangle(0, 0, 0, 0)
                self.DSM_layer = None
                return obstruction

        DTM_value = self.DTM_layer.dataProvider().sample(point, 1)[0]
        DSM_value = self.DSM_layer.dataProvider().sample(point, 1)[0]
        obstruction = DSM_value - DTM_value
        return obstruction

    def get_terrain_type(self, point, road_radius=50):

        x = point.x()
        y = point.y()
        terrain = dict()

        # Find names of all tiles within radius of points
        tile_names = []
        for i in range(-1 * road_radius, road_radius + 1, road_radius):
            for j in range(-1 * road_radius, road_radius + 1, road_radius):
                tile = get_tile_name(x + i, y + j)[0][0:4]
                if tile not in tile_names:
                    tile_names.append(tile)

        xmin, ymin, xmax, ymax = x - road_radius, y - road_radius, x + road_radius, y + road_radius
        road_bbox = (xmin, ymin, xmax, ymax)

        for tile in tile_names:
            terrain_area_path = self.OS_terrain_path + '/' + tile[0:2] + "_edu_land_extract.gdb"
            osm_line_path = self.OSM_road_path + "/" + tile[0:2] + "/" + tile + ".gpkg"

            try:
                terrain_area = gpd.read_file(terrain_area_path, bbox=(x, y, x, y), layer="topographicarea")
                osm_line = gpd.read_file(osm_line_path, bbox=road_bbox)
            except:
                continue

            # Find terrain areas
            for feature in terrain_area.iterrows():
                feat = feature[1]
                # Land Type
                if (feat['descriptiveterm'] is not None) or ("General Surface" in feat['descriptivegroup']):
                    to_add = False
                    if feat['descriptiveterm'] is not None:
                        for landform in ["Agricultural Land", "Cliff", "Slope"]:
                            if landform in feat['descriptiveterm']:
                                to_add = True
                                break
                    if "Natural Environment" in feat['descriptivegroup'] or to_add:
                        landtypes = feat['descriptiveterm'].split(",")
                        for land in landtypes:
                            terrain[land] = 1
                    if ("General Surface" in feat['descriptivegroup']) and (not to_add):
                        terrain['General Surface'] = 1

            # Look for road/paths in OpenStreetMap data
            for feature in osm_line.iterrows():
                feat = feature[1]
                terrain['OSM_' + feat['fclass']] = 1

        return terrain

    def get_terrain_data(self, point, get_elevation_slope=True, get_obstruction=True,
                         get_terrain_type=True, road_radius=50):

        x = point.x()
        y = point.y()
        elevation = None
        slope = None
        obstruction = math.nan
        terrain_types = dict()

        short_tile, long_tile = get_tile_name(x, y)
        if short_tile == "XXXXXX":
            return elevation, slope, obstruction, terrain_types

        if get_elevation_slope:
            elevation, slope = self.get_elevation_slope(point, short_tile)
        if get_obstruction:
            obstruction = self.get_obstruction_height(point, long_tile)
        if get_terrain_type:
            terrain_types = self.get_terrain_type(point, road_radius)
        return elevation, slope, obstruction, terrain_types