"""
Finds breaks in gps tracks
"""
from PyQt5.QtCore import QVariant
import math
from qgis.core import QgsVectorLayer, QgsExpression, QgsExpressionContext, QgsExpressionContextUtils, QgsField

def get_segment_info(datalayer):
    """
    Read attributes from gps segment
    :param datalayer: gps segment to be read
    :return: start feature id,
            end feature id,
            median speed,
            median distance,
            total distance,
            total duration
    """
    exp = QgsExpression('array('
                        'minimum($id),'
                        'maximum($id),'
                        'median(to_real("speed"), filter:=to_real("speed")>0),'
                        'median(to_real("distance"), filter:=to_real("distance")>0),'
                        'sum(to_real("distance")),'
                        'sum(to_real("duration")))')
    context = QgsExpressionContext()
    context.appendScopes(QgsExpressionContextUtils.globalProjectLayerScopes(datalayer))
    first_feature, last_feature, median_speed, median_distance, total_dist, total_time = exp.evaluate(context)

    return int(first_feature), int(last_feature), median_speed, median_distance, total_dist, total_time

def break_likelihood(datalayer, feature, median_speed):
    """
    Calculate break_likelihood for a point based on point speed & angle between previous & next points
    :param datalayer: gps segment
    :param feature: gps point id to check
    :param median_speed: median speed for gps segment
    :return: category_break: High/Medium/Low break likelihood for point
            category_speed: High/Medium/Low point speed
            category_angle: Wide/Narrow point angle
            line_direction: Quadrant the direction of travel is heading
    """
    prevfeature = datalayer.getFeature(feature - 1)
    feature = datalayer.getFeature(feature)

    a1 = prevfeature.geometry().angleAtVertex(0) * 180 / math.pi
    a2 = feature.geometry().angleAtVertex(0) * 180 / math.pi
    speed = float(feature.attribute('speed'))

    #Set angle = 180 for first point in segment
    try:
        if feature["segment_no"] == prevfeature["segment_no"]:
            angle = abs(180 - abs(a1 - a2))
        else:
            angle = 180
    except:
        angle = 180

    if speed > 10:
        category_speed = 'High'
    elif speed <= median_speed / 2:
        category_speed = 'Zero'
    else:
        category_speed = 'Low'

    if angle > 90:
        category_angle = 'Wide'
        if category_speed == 'Zero' or category_speed == 'High':
            category_break = 'Medium'
        else:
            category_break = 'Low'
    else:
        category_angle = 'Narrow'
        if category_speed == 'Low' or category_speed == 'Zero':
            category_break = 'High'
        else:
            category_break = 'Medium'

    if 0 <= a2 < 90:
        line_direction = 1
    elif 90 <= a2 < 180:
        line_direction = 2
    elif 180 <= a2 < 270:
        line_direction = 3
    else:
        line_direction = 4

    return category_break, category_speed, category_angle, line_direction


def rangequery(feature, datalayer, median_speed, median_distance):
    """
    Finds all features within 10 minutes of given point which are under median distance away,
    plus the previous point if speed > 2 * median speed

    :param feature: point id for epicentre of search
    :param datalayer: gps segment to check
    :param median_speed: median segment point speed
    :param median_distance: median distance between points in segment
    :return: n.keys(): point ids of features in range
            extreme_speed: if point list contains exceptionally fast or slow points
            s_line: if no gaps exist in ids of points found
    """
    featureID = feature.id()
    extreme_speed = False
    sline = False
    a = feature["time"]
    #com = coordinates of centre of mass
    com = (feature.geometry().asPolyline()[0])
    n = dict()

    #QGIS expression to search layer for points
    expression = 'abs(second(to_datetime(\"time\") - to_datetime(\'' + a + '\'))) < 600 and ' \
                 '((distance(transform(start_point($geometry) , \'EPSG:4326\',\'EPSG:27700\'), ' + \
                 'transform(make_point(\'' + str(com.x()) + '\',\'' + str(com.y()) + '\'), ' \
                 '\'EPSG:4326\', \'EPSG:27700\'))<=' + str(median_distance) + ') or ' \
                 '($id = ' + str(featureID - 1) + 'and to_real(\"speed\") > ' + str(float(median_speed * 2)) + '))'

    datalayer.selectByExpression(expression)
    for feat in datalayer.selectedFeatures():
        p = feat.id()
        n[p] = True
        if float(feat["speed"]) > float(median_speed * 2) or float(feat["speed"]) < 0.01:
            n[p + 1] = True
            extreme_speed = True

    if len(n) == (max(n) - min(n)) + 1:
        sline = True

    return list(n.keys()), extreme_speed, sline


class BreakFinder:
    """
    Class to find valid breakpoints in gps track
    """

    def __init__(self):
        pass

    def find_breaks(self, datalayer):
        """
        Method to loop over points in gps track and check for valid breakpoints

        :param data_path: gpkg file containing gps track
        :return: updated gpkg file with OnBreak = 1 if point is breakpoint
        """

        selectedFeats = list()

        first, last, median_speed, median_distance, total_dist, total_time = get_segment_info(datalayer)

        #Ignore tracks with under 250m or 2.5 min of travel, or high median speed (non walking)
        if total_dist < 250 or total_time < 150 or median_speed > 10:
            datalayer.dataProvider().truncate()
            return False

        point_dict = dict.fromkeys(list(range(first, last + 1)), 'unknown')

        for point in point_dict:
            #Ignore points which have already been checked
            if point_dict[point] != 'unknown':
                continue

            feature = datalayer.getFeature(point)
            neighbourhood, extreme_speed, line = rangequery(feature, datalayer, median_speed, median_distance)
            #Ignore very small point clusters/ possible clustered straight lines containing no extreme point speeds
            if (len(neighbourhood) <= 4 or line) and not extreme_speed and len(neighbourhood) <= 10:
                point_dict[point] = 'walk'
                continue

            point_dict[point] = 'cluster'
            neighbour_dict = dict()
            neighbour_direction = dict.fromkeys(list(range(1, 5)), False)

            for neighbour in neighbourhood:
                if not (first <= neighbour <= last):
                    continue
                #Check break likelihood & walking direction of all points in cluster
                break_chance, speed, angle, line_direction = break_likelihood(datalayer, neighbour, median_speed)
                neighbour_dict[neighbour] = [break_chance, speed, angle, line_direction]

                if point_dict[neighbour] == 'walk':
                    point_dict[neighbour] = 'cluster'
                #Ignore points which have already been checked
                if point_dict[neighbour] != 'unknown':
                    continue

                point_dict[neighbour] = 'cluster'

                #Find extent of cluster by checking each point
                new_neighbours, extreme_speed, line = rangequery(datalayer.getFeature(neighbour), datalayer,
                                                                      median_speed, median_distance)
                if (len(new_neighbours) > 4 and not line) or extreme_speed or len(new_neighbours) > 10:
                    for new_neighbour in new_neighbours:
                        if new_neighbour not in neighbourhood:
                            neighbourhood.append(new_neighbour)

            min_breakpoint = math.inf
            max_breakpoint = 0
            breakcount = 0

            for k, v in sorted(neighbour_dict.items()):
                if v[0] != 'Low':
                    if k < min_breakpoint:
                        min_breakpoint = k
                    if k > max_breakpoint:
                        max_breakpoint = k
                    breakcount += 1

            #If no points have medium/high break likelihood, ignore cluster
            if breakcount == 0 or len(neighbourhood) <= 4:
                for neighbour in neighbour_dict:
                    point_dict[neighbour] = 'walk'
                continue

            breakpoints = list(range(min_breakpoint, max_breakpoint + 1))

            #Check break likelihood of 'gaps' in cluster id list (ie cluster = points [1,2,5,6], check ids 3 & 4)
            for item in breakpoints:
                if item not in neighbourhood:
                    break_chance, speed, angle, line_direction = break_likelihood(datalayer, item, median_speed)
                    neighbour_dict[item] = [break_chance, speed, angle, line_direction]
                    if break_chance != 'Low':
                        breakcount += 1
                neighbour_direction[neighbour_dict[item][3]] = True

            #Check to ensure track doubles back on itself
            if (neighbour_direction[1] & neighbour_direction[3]) or \
                    (neighbour_direction[2] & neighbour_direction[4]):

                #Check less than half the points have low break likelihood
                if breakcount / len(breakpoints) >= 0.5:

                    selectedFeats.extend(breakpoints)
                    point_dict.update(dict.fromkeys(breakpoints, 'cluster'))

        #Update/Add OnBreak field to
        field_no = datalayer.dataProvider().fieldNameIndex("OnBreak")
        if field_no != -1:
            datalayer.dataProvider().deleteAttributes([field_no])
            datalayer.updateFields()

        newAttribute = [QgsField('OnBreak', QVariant.Int, 'Integer')]
        datalayer.dataProvider().addAttributes(newAttribute)
        field_no = datalayer.dataProvider().fieldNameIndex("OnBreak")

        change_dict = {field_no: 1}
        add_breaks = dict.fromkeys(selectedFeats, change_dict)
        datalayer.startEditing()
        datalayer.dataProvider().changeAttributeValues(add_breaks)
        datalayer.commitChanges()

        return datalayer

