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
from datetime import *
import re

class DataTypeDefinition:
    """ Datatype definition class """

    def __init__(self, attribute_key, datatype, selected, prefix=''):
        self.attribute_key = prefix+attribute_key
        self.datatype = datatype
        self.selected = selected

# https://stackoverflow.com/questions/702834/whats-the-common-practice-for-enums-in-python
class DataTypes:
    # _unused, Integer, Double, Boolean, String, Date = range(6)
    Undefined = None
    Integer = 'Integer'
    Double = 'Double'
    Boolean = 'Boolean'
    String = 'String'
    Date = 'Date'

    @classmethod
    def parse(cls, value):
        if value == 'Integer':
            return DataTypes.Integer
        elif value == 'Double':
            return DataTypes.Double
        elif value == 'Boolean':
            return DataTypes.Boolean
        elif value == 'String':
            return DataTypes.String
        elif value == 'Date':
            return DataTypes.Date
        return DataTypes.Undefined

    @staticmethod
    def detect_data_type(text):
        if DataTypes.value_is_int(text):
            return DataTypes.Integer
        elif DataTypes.value_is_double(text):
            return DataTypes.Double
        elif DataTypes.value_is_boolean(text):
            return DataTypes.Boolean
        # elif self.str_is_date(extension.text):
        #     return DataTypes.Date
        else:
            return DataTypes.String

    @staticmethod
    def value_is_int(value):
        if type(value) is str:
            if value is None:
                return False
            try:
                int(value)
                return True
            except ValueError:
                return False
            # except TypeError:
            #     print "TypeError int " + str(string)
            #     return False
        elif type(value) is int:
            return True
        else:
            return False

    @staticmethod
    def value_is_boolean(value):
        if type(value) is str:
            if value is None:
                return False
            if value in ['true', 'false', 'TRUE', 'FALSE', 1, 0, 't', 'f']:
                return True
            return False
        elif type(value) is bool:
            return True
        else:
            return False

    @staticmethod
    def value_is_double(value):
        if type(value) is str:
            if value is None:
                return False
            try:
                float(value)
                return True
            except ValueError:
                return False
            except TypeError:
                print("TypeError double " + str(value))
                return False
        elif type(value) is float:
            return True
        else:
            return False

    @staticmethod
    def value_is_date(value):
        if type(value) is str:
            if value is None:
                return None
            elif DataTypes.create_date(value) is not None:
                return True
            else:
                return False
        elif type(value) is datetime:
            return True
        else:
            return False

    @staticmethod
    def string_to_boolean(string):
        if string is True or string in ['true', 'TRUE', '1', 't']:
            return True
        return False

    @staticmethod
    def create_date(s, custom_format=None):
        if s is None:
            return None
        # TODO explain following line
        s = re.sub(r'([-+]\d{2}):(\d{2})(?:(\d{2}))?$', r'\1\2\3', s)
        # https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior
        try:
            if custom_format is not None:
                return datetime.strptime(s, custom_format)
            else:
                raise ValueError('')
        except ValueError:
            try:
                return datetime.strptime(s, '%Y-%m-%dT%H:%M:%SZ')
            except ValueError:
                try:
                    return datetime.strptime(s, '%Y-%m-%dT%H:%M:%S%z')
                except ValueError:
                    try:
                        return datetime.strptime(s, '%Y-%m-%dT%H:%M:%S.%fZ')
                    except ValueError:
                        try:
                            return datetime.strptime(s, '%Y-%m-%dT%H:%M:%S')
                        except ValueError:
                            try:
                                return datetime.strptime(s, '%Y-%m-%dT%H:%M:%S.%f')
                            except ValueError:
                                pass
        return None
