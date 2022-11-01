import scrapy
from xml.etree import ElementTree
import requests


def _includes_timestamps(response):
    """
    Check if file includes timestamps to calculate walking speeds
    :param response:
    :return:
    """

    file = requests.get(response).text
    root = ElementTree.fromstring(file)
    if root.tag[0] == "{":
        uri, ignore, tag = root.tag[1:].partition("}")
        namespace = {'gpx': uri}
    time = root.find('gpx:trk/gpx:trkseg/gpx:trkpt/gpx:time', namespace)
    if time is not None:
        return True
    else:
        return False


def _parse_detail(response):
    for href in response.css('#geo_table a::attr(href)').extract():
        if href.endswith(".gpx"):
            if _includes_timestamps(href):
                yield {'filepath': href}

    """
    If website is offline try this
    """

    """
    minimap = response.css('#minimap_total script::text').extract_first()
    result = re.search("gps_id:'(.*)'", minimap)
    url = 'https://www.hikr.org/files/gps'
    if result:
        yield {'filepath': url + result.group(1) + '.gpx'}
    else:
        pass
    """


class GPXSpider(scrapy.Spider):
    """
    Class to parse Hikr.org results and find gpx files containing timestamped tracks
    """

    name = 'gpx'

    def parse(self, response):
        for link in response.css('.content-list a::attr(href)'):
            yield response.follow(link, _parse_detail)

        for nextLink in response.css('#NextLink::attr(href)'):
            yield response.follow(nextLink, self.parse)

