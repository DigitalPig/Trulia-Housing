# -*- coding: utf-8 -*-
"""
Created on Fri Oct 30 09:08:40 2015
@author: Zhenqing Li
"""

import xml.etree.ElementTree as etree
import csv
import requests
import time

# Define some global variables

apikey = "CHANGE TO YOUR OWN KEY HERE" # Remember to remove it before upload to GitHub
# trulia_state = 'CA' 
startDate = "2007-01-01"
endDate = "2014-12-31"

# Define a function to scarp the data from Trulia

def get_trulia_states():
    '''
    This function get the state list from Trulia
    '''
    url = "http://api.trulia.com/webservices.php?library=LocationInfo&function=getStates"
    par = {"apikey": apikey}
    time.sleep(1)    
    r = requests.get(url, params = par)
    tree_states = etree.fromstring(r.content)
    states = tree_states.findall('.//stateCode')
    state_res = []    
    for state in states:
        if (state.text == "AK" or state.text == "HI"):
            continue
        else:
            state_res.append(state.text)
    return state_res
    
    


def get_trulia_cities(state):
    ''' 
    This function uses trulia API to get city list from a state
    '''
    url = "http://api.trulia.com/webservices.php?library=LocationInfo&function=getCitiesInState"
    par = dict(zip(["state", "apikey"], [state, apikey]))
    time.sleep(1)    
    r = requests.get(url, params = par)
    return r
    
def get_trulia_neighbor(city_name, state):
    '''
    This function uses trulia API to get Neighborhood list from a city\
    '''
    url = "http://api.trulia.com/webservices.php?library=LocationInfo&function=getNeighborhoodsInCity"
    par = dict(zip(["city","state","apikey"],[city_name,state,apikey]))
    time.sleep(1)    
    r = requests.get(url,params = par)
    return r

def get_trulia_city_stat(city, state, startDate, endDate):
    '''
    This function grab the city level average data
    '''
    url = "http://api.trulia.com/webservices.php?library=TruliaStats&function=getCityStats"
    par = dict(zip(["city","state","startDate","endDate","apikey"],
                   [city, state, startDate, endDate, apikey]))
    time.sleep(1)
    r = requests.get(url, params = par)
    return r
    
    
def parse_city(tree):
    cities_res = []
    
    cities = tree.findall(".//city")
    
    for city in cities:
        city_id = city.find('cityId').text
        city_name = city.find('name').text
        city_lat = city.find('latitude').text
        city_lon = city.find('longitude').text
        # Build dictionary
        dic_city = dict(zip(["city id", "city_name", "city_lat", "city_lon"],
                            [city_id, city_name, city_lat, city_lon]))
        cities_res.append(dic_city)
    return cities_res
    
def parse_neighborhood(tree):
    neighborhoods = tree.findall(".//neighborhood")
    city = tree.find(".//city").text
    state = tree.find(".//state").text
    neighbor_res = []    
    if not neighborhoods:
        dic_neighbor = dict(zip(["neighbor_id","neighbor_name", "city", "state"],
                                ["NA","NA", city, state]))
        neighbor_res.append(dic_neighbor)
        return neighbor_res
    
    for neighbor in neighborhoods:
        neighbor_id = neighbor.find('id').text
        neighbor_name = neighbor.find('name').text
        dic_neighbor = dict(zip(["neighbor_id","neighbor_name", "city", "state"],
                                [neighbor_id,neighbor_name, city, state]))
        neighbor_res.append(dic_neighbor)
    return neighbor_res

# This is the most difficult part of parsing

def parse_citystat(tree, state, city_name, city_lat, city_lon):
    trafficStats = tree.findall(".//trafficStat")
    #may need to become a global variable 
    traffic_res = []
    listing_res = []
#    state = tree.find(".//state").text # giving random ERROR@!
    # Need to put State somewhere
    
    for traffic in trafficStats:
        traffic_dict = {}
        for node in traffic.iter():
            if (node.tag == 'trafficStat'):
                continue
            else:
                traffic_dict[node.tag]=node.text
        traffic_dict["City"] = city_name
        traffic_dict["State"] = state
        traffic_dict["Latitude"] = city_lat
        traffic_dict["Longitude"] = city_lon
        traffic_res.append(traffic_dict)
    listingStats = tree.findall(".//listingStat")
    for listingstat in listingStats:
        date = listingstat.find(".//weekEndingDate").text
        subcategories = listingstat.findall(".//subcategory")
        for sub in subcategories:
            listing_dict = {}
            for node in sub.iter():
                if (node.tag == "subcategory"):
                    continue
                if (node.tag == "type"):
                    string = node.text.split(" ")
                    listing_dict["bedrooms"] = string[0]
                else:
                    listing_dict[node.tag] = node.text
            listing_dict["Week of Day"] = date
            listing_dict["City"] = city_name
            listing_dict["State"] = state
            listing_dict["Latitude"] = city_lat
            listing_dict["Longitude"] = city_lon
            listing_res.append(listing_dict)
    return traffic_res, listing_res
            
        
    

# Let's get the city list from a state from Trulia



# The next step to iterate the city list and get the list of neighborhoods
csv_traffic = open('traffic.csv', 'w')
csv_listing = open('listing.csv','w')

writer_traffic = csv.writer(csv_traffic)
writer_traffic_counter = 0
writer_listing = csv.writer(csv_listing)
writer_listing_counter = 0

res_traffic = []
res_listing = []

states = get_trulia_states()

for trulia_state in states:
    tree = etree.fromstring(get_trulia_cities(trulia_state).content)
    cities_res = parse_city(tree)   
    for city in cities_res:
        city_name = city["city_name"]
        city_lat = city["city_lat"]
        city_lon = city["city_lon"]
        print("city:{0}, {1}".format(city_name,trulia_state))
        tree_city = etree.fromstring(get_trulia_city_stat(city_name,trulia_state,
                                                          startDate, endDate).content)
        traffic_res, listing_res = parse_citystat(tree_city, trulia_state, city_name, 
                                                  city_lat, city_lon)
        if (writer_traffic_counter == 0):
            writer_traffic = csv.DictWriter(csv_traffic, list(traffic_res[0].keys()))
            writer_traffic.writeheader()
            writer_traffic_counter += 1
        else:
            for traffic in traffic_res:
                writer_traffic.writerow(traffic)
        if (writer_listing_counter == 0):
            writer_listing = csv.DictWriter(csv_listing, list(listing_res[0].keys()))
            writer_listing.writeheader()
            writer_listing_counter += 1
        else:
            for listing in listing_res:
                writer_listing.writerow(listing)
       
        
        
        

csv_traffic.close()
csv_listing.close()
            
