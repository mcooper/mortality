from datetime import timedelta
import ee
import pandas as pd
import numpy as np

ee.Initialize()

dat = pd.read_csv('~/mortalityblob/dhs/Mortality_geodata.csv')

temps = ee.ImageCollection("NCEP_RE/surface_temp")

dat['x_u'] = np.ceil((dat.longitude - 1.25)/2.5)*2.5 + 1.25
dat['y_u'] = np.ceil((dat.latitude - 1.25)/2.5)*2.5 + 1.25
dat['x_l'] = np.floor((dat.longitude - 1.25)/2.5)*2.5 + 1.25
dat['y_l'] = np.floor((dat.latitude - 1.25)/2.5)*2.5 + 1.25

sel = pd.concat([dat.rename(columns={"x_l": "x", "y_l": "y"})[['x', 'y']],
                 dat.rename(columns={"x_l": "x", "y_u": "y"})[['x', 'y']],
                 dat.rename(columns={"x_u": "x", "y_l": "y"})[['x', 'y']],
                 dat.rename(columns={"x_u": "x", "y_u": "y"})[['x', 'y']]]).drop_duplicates()

points = []
for row in sel.iterrows():
    geom = ee.Geometry.Point(row[1]['x'], row[1]['y'])
    feat = ee.Feature(geom)
    points.append(feat)

fc = ee.FeatureCollection(points)

def getCoordsAndTemp(result):
    x = [x['geometry']['coordinates'][0] for x in result]
    y = [x['geometry']['coordinates'][1] for x in result]
    t = [x['properties']['first'] for x in result]
    id = [x['id'] for x in result]   
    
    return(pd.DataFrame({'x': x, 'y': y, 'temp': t, 'id': id}))

#alldat = pd.DataFrame({})
#Iteration failed on Dec 27, 2016
#Will need to re-load Temps_res_GEE.csv and resume from there
#Seems like a problem on googles servers
#Issue: https://issuetracker.google.com/issues/156965161

for d in pd.date_range('2016-12-27', '2019-12-31'):
    start = d.strftime('%Y-%m-%d')
    end = (d + timedelta(days=1)).strftime('%Y-%m-%d')
    print(start)
    img = temps.filterDate(start, end).reduce(reducer=ee.Reducer.max())
    res = img.reduceRegions(collection=fc, reducer=ee.Reducer.first(), scale=100000).getInfo()
    resdf = getCoordsAndTemp(res['features'])
    resdf['date'] = start
    
    alldat = pd.concat([alldat, resdf], ignore_index=True)

alldat.to_csv('~/mortalityblob/dhs/Temps_res_GEE.csv', index=False)

