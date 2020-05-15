from datetime import timedelta
import ee
import pandas as pd

ee.Initialize()

dat = pd.read_csv('~/mortalityblob/dhs/Mortality_geodata.csv')

temps = ee.ImageCollection("NCEP_RE/surface_temp")

lons = [x * 2.5 for x in range(-72, 73)]
lats = [x * 2.5 for x in range(-36, 37)]

dat['lat_bin'] = pd.cut(dat['latitude'], bins=lats, labels=[(x + 1.25) for x in lats[:-1]])
dat['lon_bin'] = pd.cut(dat['longitude'], bins=lons, labels=[(x + 1.25) for x in lons[:-1]])

sel = dat[['lat_bin', 'lon_bin']].drop_duplicates()

points = []
for row in sel.iterrows():
    geom = ee.Geometry.Point(row[1]['lon_bin'], row[1]['lat_bin'])
    feat = ee.Feature(geom)
    points.append(feat)

fc = ee.FeatureCollection(points)

def getCoordsAndTemp(result):
    x = [x['geometry']['coordinates'][0] for x in result]
    y = [x['geometry']['coordinates'][1] for x in result]
    t = [x['properties']['first'] for x in result]
    id = [x['id'] for x in result]   
    
    return(pd.DataFrame({'x': x, 'y': y, 'temp': t, 'id': id}))

alldat = pd.DataFrame({})
for d in pd.date_range('1973-04-01', '2019-12-31'):
    start = d.strftime('%Y-%m-%d')
    end = (d + timedelta(days=1)).strftime('%Y-%m-%d')
    print(start)
    ic = temps.filterDate(start, end)
    res = ic.map(lambda x: x.reduceRegions(collection=fc, reducer=ee.Reducer.first()))
    resp = res.flatten().getInfo()
    resdf = getCoordsAndTemp(resp['features'])
    resdf['date'] = start
    
    alldat = pd.concat([alldat, resdf], ignore_index=True)

alldat.to_csv('~/mortalityblob/dhs/Temps_res_GEE.csv', index=False)

import os
os.system('~/telegram.sh "Done with Temperature Extracts!"')


