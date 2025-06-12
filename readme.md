# CALPREP        

> Minimalistic preprocesor for CALMET/CALPUFF.

## Dependencies

- Fortran GNU compiler (gfortran)
- ZLIB.
- PROJ.


## How to build it:

Enter the source code directory, edit the `Makefile` and run `make`:
```shell
cd src/
make
```

And executable named `CALPREP_vx.x.EXE` should be created.


## Get Input Data:

First, get surface, upperair, topography and land-use data from:


|  Data type   | Description          | Format   | Download URL                            |
|--------------|----------------------|----------|-----------------------------------------|
|  Geophysical | Elevation (Terrain)  | GeoTIFF  | [https://calpuff.org/data/Terrain   ]() |
|              | Land Use (LULC)      | GeoTIFF  | [https://calpuff.org/data/LULC      ]() |
|  Meteorology | Surface data         | ISH      | [https://ncei.noaa.gov/pub/data/noaa]() |
|              | Upper-Air data       | IGRA     | [https://ncei.noaa.gov/pub/data/igra]() |


## How to run it:

Edit the control file `namelist.calprep`:

```fortran
&control
  start_date = '2024-03-21 00:00:00',  !"YYYY-MM-DD HH:MM:SS"
    end_date = '2024-03-23 00:00:00',  !"YYYY-MM-DD HH:MM:SS"
  !time_zone = -3,                     !Time Zone
  
  proj       = 'EPSG:32616',           !A valid PROJ CRS definition.
  xc         = 596740,                 !domain center point X-coordinate 
  yc         = 4194370,                !domain center point Y-coordinate 
  dx         = 4000,                   !cell dimension-X [m]
  dy         = 4000,                   !cell dimension-Y [m]
  nx         = 70,                     !domain X-size    [n-cells]
  ny         = 70,                     !domain Y-size    [n-cells]
  
  prep_surf=.true.,                    !prepare surf.dat?
  prep_up  =.true.,                    !prepare upnn.dat?
  prep_geo =.true.,                    !prepare geo.dat?
/

&surface
  surface_nsta  = 5,
  surface_files ='./Met/Surface/720379-63882-2024', './Met/Surface/720445-99999-2024',
               './Met/Surface/720446-99999-2024', './Met/Surface/720448-00144-2024',
               './Met/Surface/720451-99999-2024', './Met/Surface/720903-00441-2024',
/

&upperair
  upperair_ptop = 500.,
  upperair_files='./Met/UpperAir/USM00072327-data.txt','./Met/UpperAir/USM00072426-data.txt',
/

&geo
  terrain_file  = './Geo/Terrain/DEM.tif',
  lulc_file     = './Geo/LULC/nlcd_2021_LULC_cropped.tif', 
  
  !NLCD (>2021) to CALMET (Level II) mapping:
  lulc_lookup = '11:52', '12:91', '21:31', '22:11', '23:11', '24:13', '31:74', '32:72', '41:41', '42:42', '43:43', '51:32', '52:32', '71:31', '72:82', '73:82', '74:82', '81:21', '82:21', '90:61','91:61', '95:62'
/
```

Notice that the `lulc_lookup` variable defines the mappings between your land use data categories to CALMET land use categories:



| id | CALMET (level I)         | id  | CALMET (level II)                             |
|----|--------------------------|-----|-----------------------------------------------|
| 10 |  Urban or Built-up Land  |  11 | Residential                                   | 
|    |                          |  12 | Comercial and services                        | 
|    |                          |  13 | Industrial                                    | 
|    |                          |  14 | Transportation, communications and utilities  | 
|    |                          |  15 | Industrial and compercial complexes           | 
|    |                          |  16 | Mixed Urban or Built-up Land                  | 
|    |                          |  17 | Other Urban or Built-up Land                  | 
| 20 |  Agricultural Land       |  21 | Cropland and pasture                          |                          
|    |                          |  22 | Orchards, groves, vineyards, nurseries and ornamental horticultural|
|    |                          |  23 | Confined Feeding operation                    | 
|    |                          |  24 | Other agricultural land                       | 
| 30 |  Rangeland               |  31 | Herbaceous rangeland                          | 
|    |                          |  32 | Shrub and brush rangeland                     | 
|    |                          |  33 | Mixed rangeland                               | 
| 40 |  Forest Land             |  41 | Deciduous forest                              | 
|    |                          |  42 | Evergreen forest                              | 
|    |                          |  43 | Mixed forest                                  | 
| 50 |  Water                   |  51 | Streams and canals                            | 
|    |                          |  52 | Lakes                                         | 
|    |                          |  53 | Reservoirs                                    | 
|    |                          |  54 | Bays and estuaries                            | 
|    |                          |  55 | Oceand and seas                               | 
| 60 |  Wetland                 |  61 | Forested wetland                              | 
|    |                          |  62 | Non-forested wetland                          | 
|    |                          |  71 | Dry salt flats                                | 
| 70 |  Barren Land             |  72 | Beaches                                       | 
|    |                          |  73 | Sandy areas                                   | 
|    |                          |  74 | Bare exposed rock                             | 
|    |                          |  75 | Strip mines                                   | 
|    |                          |  76 | Transitional areas                            | 
|    |                          |  77 | Mixed barren                                  | 
| 80 |  Tundra                  |  81 | Shrub and brush tundra                        | 
|    |                          |  82 | Herbaceous tundra                             | 
|    |                          |  83 | Bare ground                                   | 
|    |                          |  84 | Wet tundra                                    | 
|    |                          |  85 | Mixed tundra                                  | 
| 90 |  Perennial Snow or Ice   |  91 | Perenial snowfields                           | 
|    |                          |  92 | Glaciers                                      | 


Finally, place the executable file in your work directory and run it:

```shell
./CALPREP.EXE

```
