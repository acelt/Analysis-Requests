# Setup
import arcpy
from arcpy import env
import arcgis

env.workspace = r"C:\Users\alaurencetraynor\Documents\OR\ValeDO\Remote sensing benchmarks"

# Define Analysis Area
analysis_area = r'C:\Users\alaurencetraynor\Documents\OR\ValeDO\Remote sensing benchmarks\Remote sensing benchmarks.gdb\CowLakesAssesmentArea'

# creating bounding box for clip function below
desc = arcpy.Describe(analysis_area).extent
bb = str(desc.XMin) + " " + str(desc.YMin) + " " + str(desc.XMax) + " " + str(desc.YMax)

# Grab RCMAP service for each relevant indicator
# theres also a number fo other RCMAP WCS services available
# Parameterising this for now in case i want to make it into a tool at some point
indicator_list = 'annual_herbaceous_trend' #'annual herbaceous' + 'bare ground' + 'perennial herbaceous'
server = "https://www.mrlc.gov/geoserver/rcmap"
output_dir = r"C:\Users\alaurencetraynor\Documents\OR\ValeDO\Remote sensing benchmarks\Remote sensing benchmarks.gdb"
# minimum value is 1985, max value is 2021
year = ["2017","2018", "2019", "2020", "2021"] # if we want multiple years just need to put this in a loop in cell below
# should add some input checks here to make sure this is a year formatted in string

# clipping these to the extent of the analysis area
# need to also specify which years to grab
# there are other indicators but just using these 3 for now
if 'annual herbaceous' in indicator_list:
    for i in year:
        wcs_url = server + "_anhb/wcs?coverage=rcmap_annual_herbaceous_" + i
        ah = arcpy.management.MakeWCSLayer(wcs_url,("annual_herb"+ i), analysis_area)
        # clip to poly
        ah_output_dir = output_dir + ("/annual_herb_Clip"+ i)
        arcpy.management.Clip(ah, bb, ah_output_dir, analysis_area, "256", "ClippingGeometry", "NO_MAINTAIN_EXTENT")

if 'bare ground' in indicator_list:
    for i in year:
        wcs_url = server + "_bare/wcs?coverage=rcmap_bare_ground_"+ i
        bg = arcpy.management.MakeWCSLayer(wcs_url,("bare_soil"+ i), analysis_area)
        # clip to poly
        bg_output_dir = output_dir + ("/bare_ground_Clip")+ i
        arcpy.management.Clip(bg, bb, bg_output_dir, analysis_area, "256", "ClippingGeometry", "NO_MAINTAIN_EXTENT")

if 'perennial herbaceous' in indicator_list:
    for i in year :
        wcs_url = server + "_perennial_herbaceous/wcs?coverage=rcmap_perennial_herbaceous_" + i
        ph = arcpy.management.MakeWCSLayer(wcs_url,("perennial_herb"+ i), analysis_area)
        # clip to poly
        bg_output_dir = output_dir + ("/perennial_herb_Clip")+ i
        arcpy.management.Clip(ph, bb, bg_output_dir, analysis_area, "256", "ClippingGeometry", "NO_MAINTAIN_EXTENT")

if 'annual_herbaceous_trend' in indicator_list:
    server = "https://www.mrlc.gov/geoserver/mrlc_download/wcs?coverage=rcmap_annual_herbaceous_linear_model_slope"
    ah_trend = arcpy.management.MakeWCSLayer(server,"annual_herb_trend", analysis_area)
    # clip to poly
    aht_output_dir = output_dir + "/annual_herb_trend_Clip"
    arcpy.management.Clip(ah_trend, bb, aht_output_dir, analysis_area, "256", "ClippingGeometry", "NO_MAINTAIN_EXTENT")

# benchmark using reclassify
ah_output_dir = output_dir + "/annual_herb_Clip"

# do some raster stats to get acreage by category

# calculate some trends/timeseries if year is more than 5
# create a difference/trend raster
if len(year)>5:
    for indicator in indicator_list:
        print(indicator)


# add to ArcPro project?


