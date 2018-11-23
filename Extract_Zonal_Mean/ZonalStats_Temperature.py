import arcpy, os
from arcpy import env
from arcpy.sa import *

arcpy.CheckOutExtension("Spatial")

#OPEN MXD
#Open the Wshd25 Raster from the "C:/.../WatershedRaster" folder
#PAUSE DRAWING BEFORE RUNNING (Go to VIEW > Pause Drawing)
#Also - make sure Background Processing is disabled - this makes it less likely that Arc will crash while running
#  (Go to Geoprocessing > Geoprocessing Options, Uncheck the "Enable" box)
#  While you are in Geoprocessing Options, it's a good idea to check the box to Overwrite the Outputs

#UPDATE BOTH FOLDER LOCATIONS for input & output:
# Set environment settings
env.workspace =r"D:/01_Data/Temperature/Tmax/oa_Daily_Tmax"

#Open Watershed Raster file (zones) and set output workspace
Wshds = "D:/01_Data/Rainfall/Rainfall_Atlas/daily/All_Islands_Daily_RF/wshd/AlaWai_subwatershed_raster1.tif"
outws=r"G:/My Drive/Lab_Tsang/Project_SedimentFlux/SWAT/Zonal_AlaWai_Tmax_2"

#get list of all raster grid files in my current directory
grd_List = arcpy.ListRasters()

for layer in grd_List[78:len(grd_List)-1]:   #len(grd_List)-1 while you put your watershed raster file at the same place
   #Define Projection
    sr = arcpy.SpatialReference("WGS 1984") #The code for WGS 1984
    arcpy.DefineProjection_management(layer,sr)
	#print layer
    #Set Variables:
    inZoneData = Wshds
    zoneField = "Value"
    inValueRaster = layer
    #for some reason when I join the file name to the path it gives mixed slashes
    #  The .replace fixes it
    outTable = os.path.join(outws,layer+"_zonal.dbf").replace("\\","/").replace('.asc',"")
    outTable = str(outTable)
    # Execute ZonalStatisticsAsTable
    outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, outTable, "DATA", "MEAN") #change mean to "ALL" for all stats