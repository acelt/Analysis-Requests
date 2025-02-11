import arcpy, csv, os, glob
from arcgis.features import GeoAccessor, GeoSeriesAccessor
from arcgis.gis import GIS, Item
from pandas import DataFrame
from arcgis.features import FeatureLayer
from arcgis.features import FeatureLayerCollection
import pandas as pd
import numpy as np

gis = GIS("pro")

#d = r'C:\TerrestrialCalcs\TE_Calcs_CSVs'
# CHECK FOR WORKSPACE DIR AND CREATE IF MISSING
wd = r'C:\TerrestrialCalcs'
#os.chdir(wd)
if os.path.exists(wd):
    print("Directory C:\TerrestrialCalcs Exists..")
else:
    os.mkdir(wd)
    print("Creating Work Directory C:\TerrestrialCalcs...")

os.chdir(wd)
parent_dir = os.getcwd()

print(parent_dir)


# CHECK FOR FOLDER THAT CSV ERROR FILES WILL BE PLACED
if not os.path.exists("TE_Calcs_CSVs"):
    os.mkdir("TE_Calcs_CSVs")
    os.chdir(os.path.join(parent_dir,"TE_Calcs_CSVs"))
    print("Creating TE_Calcs_CSVs Folder...")
else:
    os.chdir(os.path.join(parent_dir,"TE_Calcs_CSVs"))
    pattern = '*.csv'
    files = glob.glob(pattern)
    for file in files:
        os.remove(file)



insertFields =  [
     'PlotKey',
     'BareSoilCover',
     'TotalFoliarCover',
     'GapCover_25_50',
     'GapCover_51_100',
     'GapCover_101_200',
     'GapCover_200_plus',
     'GapCover_25_plus',
     'SoilStability_All',
     'SoilStability_Protected',
     'SoilStability_Unprotected',
     'AH_ForbCover',
     'AH_NonNoxPerenForbCover',
     'FH_NonNoxPerenForbCover',
     'AH_NonNoxAnnForbCover',
     'FH_NonNoxAnnForbCover',
     'AH_NonNoxPerenGrassCover',
     'FH_NonNoxPerenGrassCover',
     'AH_GrassCover',
     'AH_AnnGrassCover',
     'AH_NonNoxAnnGrassCover',
     'FH_NonNoxAnnGrassCover',
     'AH_NonNoxAnnForbGrassCover',
     'AH_NonNoxPerenForbGrassCover',
     'AH_NonNoxSucculentCover',
     'FH_NonNoxSucculentCover',
     'AH_NonNoxShrubCover',
     'FH_NonNoxShrubCover',
     'AH_NonNoxSubShrubCover',
     'FH_NonNoxSubShrubCover',
     'AH_NonNoxTreeCover',
     'FH_NonNoxTreeCover',
     'AH_NoxPerenForbCover',
     'FH_NoxPerenForbCover',
     'AH_NoxAnnForbCover',
     'FH_NoxAnnForbCover',
     'AH_NoxPerenGrassCover',
     'FH_NoxPerenGrassCover',
     'AH_NoxAnnGrassCover',
     'FH_NoxAnnGrassCover',
     'AH_NoxAnnForbGrassCover',
     'AH_NoxPerenForbGrassCover',
     'AH_NoxSucculentCover',
     'FH_NoxSucculentCover',
     'AH_NoxShrubCover',
     'FH_NoxShrubCover',
     'AH_NoxSubShrubCover',
     'FH_NoxSubShrubCover',
     'AH_NoxTreeCover',
     'FH_NoxTreeCover',
     'AH_PerenForbGrassCover',
     'AH_PerenForbCover',
     'AH_PerenGrassCover',
     'AH_PreferredForbCover',
     'AH_ShortPerenGrassCover',
     'AH_TallPerenGrassCover',
     'AH_SagebrushCover',
     'FH_SagebrushCover',
     'AH_SagebrushCover_Live',
     'AH_NonSagebrushShrubCover',
     'AH_ShrubCover',
     'AH_NonNoxCover',
     'AH_NoxCover',
     'AH_TotalLitterCover',
     'AH_HerbLitterCover',
     'AH_WoodyLitterCover',
     'FH_TotalLitterCover',
     'FH_WoodyLitterCover',
     'FH_HerbLitterCover',
     'FH_DuffCover',
     'FH_EmbLitterCover',
     'FH_VagrLichenCover',
     'FH_LichenCover',
     'FH_MossCover',
     'FH_CyanobacteriaCover',
     'FH_RockCover',
     'FH_WaterCover',
     'FH_DepSoilCover',
     'Hgt_Woody_Avg',
     'Hgt_Herbaceous_Avg',
     'Hgt_Forb_Avg',
     'Hgt_PerenForb_Avg',
     'Hgt_Grass_Avg',
     'Hgt_PerenGrass_Avg',
     'Hgt_NoxPerenGrass_Avg',
     'Hgt_NonNoxPerenGrass_Avg',
     'Hgt_TallPerenGrass_Avg',
     'Hgt_ShortPerenGrass_Avg',
     'Hgt_PerenForbGrass_Avg',
     'Hgt_Shrub_Avg',
     'Hgt_NonSagebrushShrub_Avg',
     'Hgt_Sagebrush_Avg',
     'Hgt_Sagebrush_Live_Avg',
     'SagebrushShape_All_ColumnCount',
     'SagebrushShape_All_SpreadCount',
     'SagebrushShape_All_Predominant',
     'SagebrushShape_Live_ColumnCount',
     'SagebrushShape_Live_SpreadCount',
     'SagebrushShape_Live_Predominant',
     'NumSpp_NoxPlant',
     'Spp_Nox',
     'NumSpp_NonNoxPlant',
     'NumSpp_PreferredForb',
     'Spp_Sagebrush',
     'Spp_PreferredForb',
     'Spp_TallPerenGrass',
     'Spp_ShortPerenGrass']



def RunCalcs(adminstate,item_id):

    def myLen(lookupTable, lookupFields, lookupWhere):
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            return(len(list(cursor)))

    # Setup output dir and csv name
    outcsvname = 'InSeasonCalcs_' + adminstate + '.csv'
    outDir = r'C:\TerrestrialCalcs\TE_Calcs_CSVs'
    outcsv = os.path.join(outDir,outcsvname)

    #lpiDetail - pointLayer
    item = gis.content.get(item_id)
    plots_lyr = item.tables[2]
    plots_url = plots_lyr.url
    pointLayer = arcpy.TableToTable_conversion(plots_url, "memory", "pointLayer")
    arcpy.AddField_management(pointLayer, "PlotKey", "Text")
    arcpy.CalculateField_management(pointLayer, "PlotKey",  "!RecKey![:-2]", "PYTHON_9.3")

    # Get gapDetail table - gapLayer
    item = gis.content.get(item_id)
    gapdet_lyr = item.tables[1]
    gapdet_url = gapdet_lyr.url
    gapLayer = arcpy.TableToTable_conversion(gapdet_url, "memory", "gapLayer")
    arcpy.AddField_management(gapLayer, "PlotKey", "Text")
    arcpy.CalculateField_management(gapLayer, "PlotKey",  "!RecKey![:-2]", "PYTHON_9.3")

    # Get Soilstability Layer - gapHeaderLayer
    item = gis.content.get(item_id)
    soilstab_lyr = item.layers[7]
    soilstab_url = soilstab_lyr.url
    soilLayer = arcpy.TableToTable_conversion(soilstab_url, "memory", "soilLayer")

    # Get Gap Layer - gapHeaderLayer
    item = gis.content.get(item_id)
    gap_lyr = item.layers[2]
    gap_url = gap_lyr.url
    gapHeaderLayer = arcpy.TableToTable_conversion(gap_url, "memory", "gapHeaderLayer")

    # Get LPI Layer - lpiLayer
    item = gis.content.get(item_id)
    lpi_lyr = item.layers[4]
    lpi_url = lpi_lyr.url
    lpiLayer = arcpy.TableToTable_conversion(lpi_url, "memory", "lpiLayer")


    # Get speciesrichDetail table - speciesdetailLayer
    item = gis.content.get(item_id)
    specdetail_lyr = item.tables[4]
    specdetail_url = specdetail_lyr.url
    speciesdetailLayer = arcpy.TableToTable_conversion(specdetail_url, "memory", "speciesdetailLayer")
    arcpy.AddField_management(speciesdetailLayer, "PlotKey", "Text")
    arcpy.CalculateField_management(speciesdetailLayer, "PlotKey",  "!RecKey!", "PYTHON_9.3")


    # Get Plots Layer - plotLayer
    item = gis.content.get(item_id)
    plots_lyr = item.layers[0]
    plots_url = plots_lyr.url
    plotLayer = arcpy.TableToTable_conversion(plots_url, "memory", "plotLayer")

    # Get PlotChar Layer - plotLayer
    item = gis.content.get(item_id)
    plotchar_lyr = item.layers[5]
    plotchar_url = plotchar_lyr.url
    plotCharLayer = arcpy.TableToTable_conversion(plotchar_url, "memory", "plotCharLayer")

    # Get PlotObs Layer - plotLayer
    item = gis.content.get(item_id)
    plotobs_lyr = item.layers[6]
    plotobs_url = plotobs_lyr.url
    plotobsLayer = arcpy.TableToTable_conversion(plotobs_url, "memory", "plotobsLayer")

    # Get SpeciesList from Nathans SpeciesList HFS Table
    species_tbl_id = 'ab8e6bcde95746eaa17b76803c898ef3'
    item = gis.content.get(species_tbl_id)
    species_lyr = item.tables[0]
    species_url = species_lyr.url
    speciesLayer = arcpy.TableToTable_conversion(species_url, "memory", "speciesLayer")


    # initilize lists
    allDataValues = []
    dataValues = []

    invasiveSpeciesList = []
    nonnoxList = []

    treeList = []
    gramList = []
    forbList = []
    shrubList = []
    succList = []
    subShrubList = []

    annualList = []
    perennialList = []

    sageList = []
    nonsageList = []
    prefforbList = []
    shortpgrList = []
    tallpgrList = []

    litterList = ['L', 'HL', 'WL', 'NL'] #add DS ?
    soilslitterList = ['EL'] #add D ?
    rockList = ['R', 'GR', 'CB', 'ST', 'BY', 'BR']
    baresoilsurf = ['S','AG','CM','LM','FG']
    alldigitsList = ['0','1','2','3','4','5','6','7','8','9']
    unktypeList = ['AF', 'PF', 'AG', 'PG', 'SU', 'SH', 'TR', 'AS', 'PS']


    # Read in the plants table and create all the lists needed
    fields = ["SpeciesCode", "GrowthHabitSub", "Duration", "Noxious", "SG_Group", "SpeciesState"]
    whereClause = "SpeciesState = '" + adminstate + "'"
    with arcpy.da.SearchCursor(speciesLayer, fields) as cursor:
        for row in cursor:
            # add to invasive list
            if row[3] == "YES":# and row[6] == "allcalcs":#build the all calc List to be used for all calculations except total foliar cover
                invasiveSpeciesList.append(row[0])

            # build the non Noxious List
            if row[3] == "NO":
                nonnoxList.append(row[0])
            # build GrowthHabitSub Lists
            if row[1] == "Tree":# and row[6] == "allcalcs":
                treeList.append(row[0])


            elif row[1] == "Graminoid":# and row[6] == "allcalcs":
                gramList.append(row[0])

            elif row[1] == "Forb":# and row[6] == "allcalcs":
                forbList.append(row[0])

            elif row[1] == "Shrub":# and row[6] == "allcalcs":
                shrubList.append(row[0])


            elif row[1] == "SubShrub":# and row[6] == "allcalcs":
                subShrubList.append(row[0])

            elif row[1] == "Succulent":# and row[6] == "allcalcs":
                succList.append(row[0])



            # build Duration Lists
            if row[2] == "Annual":# and row[6] == "allcalcs":
                annualList.append(row[0])

            if row[2] == "Perennial":# and row[6] == "allcalcs":
                perennialList.append(row[0])


            # build sagebrush list
            # Sagebrush
            if row[4] == "Sagebrush":# and row[6] == "allcalcs":
                sageList.append(row[0])

            # Preferred Forb List
            if row[4] == "PreferredForb":# and row[6] == "allcalcs":
                prefforbList.append(row[0])

            # Short Stature Grass List
            if row[4] == "ShortStaturePerennialGrass":# and row[6] == "allcalcs":
                shortpgrList.append(row[0])

            # Tall Stature Grass List
            if row[4] == "TallStaturePerennialGrass":# and row[6] == "allcalcs":
                tallpgrList.append(row[0])

            # NonSagebrushShrub
            if row[4] == "NonSagebrushShrub" and (row[1] == "Shrub" or row[1] == "SubShrub"):# and row[6] == "allcalcs":
                nonsageList.append(row[0])



    # Now make sure all the lists are unique values only - SynonymOf can cause dups
    invasiveSpeciesList = list(set(invasiveSpeciesList))
    treeList = list(set(treeList))
    gramList = list(set(gramList))
    forbList = list(set(forbList))
    shrubList = list(set(shrubList))
    subShrubList = list(set(subShrubList))
    succList = list(set(succList))
    annualList = list(set(annualList))
    perennialList = list(set(perennialList))
    sageList = list(set(sageList))
    nonsageList = list(set(nonsageList))
    tallpgrList = list(set(tallpgrList))
    shortpgrList = list(set(shortpgrList))
    prefforbList = list(set(prefforbList))


    # Get Plots Layer
    item = gis.content.get(item_id)
    plotkey_sdf = item.layers[0].query(where="EvalStatus = 'Eval'",out_fields=['PlotKey']).sdf
    plotkey_sdf

    lyr_dup_pk_df = plotkey_sdf.groupby(["PlotKey"], as_index=False).agg(count=("PlotKey", "count"))
    lyr_dup_pk_df1 = lyr_dup_pk_df[lyr_dup_pk_df['count'] == 1]
    primaryKeys = lyr_dup_pk_df1['PlotKey'].tolist()

    pkField = 'PlotKey'

    for plot in primaryKeys:
        dataValues = []  # reset in case of records not lining up

        # 123 Plots layer does not have most of these
        fields = [pkField]
        whereClause = "PlotKey = '" + plot + "'"   # only want completed plots from above
        with arcpy.da.SearchCursor(plotLayer, fields, whereClause) as cursor:
            for row in cursor:
                dataValues.append(row[0])

        totalPoints = 0
        # 123 has no PlotKey here just RecKey
        fields = ["PlotKey"]
        whereClause = "PlotKey = '" + plot + "'"
        totalPoints = myLen(pointLayer, fields, whereClause)


        #TEMP CODE
        if totalPoints == 0:
            print("Warning found a line with no points " + plot)
            continue

        # 1st Hit Bare Soil Cover (%)
        #15 BareSoilCover
        fields = [pkField]
        whereClause = "PlotKey = '" + plot + "' AND TopCanopy = 'N' AND Lower1 IS NULL AND Lower2 IS NULL AND Lower3 IS NULL AND Lower4 IS NULL AND Lower5 IS NULL AND SoilSurface = 'S'"
        nom = myLen(pointLayer, fields, whereClause)
        dataValues.append((float(nom)/totalPoints) * 100)

        # 1st Hit Total Foliar Cover (%)
        #16 TotalFoliarCover
        fields = [pkField]
        whereClause = "PlotKey = '" + plot + "' AND TopCanopy IS NOT NULL AND TopCanopy <> 'N'"
        nom = myLen(pointLayer, fields, whereClause)
        dataValues.append((float(nom)/totalPoints) * 100)

        # Get the transect length which is used in all Gap calcs.  Note just assuming all 3 lines are the same for now to match other calcs
        fields = [pkField, "LineLength"]
        whereClause = "PlotKey = '" + plot + "'"
        transList = []
        transListlen = 0
        with arcpy.da.SearchCursor(gapHeaderLayer, fields, whereClause) as cursor:
                for row in cursor:
                    transList.append(row[1])
                    transListlen += int(row[1])
        # make sure all the same
        if len(set(transList)) > 1:
            print("Warning diff Transect Lengths found in Plot " + plot)
            print(plot + " line length " + str(transListlen) + " " + str(transList[:]))
        # convert to CM
        transLength = transListlen * 100

        # Proportion of Intercanopy Gaps 25-50 cm (% of lines)
        #17 GapCover_25_50
        lineLength = 0
        nom = 0
        fields = [pkField, "RecKey", "Gap", "RecType"]
        whereClause = "PlotKey = '" + plot + "' AND RecType ='C'"
        with arcpy.da.SearchCursor(gapLayer, fields, whereClause,sql_clause=('DISTINCT', None)) as cursor:
            # Build a RecKey (linekey) list - make sure unique values only - DISTINT is not working
            lineKeys = set(row[1] for row in cursor)

        # run loop for each line
        for line in lineKeys:
            nom = 0   # reset for each line
            dom = transLength
            whereClause = "RecKey = '" + line + "'"
            with arcpy.da.SearchCursor(gapLayer, fields, whereClause) as cursor:
                for row in cursor:
                    if row[2] != None and int(row[2]) >= 25 and int(row[2]) <= 50:
                        nom += int(row[2])
            lineLength += float(nom)/dom
        if len(lineKeys) == 0:
            dataValues.append(0)
        else:
            dataValues.append((lineLength) * 100)
        #        dataValues.append((lineLength / len(lineKeys) * 100)

        # Proportion of Intercanopy Gaps 51-100 cm (% of lines)
        #18 GapCover_51_100
        lineLength = 0
        nom = 0
        fields = [pkField, "RecKey", "Gap", "RecType"]
        whereClause = "PlotKey = '" + plot + "' AND RecType ='C'"
        with arcpy.da.SearchCursor(gapLayer, fields, whereClause,sql_clause=('DISTINCT', None)) as cursor:
            # Build a RecKey (linekey) list - make sure unique values only - DISTINT is not working
            lineKeys = set(row[1] for row in cursor)
        # run loop for each line
        for line in lineKeys:
            nom = 0   # reset for each line
            dom = transLength
            whereClause = "RecKey = '" + line + "'"
            with arcpy.da.SearchCursor(gapLayer, fields, whereClause) as cursor:
                for row in cursor:
                    if row[2] != None and int(row[2]) >= 51 and int(row[2]) <= 100:
                        nom += int(row[2])
            lineLength += float(nom)/dom
        if len(lineKeys) == 0:
            dataValues.append(0)
        else:
            dataValues.append((lineLength) * 100)
        #        dataValues.append((lineLength / len(lineKeys)) * 100)

        # Proportion of Intercanopy Gaps 101-200 cm (% of lines)
        #19 GapCover_101_200
        lineLength = 0
        nom = 0
        fields = [pkField, "RecKey", "Gap", "RecType"]
        whereClause = "PlotKey = '" + plot + "' AND RecType ='C'"
        with arcpy.da.SearchCursor(gapLayer, fields, whereClause,sql_clause=('DISTINCT', None)) as cursor:
            # Build a RecKey (linekey) list - make sure unique values only - DISTINT is not working
            lineKeys = set(row[1] for row in cursor)
        # run loop for each line
        for line in lineKeys:
            nom = 0   # reset for each line
            dom = transLength
            whereClause = "RecKey = '" + line + "' AND RecType ='C'"
            with arcpy.da.SearchCursor(gapLayer, fields, whereClause) as cursor:
                for row in cursor:
                    if row[2] != None and int(row[2]) >= 101 and int(row[2]) <= 200:
                        nom += int(row[2])
            lineLength += float(nom)/dom
        if len(lineKeys) == 0:
            dataValues.append(0)
        else:
            dataValues.append((lineLength) * 100)
        #        dataValues.append((lineLength / len(lineKeys)) * 100)

        # Proportion of Intercanopy Gaps > 200 cm (% of lines)
        #20 GapCover_200_plus
        lineLength = 0
        nom = 0
        fields = [pkField, "RecKey", "Gap", "RecType"]
        whereClause = "PlotKey = '" + plot + "' AND RecType ='C'"
        with arcpy.da.SearchCursor(gapLayer, fields, whereClause,sql_clause=('DISTINCT', None)) as cursor:
            # Build a RecKey (linekey) list - make sure unique values only - DISTINT is not working
            lineKeys = set(row[1] for row in cursor)
        # run loop for each line
        for line in lineKeys:
            nom = 0   # reset for each line
            dom = transLength
            whereClause = "RecKey = '" + line + "'"
            with arcpy.da.SearchCursor(gapLayer, fields, whereClause) as cursor:
                for row in cursor:
                    if row[2] != None and int(row[2]) > 200:
                        nom += int(row[2])
            lineLength += float(nom)/dom
        if len(lineKeys) == 0:
            dataValues.append(0)
        else:
            dataValues.append((lineLength) * 100)
        #        dataValues.append((lineLength / len(lineKeys)) * 100)

        # Proportion of Intercanopy Gaps >25 cm (% of lines)
        #21 GapCover_25_plus
        lineLength = 0
        nom = 0
        fields = [pkField, "RecKey", "Gap", "RecType"]
        whereClause = "PlotKey = '" + plot + "' AND RecType ='C'"
        with arcpy.da.SearchCursor(gapLayer, fields, whereClause,sql_clause=('DISTINCT', None)) as cursor:
            # Build a RecKey (linekey) list - make sure unique values only - DISTINT is not working
            lineKeys = set(row[1] for row in cursor)
        # run loop for each line
        for line in lineKeys:
            nom = 0   # reset for each line
            dom = transLength
            whereClause = "RecKey = '" + line + "'"
            with arcpy.da.SearchCursor(gapLayer, fields, whereClause) as cursor:
                for row in cursor:
                    if row[2] != None and int(row[2]) >= 25:
                        nom += int(row[2])
            lineLength += float(nom)/dom
        if len(lineKeys) == 0:
            dataValues.append(0)
        else:
            dataValues.append((lineLength) * 100)
        #        dataValues.append((lineLength / len(lineKeys)) * 100)

        # Soil Aggregate Stability – All (Average Rating 1-6)
        #22 SoilStability_All
        nom = 0
        dom = 0
        # build field list
        fields = [pkField]
        #fields = ["RecKey"]
        for i in range(1,19):
            fields.append( "Rating" + str(i))
        #fields = ["RecKey", "SpeciesWoody", "HeightWoody"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(soilLayer, fields, whereClause) as cursor:
            for row in cursor:
                for i in range(1,19):
                    # Only count rows with values - does 0 count here??
                    if row[i] != None:
                        nom += int(row[i])
                        dom += 1
        if dom == 0:
            dataValues.append(None)
        else:
            dataValues.append(float(nom)/dom)

        # Soil Aggregate Stability – Protected Surface (Average Rating 1-6)
        #23 SoilStability_Protected
        nom = 0
        dom = 0
        # list from the How to load the new AIM Collector and Survey123 Project.doc
        protectedList = ["G", "F", "Sh", "T", "M", "U"]
        # build field list
        fields = [pkField]
        for i in range(1,19):
            fields.append( "Rating" + str(i))
            fields.append( "Veg" + str(i))
        #fields = ["RecKey", "SpeciesWoody", "HeightWoody"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(soilLayer, fields, whereClause) as cursor:
            for row in cursor:
                for i in range(1,37,2):  # step by 2 since we have rating/veg pairs now
                    # Only count rows with values - does 0 count here??
                    if row[i] != None:
                        if row[i+1] in protectedList:
                            nom += int(row[i])
                            dom += 1
        if dom == 0:
            dataValues.append(None)
        else:
            dataValues.append(float(nom)/dom)


        # Soil Aggregate Stability – Unprotected Surface (Average Rating 1-6)
        #24 SoilStability_Unprotected
        nom = 0
        dom = 0
        # list from the How to load the new AIM Collector and Survey123 Project.doc
        protectedList = ["G", "F", "Sh", "T", "M", "U"]
        # build field list
        fields = [pkField]
        for i in range(1,19):
            fields.append( "Rating" + str(i))
            fields.append( "Veg" + str(i))
        #fields = ["RecKey", "SpeciesWoody", "HeightWoody"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(soilLayer, fields, whereClause) as cursor:
            for row in cursor:
                for i in range(1,37,2):  # step by 2 since we have rating/veg pairs now
                    # Only count rows with values - does 0 count here??
                    if row[i] != None:
                        if row[i+1] == "NC":
                            nom += int(row[i])
                            dom += 1
        if dom == 0:
            dataValues.append(None)
        else:
            dataValues.append(float(nom)/dom)

        #25 AH_ForbCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in forbList) or (row[1] == "PFXXXX") or (row[1] == "AFXXXX")):
                    nom += 1
                elif row[2] != None and ((row[2] in forbList) or (row[2] == "PFXXXX") or (row[2] == "AFXXXX")):
                    nom += 1
                elif row[3] != None and ((row[3] in forbList) or (row[3] == "PFXXXX") or (row[3] == "AFXXXX")):
                    nom += 1
                elif row[4] != None and ((row[4] in forbList) or (row[4] == "PFXXXX") or (row[4] == "AFXXXX")):
                    nom += 1
                elif row[5] != None and ((row[5] in forbList) or (row[5] == "PFXXXX") or (row[5] == "AFXXXX")):
                    nom += 1
                elif row[6] != None and ((row[6] in forbList) or (row[6] == "PFXXXX") or (row[6] == "AFXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit NonInv Perennial Forbs (%) and FH
        #26 AH_NonNoxPerenForbCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in forbList and row[1] in perennialList and not row[1] in invasiveSpeciesList) or (row[1] == "PFXXXX")):
                    nom += 1
                elif row[2] != None and ((row[2] in forbList and row[2] in perennialList and not row[2] in invasiveSpeciesList) or (row[2] == "PFXXXX")):
                    nom += 1
                elif row[3] != None and ((row[3] in forbList and row[3] in perennialList and not row[3] in invasiveSpeciesList) or (row[3] == "PFXXXX")):
                    nom += 1
                elif row[4] != None and ((row[4] in forbList and row[4] in perennialList and not row[4] in invasiveSpeciesList) or (row[4] == "PFXXXX")):
                    nom += 1
                elif row[5] != None and ((row[5] in forbList and row[5] in perennialList and not row[5] in invasiveSpeciesList) or (row[5] == "PFXXXX")):
                    nom += 1
                elif row[6] != None and ((row[6] in forbList and row[6] in perennialList and not row[6] in invasiveSpeciesList) or (row[6] == "PFXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #27 FH_NonNoxPerenForbCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in forbList and row[1] in perennialList and not row[1] in invasiveSpeciesList) or (row[1] == "PFXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit NonInv Annual Forbs (%) and FH
        #28 AH_NonNoxAnnForbCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in forbList and row[1] in annualList and not row[1] in invasiveSpeciesList) or (row[1] == "AFXXXX")):
                    nom += 1
                elif row[2] != None and ((row[2] in forbList and row[2] in annualList and not row[2] in invasiveSpeciesList) or (row[2] == "AFXXXX")):
                    nom += 1
                elif row[3] != None and ((row[3] in forbList and row[3] in annualList and not row[3] in invasiveSpeciesList) or (row[3] == "AFXXXX")):
                    nom += 1
                elif row[4] != None and ((row[4] in forbList and row[4] in annualList and not row[4] in invasiveSpeciesList) or (row[4] == "AFXXXX")):
                    nom += 1
                elif row[5] != None and ((row[5] in forbList and row[5] in annualList and not row[5] in invasiveSpeciesList) or (row[5] == "AFXXXX")):
                    nom += 1
                elif row[6] != None and ((row[6] in forbList and row[6] in annualList and not row[6] in invasiveSpeciesList) or (row[6] == "AFXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #29 FH_NonNoxAnnForbCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in forbList and row[1] in annualList and not row[1] in invasiveSpeciesList) or (row[1] == "AFXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)


        # Any Hit NonInv Perennial Grass (%) and FH
        #30 AH_NonNoxPerenGrassCover

        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in gramList and row[1] in perennialList and not row[1] in invasiveSpeciesList) or (row[1] == "PGXXXX")):
                    nom += 1
                elif row[2] != None and ((row[2] in gramList and row[2] in perennialList and not row[2] in invasiveSpeciesList) or (row[2] == "PGXXXX")):
                    nom += 1
                elif row[3] != None and ((row[3] in gramList and row[3] in perennialList and not row[3] in invasiveSpeciesList) or (row[3] == "PGXXXX")):
                    nom += 1
                elif row[4] != None and ((row[4] in gramList and row[4] in perennialList and not row[4] in invasiveSpeciesList) or (row[4] == "PGXXXX")):
                    nom += 1
                elif row[5] != None and ((row[5] in gramList and row[5] in perennialList and not row[5] in invasiveSpeciesList) or (row[5] == "PGXXXX")):
                    nom += 1
                elif row[6] != None and ((row[6] in gramList and row[6] in perennialList and not row[6] in invasiveSpeciesList) or (row[6] == "PGXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #31 FH_NonNoxPerenGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in gramList and row[1] in perennialList and not row[1] in invasiveSpeciesList) or (row[1] == "PGXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #32 AH_GrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in gramList) or (row[1] == "AGXXXX") or (row[1] == "PGXXXX")):
                    nom += 1
                elif row[2] != None and ((row[2] in gramList) or (row[2] == "AGXXXX") or (row[2] == "PGXXXX")):
                    nom += 1
                elif row[3] != None and ((row[3] in gramList) or (row[3] == "AGXXXX") or (row[3] == "PGXXXX")):
                    nom += 1
                elif row[4] != None and ((row[4] in gramList) or (row[4] == "AGXXXX") or (row[4] == "PGXXXX")):
                    nom += 1
                elif row[5] != None and ((row[5] in gramList) or (row[5] == "AGXXXX") or (row[5] == "PGXXXX")):
                    nom += 1
                elif row[6] != None and ((row[6] in gramList) or (row[6] == "AGXXXX") or (row[6] == "PGXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)


        #33 AH_AnnGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in gramList and row[1] in annualList) or (row[1] == "AGXXXX")):
                    nom += 1
                elif row[2] != None and ((row[2] in gramList and row[2] in annualList) or (row[2] == "AGXXXX")):
                    nom += 1
                elif row[3] != None and ((row[3] in gramList and row[3] in annualList) or (row[3] == "AGXXXX")):
                    nom += 1
                elif row[4] != None and ((row[4] in gramList and row[4] in annualList) or (row[4] == "AGXXXX")):
                    nom += 1
                elif row[5] != None and ((row[5] in gramList and row[5] in annualList) or (row[5] == "AGXXXX")):
                    nom += 1
                elif row[6] != None and ((row[6] in gramList and row[6] in annualList) or (row[6] == "AGXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit NonInv Annual Grass (%) and FH
        #34 AH_NonNoxAnnGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in gramList and row[1] in annualList and not row[1] in invasiveSpeciesList) or (row[1] == "AGXXXX")):
                    nom += 1
                elif row[2] != None and ((row[2] in gramList and row[2] in annualList and not row[2] in invasiveSpeciesList) or (row[2] == "AGXXXX")):
                    nom += 1
                elif row[3] != None and ((row[3] in gramList and row[3] in annualList and not row[3] in invasiveSpeciesList) or (row[3] == "AGXXXX")):
                    nom += 1
                elif row[4] != None and ((row[4] in gramList and row[4] in annualList and not row[4] in invasiveSpeciesList) or (row[4] == "AGXXXX")):
                    nom += 1
                elif row[5] != None and ((row[5] in gramList and row[5] in annualList and not row[5] in invasiveSpeciesList) or (row[5] == "AGXXXX")):
                    nom += 1
                elif row[6] != None and ((row[6] in gramList and row[6] in annualList and not row[6] in invasiveSpeciesList) or (row[6] == "AGXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)


        #35 FH_NonNoxAnnGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if (row[1] in gramList and row[1] in annualList and not row[1] in invasiveSpeciesList) or (row[1] == "AGXXXX"):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit NonInv Annual Forb or Grass (%)
        #36 AH_NonNoxAnnForbGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((((row[1] in gramList or row[1] in forbList) and row[1] in annualList and not row[1] in invasiveSpeciesList) or (row[1] == "AGXXXX" or row[1] == "AFXXXX"))):
                    nom += 1
                elif row[2] != None and ((((row[2] in gramList or row[2] in forbList) and row[2] in annualList and not row[2] in invasiveSpeciesList) or (row[2] == "AGXXXX" or row[2] == "AFXXXX"))):
                    nom += 1
                elif row[3] != None and ((((row[3] in gramList or row[3] in forbList) and row[3] in annualList and not row[3] in invasiveSpeciesList) or (row[3] == "AGXXXX" or row[3] == "AFXXXX"))):
                    nom += 1
                elif row[4] != None and ((((row[4] in gramList or row[4] in forbList) and row[4] in annualList and not row[4] in invasiveSpeciesList) or (row[4] == "AGXXXX" or row[4] == "AFXXXX"))):
                    nom += 1
                elif row[5] != None and ((((row[5] in gramList or row[5] in forbList) and row[5] in annualList and not row[5] in invasiveSpeciesList) or (row[5] == "AGXXXX" or row[5] == "AFXXXX"))):
                    nom += 1
                elif row[6] != None and ((((row[6] in gramList or row[6] in forbList) and row[6] in annualList and not row[6] in invasiveSpeciesList) or (row[6] == "AGXXXX" or row[6] == "AFXXXX"))):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit NonInv Peren Forb or Grass (%)
        #37 AH_NonNoxPerenForbGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((((row[1] in gramList or row[1] in forbList) and row[1] in perennialList and not row[1] in invasiveSpeciesList) or (row[1] == "PGXXXX" or row[1] == "PFXXXX"))):
                    nom += 1
                elif row[2] != None and ((((row[2] in gramList or row[2] in forbList) and row[2] in perennialList and not row[2] in invasiveSpeciesList) or (row[2] == "PGXXXX" or row[2] == "PFXXXX"))):
                    nom += 1
                elif row[3] != None and ((((row[3] in gramList or row[3] in forbList) and row[3] in perennialList and not row[3] in invasiveSpeciesList) or (row[3] == "PGXXXX" or row[3] == "PFXXXX"))):
                    nom += 1
                elif row[4] != None and ((((row[4] in gramList or row[4] in forbList) and row[4] in perennialList and not row[4] in invasiveSpeciesList) or (row[4] == "PGXXXX" or row[4] == "PFXXXX"))):
                    nom += 1
                elif row[5] != None and ((((row[5] in gramList or row[5] in forbList) and row[5] in perennialList and not row[5] in invasiveSpeciesList) or (row[5] == "PGXXXX" or row[5] == "PFXXXX"))):
                    nom += 1
                elif row[6] != None and ((((row[6] in gramList or row[6] in forbList) and row[6] in perennialList and not row[6] in invasiveSpeciesList) or (row[6] == "PGXXXX" or row[6] == "PFXXXX"))):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit NonInv Succulent (%) and FH
        #38 AH_NonNoxSucculentCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in succList and not row[1] in invasiveSpeciesList) or (row[1] == "SUXXXX")):
                    nom += 1
                elif row[2] != None and ((row[2] in succList and not row[2] in invasiveSpeciesList) or (row[2] == "SUXXXX")):
                    nom += 1
                elif row[3] != None and ((row[3] in succList and not row[3] in invasiveSpeciesList) or (row[3] == "SUXXXX")):
                    nom += 1
                elif row[4] != None and ((row[4] in succList and not row[4] in invasiveSpeciesList) or (row[4] == "SUXXXX")):
                    nom += 1
                elif row[5] != None and ((row[5] in succList and not row[5] in invasiveSpeciesList) or (row[5] == "SUXXXX")):
                    nom += 1
                elif row[6] != None and ((row[6] in succList and not row[6] in invasiveSpeciesList) or (row[6] == "SUXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #39 FH_NonNoxSucculentCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in succList and not row[1] in invasiveSpeciesList) or (row[1] == "SUXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit NonInv Shrub (%) and FH
        #40 AH_NonNoxShrubCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in shrubList and not row[1] in invasiveSpeciesList) or (row[1] == "SHXXXX")):
                    nom += 1
                elif row[2] != None and ((row[2] in shrubList and not row[2] in invasiveSpeciesList) or (row[2] == "SHXXXX")):
                    nom += 1
                elif row[3] != None and ((row[3] in shrubList and not row[3] in invasiveSpeciesList) or (row[3] == "SHXXXX")):
                    nom += 1
                elif row[4] != None and ((row[4] in shrubList and not row[4] in invasiveSpeciesList) or (row[4] == "SHXXXX")):
                    nom += 1
                elif row[5] != None and ((row[5] in shrubList and not row[5] in invasiveSpeciesList) or (row[5] == "SHXXXX")):
                    nom += 1
                elif row[6] != None and ((row[6] in shrubList and not row[6] in invasiveSpeciesList) or (row[6] == "SHXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #41 FH_NonNoxShrubCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in shrubList and not row[1] in invasiveSpeciesList) or (row[1] == "SHXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)


        # Any Hit NonInv Sub-Shrub (%) and FH Does SHXXXX count here - NO it might be SSXXXXX not sure
        #42 AH_NonNoxSubShrubCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] in subShrubList and not row[1] in invasiveSpeciesList:
                    nom += 1
                elif row[2] != None and row[2] in subShrubList and not row[2] in invasiveSpeciesList:
                    nom += 1
                elif row[3] != None and row[3] in subShrubList and not row[3] in invasiveSpeciesList:
                    nom += 1
                elif row[4] != None and row[4] in subShrubList and not row[4] in invasiveSpeciesList:
                    nom += 1
                elif row[5] != None and row[5] in subShrubList and not row[5] in invasiveSpeciesList:
                    nom += 1
                elif row[6] != None and row[6] in subShrubList and not row[6] in invasiveSpeciesList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #43 FH_NonNoxSubShrubCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] in subShrubList and not row[1] in invasiveSpeciesList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit NonInv Tree (%) and FH
        #44 AH_NonNoxTreeCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in treeList and not row[1] in invasiveSpeciesList) or (row[1] == "TRXXXX")):
                    nom += 1
                elif row[2] != None and ((row[2] in treeList and not row[2] in invasiveSpeciesList) or (row[2] == "TRXXXX")):
                    nom += 1
                elif row[3] != None and ((row[3] in treeList and not row[3] in invasiveSpeciesList) or (row[3] == "TRXXXX")):
                    nom += 1
                elif row[4] != None and ((row[4] in treeList and not row[4] in invasiveSpeciesList) or (row[4] == "TRXXXX")):
                    nom += 1
                elif row[5] != None and ((row[5] in treeList and not row[5] in invasiveSpeciesList) or (row[5] == "TRXXXX")):
                    nom += 1
                elif row[6] != None and ((row[6] in treeList and not row[6] in invasiveSpeciesList) or (row[6] == "TRXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #45 FH_NonNoxTreeCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in treeList and row[1] not in invasiveSpeciesList) or (row[1] == "TRXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit Perennial Forbs - Invasive (%) and FH
        #46 AH_NoxPerenForbCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in forbList and row[1] in perennialList and row[1] in invasiveSpeciesList):
                    nom += 1
                elif row[2] != None and (row[2] in forbList and row[2] in perennialList and row[2] in invasiveSpeciesList):
                    nom += 1
                elif row[3] != None and (row[3] in forbList and row[3] in perennialList and row[3] in invasiveSpeciesList):
                    nom += 1
                elif row[4] != None and (row[4] in forbList and row[4] in perennialList and row[4] in invasiveSpeciesList):
                    nom += 1
                elif row[5] != None and (row[5] in forbList and row[5] in perennialList and row[5] in invasiveSpeciesList):
                    nom += 1
                elif row[6] != None and (row[6] in forbList and row[6] in perennialList and row[6] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #47 FH_NoxPerenForbCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in forbList and row[1] in perennialList and row[1] in invasiveSpeciesList)):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)


        # Any Hit Annual Forbs - Invasive (%) and FH
        #48 AH_NoxAnnForbCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in forbList and row[1] in annualList and row[1] in invasiveSpeciesList):
                    nom += 1
                elif row[2] != None and (row[2] in forbList and row[2] in annualList and row[2] in invasiveSpeciesList):
                    nom += 1
                elif row[3] != None and (row[3] in forbList and row[3] in annualList and row[3] in invasiveSpeciesList):
                    nom += 1
                elif row[4] != None and (row[4] in forbList and row[4] in annualList and row[4] in invasiveSpeciesList):
                    nom += 1
                elif row[5] != None and (row[5] in forbList and row[5] in annualList and row[5] in invasiveSpeciesList):
                    nom += 1
                elif row[6] != None and (row[6] in forbList and row[6] in annualList and row[6] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #49 FH_NoxAnnForbCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in forbList and row[1] in annualList and row[1] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit Perennial Grass - Invasive (%) also adding in FH to reduce size of code
        #50 AH_NoxPerenGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in gramList and row[1] in perennialList and row[1] in invasiveSpeciesList):
                    nom += 1
                elif row[2] != None and (row[2] in gramList and row[2] in perennialList and row[2] in invasiveSpeciesList):
                    nom += 1
                elif row[3] != None and (row[3] in gramList and row[3] in perennialList and row[3] in invasiveSpeciesList):
                    nom += 1
                elif row[4] != None and (row[4] in gramList and row[4] in perennialList and row[4] in invasiveSpeciesList):
                    nom += 1
                elif row[5] != None and (row[5] in gramList and row[5] in perennialList and row[5] in invasiveSpeciesList):
                    nom += 1
                elif row[6] != None and (row[6] in gramList and row[6] in perennialList and row[6] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #51 FH_NoxPerenGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in gramList and row[1] in perennialList and row[1] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit Annual Grass - Invasive (%) and FH
        #52 AH_NoxAnnGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in gramList and row[1] in annualList and row[1] in invasiveSpeciesList):
                    nom += 1
                elif row[2] != None and (row[2] in gramList and row[2] in annualList and row[2] in invasiveSpeciesList):
                    nom += 1
                elif row[3] != None and (row[3] in gramList and row[3] in annualList and row[3] in invasiveSpeciesList):
                    nom += 1
                elif row[4] != None and (row[4] in gramList and row[4] in annualList and row[4] in invasiveSpeciesList):
                    nom += 1
                elif row[5] != None and (row[5] in gramList and row[5] in annualList and row[5] in invasiveSpeciesList):
                    nom += 1
                elif row[6] != None and (row[6] in gramList and row[6] in annualList and row[6] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #53 FH_NoxAnnGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in gramList and row[1] in annualList and row[1] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)


        # Any Hit Annual Forb or Annual Grass - Invasive (%)
        #54 AH_NoxAnnForbGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in gramList or row[1] in forbList) and row[1] in annualList and row[1] in invasiveSpeciesList):
                    nom += 1
                elif row[2] != None and ((row[2] in gramList or row[2] in forbList) and row[2] in annualList and row[2] in invasiveSpeciesList):
                    nom += 1
                elif row[3] != None and ((row[3] in gramList or row[3] in forbList) and row[3] in annualList and row[3] in invasiveSpeciesList):
                    nom += 1
                elif row[4] != None and ((row[4] in gramList or row[4] in forbList) and row[4] in annualList and row[4] in invasiveSpeciesList):
                    nom += 1
                elif row[5] != None and ((row[5] in gramList or row[5] in forbList) and row[5] in annualList and row[5] in invasiveSpeciesList):
                    nom += 1
                elif row[6] != None and ((row[6] in gramList or row[6] in forbList) and row[6] in annualList and row[6] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit Perennial Forb or Grass - Invasive (%)
        #55 AH_NoxPerenForbGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in gramList or row[1] in forbList) and row[1] in perennialList and row[1] in invasiveSpeciesList):
                    nom += 1
                elif row[2] != None and ((row[2] in gramList or row[2] in forbList) and row[2] in perennialList and row[2] in invasiveSpeciesList):
                    nom += 1
                elif row[3] != None and ((row[3] in gramList or row[3] in forbList) and row[3] in perennialList and row[3] in invasiveSpeciesList):
                    nom += 1
                elif row[4] != None and ((row[4] in gramList or row[4] in forbList) and row[4] in perennialList and row[4] in invasiveSpeciesList):
                    nom += 1
                elif row[5] != None and ((row[5] in gramList or row[5] in forbList) and row[5] in perennialList and row[5] in invasiveSpeciesList):
                    nom += 1
                elif row[6] != None and ((row[6] in gramList or row[6] in forbList) and row[6] in perennialList and row[6] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit Succulent - Invasive (%) and FH
        #56 AH_NoxSucculentCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in succList and row[1] in invasiveSpeciesList):
                    nom += 1
                elif row[2] != None and (row[2] in succList and row[2] in invasiveSpeciesList):
                    nom += 1
                elif row[3] != None and (row[3] in succList and row[3] in invasiveSpeciesList):
                    nom += 1
                elif row[4] != None and (row[4] in succList and row[4] in invasiveSpeciesList):
                    nom += 1
                elif row[5] != None and (row[5] in succList and row[5] in invasiveSpeciesList):
                    nom += 1
                elif row[6] != None and (row[6] in succList and row[6] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #57 FH_NoxSucculentCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in succList and row[1] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit Shrub - Invasive (%) and FH
        #58 AH_NoxShrubCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in shrubList and row[1] in invasiveSpeciesList):
                    nom += 1
                elif row[2] != None and (row[2] in shrubList and row[2] in invasiveSpeciesList):
                    nom += 1
                elif row[3] != None and (row[3] in shrubList and row[3] in invasiveSpeciesList):
                    nom += 1
                elif row[4] != None and (row[4] in shrubList and row[4] in invasiveSpeciesList):
                    nom += 1
                elif row[5] != None and (row[5] in shrubList and row[5] in invasiveSpeciesList):
                    nom += 1
                elif row[6] != None and (row[6] in shrubList and row[6] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #59 FH_NoxShrubCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in shrubList and row[1] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit Sub-Shrub – Invasive (%) and FH aadd SSXXXX??
        #60 AH_NoxSubShrubCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] in subShrubList and row[1] in invasiveSpeciesList:
                    nom += 1
                elif row[2] != None and row[2] in subShrubList and row[2] in invasiveSpeciesList:
                    nom += 1
                elif row[3] != None and row[3] in subShrubList and row[3] in invasiveSpeciesList:
                    nom += 1
                elif row[4] != None and row[4] in subShrubList and row[4] in invasiveSpeciesList:
                    nom += 1
                elif row[5] != None and row[5] in subShrubList and row[5] in invasiveSpeciesList:
                    nom += 1
                elif row[6] != None and row[6] in subShrubList and row[6] in invasiveSpeciesList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #61 FH_NoxSubShrubCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] in subShrubList and row[1] in invasiveSpeciesList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)



        # Any Hit Tree - Invasive (%) and FH
        #62 AH_NoxTreeCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in treeList and row[1] in invasiveSpeciesList):
                    nom += 1
                elif row[2] != None and (row[2] in treeList and row[2] in invasiveSpeciesList):
                    nom += 1
                elif row[3] != None and (row[3] in treeList and row[3] in invasiveSpeciesList):
                    nom += 1
                elif row[4] != None and (row[4] in treeList and row[4] in invasiveSpeciesList):
                    nom += 1
                elif row[5] != None and (row[5] in treeList and row[5] in invasiveSpeciesList):
                    nom += 1
                elif row[6] != None and (row[6] in treeList and row[6] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #63 FH_NoxTreeCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in treeList and row[1] in invasiveSpeciesList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit Perennial Forb or Grass - Invasive or Not
        #64 AH_PerenForbGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (((row[1] in gramList or row[1] in forbList) and row[1] in perennialList) or ((row[1] == "PGXXXX" or row[1] == "PFXXXX"))):
                    nom += 1
                elif row[2] != None and (((row[2] in gramList or row[2] in forbList) and row[2] in perennialList) or ((row[2] == "PGXXXX" or row[2] == "PFXXXX"))):
                    nom += 1
                elif row[3] != None and (((row[3] in gramList or row[3] in forbList) and row[3] in perennialList) or ((row[3] == "PGXXXX" or row[3] == "PFXXXX"))):
                    nom += 1
                elif row[4] != None and (((row[4] in gramList or row[4] in forbList) and row[4] in perennialList) or ((row[4] == "PGXXXX" or row[4] == "PFXXXX"))):
                    nom += 1
                elif row[5] != None and (((row[5] in gramList or row[5] in forbList) and row[5] in perennialList) or ((row[5] == "PGXXXX" or row[5] == "PFXXXX"))):
                    nom += 1
                elif row[6] != None and (((row[6] in gramList or row[6] in forbList) and row[6] in perennialList) or ((row[6] == "PGXXXX" or row[6] == "PFXXXX"))):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit Perennial Forbs - Invasive (%) and Not
        #65 AH_PerenForbCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (((row[1] in forbList and row[1] in perennialList) or (row[1] == "PFXXXX"))):
                    nom += 1
                elif row[2] != None and (((row[2] in forbList and row[2] in perennialList) or (row[2] == "PFXXXX"))):
                    nom += 1
                elif row[3] != None and (((row[3] in forbList and row[3] in perennialList) or (row[3] == "PFXXXX"))):
                    nom += 1
                elif row[4] != None and (((row[4] in forbList and row[4] in perennialList) or (row[4] == "PFXXXX"))):
                    nom += 1
                elif row[5] != None and (((row[5] in forbList and row[5] in perennialList) or (row[5] == "PFXXXX"))):
                    nom += 1
                elif row[6] != None and (((row[6] in forbList and row[6] in perennialList) or (row[6] == "PFXXXX"))):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit Perennial Grass - Invasive (%) and Not
        #66 AH_PerenGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (((row[1] in gramList and row[1] in perennialList) or (row[1] == "PGXXXX"))):
                    nom += 1
                elif row[2] != None and (((row[2] in gramList and row[2] in perennialList) or (row[2] == "PGXXXX"))):
                    nom += 1
                elif row[3] != None and (((row[3] in gramList and row[3] in perennialList) or (row[3] == "PGXXXX"))):
                    nom += 1
                elif row[4] != None and (((row[4] in gramList and row[4] in perennialList) or (row[4] == "PGXXXX" ))):
                    nom += 1
                elif row[5] != None and (((row[5] in gramList and row[5] in perennialList) or (row[5] == "PGXXXX"))):
                    nom += 1
                elif row[6] != None and (((row[6] in gramList and row[6] in perennialList) or (row[6] == "PGXXXX"))):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #67 AH_PreferredForbCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] in prefforbList:
                    nom += 1
                elif row[2] != None and row[2] in prefforbList:
                    nom += 1
                elif row[3] != None and row[3] in prefforbList:
                    nom += 1
                elif row[4] != None and row[4] in prefforbList:
                    nom += 1
                elif row[5] != None and row[5] in prefforbList:
                    nom += 1
                elif row[6] != None and row[6] in prefforbList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #68  AH_ShortPerenGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] in shortpgrList:
                    nom += 1
                elif row[2] != None and row[2] in shortpgrList:
                    nom += 1
                elif row[3] != None and row[3] in shortpgrList:
                    nom += 1
                elif row[4] != None and row[4] in shortpgrList:
                    nom += 1
                elif row[5] != None and row[5] in shortpgrList:
                    nom += 1
                elif row[6] != None and row[6] in shortpgrList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)


        #69  AH_TallPerenGrassCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] in tallpgrList:
                    nom += 1
                elif row[2] != None and row[2] in tallpgrList:
                    nom += 1
                elif row[3] != None and row[3] in tallpgrList:
                    nom += 1
                elif row[4] != None and row[4] in tallpgrList:
                    nom += 1
                elif row[5] != None and row[5] in tallpgrList:
                    nom += 1
                elif row[6] != None and row[6] in tallpgrList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)


        # Any Hit Sagebrush (%) and FH
        #70 AH_SagebrushCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in sageList:
                    nom += 1
                elif row[2] in sageList:
                    nom += 1
                elif row[3] in sageList:
                    nom += 1
                elif row[4] in sageList:
                    nom += 1
                elif row[5] in sageList:
                    nom += 1
                elif row[6] in sageList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #71 FH_SagebrushCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in sageList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit Sagebrush5 Live only  Live = 0
        #72 AH_SagebrushCover_Live
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "ChkboxTop", "ChkboxLower1", "ChkboxLower2", "ChkboxLower3", "ChkboxLower4", "ChkboxLower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in sageList and row[7] == 0:
                    nom += 1
                elif row[2] in sageList and row[8] == 0:
                    nom += 1
                elif row[3] in sageList and row[9] == 0:
                    nom += 1
                elif row[4] in sageList and row[10] == 0:
                    nom += 1
                elif row[5] in sageList and row[11] == 0:
                    nom += 1
                elif row[6] in sageList and row[12] == 0:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)


        # Non Sagebrush Cover
        #73 AH_NonSagebrushShrubCover
        nom = 0
        nomFH = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in nonsageList:
                    nom += 1
                elif row[2] in nonsageList:
                    nom += 1
                elif row[3] in nonsageList:
                    nom += 1
                elif row[4] in nonsageList:
                    nom += 1
                elif row[5] in nonsageList:
                    nom += 1
                elif row[6] in nonsageList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Hit Shrub
        #74 AH_ShrubCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in shrubList) or (row[1] == "SHXXXX")):
                    nom += 1
                elif row[2] != None and ((row[2] in shrubList) or (row[2] == "SHXXXX")):
                    nom += 1
                elif row[3] != None and ((row[3] in shrubList) or (row[3] == "SHXXXX")):
                    nom += 1
                elif row[4] != None and ((row[4] in shrubList) or (row[4] == "SHXXXX")):
                    nom += 1
                elif row[5] != None and ((row[5] in shrubList) or (row[5] == "SHXXXX")):
                    nom += 1
                elif row[6] != None and ((row[6] in shrubList) or (row[6] == "SHXXXX")):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Any Non Nox
        #75 AH_NonNoxCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in nonnoxList or (row[1][:2] in unktypeList)):
                    nom += 1
                elif row[2] != None and (row[2] in nonnoxList or (row[2][:2] in unktypeList)):
                    nom += 1
                elif row[3] != None and (row[3] in nonnoxList or (row[3][:2] in unktypeList)):
                    nom += 1
                elif row[4] != None and (row[4] in nonnoxList or (row[4][:2] in unktypeList)):
                    nom += 1
                elif row[5] != None and (row[5] in nonnoxList or (row[5][:2] in unktypeList)):
                    nom += 1
                elif row[6] != None and (row[6] in nonnoxList or (row[6][:2] in unktypeList)):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #76 AH_NoxCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] in invasiveSpeciesList:
                    nom += 1
                elif row[2] != None and row[2] in invasiveSpeciesList:
                    nom += 1
                elif row[3] != None and row[3] in invasiveSpeciesList:
                    nom += 1
                elif row[4] != None and row[4] in invasiveSpeciesList:
                    nom += 1
                elif row[5] != None and row[5] in invasiveSpeciesList:
                    nom += 1
                elif row[6] != None and row[6] in invasiveSpeciesList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #77 AH_TotalLitterCover
        nom = 0
        fields = [pkField, "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "SoilSurface"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] in litterList or (row[1] == None and [6] in soilslitterList):
                    nom += 1
                elif row[2] != None and row[2] in litterList or (row[2] == None and row[6] in soilslitterList):
                    nom += 1
                elif row[3] != None and row[3] in litterList or (row[3] == None and row[6] in soilslitterList):
                    nom += 1
                elif row[4] != None and row[4] in litterList or (row[4] == None and row[6] in soilslitterList):
                    nom += 1
                elif row[5] != None and row[5] in litterList or (row[5] == None and row[6] in soilslitterList):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #78 AH_HerbLitterCover
        nom = 0
        fields = [pkField, "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] == "HL" or row[1] == "L"):
                    nom += 1
                elif row[2] != None and (row[2] == "HL" or row[2] == "L"):
                    nom += 1
                elif row[3] != None and (row[3] == "HL" or row[3] == "L"):
                    nom += 1
                elif row[4] != None and (row[4] == "HL" or row[4] == "L"):
                    nom += 1
                elif row[5] != None and (row[5] == "HL" or row[5] == "L"):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #79 AH_WoodyLitterCover
        nom = 0
        fields = [pkField, "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] == "WL":
                    nom += 1
                elif row[2] != None and row[2] == "WL":
                    nom += 1
                elif row[3] != None and row[3] == "WL":
                    nom += 1
                elif row[4] != None and row[4] == "WL":
                    nom += 1
                elif row[5] != None and row[5] == "WL":
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #80 FH_TotalLitterCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "SoilSurface"]
        whereClause = "PlotKey = '" + plot + "' and TopCanopy = 'N'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[2] in litterList:
                    nom += 1
                elif row[2] == None and row[3] in litterList:
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] in litterList:
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] == None and row[5] in litterList:
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] == None and row[5] == None and row[6] in soilslitterList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #81 FH_WoodyLitterCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "SoilSurface"]
        whereClause = "PlotKey = '" + plot + "' and TopCanopy = 'N'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[2] == 'WL':
                    nom += 1
                elif row[2] == None and row[3] == 'WL':
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] == 'WL':
                    nom += 1
        ##            if row[2] == None and row[3] == None and row[4] == None and row[5] = 'EL':
        ##                nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #82 FH_HerbLitterCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3"]
        whereClause = "PlotKey = '" + plot + "' and TopCanopy = 'N'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[2] == 'HL' or row[2] == 'L':
                    nom += 1
                elif row[2] == None and (row[3] == 'HL' or row[3] == 'L'):
                    nom += 1
                elif row[2] == None and row[3] == None and (row[4] == 'HL' or row[4] == 'L'):
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #83 FH_DuffCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "SoilSurface"]
        whereClause = "PlotKey = '" + plot + "' and TopCanopy = 'N' and Lower1 IS NULL and Lower2 IS NULL and Lower3 IS NULL and Lower4 IS NULL and Lower5 IS NULL"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[7] == 'D':
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #84 FH_EmbLitterCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "SoilSurface"]
        whereClause = "PlotKey = '" + plot + "' and TopCanopy = 'N' and Lower1 IS NULL and Lower2 IS NULL and Lower3 IS NULL and Lower4 IS NULL and Lower5 IS NULL"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[7] == 'EL':
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #85 FH_VagrLichenCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3"]
        whereClause = "PlotKey = '" + plot + "' and TopCanopy = 'N'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[2] == 'VL':
                    nom += 1
                elif row[2] == None and row[3] == 'VL':
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] == 'VL':
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #86 FH_LichenCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "SoilSurface"]
        whereClause = "PlotKey = '" + plot + "' and TopCanopy = 'N' and Lower1 IS NULL and Lower2 IS NULL and Lower3 IS NULL and Lower4 IS NULL and Lower5 IS NULL"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[7] == 'LC':
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #87 FH_MossCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "SoilSurface"]
        whereClause = "PlotKey = '" + plot + "' and TopCanopy = 'N' and Lower1 IS NULL and Lower2 IS NULL and Lower3 IS NULL and Lower4 IS NULL and Lower5 IS NULL"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[7] == 'M':
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #88 FH_CyanobacteriaCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "SoilSurface"]
        whereClause = "PlotKey = '" + plot + "' and TopCanopy = 'N' and Lower1 IS NULL and Lower2 IS NULL and Lower3 IS NULL and Lower4 IS NULL and Lower5 IS NULL"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[7] == 'CY':
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #89 FH_RockCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "SoilSurface"]
        whereClause = "PlotKey = '" + plot + "' and TopCanopy = 'N'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[2] in rockList:
                    nom += 1
                elif row[2] == None and row[3] in rockList:
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] in rockList:
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] == None and row[5] in rockList:
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] == None and row[5] == None and row[6] in rockList:
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] == None and row[5] == None and row[6] == None and row[7] in rockList:
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        #90 FH_WaterCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "SoilSurface"]
        whereClause = "PlotKey = '" + plot + "' and TopCanopy = 'N'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[2] == 'W':
                    nom += 1
                elif row[2] == None and row[3] == 'W':
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] == 'W':
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] == None and row[5] == 'W':
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] == None and row[5] == None and row[6] == 'W':
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] == None and row[5] == None and row[6] == None and row[7] == 'W':
                    nom += 1

        dataValues.append((float(nom)/totalPoints) * 100)

        #91 FH_DepSoilCover
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3"]
        whereClause = "PlotKey = '" + plot + "' and TopCanopy = 'N'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[2] == 'DS':
                    nom += 1
                elif row[2] == None and row[3] == 'DS':
                    nom += 1
                elif row[2] == None and row[3] == None and row[4] == 'DS':
                    nom += 1
        dataValues.append((float(nom)/totalPoints) * 100)

        # Average Woody Height (cm)
        #92 Hgt_Woody_Avg
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesWoody", "HeightWoody"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                # Only count rows with values - 0 DOES NOT count here?
                if row[2] != None and row[2] != '' and row[2] != 0:
                    nom += int(row[2])
                    dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/dom)

        # Average Herbaceous Height (cm)
        #93 Hgt_Herbaceous_Avg
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesHerbaceous", "HeightHerbaceous", "SpeciesHerbaceous2", "HeightHerbaceous2"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                # Only count rows with values - 0 DOES NOT count here?
                if row[2] != None and row[2] != '' and row[2] != 0:
                    nom += int(row[2])
                    dom += 1
                # need to do lower herb also? TerraDat is not so checking on this
                # Only count rows with values  - does NOT 0 count here??
        ##            if row[4] != None and row[4] != '' and row[4] != 0:
        ##                nom += int(row[4])
        ##                dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))

        #94 Hgt_Forb_Avg
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesHerbaceous", "HeightHerbaceous", "SpeciesHerbaceous2", "HeightHerbaceous2"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                # Only count rows with values - 0 DOES NOT count here?
                if row[2] != None and row[2] != '' and row[2] != 0 and (row[1] in forbList or row[1] == "PFXXXX" or row[1] == "AFXXXX"):
                    nom += int(row[2])
                    dom += 1
                # need to do lower herb also? TerraDat is not so checking on this
                # Only count rows with values  - does NOT 0 count here??
                if row[4] != None and row[4] != '' and row[4] != 0 and row[3] != None and (row[3] in forbList or row[3] == "PFXXXX" or row[3] == "AFXXXX"):
                    nom += int(row[4])
                    dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))


        #95 Hgt_PerenForb_Avg Invasive and not
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesHerbaceous", "HeightHerbaceous", "SpeciesHerbaceous2", "HeightHerbaceous2"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[2] != None and row[2] != '' and row[2] != 0 and ((row[1] in forbList and row[1] in perennialList) or (row[1] == "PFXXXX")):
                    nom += int(row[2])
                    dom += 1
                # need to do lower herb also
                if row[4] != None and row[4] != '' and row[4] != 0 and row[3] != None and ((row[3] in forbList and row[3] in perennialList) or (row[3] == "PFXXXX")):
                    nom += int(row[4])
                    dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))


        #96 Hgt_Grass_Avg
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesHerbaceous", "HeightHerbaceous", "SpeciesHerbaceous2", "HeightHerbaceous2"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                # Only count rows with values - 0 DOES NOT count here?
                if row[2] != None and row[2] != '' and row[2] != 0 and (row[1] in gramList or row[1] == "PGXXXX" or row[1] == "AGXXXX"):
                    nom += int(row[2])
                    dom += 1
                # need to do lower herb also? TerraDat is not so checking on this
                # Only count rows with values  - does NOT 0 count here??
                if row[4] != None and row[4] != '' and row[4] != 0 and row[3] != None and (row[3] in gramList or row[3] == "PGXXXX" or row[3] == "AGXXXX"):
                    nom += int(row[4])
                    dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))


        #97 Hgt_PerenGrass_Avg Invasive and not
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesHerbaceous", "HeightHerbaceous", "SpeciesHerbaceous2", "HeightHerbaceous2"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in gramList and row[1] in perennialList) or (row[1] == "PGXXXX"):
                    # Only count rows with values  - does 0 count here??
                    if row[2] != None and row[2] != '' and row[2] != 0:
                        nom += int(row[2])
                        dom += 1
                # need to do lower herb also
                if row[3] != None and (row[3] in gramList and row[3] in perennialList) or (row[3] == "PGXXXX"):
                    # Only count rows with values  - does NOT 0 count here??
                    if row[4] != None and row[4] != '' and row[4] != 0:
                        nom += int(row[4])
                        dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))

        # Average Perennial Grass Height - Invasive (cm)
        #98 Hgt_NoxPerenGrass_Avg
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesHerbaceous", "HeightHerbaceous", "SpeciesHerbaceous2", "HeightHerbaceous2"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in gramList and row[1] in perennialList and row[1] in invasiveSpeciesList):
                    # Only count rows with values  - does 0 count here??
                    if row[2] != None and row[2] != '' and row[2] != 0:
                        nom += int(row[2])
                        dom += 1
                # need to do lower herb also
                if row[3] != None and (row[3] in gramList and row[3] in perennialList and row[3] in invasiveSpeciesList):
                    # Only count rows with values  - does NOT 0 count here??
                    if row[4] != None and row[4] != '' and row[4] != 0:
                        nom += int(row[4])
                        dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))

        # Average NonInv Perennial Grass Height (cm)
        #99 Hgt_NonNoxPerenGrass_Avg
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesHerbaceous", "HeightHerbaceous", "SpeciesHerbaceous2", "HeightHerbaceous2"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and ((row[1] in gramList and row[1] in perennialList and not row[1] in invasiveSpeciesList) or (row[1] == "PGXXXX")):
                    # Only count rows with values  - does 0 count here??
                    if row[2] != None and row[2] != '' and row[2] != 0:
                        nom += int(row[2])
                        dom += 1
                # need to do lower herb also
                if row[3] != None and ((row[3] in gramList and row[3] in perennialList and not row[3] in invasiveSpeciesList) or (row[3] == "PGXXXX")):
                    # Only count rows with values  - does NOT 0 count here??
                    if row[4] != None and row[4] != '' and row[4] != 0:
                        nom += int(row[4])
                        dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))


        #100 Hgt_TallPerenGrass_Avg

        nom = 0
        dom = 0
        fields = [pkField, "SpeciesHerbaceous", "HeightHerbaceous", "SpeciesHerbaceous2", "HeightHerbaceous2"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                # Only count rows with values - 0 DOES NOT count here?
                if row[2] != None and row[2] != '' and row[2] != 0 and row[1] in tallpgrList:
                    nom += int(row[2])
                    dom += 1
                # need to do lower herb also? TerraDat is not so checking on this
                # Only count rows with values  - does NOT 0 count here??
                if row[4] != None and row[4] != '' and row[4] != 0 and row[3] in tallpgrList:
                    nom += int(row[4])
                    dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))

        #101 Hgt_ShortPerenGrass_Avg
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesHerbaceous", "HeightHerbaceous", "SpeciesHerbaceous2", "HeightHerbaceous2"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                # Only count rows with values - 0 DOES NOT count here?
                if row[2] != None and row[2] != '' and row[2] != 0 and row[1] in shortpgrList:
                    nom += int(row[2])
                    dom += 1
                # need to do lower herb also? TerraDat is not so checking on this
                # Only count rows with values  - does NOT 0 count here??
                if row[4] != None and row[4] != '' and row[4] != 0 and row[3] in shortpgrList:
                    nom += int(row[4])
                    dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))


        #102 Hgt_PerenForbGrass_Avg Invasive and not
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesHerbaceous", "HeightHerbaceous", "SpeciesHerbaceous2", "HeightHerbaceous2"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (((row[1] in forbList or row[1] in gramList) and row[1] in perennialList) or (row[1] == "PFXXXX" or (row[1] == "PGXXXX"))):
                    # Only count rows with values  - does 0 count here??
                    if row[2] != None and row[2] != '' and row[2] != 0:
                        nom += int(row[2])
                        dom += 1
        ##            # need to do lower herb also
                if row[3] != None and (((row[3] in forbList or row[3] in gramList) and row[3] in perennialList) or (row[3] == "PFXXXX" or (row[3] == "PGXXXX"))):
        ##                # Only count rows with values  - does NOT 0 count here??
                    if row[4] != None and row[4] != '' and row[4] != 0:
                        nom += int(row[4])
                        dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))


        #103 Hgt_Shrub_Avg
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesWoody", "HeightWoody"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                # Only count rows with values - 0 DOES NOT count here?
                if row[2] != None and row[2] != '' and row[2] != 0 and (row[1] in shrubList or (row[1] == "SHXXXX")):
                    nom += int(row[2])
                    dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))

        # Average Other Shrub Height (cm)
        #104 Hgt_NonSagebrushShrub_Avg
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesWoody", "HeightWoody"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] != '' and ((row[1] in nonsageList)):
                    # Only count rows with values - does 0 count here??
                    if row[2] != None and row[2] != '' and row[2] != 0:
                        nom += int(row[2])
                        dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))

        # Average Sagebrush Height (cm)
        #105 Hgt_Sagebrush_Avg
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesWoody", "HeightWoody"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in sageList:
                    # Only count rows with values - does 0 count here??
                    if row[2] != None and row[2] != '' and row[2] != 0:
                        nom += int(row[2])
                        dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))


        # Average Sagebrush Height (cm) Live only
        #106 Hgt_Sagebrush_Live_Avg
        nom = 0
        dom = 0
        fields = [pkField, "SpeciesWoody", "HeightWoody", "ChkboxWoody"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in sageList:
                    if row[2] != None and row[2] != '' and row[2] != 0 and row[3] == 0:
                        nom += int(row[2])
                        dom += 1
        if dom == 0:
            dataValues.append(0)
        else:
            dataValues.append(float(nom)/float(dom))




        # Count of all Columnar Sagebrush hit on LPI
        #107 SagebrushShape_All_ColumnCount
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot + "' and ShrubShape = 'C'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] in sageList:
                    nom += 1
                elif row[2] != None and row[2] in sageList:
                    nom += 1
                elif row[3] != None and row[3] in sageList:
                    nom += 1
                elif row[4] != None and row[4] in sageList:
                    nom += 1
                elif row[5] != None and row[5] in sageList:
                    nom += 1
                elif row[6] != None and row[6] in sageList:
                    nom += 1
        allcolumn = float(nom)
        dataValues.append(float(nom))

        # Count of all Spreading Sagebrush hit on LPI
        #108 SagebrushShape_All_SpreadCount
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5"]
        whereClause = "PlotKey = '" + plot +  "' and ShrubShape = 'S'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and row[1] in sageList:
                    nom += 1
                elif row[2] != None and row[2] in sageList:
                    nom += 1
                elif row[3] != None and row[3] in sageList:
                    nom += 1
                elif row[4] != None and row[4] in sageList:
                    nom += 1
                elif row[5] != None and row[5] in sageList:
                    nom += 1
                elif row[6] != None and row[6] in sageList:
                    nom += 1
        allspreading = float(nom)
        dataValues.append(float(nom))

        #109 SagebrushShape_All_Predominant
        if allcolumn == 0 and allspreading == 0:
            dataValues.append(None)
        elif allspreading > allcolumn:
            dataValues.append('S')
        elif allcolumn > allspreading:
            dataValues.append('C')
        else:
            dataValues.append('S/C')

        # Count of all Live Columnar Sagebrush hit on LPI
        #110 SagebrushShape_Live_ColumnCount
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "ChkboxTop", "ChkboxLower1", "ChkboxLower2", "ChkboxLower3", "ChkboxLower4", "ChkboxLower5"]
        whereClause = "PlotKey = '" + plot +  "' and ShrubShape = 'C'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in sageList and row[7] == 0):
                    nom += 1
                elif row[2] != None and (row[2] in sageList and row[8] == 0):
                    nom += 1
                elif row[3] != None and (row[3] in sageList and row[9] == 0):
                    nom += 1
                elif row[4] != None and (row[4] in sageList and row[10] == 0):
                    nom += 1
                elif row[5] != None and (row[5] in sageList and row[11] == 0):
                    nom += 1
                elif row[6] != None and (row[6] in sageList and row[12] == 0):
                    nom += 1
        livecolumn = float(nom)
        dataValues.append(float(nom))

        # Count of all Live Spreading Sagebrush hit on LPI
        #111 SagebrushShape_Live_SpreadCount
        nom = 0
        fields = [pkField, "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "ChkboxTop", "ChkboxLower1", "ChkboxLower2", "ChkboxLower3", "ChkboxLower4", "ChkboxLower5"]
        whereClause = "PlotKey = '" + plot +  "' and ShrubShape = 'S'"
        with arcpy.da.SearchCursor(pointLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] != None and (row[1] in sageList and row[7] == 0):
                    nom += 1
                elif row[2] != None and (row[2] in sageList and row[8] == 0):
                    nom += 1
                elif row[3] != None and (row[3] in sageList and row[9] == 0):
                    nom += 1
                elif row[4] != None and (row[4] in sageList and row[10] == 0):
                    nom += 1
                elif row[5] != None and (row[5] in sageList and row[11] == 0):
                    nom += 1
                elif row[6] != None and (row[6] in sageList and row[12] == 0):
                    nom += 1
        livespreading = float(nom)
        dataValues.append(float(nom))


        #112 SagebrushShape_Live_Predominant
        if livecolumn == 0 and livespreading == 0:
            dataValues.append(None)
        elif livespreading > livecolumn:
            dataValues.append('S')
        elif livecolumn > livespreading:
            dataValues.append('C')
        else:
            dataValues.append('SC')

        #113 NumSpp_NoxPlant
        nom = 0
        Sppcheck = []
        fields = [pkField, "SpeciesList"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(speciesdetailLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in invasiveSpeciesList and not row[1] in Sppcheck:
                    nom += 1
                    Sppcheck.append(row[1])
        if nom == 0:
            dataValues.append(0)
        else:
            dataValues.append(nom)

        #114 Spp_Nox
        nom = ''
        Sppcheck = []
        fields = [pkField, "SpeciesList"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(speciesdetailLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in invasiveSpeciesList and not row[1] in Sppcheck:
                    nom += str(row[1]) + "; "
                    Sppcheck.append(row[1])
        if nom == '':
            dataValues.append(None)
        else:
            dataValues.append(nom[:-2])

        #115 NumSpp_NonNoxPlant
        nom = 0
        Sppcheck = []
        fields = [pkField, "SpeciesList"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(speciesdetailLayer, fields, whereClause) as cursor:
            for row in cursor:
                # some blanks not sure why
                if row[1]:
                    if row[1] in nonnoxList or row[1][:2] in unktypeList and not row[1] in Sppcheck:
                        nom += 1
                        Sppcheck.append(row[1])
        if nom == 0:
            dataValues.append(0)
        else:
            dataValues.append(nom)


        #116 NumSpp_PreferredForb
        nom = 0
        Sppcheck = []
        fields = [pkField, "SpeciesList"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(speciesdetailLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in prefforbList and not row[1] in Sppcheck:
                    nom += 1
                    Sppcheck.append(row[1])
        if nom == 0:
            dataValues.append(0)
        else:
            dataValues.append(nom)

        #117 Spp_Sagebrush
        nom = ''
        Sppcheck = []
        fields = [pkField, "SpeciesList"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(speciesdetailLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in sageList and not row[1] in Sppcheck:
                    nom += str(row[1]) + "; "
                    Sppcheck.append(row[1])
        if nom == '':
            dataValues.append(None)
        else:
            dataValues.append(nom[:-2])


        #118 Spp_PreferredForb
        nom = ''
        Sppcheck = []
        fields = [pkField, "SpeciesList"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(speciesdetailLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in prefforbList and not row[1] in Sppcheck:
                    nom += str(row[1]) + "; "
                    Sppcheck.append(row[1])
        if nom == '':
            dataValues.append(None)
        else:
            dataValues.append(nom[:-2])

        #119 Spp_TallPerenGrass
        nom = ''
        Sppcheck = []
        fields = [pkField, "SpeciesList"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(speciesdetailLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in tallpgrList and not row[1] in Sppcheck:
                    nom += str(row[1]) + "; "
                    Sppcheck.append(row[1])
        if nom == '':
            dataValues.append(None)
        else:
            dataValues.append(nom[:-2])

        #120
        nom = ''
        Sppcheck = []
        fields = [pkField, "SpeciesList"]
        whereClause = "PlotKey = '" + plot + "'"
        with arcpy.da.SearchCursor(speciesdetailLayer, fields, whereClause) as cursor:
            for row in cursor:
                if row[1] in shortpgrList and not row[1] in Sppcheck:
                    nom += str(row[1]) + "; "
                    Sppcheck.append(row[1])
        if nom == '':
            dataValues.append(None)
        else:
            dataValues.append(nom[:-2])


        #print(dataValues)
        allDataValues.append(dataValues)

        dataValues = []  # reset


    #write to csv for now - did this all at the bottom in case it changes
    print("Creating the CSV")
    if os.path.exists(outcsv):
        print("Found exsisting CSV deleting")
        os.remove(outcsv)
    with open(outcsv, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)

        # write out field names to first row
        writer.writerow(insertFields)

        for eachData in allDataValues:
            writer.writerow(eachData)


    # close the csv
    csvfile.close()
    arcpy.ClearWorkspaceCache_management()

# Get All field Values that are not Calcs to Join to CalcsCSV.  Had to split this out of the original script because of memeory errors.  Used python api instead.

fcs_dict = {
            "CA":'e3f97c3f04f24d38ba79bc84e9e6a4ff',
            }


for key in fcs_dict:
    adminstate = key
    item_id = fcs_dict[key]
    print("Starting run for: " + adminstate)

    RunCalcs(adminstate,item_id)


path = r'C:\TerrestrialCalcs\TE_Calcs_CSVs'
os.chdir(path)
print(os.getcwd())

files = glob.glob("InSeasonCalcs_*.csv")
# merging the files
result = pd.concat([pd.read_csv(f,skiprows = 1, names= insertFields) for f in files], ignore_index=True)


result.to_csv(r'C:\TerrestrialCalcs\TE_Calcs_CSVs\InSeasonCalcsAllStates.csv')

# Get All field Values that are not Calcs to Join to CalcsCSV.  Had to split this out of the original script because of memeory errors.  Used python api instead.

# CHECK FOR FOLDER THAT CSV ERROR FILES WILL BE PLACED

path = r'C:\TerrestrialCalcs'
os.chdir(path)
if not os.path.exists("TE_PlotInfo_CSVs"):
    os.mkdir("TE_PlotInfo_CSVs")
    os.chdir(os.path.join(path,"TE_PlotInfo_CSVs"))
    print("Creating TE_PlotInfo_CSVs Folder...")
else:
    os.chdir(os.path.join(path,"TE_PlotInfo_CSVs"))
    pattern = '*.csv'
    files = glob.glob(pattern)
    for file in files:
        os.remove(file)

fcs_dict = {"CA":'d4e640e42f4842009901fb9e178c1f9c',
            }

for key in fcs_dict:
    adminstate = key
    item_id = fcs_dict[key]
    print("Starting run for: " + adminstate)

    eval_plots_out_fields = ['PlotKey','PlotID','State','AdminState','County','ProjectName','Stratum']
    plotchar_out_fields = ['PlotKey','Ecolsite','ESD_Label']
    plotobs_out_fields = ["PlotKey", "Northing","Easting", "EstablishDate", "DateModified"]

    # Get Plots Layer
    item = gis.content.get(item_id)
    plots_sdf = item.layers[0].query(where="EvalStatus = 'Eval'",out_fields=eval_plots_out_fields).sdf
    plots_sdf.drop(columns=['OBJECTID','SHAPE'], inplace=True)
    plots_sdf


    # Get PlotChar Layer
    item = gis.content.get(item_id)
    plotchar_sdf = item.layers[5].query(out_fields=plotchar_out_fields).sdf
    plotchar_sdf.drop(columns=['objectid','SHAPE'], inplace=True)
    plotchar_sdf

    # Get PlotObs Layer
    item = gis.content.get(item_id)
    plotobs_sdf = item.layers[6].query(out_fields=plotobs_out_fields).sdf
    plotobs_sdf.drop(columns=['objectid','SHAPE'], inplace=True)
    plotobs_sdf

    # Merge Plots Layer and PlotChar on PlotKey- Add PhotoLink
    plotkey_merge_df = pd.merge(plots_sdf, plotchar_sdf, on = ['PlotKey'], how="outer",indicator=True)
    df = plotkey_merge_df.astype({"_merge":"object"})
    plotkey_base_df = df[df['_merge'].isin(["both","left_only"])]
    plotkey_base_df.drop(columns=['_merge'], inplace=True)
    plotkey_base_df1 = plotkey_base_df.rename(columns={'AdminState':'SpeciesState','Ecolsite':'EcologicalSiteId','ESD_Label':'EcolSiteName'})
    #display(plotkey_base_df1)


    # Merge Plots Layer and PlotObs on PlotKey- Add PhotoLink
    plotkey_merge_df1 = pd.merge(plotkey_base_df1, plotobs_sdf, on = ['PlotKey'], how="outer",indicator=True)
    df2 = plotkey_merge_df1.astype({"_merge":"object"})
    df3 = df2[df2['_merge'].isin(["both","left_only"])]
    df4 = df3.rename(columns={'Northing':'Latitude_NAD83','Easting':'Longitude_NAD83','EstablishDate':'DateEstablished','DateModified':'DateVisited'})
    #display(merge_calcs1)

    # Create the dictionary
    event_dictionary ={'left_only' : 'PlotChar Not Found'}

    # Add a new column nam
    df4['PhotoLink'] = df4['_merge'].map(event_dictionary)
    df5 = df4[['PlotKey','PlotID','State','SpeciesState','County','ProjectName','Stratum','PhotoLink','EcologicalSiteId','EcolSiteName','Latitude_NAD83','Longitude_NAD83',"DateEstablished", "DateVisited"]]
    display(df5)

    outdirfinal = r'C:\TerrestrialCalcs\TE_PlotInfo_CSVs'
    outcsvname_final = 'InSeasonCalcs_' + adminstate + '.csv'
    result = os.path.join(outdirfinal,outcsvname_final)
    df5.to_csv(result)

outfields = ['PlotKey','PlotID','State','SpeciesState','County','ProjectName','Stratum','PhotoLink','EcologicalSiteId','EcolSiteName','Latitude_NAD83','Longitude_NAD83',"DateEstablished", "DateVisited"]
path = r'C:\TerrestrialCalcs\TE_PlotInfo_CSVs'
os.chdir(path)
print(os.getcwd())
files = glob.glob("InSeasonCalcs_*.csv")
# merging the files

result = pd.concat([pd.read_csv(f,skiprows = 1, names= outfields) for f in files], ignore_index=True)
result['DateEstablished'] = pd.to_datetime(result['DateEstablished']).dt.normalize()
result['DateVisited'] = pd.to_datetime(result['DateVisited']).dt.normalize()

result.to_csv(r'C:\TerrestrialCalcs\TE_PlotInfo_CSVs\AllStatesOtherFields.csv')


###########################################################################################################################

# Combine the Calcs CSV with the Plotkey etc fields
path = r'C:\TerrestrialCalcs'
os.chdir(path)
if not os.path.exists("TE_Final_AllStates_CSV"):
    os.mkdir("TE_Final_AllStates_CSV")
    os.chdir(os.path.join(path,"TE_Final_AllStates_CSV"))
    print("Creating TE_Final_AllStates_CSV Folder...")
else:
    os.chdir(os.path.join(path,"TE_Final_AllStates_CSV"))
    pattern = '*.csv'
    files = glob.glob(pattern)
    for file in files:
        os.remove(file)


csv_file = 'UpdatedInSeasonCalcsAllStates.csv'
data_path = r'C:\TerrestrialCalcs\TE_Final_AllStates_CSV'
outName = os.path.join(data_path, csv_file)


csv1 = r'C:\TerrestrialCalcs\TE_Calcs_CSVs\InSeasonCalcsAllStates.csv'
csv2 = r'C:\TerrestrialCalcs\TE_PlotInfo_CSVs\AllStatesOtherFields.csv'
df_out1 = pd.read_csv(csv1)
df_out2 = pd.read_csv(csv2)
merge_calcs = pd.merge(df_out1, df_out2, how="left", on="PlotKey")
merge_calcs1 = merge_calcs[['PlotKey',
     'PlotID',
     'State',
     'SpeciesState',
     'County',
     'ProjectName',
     'Stratum',
     'PhotoLink',
     'EcologicalSiteId',
     'EcolSiteName',
     'Latitude_NAD83',
     'Longitude_NAD83',
     'DateEstablished',
     'DateVisited',
     'BareSoilCover',
     'TotalFoliarCover',
     'GapCover_25_50',
     'GapCover_51_100',
     'GapCover_101_200',
     'GapCover_200_plus',
     'GapCover_25_plus',
     'SoilStability_All',
     'SoilStability_Protected',
     'SoilStability_Unprotected',
     'AH_ForbCover',
     'AH_NonNoxPerenForbCover',
     'FH_NonNoxPerenForbCover',
     'AH_NonNoxAnnForbCover',
     'FH_NonNoxAnnForbCover',
     'AH_NonNoxPerenGrassCover',
     'FH_NonNoxPerenGrassCover',
     'AH_GrassCover',
     'AH_AnnGrassCover',
     'AH_NonNoxAnnGrassCover',
     'FH_NonNoxAnnGrassCover',
     'AH_NonNoxAnnForbGrassCover',
     'AH_NonNoxPerenForbGrassCover',
     'AH_NonNoxSucculentCover',
     'FH_NonNoxSucculentCover',
     'AH_NonNoxShrubCover',
     'FH_NonNoxShrubCover',
     'AH_NonNoxSubShrubCover',
     'FH_NonNoxSubShrubCover',
     'AH_NonNoxTreeCover',
     'FH_NonNoxTreeCover',
     'AH_NoxPerenForbCover',
     'FH_NoxPerenForbCover',
     'AH_NoxAnnForbCover',
     'FH_NoxAnnForbCover',
     'AH_NoxPerenGrassCover',
     'FH_NoxPerenGrassCover',
     'AH_NoxAnnGrassCover',
     'FH_NoxAnnGrassCover',
     'AH_NoxAnnForbGrassCover',
     'AH_NoxPerenForbGrassCover',
     'AH_NoxSucculentCover',
     'FH_NoxSucculentCover',
     'AH_NoxShrubCover',
     'FH_NoxShrubCover',
     'AH_NoxSubShrubCover',
     'FH_NoxSubShrubCover',
     'AH_NoxTreeCover',
     'FH_NoxTreeCover',
     'AH_PerenForbGrassCover',
     'AH_PerenForbCover',
     'AH_PerenGrassCover',
     'AH_PreferredForbCover',
     'AH_ShortPerenGrassCover',
     'AH_TallPerenGrassCover',
     'AH_SagebrushCover',
     'FH_SagebrushCover',
     'AH_SagebrushCover_Live',
     'AH_NonSagebrushShrubCover',
     'AH_ShrubCover',
     'AH_NonNoxCover',
     'AH_NoxCover',
     'AH_TotalLitterCover',
     'AH_HerbLitterCover',
     'AH_WoodyLitterCover',
     'FH_TotalLitterCover',
     'FH_WoodyLitterCover',
     'FH_HerbLitterCover',
     'FH_DuffCover',
     'FH_EmbLitterCover',
     'FH_VagrLichenCover',
     'FH_LichenCover',
     'FH_MossCover',
     'FH_CyanobacteriaCover',
     'FH_RockCover',
     'FH_WaterCover',
     'FH_DepSoilCover',
     'Hgt_Woody_Avg',
     'Hgt_Herbaceous_Avg',
     'Hgt_Forb_Avg',
     'Hgt_PerenForb_Avg',
     'Hgt_Grass_Avg',
     'Hgt_PerenGrass_Avg',
     'Hgt_NoxPerenGrass_Avg',
     'Hgt_NonNoxPerenGrass_Avg',
     'Hgt_TallPerenGrass_Avg',
     'Hgt_ShortPerenGrass_Avg',
     'Hgt_PerenForbGrass_Avg',
     'Hgt_Shrub_Avg',
     'Hgt_NonSagebrushShrub_Avg',
     'Hgt_Sagebrush_Avg',
     'Hgt_Sagebrush_Live_Avg',
     'SagebrushShape_All_ColumnCount',
     'SagebrushShape_All_SpreadCount',
     'SagebrushShape_All_Predominant',
     'SagebrushShape_Live_ColumnCount',
     'SagebrushShape_Live_SpreadCount',
     'SagebrushShape_Live_Predominant',
     'NumSpp_NoxPlant',
     'Spp_Nox',
     'NumSpp_NonNoxPlant',
     'NumSpp_PreferredForb',
     'Spp_Sagebrush',
     'Spp_PreferredForb',
     'Spp_TallPerenGrass',
     'Spp_ShortPerenGrass']]

merge_calcs2 = merge_calcs1.astype({"PhotoLink":"object"})
merge_calcs2.to_csv(outName)

# Get the PlotObs Geometry
path = r'C:\TerrestrialCalcs'
os.chdir(path)
if not os.path.exists("TE_Geometry_filegdb"):
    os.mkdir("TE_Geometry_filegdb")
    os.chdir(os.path.join(path,"TE_Geometry_filegdb"))
    print("Creating TE_Geometry_filegdb Folder...")
else:
    os.chdir(os.path.join(path,"TE_Geometry_filegdb"))
    filegdb = r'C:\TerrestrialCalcs\TE_Geometry_filegdb\PlotObsGeometry.gdb'
    arcpy.Delete_management(filegdb)


arcpy.management.CreateFileGDB(r'C:\TerrestrialCalcs\TE_Geometry_filegdb', 'PlotObsGeometry')


fcs_dict = {"CA":'d4e640e42f4842009901fb9e178c1f9c',
            }

for key in fcs_dict:
    adminstate = key
    item_id = fcs_dict[key]
    print("Starting run for: " + adminstate)

    # Out Filegdb
    filegdb = r'C:\TerrestrialCalcs\TE_Geometry_filegdb\PlotObsGeometry.gdb'
    outname = 'InSeasonCalcs' + adminstate
    geom_output = os.path.join(filegdb,outname)

    plotobs_out_fields = ['PlotKey']
    # Get PlotObs Layer
    item = gis.content.get(item_id)
    plotobs_lyr = item.layers[6].query(out_fields=plotobs_out_fields)
    plotobs_sdf1 = plotobs_lyr.sdf

    plotobs_sdf1.spatial.to_featureclass(geom_output)

# Set workspace
arcpy.env.workspace = r'C:\TerrestrialCalcs\TE_Geometry_filegdb\PlotObsGeometry.gdb'

# Add xy fields
for fc in arcpy.ListFeatureClasses():
    arcpy.AddXY_management(fc)

fclist = ['InSeasonCalcsAZ','InSeasonCalcsCA','InSeasonCalcsCO','InSeasonCalcsID','InSeasonCalcsMT','InSeasonCalcsNMN','InSeasonCalcsNV','InSeasonCalcsOR','InSeasonCalcsUT','InSeasonCalcsWY']
arcpy.management.Merge(fclist, "allplotobs")

# Combining CSV with Plotobs Layer Geometry
allstatescalcs_csv = r'C:\TerrestrialCalcs\TE_Final_AllStates_CSV\UpdatedInSeasonCalcsAllStates.csv'
allstatesgeomfc = r'C:\TerrestrialCalcs\TE_Geometry_filegdb\PlotObsGeometry.gdb\allplotobs'


fcls_whr = pd.DataFrame.spatial.from_featureclass(location=allstatesgeomfc,where_clause="plot_key IS NOT NULL")
fcls_whr1 = fcls_whr.rename(columns={'plot_key':'PlotKey'})
fcls_whr2 = fcls_whr1[['PlotKey','POINT_X','POINT_Y']]

calcs_df = pd.read_csv(allstatescalcs_csv)
result_df = pd.merge(calcs_df,fcls_whr2, on = ['PlotKey'], how="inner", indicator=True)

result_df1 = result_df[['PlotKey',
     'PlotID',
     'State',
     'SpeciesState',
     'County',
     'ProjectName',
     'Stratum',
     'PhotoLink',
     'EcologicalSiteId',
     'EcolSiteName',
     'Latitude_NAD83',
     'Longitude_NAD83',
     'DateEstablished',
     'DateVisited',
     'BareSoilCover',
     'TotalFoliarCover',
     'GapCover_25_50',
     'GapCover_51_100',
     'GapCover_101_200',
     'GapCover_200_plus',
     'GapCover_25_plus',
     'SoilStability_All',
     'SoilStability_Protected',
     'SoilStability_Unprotected',
     'AH_ForbCover',
     'AH_NonNoxPerenForbCover',
     'FH_NonNoxPerenForbCover',
     'AH_NonNoxAnnForbCover',
     'FH_NonNoxAnnForbCover',
     'AH_NonNoxPerenGrassCover',
     'FH_NonNoxPerenGrassCover',
     'AH_GrassCover',
     'AH_AnnGrassCover',
     'AH_NonNoxAnnGrassCover',
     'FH_NonNoxAnnGrassCover',
     'AH_NonNoxAnnForbGrassCover',
     'AH_NonNoxPerenForbGrassCover',
     'AH_NonNoxSucculentCover',
     'FH_NonNoxSucculentCover',
     'AH_NonNoxShrubCover',
     'FH_NonNoxShrubCover',
     'AH_NonNoxSubShrubCover',
     'FH_NonNoxSubShrubCover',
     'AH_NonNoxTreeCover',
     'FH_NonNoxTreeCover',
     'AH_NoxPerenForbCover',
     'FH_NoxPerenForbCover',
     'AH_NoxAnnForbCover',
     'FH_NoxAnnForbCover',
     'AH_NoxPerenGrassCover',
     'FH_NoxPerenGrassCover',
     'AH_NoxAnnGrassCover',
     'FH_NoxAnnGrassCover',
     'AH_NoxAnnForbGrassCover',
     'AH_NoxPerenForbGrassCover',
     'AH_NoxSucculentCover',
     'FH_NoxSucculentCover',
     'AH_NoxShrubCover',
     'FH_NoxShrubCover',
     'AH_NoxSubShrubCover',
     'FH_NoxSubShrubCover',
     'AH_NoxTreeCover',
     'FH_NoxTreeCover',
     'AH_PerenForbGrassCover',
     'AH_PerenForbCover',
     'AH_PerenGrassCover',
     'AH_PreferredForbCover',
     'AH_ShortPerenGrassCover',
     'AH_TallPerenGrassCover',
     'AH_SagebrushCover',
     'FH_SagebrushCover',
     'AH_SagebrushCover_Live',
     'AH_NonSagebrushShrubCover',
     'AH_ShrubCover',
     'AH_NonNoxCover',
     'AH_NoxCover',
     'AH_TotalLitterCover',
     'AH_HerbLitterCover',
     'AH_WoodyLitterCover',
     'FH_TotalLitterCover',
     'FH_WoodyLitterCover',
     'FH_HerbLitterCover',
     'FH_DuffCover',
     'FH_EmbLitterCover',
     'FH_VagrLichenCover',
     'FH_LichenCover',
     'FH_MossCover',
     'FH_CyanobacteriaCover',
     'FH_RockCover',
     'FH_WaterCover',
     'FH_DepSoilCover',
     'Hgt_Woody_Avg',
     'Hgt_Herbaceous_Avg',
     'Hgt_Forb_Avg',
     'Hgt_PerenForb_Avg',
     'Hgt_Grass_Avg',
     'Hgt_PerenGrass_Avg',
     'Hgt_NoxPerenGrass_Avg',
     'Hgt_NonNoxPerenGrass_Avg',
     'Hgt_TallPerenGrass_Avg',
     'Hgt_ShortPerenGrass_Avg',
     'Hgt_PerenForbGrass_Avg',
     'Hgt_Shrub_Avg',
     'Hgt_NonSagebrushShrub_Avg',
     'Hgt_Sagebrush_Avg',
     'Hgt_Sagebrush_Live_Avg',
     'SagebrushShape_All_ColumnCount',
     'SagebrushShape_All_SpreadCount',
     'SagebrushShape_All_Predominant',
     'SagebrushShape_Live_ColumnCount',
     'SagebrushShape_Live_SpreadCount',
     'SagebrushShape_Live_Predominant',
     'NumSpp_NoxPlant',
     'Spp_Nox',
     'NumSpp_NonNoxPlant',
     'NumSpp_PreferredForb',
     'Spp_Sagebrush',
     'Spp_PreferredForb',
     'Spp_TallPerenGrass',
     'Spp_ShortPerenGrass',
     'POINT_X',
     'POINT_Y']]

calcs_sedf = pd.DataFrame.spatial.from_xy(df=result_df1, x_column='POINT_X', y_column='POINT_Y', sr=3857)


# Get the PlotObs Geometry
path = r'C:\TerrestrialCalcs'
os.chdir(path)
if not os.path.exists("TE_FinalCalc_filegdb"):
    os.mkdir("TE_FinalCalc_filegdb")
    os.chdir(os.path.join(path,"TE_FinalCalc_filegdb"))
    print("Creating TE_FinalCalc_filegdb Folder...")
else:
    os.chdir(os.path.join(path,"TE_FinalCalc_filegdb"))
    filegdb = r'C:\TerrestrialCalcs\TE_FinalCalc_filegdb\FinalCalcsFC.gdb'
    arcpy.Delete_management(filegdb)


arcpy.management.CreateFileGDB(r'C:\TerrestrialCalcs\TE_FinalCalc_filegdb', 'FinalCalcsFC')

calcs_sedf.spatial.to_featureclass(r'C:\TerrestrialCalcs\TE_FinalCalc_filegdb\FinalCalcsFC.gdb\FinalCalcFC', sanitize_columns=False )

import zipfile
gdb = r'C:\TerrestrialCalcs\TE_FinalCalc_filegdb\FinalCalcsFC.gdb'
zipName = "TerrestrialIndicatorCalcs"
zipLoc = r'C:\TerrestrialCalcs\TE_FinalCalc_filegdb'
zipFull = os.path.join(zipLoc,zipName + ".zip")

writeZip = zipfile.ZipFile(zipFull,'w')
walk = os.walk(zipLoc)

for dirpath,dirnames,filenames in walk:
    if dirpath == gdb:
        for filename in filenames:
            if filename[-5:] != ".lock":
                writeZip.write(os.path.join(gdb,filename), arcname = os.path.join(os.path.basename(gdb),filename))
writeZip.close()


# Hosted Feature Service Layer Itemid
itemid = 'b6adc17ca22648d2a9c8eac5b07df0e9'

dataitem = gis.content.get(itemid)
flayercol = FeatureLayerCollection.fromitem(dataitem)
flayercol.manager.overwrite(zipFull)

# # Prep flayer properties
# fmNewProperties = {"title":"BLM_Natl_AIM_Terrestrial_2023_InSeasonCalcs",
#                    "type":"File Geodatabase",
#                    "tags":"Terrestrial Calcs",
#                    "snippet":"Mid Season Indicator Calcs",
#                    "description":"Mid Season Terrestrial Indicator Calcs used for QAQC"
#                    }

# zipFull = r'C:\TerrestrialCalcs\TE_FinalCalc_filegdb\TerrestrialIndicatorCalcs.zip'

# fmNewGdb = gis.content.add(item_properties=fmNewProperties, data=zipFull)
# calcsFL = fmNewGdb.publish()
# #calcsFL

# insertFields =  {
#  'PlotKey':['PlotKey','STRING'],
#  'PlotID':['PlotID','STRING'],
#  'State':['State','STRING'],
#  'SpeciesState':['SpeciesState','STRING'],
#  'County':['County','STRING'],
#  'ProjectName':['ProjectName','STRING'],
#  'PhotoLink':['PhotoLink','STRING'],
#  'EcologicalSiteId':['EcologicalSiteId','STRING'],
#  'EcolSiteName':['EcolSiteName','STRING'],
#  'DateEstablished':['DateEstablished','STRING'],
#  'DateVisited':['DateVisited','STRING'],
#  'BareSoilCover':['BareSoilCover','FLOAT'],
#  'TotalFoliarCover':['TotalFoliarCover','FLOAT'],
#  'GapCover_25_50':['GapCover_25_50','FLOAT'],
#  'GapCover_51_100':['GapCover_51_100','FLOAT'],
#  'GapCover_101_200':['GapCover_101_200','FLOAT'],
#  'GapCover_200_plus':['GapCover_200_plus','FLOAT'],
#  'GapCover_25_plus':['GapCover_25_plus','FLOAT'],
#  'SoilStability_All':['SoilStability_All','FLOAT'],
#  'SoilStability_Protected':['SoilStability_Protected','FLOAT'],
#  'SoilStability_Unprotected':['SoilStability_Unprotected','FLOAT'],
#  'AH_ForbCover':['AH_ForbCover','FLOAT'],
#  'AH_NonNoxPerenForbCover':['AH_NonNoxPerenForbCover','FLOAT'],
#  'FH_NonNoxPerenForbCover':['FH_NonNoxPerenForbCover','FLOAT'],
#  'AH_NonNoxAnnForbCover':['AH_NonNoxAnnForbCover','FLOAT'],
#  'FH_NonNoxAnnForbCover':['FH_NonNoxAnnForbCover','FLOAT'],
#  'AH_NonNoxPerenGrassCover':['AH_NonNoxPerenGrassCover','FLOAT'],
#  'FH_NonNoxPerenGrassCover':['FH_NonNoxPerenGrassCover','FLOAT'],
#  'AH_GrassCover':['AH_GrassCover','FLOAT'],
#  'AH_AnnGrassCover':['AH_AnnGrassCover','FLOAT'],
#  'AH_NonNoxAnnGrassCover':['AH_NonNoxAnnGrassCover','FLOAT'],
#  'FH_NonNoxAnnGrassCover':['FH_NonNoxAnnGrassCover','FLOAT'],
#  'AH_NonNoxAnnForbGrassCover':['AH_NonNoxAnnForbGrassCover','FLOAT'],
#  'AH_NonNoxPerenForbGrassCover':['AH_NonNoxPerenForbGrassCover','FLOAT'],
#  'AH_NonNoxSucculentCover':['AH_NonNoxSucculentCover','FLOAT'],
#  'FH_NonNoxSucculentCover':['FH_NonNoxSucculentCover','FLOAT'],
#  'AH_NonNoxShrubCover':['AH_NonNoxShrubCover','FLOAT'],
#  'FH_NonNoxShrubCover':['FH_NonNoxShrubCover','FLOAT'],
#  'AH_NonNoxSubShrubCover':['AH_NonNoxSubShrubCover','FLOAT'],
#  'FH_NonNoxSubShrubCover':['FH_NonNoxSubShrubCover','FLOAT'],
#  'AH_NonNoxTreeCover':['AH_NonNoxTreeCover','FLOAT'],
#  'FH_NonNoxTreeCover':['FH_NonNoxTreeCover','FLOAT'],
#  'AH_NoxPerenForbCover':['AH_NoxPerenForbCover','FLOAT'],
#  'FH_NoxPerenForbCover':['FH_NoxPerenForbCover','FLOAT'],
#  'AH_NoxAnnForbCover':['AH_NoxAnnForbCover','FLOAT'],
#  'FH_NoxAnnForbCover':['FH_NoxAnnForbCover','FLOAT'],
#  'AH_NoxPerenGrassCover':['AH_NoxPerenGrassCover','FLOAT'],
#  'FH_NoxPerenGrassCover':['FH_NoxPerenGrassCover','FLOAT'],
#  'AH_NoxAnnGrassCover':['AH_NoxAnnGrassCover','FLOAT'],
#  'FH_NoxAnnGrassCover':['FH_NoxAnnGrassCover','FLOAT'],
#  'AH_NoxAnnForbGrassCover':['AH_NoxAnnForbGrassCover','FLOAT'],
#  'AH_NoxPerenForbGrassCover':['AH_NoxPerenForbGrassCover','FLOAT'],
#  'AH_NoxSucculentCover':['AH_NoxSucculentCover','FLOAT'],
#  'FH_NoxSucculentCover':['FH_NoxSucculentCover','FLOAT'],
#  'AH_NoxShrubCover':['AH_NoxShrubCover','FLOAT'],
#  'FH_NoxShrubCover':['FH_NoxShrubCover','FLOAT'],
#  'AH_NoxSubShrubCover':['AH_NoxSubShrubCover','FLOAT'],
#  'FH_NoxSubShrubCover':['FH_NoxSubShrubCover','FLOAT'],
#  'AH_NoxTreeCover':['AH_NoxTreeCover','FLOAT'],
#  'FH_NoxTreeCover':['FH_NoxTreeCover','FLOAT'],
#  'AH_PerenForbGrassCover':['AH_PerenForbGrassCover','FLOAT'],
#  'AH_PerenForbCover':['AH_PerenForbCover','FLOAT'],
#  'AH_PerenGrassCover':['AH_PerenGrassCover','FLOAT'],
#  'AH_PreferredForbCover':['AH_PreferredForbCover','FLOAT'],
#  'AH_ShortPerenGrassCover':['AH_ShortPerenGrassCover','FLOAT'],
#  'AH_TallPerenGrassCover':['AH_TallPerenGrassCover','FLOAT'],
#  'AH_SagebrushCover':['AH_SagebrushCover','FLOAT'],
#  'FH_SagebrushCover':['FH_SagebrushCover','FLOAT'],
#  'AH_SagebrushCover_Live':['AH_SagebrushCover_Live','FLOAT'],
#  'AH_NonSagebrushShrubCover':['AH_NonSagebrushShrubCover','FLOAT'],
#  'AH_ShrubCover':['AH_ShrubCover','FLOAT'],
#  'AH_NonNoxCover':['AH_NonNoxCover','FLOAT'],
#  'AH_NoxCover':['AH_NoxCover','FLOAT'],
#  'AH_TotalLitterCover':['AH_TotalLitterCover','FLOAT'],
#  'AH_HerbLitterCover':['AH_HerbLitterCover','FLOAT'],
#  'AH_WoodyLitterCover':['AH_WoodyLitterCover','FLOAT'],
#  'FH_TotalLitterCover':['FH_TotalLitterCover','FLOAT'],
#  'FH_WoodyLitterCover':['FH_WoodyLitterCover','FLOAT'],
#  'FH_HerbLitterCover':['FH_HerbLitterCover','FLOAT'],
#  'FH_DuffCover':['FH_DuffCover','FLOAT'],
#  'FH_EmbLitterCover':['FH_EmbLitterCover','FLOAT'],
#  'FH_VagrLichenCover':['FH_VagrLichenCover','FLOAT'],
#  'FH_LichenCover':['FH_LichenCover','FLOAT'],
#  'FH_MossCover':['FH_MossCover','FLOAT'],
#  'FH_CyanobacteriaCover':['FH_CyanobacteriaCover','FLOAT'],
#  'FH_RockCover':['FH_RockCover','FLOAT'],
#  'FH_WaterCover':['FH_WaterCover','FLOAT'],
#  'FH_DepSoilCover':['FH_DepSoilCover','FLOAT'],
#  'Hgt_Woody_Avg':['Hgt_Woody_Avg','FLOAT'],
#  'Hgt_Herbaceous_Avg':['Hgt_Herbaceous_Avg','FLOAT'],
#  'Hgt_Forb_Avg':['Hgt_Forb_Avg','FLOAT'],
#  'Hgt_PerenForb_Avg':['Hgt_PerenForb_Avg','FLOAT'],
#  'Hgt_Grass_Avg':['Hgt_Grass_Avg','FLOAT'],
#  'Hgt_PerenGrass_Avg':['Hgt_PerenGrass_Avg','FLOAT'],
#  'Hgt_NoxPerenGrass_Avg':['Hgt_NoxPerenGrass_Avg','FLOAT'],
#  'Hgt_NonNoxPerenGrass_Avg':['Hgt_NonNoxPerenGrass_Avg','FLOAT'],
#  'Hgt_TallPerenGrass_Avg':['Hgt_TallPerenGrass_Avg','FLOAT'],
#  'Hgt_ShortPerenGrass_Avg':['Hgt_ShortPerenGrass_Avg','FLOAT'],
#  'Hgt_PerenForbGrass_Avg':['Hgt_PerenForbGrass_Avg','FLOAT'],
#  'Hgt_Shrub_Avg':['Hgt_Shrub_Avg','FLOAT'],
#  'Hgt_NonSagebrushShrub_Avg':['Hgt_NonSagebrushShrub_Avg','FLOAT'],
#  'Hgt_Sagebrush_Avg':['Hgt_Sagebrush_Avg','FLOAT'],
#  'Hgt_Sagebrush_Live_Avg':['Hgt_Sagebrush_Live_Avg','FLOAT'],
#  'SagebrushShape_All_ColumnCount':['SagebrushShape_All_ColumnCount','FLOAT'],
#  'SagebrushShape_All_SpreadCount':['SagebrushShape_All_SpreadCount','FLOAT'],
#  'SagebrushShape_All_Predominant':['SagebrushShape_All_Predominant','FLOAT'],
#  'SagebrushShape_Live_ColumnCount':['SagebrushShape_Live_ColumnCount','FLOAT'],
#  'SagebrushShape_Live_SpreadCount':['SagebrushShape_Live_SpreadCount','FLOAT'],
#  'SagebrushShape_Live_Predominant':['SagebrushShape_Live_Predominant','FLOAT'],
#  'NumSpp_NoxPlant':['NumSpp_NoxPlant','FLOAT'],
#  'Spp_Nox':['Spp_Nox','FLOAT'],
#  'NumSpp_NonNoxPlant':['NumSpp_NonNoxPlant','FLOAT'],
#  'NumSpp_PreferredForb':['NumSpp_NonNoxPlant','FLOAT'],
#  'Spp_Sagebrush':['Spp_Sagebrush','FLOAT'],
#  'Spp_PreferredForb':['Spp_PreferredForb','FLOAT'],
#  'Spp_TallPerenGrass':['Spp_TallPerenGrass','FLOAT'],
#  'Spp_ShortPerenGrass':['Spp_ShortPerenGrass','FLOAT']
# }
