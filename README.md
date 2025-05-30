# Central Sands Groundwater – Decision Support Application

## Project Goal

Originally, the goal was to provide a tool that can be used to inform actions for the reduction of groundwater nitrate in central Wisconsin. However, we moved away from trying to predict/quantify nitrate. Instead, we focused primarily on showing contributing zones, flow paths, and transit times.

---

## How to Run the Code

There are several ways to run the application:

- **Connect URL:**  
  The simplest way is to click the URL on the connect server:  
  [https://connect.doit.wisc.edu/CentralSandsGroundwater_ub2yZBKiptcPKk/](https://connect.doit.wisc.edu/CentralSandsGroundwater_ub2yZBKiptcPKk/)

- **Use RStudio to run the Shiny App:**  
  Open either the UI, Server, or Global files in RStudio, and click the “Run App” button in the code editor’s toolbar (as with other Shiny Apps).

- **Use RStudio to run the core code:**  
  Open the Central Sands Nitrate Application file and run the `mainNitrateEstimator` function to run the logic without the Shiny App.

---

## Pseudocode

When the user clicks on the map, the app performs the following steps:

1. Grab the latitude and longitude
2. Draw a buffer zone around the latitude and longitude
3. Find MODPATH model pathlines that intersect with the buffer zone
4. Find the starting point associated with each pathline and find the land cover associated with that point
5. Display relevant information

---

## Data Needed to Run (Dependencies)

Data sets needed for the application to run are in the `Data Sets` directory:

| File | Purpose | Location |
|------|---------|----------|
| MODPATH Path lines | Stores information about the groundwater path lines. This information is split into two files (due to size): | - Data Sets/Particles_Pathlines_May2025/1particle_top_pathlines_0.shp<br>- Data Sets/Particles_Pathlines_May2025/1particle_top_pathlines_1.shp |
| MODPATH Startpoints | The starting points used by the MODPATH model. Also includes WiscLand 2 land cover associated with each starting point. | Data Sets/Particles_Pathlines_May2025/startpoints_with_wiscland.shp |

---

## Other Files

Other files needed for the application to work properly are stored in the `Misc_Shapefiles` and `www` directories:

| File | Purpose | Location |
|------|---------|----------|
| Pathline Boundary | Shows the outline of the model boundary | Misc_Shapefiles/prelim_ff_model_bounds_proposed.shp |
| Pumping Wells | Shows the locations of pumping wells | Misc_Shapefiles/pumping_well_pts.shp |
| Groundwater Diagram | A diagram explaining basic groundwater concepts | www/groundWaterDiagram.png |
| 3D Flowlines | An example of a 3D representation of flowlines | www/test3Dflowlines.html |

---

## Data Ontology and Style Guide

Below is the variable naming/ontology used for this code. While I aimed for consistency, this was implemented partway through development, so there may be some discrepancies.

| Type of Data | Name | Suffix | Example |
|--------------|------|--------|---------|
| Information downloaded from somewhere (shapefile, csv, spreadsheet, etc.) | Data set | DataSet | floDataSet, objDataSet |
| Information subsetted from a Data Set | Subset | Set | floSet, objSet |
| The row index of a given object | Index | Index | floIndex, objIndex |
| The object | ID | ID | floID, objID |

---

## General Information about the MODPATH Model

- Each pathline has one startpoint.
- Some startpoints do not have pathlines. This happens because the model lays out a grid of startpoints without “prevalidating” that it will end up with a pathline. This means the model may attempt a point, but it is invalid.
- Matching up pathlines to startpoints is non-trivial. The logic is in `getFloDataSet`. Pathlines and startpoints are stored in different shapefiles without a proper foreign key. Pathlines are split into two files, and there are no unique identifiers. The column `partidloc_` is used as the foreign key for matching pathlines to startpoints, with an offset depending on whether the startpoint is associated with the first or second pathline file.

---

## Potential Future Work

- Add user input to allow users to select which landcover data they’d like to use.
- Add an exportable table (doable in Shiny) that allows users to export land cover and transit time information about each flowline. A prototype (non-exportable) table exists in the call to `output$flowlineInfoTable` in the server file.
- Add a 3D rendering of the flowpaths.
  - A preliminary test worked on a subset of the full file using a conversion script. The full MODPATH file was not processed due to time constraints.

---

## Miscellaneous

- **Feature Switches:**  
  We initially wanted toggles for certain features (to limit functionality for some users or for A/B testing). In the end, all features remained enabled. Implementing feature switches for user-by-user control would require significant effort, and added complexity to the Shiny code without much benefit for this use case. Still, it was a worthwhile idea to explore.

- **Nitrate Prediction:**  
  The server file’s call to `output$landCoverExplainer` currently outputs nitrate predictions. It might make sense to remove it, as there is no linear regression model loaded for WiscLand data—so only placeholder lower and upper bounds are displayed.

- **Backwards Compatibility with CropScape Data:**  
  The app originally used CropScape for land cover data but switched to WiscLand for better model performance. Some legacy code allows for backwards compatibility with CropScape. Specifically, `runNitrateEstimator()` can be run with the `landCoverCode` argument set to 1. For this to work, you need a file containing the CropScape class names and must update the `getCropScapeClassNames` function accordingly.

---