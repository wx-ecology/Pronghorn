# Pronghorn Analysis folder
created: Apr 2020

## Folders
*Data* stores original raw data what are included in the subfolder analysis
*FenceBehavior_Official* analysis for the BaBA paper, include the BaBA package, data prep, data visualization
*NSDtables* early on when exploring population movement pattern in general
*sample code* movement analysis example R code
*Discard* info but we never know when it will be useful again
*script* the foundation script (script used for the paper are in *FenceBehavior_Official*). More details below.
 
## Script 
+ (Step 1-3) *preprocess*, clean data, add NA lines (where time inserted but location remains NA), create SHP files
+ (Step 4-6) *NSD* calculation and *migration cycle visualization*. Output used for summarizing data conditions in the *visual* folder
+ (Step 7-8) Classifying fence behaviors. One dividual and group processing. 
+ Step 9 Examine whether encounter types has anything to do with migration types
+ (01-04) analysis prototype for BaBA package/paper
+ Discard - script you never know whether will become useful again
