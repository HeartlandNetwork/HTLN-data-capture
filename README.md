# HTLN-Data-Capture-Scripts

Data workflows from Survey123 and other field loggers into MS Access and other 
relational databases.

# Notes

20231220

Wetlands VIBI...
Load files with NAs from left outer joins. Columns affected - 
VIBI herb: LocationID;
VIBI woody: LocationID, ScientificName, DiamID;
VIBI big trees:  LocationID, ScientificName

20231011

Taxon code duplicates leading to many-to-many join warning


20231010

BIG tree categories based on diameter classes. End-to-end to test BIG category 
counts


20230925

VIBI_woody end-to-end test following pivot_longer

20230919

Set up and moving ongoing wetlands survey 123 project to this repo. Repo layout is

./protocol_name/src                dev folder

./protocol_name/R                  executable scripts



