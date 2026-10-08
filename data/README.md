# Data (not included)

The input file, `hennepin_snap_mfip_tract_reva.csv`, is tract-by-month SNAP/MFIP enrollment data from Hennepin County joined to ACS 5-year estimates. It was provided to the capstone group and is intentionally **not** published in this repository.

Columns the scripts expect:

`GIS_Tract`, `YearMonth`, `SNAP_cases`, `SNAP_people`, `SNAP_children`, `MFIP_cases`, `MFIP_people`, `MFIP_children`, `GEOID`, `NAME`, `hh_125_povertyE`, `hh_125_povertyM`, `p_125_povertyE`, `p_125_povertyM`, plus further ACS demographic estimate and margin-of-error columns.

Census tract and place boundaries come from US Census TIGER/Line.
