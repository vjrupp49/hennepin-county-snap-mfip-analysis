### Hennepin County SNAP/MFIP Participation Gap Analysis

Group capstone project for DST 490, by **Brandon Bloss, Ini Udomah, and Vincent Rupp**.

**[Read the full report (PDF)](report/IVB_Project_Report.pdf)**

![Tract-level SNAP/MFIP gap rate across Hennepin County, all years 2020-2025 (static screenshot of the interactive map built in `scripts/IVB_Map_1.R`)](images/gap_map_static.png)

#### The question

To what extent do municipal boundaries hide hyper-local SNAP and MFIP participation hotspots, and how has the density of that unmet need changed over time across Hennepin County post-COVID, from 2020 to 2025?

City and county-level averages often suggest stable or improving benefits participation, but those aggregated numbers can mask serious disparities at the census-tract level. This project set out to find the specific tracts where eligible residents are consistently not enrolling, even when the surrounding city looks fine on paper.

#### Approach

- Built a "gap rate" metric for every census tract: (estimated eligible residents at or below 125% of the federal poverty line, minus those actually enrolled in SNAP or MFIP) divided by eligible population.
- Combined Hennepin County's SNAP/MFIP tract-level enrollment data with ACS demographic data (income, race, education, age, employment, housing).
- Trained and compared a decision tree and a random forest classifier (80/20 train/test split) to identify which tracts fall into the highest-gap category and which demographic variables predict that outcome.
- Mapped every tract's gap rate against Hennepin County and municipal boundaries to visually surface hotspots hidden inside otherwise low-gap cities.

#### Results

The random forest model reached 83.3% accuracy, 71.4% sensitivity, and 84.7% specificity identifying high-gap tracts. Poverty rate was the strongest predictor, followed by bachelor's degree attainment, percent Black population, and median income.

Several tracts near the University of Minnesota showed gap rates above 90% despite large eligible populations - for example, tract 38.02 had a 92.7% gap rate (162 enrolled out of 2,234 eligible). More broadly, cities with reassuring city-wide averages still contained individual tracts with much higher gaps: Eden Prairie's city-wide gap rate was 9.5%, but one of its tracts reached 83.9%; Brooklyn Park's city-wide rate was 4.6% against a tract as high as 70%.

The recommendation: Hennepin County should shift toward tract-level, community-based outreach rather than city-level strategy - including targeted campus outreach near the University of Minnesota and closer coordination with rural western Hennepin communities on transportation and application support.

**City-wide averages vs. individual tract gap rates:**

![City-wide average gap rate vs. individual tract gap rates](images/city_vs_tract_gap.png)

**What predicts a high-gap tract:**

![Random forest variable importance](images/variable_importance.png)

#### Files

- `report/IVB_Project_Report.pdf`: the full written report.
- `scripts/IVB_Decision_Tree.R`: decision tree and random forest models.
- `scripts/IVB_Map_1.R`: builds the tract-level gap map (interactive in R; the screenshot above is a static capture of it).
- `scripts/municipality_boxplots.R`: city-level boxplots of how much tract-level gap rates vary inside each city.
- `images/`: key result figures.
- `data/README.md`: what the input data is and why it isn't included.

#### Data

The analysis uses Hennepin County's tract-level SNAP and MFIP enrollment counts (provided to the group for the capstone) combined with American Community Survey 5-year estimates (income, poverty, race, education, age, employment, housing). The county dataset is not redistributed here, so the scripts will not run as-is; the report, figures and screenshot above show the results. Column names the scripts expect are listed in `data/README.md`.

#### Team and credits

Group capstone for DST 490 by Brandon Bloss, Ini Udomah and Vincent Rupp. The municipality boxplots and the interactive tract map were also developed further by Vincent as individual extensions of the shared dataset.

#### Tech

R (tidyverse, sf, randomForest, rpart)
      
