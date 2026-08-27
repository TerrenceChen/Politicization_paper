The repository stores the code of the paper "Tracing Politicization in the U.S. Public Sphere, 1980-2024."

## Restricted data notice

The raw article corpus is licensed from **ProQuest TDM Studio** and cannot be redistributed with this package. However, the derived tokens object `all_toks_ngram_0412.rds` of the sample of 1.35 million newspaper articles that is used to train word embeddings can be accessed here: https://osf.io/mwz2f/overview

A researcher wishing to reproduce this study from raw data must:
1. Obtain institutional access to ProQuest TDM Studio (https://tdmstudio.proquest.com), typically through a university library subscription.
2. Once logged in, navigate to Workbench and select "Create New Dataset."
3. Create the following TDM Studio datasets using these queries:
- NYT19801996: Search and select "New York Times" in Publication Titles (ID = 11561) and set Publication date between 01/01/1980 and 12/31/1996.
- NYT19972021: Search and select "New York Times" in Publication Titles (ID = 11561) and set Publication date between 01/01/1997 and 12/31/2021.
- NYT20222025: Search and select "New York Times" in Publication Titles (ID = 11561) and set Publication date between 01/01/2022 and 12/31/2025.
- WSJ: Search and select "Wall Street Journal" in Publication Titles (ID = 10482) and set Publication date with no start date, through 06/24/2024 (reflecting when the analysis of this paper starts).
- WSJ1980-83: Search and select "Wall Street Journal (1923-)" in Publication Titles (ID = 45441) and set Publication date from 01/01/1980 to 12/31/1983.
- WP19872010: Search and select "The Washington Post" (ID = 10327) and "The Washington Post (pre-1997 Fulltext)" (ID = 47014) in Publication Titles and set Publication date between 01/01/1987 and 12/31/2010.
- WP20112025: Search and select "The Washington Post" (ID = 10327) in Publication Titles and set Publication date between 01/01/2011 and 12/31/2025.
- WashingtonPost-HistoricalNewspaper: Search and select "The Washington Post (1974-)" (ID = 47130) in Publication Titles and set Publication date between 01/01/1980 and 12/31/1986.
4. Once the above data folders are transferred into your Jupyter Notebook, you can run the notebook files below to replicate the results.

Alternatively, researchers could also email tdmstudio@clarivate.com to access my workbench to replicate the analysis at no costs.

## Repository structure

`input_files`: contains all the externally imported files needed in the analysis. 

`scripts`: contain all the code needed to reproduce the results.

`sbatch`: sbatch files used to run in High Speed Computing System.

`Results_Files`: contains files produced by executing the code.

`ALC_results_topic`: dataframes for politicization scores by topic for ideological politicization.

`ALC_results_party`: dataframes for politicization scores by topic for partisan politicization.

`Graphs`: graphs used in the manuscript.

`Tables`: tables used in the manuscript.

`TDMStudio_Files`: contains the original files I used in ProQuest TDM Studio. They were streamlined for better presentation in the R files in `scripts`.


## Pipeline

The analysis is a sequential pipeline of R notebooks (R 4.2.3, `sample-r-2025.02.6` kernel). 
For Step 4, the original files for public opinion data can be downloaded from the ANES website: https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/.
Each stage reads files written by the previous one:

| Order | Code Files | Input Files | Purpose | Requires TDM Studio access? |
|---|---|---|---|---|
| 0 | `Dataset.ipynb` | `MainDictionary.txt` | Build raw article dataset from ProQuest TDM Studio exports + OCR'd historical WSJ/WP scans | Yes |
| 1 | `Step1_Preprocessing.ipynb` | `non_us_country_adj.csv` | Filter to US articles, dedupe, tokenize, sample | Yes |
| 2 | `Step2_Topic_Models.ipynb`, `topic_screening_new.R` | `topic_labels.csv`, `topics_screened_new.csv` | LDA topic modeling; manual topic/term screening | No (can run with the file `dfm_all_compressed_0412.rds`) |
| 3 | `01_build_subsample_anchors.R`, `02_alc_ideology_task.R`, `03_alc_party_task.R`, `04_analysis_and_figures.R` | `glove.rds`, `khodakA.rds` |Core analysis: ALC embeddings, politicization scores, regressions, graphs | No (can run with the derived `all_toks_ngram_0412.rds` file) |
| 4 | `makeBaldGelData.R`, `05_step4_opinion_mapping.R` | `results_with_domains.rds` | Compare public opinion trends with politicization trends | No (can run with `Results_Files/results_df_label_combined.rds`) |
| 5 | `06_step5_topic_dems_task.R`, `07_step5_anchor_dems.R` | `glove.rds`, `khodakA.rds` | Re-runs Step3's ALC embedding pipeline but saves the raw per-term DEMs | No (can run with the derived `all_toks_ngram_0412.rds` file) |
| 6 | `08_step5_closereading.R` | NA | Helpers with close reading | No |
| 7 | `AppendixF_newsonly.R`, `AppendixG_outlet_analysis.R`, `AppendixJ_highfreq.R` | NA | Appendices — robustness checks: restricted to non-opinion articles (excludes Commentary/Editorial/Review types); re-derives scores separately per outlet (NYT/WP/WSJ); robustness to term frequency | No |

Steps marked "Yes" read raw or near-raw article text and can only be executed inside a TDM Studio enclave by a researcher with their own ProQuest access.

## Contact

For questions about this reproduction package, please contact Terrence Ting-Yen Chen at tychen@nyu.edu.
