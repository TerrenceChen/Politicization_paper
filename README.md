The repository stores the code and appendices of the paper "Tracing Politicization in the U.S. Public Sphere, 1980-2024."

## Restricted data notice

The raw article corpus is licensed from **ProQuest TDM Studio** and cannot be redistributed with this package.

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


## Pipeline

The analysis is a sequential pipeline of R notebooks (R 4.2.3, `sample-r-2025.02.6` kernel). Some stages require externally imported files in the `input_files` folder. 
For Step 4, the original files for public opinion data can be downloaded from the ANES website: https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/.
Each stage reads files written by the previous one:

| Order | Code Files | Input Files | Purpose | Requires TDM Studio access? |
|---|---|---|---|---|
| 0 | `Dataset.ipynb` | `MainDictionary.txt` | Build raw article dataset from ProQuest TDM Studio exports + OCR'd historical WSJ/WP scans | Yes |
| 1 | `Step1_Preprocessing.ipynb` | `non_us_country_adj.csv` | Filter to US articles, dedupe, tokenize, sample | Yes |
| 2 | `Step2_Topic_Models.ipynb`, `topic_screening.R` | `topics_screened_new.csv` | LDA topic modeling; manual topic/term screening | No (can run with the file `dfm_all_compressed_0412.rds`) |
| 3 | `Step3_ALC_Embedding.ipynb` | `glove.rds`, `khodakA.rds` |Core analysis: ALC embeddings, politicization scores, regressions, graphs | Yes |
| 4 | `makeBaldGelData.R`, `Step4_Opinion_Mapping.ipynb` | `results_with_domains.rds` | Compare public opinion trends with politicization trends | No (can run with `results_df_label_combined.rds`) |
| 5 | `Step5_Close_Reading.ipynb` | `glove.rds`, `khodakA.rds` | Re-runs Step3's ALC embedding pipeline but saves the raw per-term DEMs | Yes |
| 6 | `AppendixF_ALC_Dems_NewsOnly.ipynb`, `AppendixG_Outlet_Analysis.ipynb`, `AppendixJ_ALC_HighFreq.ipynb` | Step 5 results | Appendices — robustness checks on the Step 3 results: restricted to non-opinion articles (excludes Commentary/Editorial/Review types); re-derives scores separately per outlet (NYT/WP/WSJ); re-derives scores restricted to high-frequency terms (≥100 occurrences in a given year) | Yes |

Steps marked "Yes" read raw or near-raw article text and can only be executed inside a TDM Studio enclave by a researcher with their own ProQuest access.
Steps that consume only already-derived, non-consumptive outputs (aggregated scores, model objects with no raw text) can in principle run outside the enclave once those intermediate files are supplied.
