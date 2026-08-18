The repository stores the codes and appendices of the paper "Tracing Politicization in the U.S. Public Sphere, 1980-2024."

## Restricted data notice

The raw article corpus is licensed from **ProQuest TDM Studio** and cannot be redistributed with this package.

A researcher wishing to reproduce this study from raw data must:
1. Obtain institutional access to ProQuest TDM Studio (https://tdmstudio.proquest.com), typically through a university library subscription.
2. Once logged in, in Workbench, select "Create New Dataset."
3. Create the following TDM Studio datasets using these queries:
- NYT19801996: Search and select "New York Times" in Publication Titles (ID = 11561) and set Publication date between 01/01/1980 to 12/31/1996.
- NYT19972021: Search and select "New York Times" in Publication Titles (ID = 11561) and set Publication date between 01/01/1997 to 12/31/2021.
- NYT20222025: Search and select "New York Times" in Publication Titles (ID = 11561) and set Publication date between 01/01/2022 to 12/31/2025.
- WSJ: Search and select "Wall Street Journal" in Publication Titles (ID = 10482) and set Publication date with no start date, through 12/31/2024.
- WSJ1980-83: Search and select "Wall Street Journal (1923-)" in Publication Titles (ID = 45441) and set Publication date from 01/01/1980 to 12/31/1983.
- WP19872010: Search and select "The Washington Post" (ID = 10327) and "The Washington Post (pre-1997 Fulltext)" (ID = 47014) in Publication Titles and set Publication date between 01/01/1987 to 12/31/2010.
- WP20112025: Search and select "The Washington Post" (ID = 10327) in Publication Titles and set Publication date between 01/01/2011 to 12/31/2025.
- WashingtonPost-HistoricalNewspaper: Search and select "The Washington Post (1974-)" (ID = 47130) in Publication Titles and set Publication date between 01/01/1980 to 12/31/1986.
4. Once the above data folders are transferred into your Jupyter Notebook, you can run the notebook files below to replicate the results.

## Pipeline

The analysis is a sequential pipeline of R notebooks (R 4.2.3, `sample-r-2025.02.6` kernel). 
Each stage reads files written by the previous one:

| Order | Notebook | Purpose | Requires TDM Studio access? |
|---|---|---|---|
| 1 | `Dataset.ipynb` | Build raw article dataset from ProQuest TDM Studio exports + OCR'd historical WSJ/WP scans | Yes |
| 2 | `Step1_Preprocessing.ipynb` | Filter to US articles, dedupe, tokenize, sample | No (consumes Step 1 output) |
| 3 | `Step2_Topic_Models.ipynb`, `topic_screening.R` | LDA topic modeling; manual topic/term screening | Yes |
| 4 | `Step3_ALC_Embedding.ipynb` | Core analysis: ALC embeddings, politicization scores, category-level regressions | Yes |
| 5 | `makeBaldGelData.R`, `Step4_Opinion_Mapping.ipynb` | Compare public opinion trends with politicization trends | No (Runs on Step 3's derived output) |
| 6 | `Step5_Close_Reading.ipynb` | Per-topic/anchor-word embeddings for qualitative validity checks | Yes |
| 7 | `AppendixF_ALC_Dems_NewsOnly.ipynb`, `AppendixG_Outlet_Analysis.ipynb`, `AppendixJ_ALC_HighFreq.ipynb` | Appendices: Analysis excluding editorials and comments; separated by outlet; and limited to high frequency terms | Runs on Step 5 derived output |

Steps marked "Yes" read raw or near-raw article text and can only be executed inside a TDM Studio enclave by a researcher with their own ProQuest access.
Steps that consume only already-derived, non-consumptive outputs (aggregated scores, modelobjects with no raw text) can in principle run outside the enclave once those intermediate files are supplied.
