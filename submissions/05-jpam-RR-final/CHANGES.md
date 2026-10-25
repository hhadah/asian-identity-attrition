# Changes in the final revision of JPAM-2025-13688.R2

Base version: the manuscript submitted as R2 (`submissions/04-jpam-RR-v2/`, byte-identical to the copy this folder started from). Page numbers refer to `submit_journal.pdf` (manuscript pages are numbered from the first page of text). Section numbers follow the compiled manuscript: 1 Introduction; 2 Theoretical Framework; 3 Data (3.1 identity, 3.2 bias); 4 From the Data; 5 Empirical Approach; 6 Results (unnumbered subsection "The interaction between state-level bias and socioeconomic status"; 6.1 Robustness Checks); 7 Conclusion.

Comment codes: E1a–E1f = editor point 1 bullets (figures); E2 = state fixed effects; E3 = policy motivation; E4a = bootstrap clarification; E4b = Table 7 clustering and footnote numbering; R1–R8 = referee comments 1–8; U = not requested, disclosed in the response letter under "Other changes".

Every entry gives: what changed, where it is (file, section or paragraph, exhibit, page), and the comment it answers.

## 1. Exhibits moved, merged, replaced, or added

| # | Change | Where | Addresses |
|---|---|---|---|
| 1 | Old Figures 3 and 4 (bias maps) moved to the appendix | `manuscript_online_appendix.tex`, Data section; now Figures A.2 (p. 70) and A.3 (p. 71); referenced once in §3.2 | E1a, R8 |
| 2 | Old Figures 5–8 (identity histograms) moved to the appendix | `manuscript_online_appendix.tex`; now Figures A.4–A.7 (pp. 72–75); referenced in a footnote in §4 | E1b, R8 |
| 3 | Old Figures 9 and 12 combined into one two-panel figure, panel (a) "Mixed-Race Second-Generation Asian Americans", panel (b) "Third-Generation Asian Americans" | `manuscript_tab_fig.tex`; now Figure 3 (p. 49); labels `fig:marginal-effects-second-pooled` / `fig:marginal-effects-third-pooled` kept as sub-labels so old references render as 3a/3b | E1c |
| 4 | Old Figures 10 and 13 (AME by parental type; by grandparent count) moved to the appendix | now Figures A.8 (p. 82) and A.10 (p. 84) | E1d, R8 |
| 5 | Old Figure 11 (adult pooled AME) moved to the appendix | now Figure A.9 (p. 83) | E1e, R8 |
| 6 | Old Figures 14–16 (interaction coefficient plots) replaced by one three-panel table "Heterogeneity in the Effect of Anti-Asian Bias on Asian Identification": Panel A mixed-race second generation (AW, WA, pooled), Panel B mixed-race adults (AW, WA, pooled), Panel C third generation (one, two, three grandparents, pooled); coefficients with state-clustered SEs in parentheses and N per row. The pooled adult regression that Figure 15 lacked is estimated and reported (female −0.012 (0.033); college −0.023 (0.039); above-median income −0.020 (0.040)). The three figure blocks are kept verbatim in `manuscript_heldback_interaction_figures.tex` (not compiled) | new Table 8 (p. 46), file `tab50-heterogeneity-interactions.tex`, built by `programs/50-make-heterogeneity-interaction-table.R` | E1f, R8 |
| 7 | Tables 4 and 5 rebuilt: one observation count per row and the shares in the multinomial-logit categories (Asian only, White only, Asian and White) plus "Other", summing to one; rows follow the estimation subsamples (both parents Asian-born; one Asian-born parent split into AW and WA; third generation by number of Asian-born grandparents). Old columns (N Asian / N not-only-Asian / shares) removed | Table 4 (p. 42) `tab02-observations-by-gen.tex`; Table 5 (p. 43) `tab54-observations-by-gen-adults.tex`; built by `programs/53-make-identity-distribution-tables.R` | R2 |
| 8 | Table 7 re-estimated with standard errors clustered at the state level (was household). Point estimates unchanged; SE on Bias 0.01 → 0.03 in all three columns; Bias is no longer significant in any column (0.04, −0.01, 0.03). Notes rewritten (couple definition, child as unit of observation, columns (2)/(3) definition) and numbered 1–2. Same change to the logit version, Appendix Table A.2 | Table 7 (p. 45) `tab41a-marriage.tex`; Table A.2 (p. 77) `tab47-marriage.tex`; `programs/17-table-eight-endo-interethnic-reg.R` | E4b, R6 |
| 9 | Table 6 notes renumbered 1–3 (were 1, 3, 4); "a Asian country" → "an Asian country"; data source corrected from "Census Data" to "American Community Survey (IPUMS USA)" | Table 6 (p. 44) `tab46-iat-migration-tab.tex`; same wording in `programs/20-acs-skin-iat-migration.R` (not re-run; estimates unchanged) | E4b, R6, U |
| 10 | New appendix table: pooled multinomial logit on AW+WA children with all coefficients common except bias × AW; state-clustered SEs (hand-built sandwich, verified against `sandwich::vcovCL` on a two-category fit); Wald test (χ²(2) = 10.48, p = 0.005) and LR test; average marginal effects of bias for AW and WA over the same pooled sample (White only: AW 0.249 (0.068), WA 0.099 (0.058), difference 0.151 (0.042)); common-effect AME of bias (White only 0.146 (0.057)); note compares clustered with conventional SEs (0.057 vs 0.018; coefficient 0.363 vs 0.128) | new Table A.5 (p. 81) `tab51-pooled-mnl-bias-x-aw.tex`, built by `programs/51-run-pooled-mnl-bias-x-aw.R` | R4, R3 |
| 11 | Captions of all marginal-effects figures corrected: "holding other variables at their sample means" → derivative evaluated at each child's own covariates and averaged over the sample; bootstrap described (1,000 resamples of children within region × year cells; median; 2.5/97.5 percentiles); sentence added that intervals do not account for dependence within state-year cells; "US-born White parent" → "parent born outside Asia" | Figure 3 caption (p. 49); Figures A.8, A.9, A.10 captions (pp. 82–84) | E4a, R3, U |

## 2. Text changes by section

| # | Change | Where | Addresses |
|---|---|---|---|
| 12 | Abstract, last sentence: "conventional estimates overestimate Asian–White disparities" → "outcomes measured on self-reported race understate the outcomes of people of Asian ancestry in areas with high levels of prejudice, biasing conventional estimates of Asian–White disparities there" | `manuscript_title_etc.tex`, `\PAPERABSTRACT` (title page, p. iii) | U (direction consistency) |
| 13 | §1, paragraph 1: last two sentences replaced by four sentences stating the policy question (self-reported race underlies the 2024 OMB race/ethnicity standards, civil-rights enforcement, fund allocation, disparity tracking, and hate-crime reporting under the COVID-19 Hate Crimes Act) and the measurement-error implication. Footnote dash fixed (`---`) | `manuscript_main.tex` line 7; p. 1 | E3 |
| 14 | §1, results paragraph (line 17), last sentence: direction restated ("outcomes measured on self-reported race understate the outcomes of people of Asian ancestry where prejudice is strongest, and conventional estimates of Asian–White disparities are biased there"); Unicode dash fixed | `manuscript_main.tex` line 17; p. 2–3 | E3, U |
| 15 | §1, new paragraph after the results paragraph: policy stakes (undercount where prejudice is strongest; negative selection on maternal education so self-reported outcomes understate the Asian-ancestry population's outcomes, distorting disparity measures used for civil-rights enforcement, program targeting, and integration assessment; link to Duncan & Trejo 2018 and the National Academies 2015 integration report) | `manuscript_main.tex` line 19; p. 3 | E3 |
| 16 | §3.2, last paragraph: unit of the index defined (IAT D-score units; "one standard deviation" means one unit); two-year measurement blocks and unbalanced panel stated (at most five distinct values per state; 29 states from 2004, 11 from 2008, 11 from 2012); SD decomposition added (state-year SD 0.42; between-state 0.18; within-state 0.39; within-year cross-state average 0.26; one unit ≈ 2.4 state-year SDs); map references redirected to Appendix Figures A.2–A.3; footnote reference to the appendix LW subsection replaced by plain text | `manuscript_main.tex` line 93; p. 18 | R1, E1a, U |
| 17 | §4 rewritten (three paragraphs) to read from the new Tables 4 and 5: shares by category (first generation 94% Asian only; second 72%; third 29%; two Asian-born parents 96%; one Asian-born parent 31% / 25% White only / 37% Asian and White; AW 36% vs WA 29% Asian only; grandparent gradient 93/74/24/8%; adults 93% vs 33%); histogram references moved to a footnote defining the categories; old sentences quoting 96%/73%/31% (binary measure) removed | `manuscript_main.tex` lines 97–101; p. 19–20 | R2, E1b |
| 18 | §5, last paragraph rewritten: exact definition of the average marginal effects (numerical derivative at own covariates, unweighted average over the sample; discrete 0→1 change for binaries), the bootstrap (1,000 resamples of children, stratified by region × year; median point estimate; percentile interval), the truncation footnote, the statement that children not states are resampled, the ~3× clustering inflation from Table A.5, and the pointer to the state-clustered pooled estimates | `manuscript_main.tex` line 121; p. 22 | E4a, R3 |
| 19 | §6, opening paragraph: exhibit references updated (Figure 3 panels a/b; Appendix Figures A.8, A.9, A.10) | `manuscript_main.tex` line 125; p. 23 | E1 |
| 20 | §6, "Second-Generation Subsample": sentence added quoting the state-clustered pooled estimate from Table A.5 (+14.6 pp (SE 5.7) for White only; offsetting decline 12.3 pp Asian only, 2.4 pp Asian and White); adult reference now Appendix Figure A.9 | `manuscript_main.tex` line 127; p. 23 | R3, E1e |
| 21 | §6, AW vs WA paragraph: hedged ("may be larger" → point estimates with the comparison deferred to the tests); reference now Appendix Figure A.8 | `manuscript_main.tex` line 129; p. 23 | R4, E1d |
| 22 | §6, between-model paragraph rewritten: why cross-model comparisons require care (covariate distributions; scale normalization); the two tests (between-model, Appendix Table A.3 / Figure A.11; pooled interaction model, Appendix Table A.5); pooled results quoted (interaction 0.89 (0.32); Wald p = 0.005; 24.9 vs 9.9 pp; difference 15.1 pp (4.2); Asian only −10.5 pp (6.4)); footnote with the pooled LPM interaction (−0.08 (0.06)); closing caveat that the between-model tests use the individual-level bootstrap and that only the AW–WA bias difference survives state-clustered inference | `manuscript_main.tex` line 131; p. 24–25 | R4, R3 |
| 23 | §6, "Third-Generation Subsample": references updated (Figure 3b; Appendix Figure A.10; Appendix Figure A.12 / Table A.4); "confirm that these differences are statistically significant" → "find these differences statistically significant by the bootstrap criterion, subject to the same caveat" | `manuscript_main.tex` line 133; p. 25 | E1, R3 |
| 24 | §6 interaction subsection, first paragraph: figure references replaced by Table 8 and its three panels | `manuscript_main.tex` line 139; p. 26 | E1f |
| 25 | §6 interaction subsection, results paragraph rewritten with the table's coefficients and SEs: AW children maternal college −0.23 (0.10) [was "approximately −0.20"]; AW adults own college −0.08 (0.05) [was −0.07]; pooled children −0.07 (0.03) [was "approximately −0.04"]; third generation −0.02/−0.08/−0.30 imprecise; pooled third −0.07 (0.03) [was −0.05]; sex, paternal education, and income interactions null in every panel | `manuscript_main.tex` line 141; p. 26–27 | E1f |
| 26 | §6 interaction subsection, implication sentence: "overestimate Asian–White gaps" → "outcomes measured on self-reported race understate the outcomes of people of Asian ancestry" | `manuscript_main.tex` line 143; p. 27 | U (direction) |
| 27 | §6.1, marriage paragraph: four threats listed (fixed state characteristics added); estimates reported with state-clustered SEs as nulls (4 pp (3), 3 pp, −1 pp); footnote disclosing the earlier household clustering; discussion of columns (2) vs (3) (not a closed market; conditions on co-resident child and on one Asian-born partner; 20,247 WA vs 9,055 AW children; consistent with an effect on marriage itself for Asian men); "US-born White" wording avoided | `manuscript_main.tex` line 147; p. 28–29 | R5, R6, E4b |
| 28 | §6.1, migration paragraph: data source "Census" → "American Community Survey"; "0.06 standard deviations greater bias … estimates understate the effect" → "0.02 (standard error 0.04), not distinguishable from zero; positive sign works against finding an effect" (text now matches Table 6, column 3) | `manuscript_main.tex` line 149; p. 29 | U |
| 29 | §6.1, state-fixed-effects paragraph replaced: no state FE in the main model is a limitation; column 8 (−0.04 (0.03)) and column 4 (−0.02 (0.03)) of Table A.1 reported with their intervals (column 8 reaches the baseline, column 4 excludes it); residual SDs after fixed effects (0.18 vs 0.22) show the imprecision is not a lack of within-state variation; two explanations left open (fixed state characteristics; attenuation from measurement error in within-state changes); column 3 identified as the only precise state-FE estimate and why it is not the baseline with state effects; "robust to fixed state characteristics" and "comparable in magnitude" claims removed | `manuscript_main.tex` line 153; p. 29–30 | E2, R7 |
| 30 | §7, paragraph 1: policy framing added (statistics count people by the race they report; relevance to agencies enforcing civil-rights law) | `manuscript_main.tex` line 159; p. 31 | E3 |
| 31 | §7, paragraph 2: "fundamentally reshapes", "significantly", "markedly" removed; between-model and pooled-model confirmations of the AW–WA bias difference both quoted (10.6 pp and 15.1 pp) | `manuscript_main.tex` line 161; p. 31–32 | R4 |
| 32 | §7, paragraph 3: AW maternal-college interaction −0.20 → −0.23 (standard error 0.10); between-model significance claims qualified ("those tests do not account for state-level dependence"; "by the same bootstrap criterion") | `manuscript_main.tex` line 163; p. 32 | E1f, R3 |
| 33 | §7, paragraph 4 rewritten: direction of the measurement bias stated conditionally (gap overstated where Asian Americans trail, lead understated where they lead); policy uses of self-reported race cited (OMB 2024; COVID-19 Hate Crimes Act); "may overstate Asian American socioeconomic success" → "may understate … in high-bias areas and understate the group's progress across generations" | `manuscript_main.tex` line 165; p. 32–33 | E3, U (direction) |
| 34 | §7, new limitation paragraph: state-FE estimates too imprecise to be informative; confounding by fixed state characteristics cannot be ruled out | `manuscript_main.tex` line 167; p. 33 | E2, R7 |
| 35 | References: three verified entries added (`ombRevisionsStatisticalPolicy2024`, 89 FR 22182; `covid19HateCrimesAct2021`, Pub. L. 117-13, 135 Stat. 265; `watersIntegrationImmigrantsAmerican2015`, NAP doi 10.17226/21746); duplicate `gover2020anti` entry removed | `AttitudesPaper.bib` | E3, U |
| 36 | Appendix: label of the unnumbered LW subsection renamed `sub:lw-bias-appendix` so `sub:lw-bias` is defined once | `manuscript_online_appendix.tex` | U (build fix) |

## 3. Response letter and cover

| # | Change | Where | Addresses |
|---|---|---|---|
| 37 | Cover retitled "Final revision (conditional acceptance)"; mentions the separate tracked-changes file | `submit_journal.tex` lines 20–24 | — |
| 38 | Part VI replaced by the new point-by-point response: E1 (a–f), E2, E3, E4; R1–R8; "Other changes" listing seven unrequested corrections. Manuscript excerpts in blue | `submit_journal.tex` lines 163–303; pp. 50–65 of `submit_journal.pdf` | all |

## 4. Code and replication files

| # | Change | Where | Addresses |
|---|---|---|---|
| 39 | New: `50-make-heterogeneity-interaction-table.R` (Table 8 + CSV manifest; pooled LPM bias × AW, `tab50-pooled-bias-x-aw-lpm.csv`) | `programs/` | E1f, R4 |
| 40 | New: `51-run-pooled-mnl-bias-x-aw.R` (Table A.5 + two CSV manifests; runtime verification of the clustered covariance against `sandwich::vcovCL` and of `solve(Hessian)` against `glm`; weights scaled to mean one; two separated region × year cells dropped and counted) | `programs/` | R4, R3 |
| 41 | New: `52-make-bias-index-variance-decomposition.R` (`bias-index-variance-decomposition.csv`: between/within SDs, residual SDs after each fixed-effect set) | `programs/` | R1, E2 |
| 42 | New: `53-make-identity-distribution-tables.R` (Tables 4–5 + CSVs; partition assertions; `tab02-mixed-parent-us-born-share.csv`) | `programs/` | R2 |
| 43 | Edited: `17-table-eight-endo-interethnic-reg.R` (`vcov = ~statefip`; notes); `20-acs-skin-iat-migration.R` (notes); `05-…`, `36-…` (superseded headers); `95-make-all.R` (new scripts listed; HPC bootstrap launches commented out) | `programs/` | E4b, R6 |

## 5. Numbers that changed in the text

| Quantity | Old | New | Location |
|---|---|---|---|
| First / second / third generation reporting Asian (binary) | 96 / 73 / 31% | 94 / 72 / 29% ("Asian only") | §4 |
| Two Asian-born parents, one Asian-born parent | 97%, 33% | 96%, 31% | §4 |
| Adults: two Asian-born parents, one | 95%, 37% | 93%, 33% | §4 |
| AW children bias × maternal college | ≈ −0.20 | −0.23 (0.10) | §6, §7 |
| AW adults bias × own college | ≈ −0.07 | −0.08 (0.05) | §6 |
| Pooled second-generation bias × maternal college | ≈ −0.04 | −0.07 (0.03) | §6 |
| Pooled third-generation bias × maternal college | ≈ −0.05 | −0.07 (0.03) | §6 |
| Table 7 Bias SEs (cols 1–3) | 0.01, 0.01, 0.01 (***, –, **) | 0.03, 0.03, 0.03 (none significant) | Table 7, §6.1 |
| Movers' bias difference | 0.06 SD | 0.02 (0.04) | §6.1 |
| State-FE claim | "remains negative and statistically significant … comparable in magnitude" | "uninformative rather than reassuring"; CIs and residual SDs reported | §6.1, §7 |

## 6. Not changed, deliberately

- The "one standard deviation" convention for the bias index is kept (one unit of the IAT-D-scaled index); §3.2 now defines it and gives its size relative to the data (2.4 state-year SDs).
- Bootstrap point estimates remain medians (the stored output has no draws; re-running the 1,000-replication bootstrap was out of scope); the text and response say so.
- Old Figures 14–16 are not in the appendix; the table replaces them.
