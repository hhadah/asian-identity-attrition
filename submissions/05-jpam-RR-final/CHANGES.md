# Changes in the final revision of JPAM-2025-13688.R2

Base version: the manuscript submitted as R2 (`submissions/04-jpam-RR-v2/`, byte-identical to the copy this folder started from). Updated after the November 2026 audit corrections. Locations use section and exhibit numbers rather than pagination, which changes when the manuscript is rebuilt. Section numbers follow the compiled manuscript: 1 Introduction; 2 Theoretical Framework; 3 Data (3.1 identity, 3.2 bias); 4 From the Data; 5 Empirical Approach; 6 Results (unnumbered subsection "The interaction between state-level bias and socioeconomic status"; 6.1 Robustness Checks); 7 Conclusion.

Comment codes: E1a–E1f = editor point 1 bullets (figures); E2 = state fixed effects; E3 = policy motivation; E4a = bootstrap clarification; E4b = Table 7 clustering and footnote numbering; R1–R8 = referee comments 1–8; U = not requested, disclosed in the response letter under "Other changes".

Every entry gives what changed, its source file and section or exhibit, and the comment it answers.

## 1. Exhibits moved, merged, replaced, or added

| # | Change | Where | Addresses |
|---|---|---|---|
| 1 | Old Figures 3 and 4 (bias maps) moved to the appendix | `manuscript_online_appendix.tex`, Data section; now Figures A.2 and A.3; referenced once in §3.2 | E1a, R8 |
| 2 | Old Figures 5–8 (identity histograms) moved to the appendix | `manuscript_online_appendix.tex`; now Figures A.4–A.7; referenced in a footnote in §4 | E1b, R8 |
| 3 | Old Figures 9 and 12 combined into one two-panel figure, panel (a) "Mixed-Race Second-Generation Asian Americans", panel (b) "Third-Generation Asian Americans" | `manuscript_tab_fig.tex`; now Figure 3; labels `fig:marginal-effects-second-pooled` / `fig:marginal-effects-third-pooled` kept as sub-labels so references render as 3a/3b | E1c |
| 4 | Old Figures 10 and 13 (AME by parental type; by grandparent count) moved to the appendix | Figures A.8 and A.10 | E1d, R8 |
| 5 | Old Figure 11 (adult pooled AME) moved to the appendix | Figure A.9 | E1e, R8 |
| 6 | Old Figures 14–16 replaced by one three-panel heterogeneity table: Panel A mixed-race second-generation children (AW, WA, pooled), Panel B mixed-race adults (AW, WA, pooled), Panel C third generation (one, two, three grandparents, pooled). It reports coefficients, state-clustered SEs, and N. The previously missing pooled-adult row reports female −0.012 (0.033), college −0.023 (0.039), and above-median income −0.020 (0.040). The old figure blocks are removed from this submission; the prior submission preserves them | Table 8, `tab50-heterogeneity-interactions.tex`; `programs/50-make-heterogeneity-interaction-table.R` | E1f, R8 |
| 7 | Tables 4 and 5 report one count per row and shares in the three MNL categories plus Other. Notes explicitly enumerate the four CPS response combinations pooled under "Asian and White", including White–Hawaiian/Pacific Islander and Asian–Hawaiian/Pacific Islander. Category membership and samples are unchanged by the note correction | Tables 4–5, `tab02-observations-by-gen.tex`, `tab54-observations-by-gen-adults.tex`; `programs/53-make-identity-distribution-tables.R` | R2 |
| 8 | Table 7 re-estimated with state rather than household clustering in the earlier final-round revision. Point estimates are unchanged; bias SEs rise from 0.01 to 0.03 and none of the three bias coefficients remains statistically significant. Notes are corrected and numbered consecutively. Appendix Table A.2 uses state clustering and also corrects a separate scale error: R2 displayed log-odds while its note described exponentiated coefficients; the final table displays odds ratios | Table 7, `tab41a-marriage.tex`; Table A.2, `tab47-marriage.tex`; `programs/17-table-eight-endo-interethnic-reg.R` | E4b, R6, U |
| 9 | Table 6 notes renumbered 1–3; "a Asian country" corrected; data source changed from "Census Data" to "American Community Survey (IPUMS USA)" | Table 6, `tab46-iat-migration-tab.tex`; same wording in `programs/20-acs-skin-iat-migration.R`; estimates unchanged | E4b, R6, U |
| 10 | New pooled AW+WA MNL with bias × AW, state-clustered SEs, Wald test (χ²(2) = 10.48, p = 0.005), and independence-based LR test. AMEs evaluated over one pooled covariate distribution: White only, AW 0.249 (0.068), WA 0.099 (0.058), difference 0.151 (0.042); common-effect AME 0.146 (0.057). The conventional-versus-clustered SE comparison does not quantify bootstrap uncertainty | Table A.5, `tab51-pooled-mnl-bias-x-aw.tex`; `programs/51-run-pooled-mnl-bias-x-aw.R` | R4, R3 |
| 11 | Marginal-effects captions describe derivatives evaluated at individual covariates and averaged over the sample, median summaries from 1,000 individual bootstrap resamples within region × year, and percentile intervals. They distinguish one index unit from an empirical SD and disclose that the bootstrap does not account for dependence within states. "US-born White parent" changed to "parent born outside Asia" | Figure 3 and Figures A.8–A.10 captions | E4a, R3, U |

The audit also found that Figure 2's panels display different places. Its title, caption, and §3.2 discussion now describe the actual panels rather than a matched-state comparison. Panel a contains Hawaii, North Dakota, Tennessee, and Vermont; panel b contains the District of Columbia, North Dakota, South Dakota, and Vermont. The PNGs are unchanged, and this clarification is disclosed in the response letter.

## 2. Text changes by section

| # | Change | Where | Addresses |
|---|---|---|---|
| 12 | Abstract qualifies bootstrap-only grandparent comparisons and the implications of selective identification rather than treating them as established state-clustered effects | `manuscript_title_etc.tex`, `\PAPERABSTRACT` | R3, U |
| 13 | Opening paragraph motivates the policy question through self-reported population counts and disparity measures. It no longer claims that nearly every policy or hate-crime incident classification rests on self-identification | `manuscript_main.tex`, §1 opening | E3 |
| 14 | Introductory results distinguish state-clustered pooled inference from bootstrap-only findings and acknowledge possible state-level confounding | `manuscript_main.tex`, §1 results summary | E2, E3, R3, R7 |
| 15 | Policy-stakes paragraph ties the maternal-college selection finding to Asian-father/White-mother families rather than generalizing across all children. Population and disparity implications remain conditional on selective identification | `manuscript_main.tex`, §1 policy paragraph | E3 |
| 16 | Bias index is expressed in IAT D-score units, not empirical SD units. The SD decomposition remains 0.42 across state-years, 0.18 between states, 0.39 within states, and 0.26 across states within years. One index unit is about 2.4 empirical state-year SDs. Two-year blocks and unbalanced coverage are disclosed | `manuscript_main.tex`, §3.2 | R1, E1a, U |
| 17 | §4 reads from the revised Tables 4 and 5. The Asian-only grandparent gradient is 93/74/24/7 percent. The last value is emitted by script 53 as `fragments/one-grandparent-asian-only-percent.tex`, correcting the former 8 percent statement without changing the underlying share. The category footnote now matches the CPS codes | `manuscript_main.tex`, §4; Tables 4–5 | R2, E1b |
| 18 | AMEs are average individual derivatives per index unit, not exact finite one-unit changes; binary covariates use 0-to-1 differences. The numerical step is one-tenth of the sample SD. Text states the unweighted average, individual resampling within region × year, median across replications, and percentile interval. The unsupported threefold bootstrap-uncertainty multiplier is removed | `manuscript_main.tex`, §5 | E4a, R3 |
| 19 | Exhibit references updated to Figure 3 panels a/b and Appendix Figures A.8–A.10 | `manuscript_main.tex`, §6 opening | E1 |
| 20 | State-clustered pooled AME reported from Table A.5: White only +14.6 percentage points per index unit, SE 5.7, with offsetting declines of 12.3 points for Asian only and 2.4 for Asian and White | `manuscript_main.tex`, §6 second-generation results | R3, E1e |
| 21 | AW–WA point-estimate comparisons defer to the explicit tests rather than relying on separate-model magnitudes | `manuscript_main.tex`, §6; Figure A.8 | R4, E1d |
| 22 | Pooled MNL comparison and common-covariate AMEs reported alongside between-model comparisons. The LPM interaction −0.08 (0.06) is statistically inconclusive. Text acknowledges sex differences reported by the individual-bootstrap tables instead of denying all remaining differences; state-clustered inference is kept separate | `manuscript_main.tex`, §6; Tables A.3 and A.5 | R4, R3 |
| 23 | Third-generation comparisons and sex differences are described under the individual-bootstrap criterion, without implying state-clustered significance | `manuscript_main.tex`, §6; Table A.4 and Figures A.10–A.12 | E1, R3 |
| 24 | Interaction figures replaced in the text by Table 8 and its three panels | `manuscript_main.tex`, §6 interaction subsection | E1f |
| 25 | Interaction magnitudes reconciled to Table 8: AW-child maternal college −0.23 (0.10), AW-adult college −0.08 (0.05), pooled-child maternal college −0.07 (0.03), and pooled-third-generation maternal college −0.07 (0.03). No new estimates were fitted during the audit correction pass | `manuscript_main.tex`, §6; Table 8 | E1f |
| 26 | Selection implications are conditional and distinguish the families supporting the maternal-college result | `manuscript_main.tex`, §6 interaction discussion | E3, U |
| 27 | Marriage results reported with state-clustered SEs and disclosure of the former household clustering. Sample correctly includes Asian–Asian and mixed-parentage couples with a co-resident child and at least one Asian-born parent. The descriptive 20,247 WA and 9,055 AW child counts are explicitly attributed to Table 4, not the Table 7 estimation sample | `manuscript_main.tex`, §6.1 marriage paragraph | R5, R6, E4b |
| 28 | Migration data source corrected to ACS; prose now reports 0.02 index units, SE 0.04, rather than 0.06 SD. The estimate is not statistically distinguishable from zero | `manuscript_main.tex`, §6.1; Table 6, column 3 | U |
| 29 | Table A.1 identified as an all-generation LPM including endogamous children and generation controls, not the headline mixed-race MNL. Adding state effects lowers the estimate from −0.10 to −0.04 while the rounded SE remains 0.03. The interval and possible state-level confounding are stated; state-FE robustness is not claimed | `manuscript_main.tex`, §6.1; Table A.1 | E2, R7 |
| 30 | Conclusion links self-identification to population and disparity statistics without generalizing to every policy or incident-classification rule | `manuscript_main.tex`, §7 opening | E3 |
| 31 | Conclusion reports both the individual-bootstrap AW–WA bias contrast and the pooled state-clustered contrast, with their different inferential bases | `manuscript_main.tex`, §7 results summary | R4 |
| 32 | Maternal-college selection and grandparent comparisons are qualified consistently with Table 8 and the individual-bootstrap limitations | `manuscript_main.tex`, §7 | E1f, R3 |
| 33 | Disparity implications are conditional on negative selection. Hate-crime incident classification is no longer described as resting solely on self-reported race | `manuscript_main.tex`, §7 policy paragraph | E3, U |
| 34 | Conclusion acknowledges that state effects reduce the estimate and leave possible state-level confounding unresolved, rather than suggesting the reported SE increased | `manuscript_main.tex`, §7 limitation paragraph | E2, R7 |
| 35 | References: three verified entries added (`ombRevisionsStatisticalPolicy2024`, 89 FR 22182; `covid19HateCrimesAct2021`, Pub. L. 117-13, 135 Stat. 265; `watersIntegrationImmigrantsAmerican2015`, NAP doi 10.17226/21746); duplicate `gover2020anti` entry removed | `AttitudesPaper.bib` | E3, U |
| 36 | Appendix: label of the unnumbered LW subsection renamed `sub:lw-bias-appendix` so `sub:lw-bias` is defined once | `manuscript_online_appendix.tex` | U (build fix) |

## 3. Response letter and cover

| # | Change | Where | Addresses |
|---|---|---|---|
| 37 | Cover retitled "Final revision (conditional acceptance)"; mentions the separate tracked-changes file | `submit_journal.tex` lines 20–24 | — |
| 38 | Part VI contains E1 (a–f), E2, E3, E4, R1–R8, and additional corrections, including the A.2 scale correction and Figure 2 caption clarification. The inconsistent four/seven counts are removed. Blue quotations match the corrected manuscript; R3 defines the median as the 50th percentile and retains "without weights"; R4 states the code's 0.1 percent covariance-check tolerance rather than an unsupported six-decimal agreement | `submit_journal.tex`, Responses to the Editor and the Referee | all |

## 4. Code and replication files

| # | Change | Where | Addresses |
|---|---|---|---|
| 39 | New: `50-make-heterogeneity-interaction-table.R` (Table 8 + CSV manifest; pooled LPM bias × AW, `tab50-pooled-bias-x-aw-lpm.csv`) | `programs/` | E1f, R4 |
| 40 | New: `51-run-pooled-mnl-bias-x-aw.R` (Table A.5 + two CSV manifests; runtime verification of the clustered covariance against `sandwich::vcovCL` and of `solve(Hessian)` against `glm`; weights scaled to mean one; two separated region × year cells dropped and counted) | `programs/` | R4, R3 |
| 41 | New: `52-make-bias-index-variance-decomposition.R` (`bias-index-variance-decomposition.csv`: between/within SDs, residual SDs after each fixed-effect set) | `programs/` | R1, E2 |
| 42 | New: `53-make-identity-distribution-tables.R` (Tables 4–5, CSVs, partition assertions, parent-birthplace shares). Audit pass corrects category notes and adds the generated one-grandparent whole-percent fragment | `programs/`; `output/tables/fragments/` | R2 |
| 43 | Edited: `17-table-eight-endo-interethnic-reg.R` (`vcov = ~statefip`; notes); `20-acs-skin-iat-migration.R` (notes); `05-…`, `36-…` (superseded headers); `95-make-all.R` (new scripts listed; HPC bootstrap launches commented out) | `programs/` | E4b, R6 |

The audit correction pass also updates index-unit wording in the Table 8 and A.5 generators. `revision-audit-2026-11-05/tools/refresh-table-notes.R` refreshes their literal note text while retaining stored numeric clauses and asserting identical table bodies; it never runs the estimation expressions. Program 53 was run to rebuild descriptive notes and the percentage fragment, with all counts and shares checked against the prior saved manifests.

The clean-PDF smoke check found that Table A.5's long notes overlapped its page footer. The generator now uses a smaller note font; the final rendered page keeps the complete note above the footer. The table body and every numeric value in the note are unchanged.

The review package now has a retained four-step rebuild under `revision-audit-2026-11-05/tools/98-run-all.sh`. It builds both comparison inputs from LaTeX and verifies native Accept All and Reject All against them before publication. Changed exhibits retain their own captions as complete old/new blocks. A native-application check also caught two notes merging on Reject All; whole-paragraph replacement at the affected insertion boundary preserves the separate notes.

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
| One Asian grandparent reporting Asian only | 8% in the pre-audit final draft | 7%, rounded from the unchanged saved share | §4 |
| Appendix A.2 reporting scale | Log-odds, incorrectly described as exponentiated | Odds ratios, matching the note | Table A.2 |
| State-FE claim | "remains negative and statistically significant … comparable in magnitude" | "uninformative rather than reassuring"; CIs and residual SDs reported | §6.1, §7 |

## 6. Not changed, deliberately

- Regression estimates, estimation samples, and submitted figure PNGs were not changed during the audit correction pass. Script 53 rebuilds descriptive tables and the prose fragment, not regressions.
- Bootstrap point summaries remain medians. Their complete checkpoint provenance remains unresolved: the manuscript-matching CSV differs from the older preserved run. See `revision-audit-2026-11-05/comment_audit.md`, §4, for evidence and recovery requirements. No bootstrap was rerun.
- Old Figures 14–16 are not in the appendix; the table replaces them.
