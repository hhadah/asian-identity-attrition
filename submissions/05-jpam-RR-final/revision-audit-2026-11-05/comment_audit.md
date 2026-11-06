# JPAM final-revision audit

Audit date: November 5, 2026. Manuscript: JPAM-2025-13688.R2.

## Verdict

The revised folder contains a direct response to every editorial instruction. The main paper has three figures rather than sixteen; the requested three-panel heterogeneity table and pooled adult estimates are present; the state-fixed-effects limitation, policy motivation, bootstrap explanation, and state-clustering correction are all present.

**All eleven manuscript and response corrections identified by the audit have been applied. The source of the bootstrap mismatch has also been identified.** The former comparison output reproduces a recovered 50-replication run, whereas the marginal-effect figure annotations agree with the preserved 1,000-replication summaries. With the author's approval, the comparison tables, comparison plots, and associated text now use the existing 1,000-replication run. Section 4 records the evidence, changed inference, and remaining limits.

No model was re-estimated and no bootstrap was rerun. The R2 sources, eight marginal-effect panels, pooled state-clustered estimates, and other saved numerical results are unchanged. Tables A.3/A.4 and the four comparison panels in Figures A.11/A.12 were regenerated. The earlier descriptive-table correction retained all counts and shares.

### Scope and evidence

- Baseline: `submissions/04-jpam-RR-v2/`.
- Revised version: `submissions/05-jpam-RR-final/`.
- Instructions: `ref-reports/JPAM-2025-13688.R2-Comments.docx` and the eight referee comments in `ref-reports/jpam-letter.pdf`.
- Evidence: actual LaTeX source differences, exhibit contents and labels, estimation code, saved tables and manifests, and isolated compilations of both source versions. `CHANGES.md` was treated as a claim to check, not as proof.
- File names and line numbers below refer to the revised folder unless a repository-relative path is given. They refer to the source snapshot audited here, not to Word pagination.

## 1. Editor's instructions

| Item | Requested action | Actual revision and evidence | Assessment |
|---|---|---|---|
| E1a | Keep one of Figures 2 through 4 in the main paper; move the other two | Figure 2 remains. Old Figures 3 and 4 are Appendix Figures A.2 and A.3. `manuscript_tab_fig.tex:99–118`; `manuscript_online_appendix.tex:61–109`. | Implemented |
| E1b | Move Figures 5 through 8 to the appendix | Now Figures A.4 through A.7. Section 4 points to them in a footnote. `manuscript_online_appendix.tex:119–260`; `manuscript_main.tex:97`. | Implemented |
| E1c | Combine Figures 9 and 12 with the specified two panels | Figure 3 has the requested title and panels for mixed-race second-generation and third-generation Asian Americans. References resolve as 3a and 3b. `manuscript_tab_fig.tex:127–150`. | Implemented |
| E1d | Move Figures 10 and 13 to the appendix | Now Figures A.8 and A.10. `manuscript_online_appendix.tex:307–325,372–398`. | Implemented |
| E1e | Move Figure 11 to the appendix | Now Figure A.9. `manuscript_online_appendix.tex:332–338`. | Implemented |
| E1f | Replace Figures 14 through 16 with one three-panel table; report coefficients and SEs; estimate the pooled adult model | Table 8 has every requested group and covariate column, plus observation counts. Standard errors appear in parentheses. The pooled adult row is supported by saved estimates, not just a statement in the response. `tab50-heterogeneity-interactions.tex`; `programs/50-make-heterogeneity-interaction-table.R`; `output/tables/tab50-heterogeneity-interactions.csv`. | Implemented |
| E2 | Withdraw the state-FE robustness claim and acknowledge possible state-level confounding | Results and Conclusion acknowledge the limitation. Table A.1 is explicitly an all-generation LPM, not the headline mixed-race MNL; loss of significance reflects a smaller estimate with the same rounded SE. `manuscript_main.tex:153,167`. | Implemented, with audit corrections |
| E3 | Make policy implications a motivating part of the introduction | Policy material motivates the opening and separate policy-stakes paragraph. The text distinguishes hate-crime incident classification from population denominators, qualifies family-specific selection, and acknowledges state-level confounding. `manuscript_main.tex:7,19,159,165`. | Implemented, with audit corrections |
| E4a | Clarify the bootstrap calculation at the end of Section 5 | Text and captions distinguish the average individual derivative from the median across replications, state the binary 0-to-1 calculation, resampling unit, strata, replication count, percentile interval, numerical step, and units. The unsupported uncertainty multiplier is removed. Comparison-table notes separately define the approximate normal tests. | Implemented; run mismatch reconciled in §4 |
| E4b | Check Table 7 clustering, explain if it was not a typo, and number Table 6/7 notes consecutively | The code uses state clustering. Table 7 bias SEs rise from 0.01 to 0.03; the three bias coefficients are no longer significant. The main text discloses that household clustering was real, not merely a note typo. Table 6 notes are 1–3 and Table 7 notes are 1–2. `tab41a-marriage.tex:12–13,26–27`; `tab46-iat-migration-tab.tex:33–37`; `manuscript_main.tex:147`; `programs/17-table-eight-endo-interethnic-reg.R:21–36`. | Implemented |

The saved Table 8 pooled-adult row reports female −0.012, SE 0.033; college graduate −0.023, SE 0.039; above-median income −0.020, SE 0.040. It uses 4,686 observations and 51 state clusters. The manifest identifies run `20261025-113240`. The table note explains that the adult interaction sample is restricted to ASEC respondents because it uses family income.

## 2. Referee comments required by the decision letter

| Comment | Response verified | Status after correction |
|---|---|---|
| R1: variation in the bias index | Section 3.2 reports the state-year SD decomposition and observation timing. One index unit is now distinguished from one empirical SD throughout the text, captions, and table notes. | Addressed |
| R2: identity distributions in Tables 4 and 5 | Tables report one count and shares in the MNL categories plus Other. Notes now enumerate the exact pooled CPS categories; the one-grandparent percentage is generated from Table 4. | Addressed; numeric cells unchanged |
| R3: bootstrap | Section 5 and captions explain the calculation. Retaining the median is disclosed, units are corrected, and clustered-versus-conventional SE ratios are not presented as bootstrap uncertainty multipliers. The response discloses the 50-replication comparison-output error and its reconciliation to the saved 1,000-replication run. | Addressed; raw-draw limits in §4 |
| R4: AW versus WA comparison | Table A.5 reports the pooled MNL, clustered Wald test, independence-based LR test, and common-distribution AMEs. Sex-difference claims now match the appendix; the LPM interaction is labelled statistically inconclusive. | Addressed |
| R5: marriage asymmetry | Discussion considers non-marriage and selection into co-resident-parent families. It correctly includes Asian–Asian couples and attributes the descriptive counts to Table 4. | Addressed |
| R6: clustering and table notes | State clustering and consecutive numbering are implemented. The separate A.2 log-odds-to-odds-ratios correction is disclosed in E4, R6, Other changes, and CHANGES.md. | Addressed |
| R7: state fixed effects | Editor E2 response and limitation paragraphs identify the actual all-generation LPM check and leave state-level confounding open. | Addressed |
| R8: exhibit consolidation | Implemented according to the editor's more specific instructions. Moving rather than dropping the maps is explained. | Addressed |

## 3. Corrections applied after the audit

The item numbers match the original audit. The locations below contain the corrected wording, not outstanding instructions.

| Item | Correction applied | Location and evidence |
|---|---|---|
| 1. Racial categories | "Asian and White" explicitly pools White–Asian, White–Hawaiian/Pacific Islander, Asian–Hawaiian/Pacific Islander, and all three components, CPS codes 803, 804, 809, 813. Other is excluded from the MNL. Category membership is unchanged. | `manuscript_main.tex:97,105`; notes to Tables 4–5; generator `programs/53-make-identity-distribution-tables.R` |
| 2. Marriage sample | The sample includes Asian–Asian and mixed-parentage couples with a co-resident child and at least one Asian-born parent. The 20,247 WA and 9,055 AW child counts are attributed to descriptive Table 4, not the Table 7 estimation sample. | `manuscript_main.tex:147`; matching R5 quotation |
| 3. One-grandparent share | The former 8 percent statement is corrected to 7 percent, rounded from the unchanged share of about 0.0745. Script 53 emits the numeric fragment, so the prose cannot drift independently of the table. | `manuscript_main.tex:99`; `fragments/one-grandparent-asian-only-percent.tex`; Table 4 CSV |
| 4. Sex differences | Under the reconciled 1,000-replication comparisons, the female AW–WA difference on Asian and White is 9.5 pp, SE 3.0. One-minus-three-grandparent female differences are −11.6 pp, SE 4.6, on White only and 8.2 pp, SE 4.2, on Asian and White; only the former is significant at five percent. These are individual-bootstrap-based normal approximations, not state-clustered tests. | `manuscript_main.tex:131,133`; Tables A.3–A.4; generated `fragments/bootstrap-*.tex` |
| 5. Bootstrap uncertainty | The unsupported threefold multiplier is removed. The dependence warning remains, and the pooled model supplies state-clustered inference about the pooled bias effect and AW–WA contrast. | `manuscript_main.tex:121,131`; response R3 and Other changes |
| 6. Bias units | Prose, captions, Table 8, and Table A.5 use index units. An AME is an average slope per unit, not an exact finite one-unit shift. The numerical step is one-tenth of the child-level sample SD, not necessarily 0.1 index units. | Section 3.2, Section 5, Results, Conclusion, figure captions; table generators 50–51 and refreshed notes |
| 7. State-FE check | Table A.1 is identified as an all-generation LPM including endogamous children and generation controls. The estimate falls from −0.10 to −0.04 while the rounded SE stays at 0.03. State-level confounding remains an explicit limitation. | `manuscript_main.tex:19,153,167`; E2 and R7 responses |
| 8. Policy premise | Text distinguishes hate-crime incident classification based on evidence of offender bias from self-identified population counts used as rate denominators. The "nearly every policy" claim is removed. | `manuscript_main.tex:7,165`; matching E3 response |
| 9. Selection scope | The maternal-college interaction is tied to Asian-father/White-mother families; the WA null and pooled ten-percent result are distinguished. Bootstrap-only grandparent comparisons are qualified in the abstract, introduction, results, and conclusion. | `manuscript_title_etc.tex:31`; `manuscript_main.tex:17,19,133,141,143,163,165` |
| 10. A.2 scale correction | The response and change record disclose that R2 showed log-odds although the note described exponentiated coefficients; the revised table shows odds ratios. This is separate from state clustering. | Response E4, R6, Other changes; `CHANGES.md`, entry 8 |
| 11. Response consistency | Four/seven counts replaced by an uncounted introduction; nonexistent held-back-file claim removed; all five blue quotations exactly match manuscript passages, including "without weights." Median means the 50th percentile. Covariance wording states the code's 0.1 percent tolerance. LPM check is labelled statistically inconclusive. | `submit_journal.tex`; `CHANGES.md`; exact quotation comparison |

The policy classification distinction is supported by the Congressional Research Service, [Federal Data on Hate Crimes in the United States, R46318](https://www.congress.gov/crs_external_products/R/PDF/R46318/R46318.14.pdf), December 16, 2025, especially the Hate Crime Statistics Program section and Table 1.

The correction pass also found a pre-existing Figure 2 caption problem. Panel a contains Hawaii, North Dakota, Tennessee, and Vermont; panel b contains the District of Columbia, North Dakota, South Dakota, and Vermont. The title, caption, and Section 3.2 now list or acknowledge the different sets rather than implying a matched-state comparison. The PNGs were not changed. Harmonizing the state sets would be a separate figure revision, not one of the editor's requested changes.

Verification of the note-only changes: script 53 rebuilt both descriptive tables with identical counts and shares, apart from run-ID metadata. `tools/refresh-table-notes.R` refreshed four Table 8/A.5 copies without executing estimation expressions, and the numeric table bodies and note values remained unchanged. Saved regression CSV hashes also matched the pre-correction files.


## 4. Bootstrap reconciliation

### Recovered source of the mismatch

The Dropbox project root contained the missing child checkpoints. Their metadata records 50 requested and effective replications. Reconstructing all 48 between-model comparisons from those summaries reproduces the former manuscript-matching CSV, to CSV rounding precision. The repository's checkpoint directory and the nested Dropbox checkout instead contain the 1,000-replication summaries; those reproduce all 48 rows of the CSV stored beside them.

| Result set | Replications | AW–WA bias contrast, White only | SE | p-value |
|---|---:|---:|---:|---:|
| Former comparison output; recovered Dropbox-root checkpoints | 50 | 0.106 | 0.044 | 0.016 |
| Reconciled output; `data/raw/bootstrapping-data/` checkpoints | 1,000 | 0.101 | 0.058 | 0.079 |

The recovered child and adult input CSV payloads are byte-identical across the repository's datasets and checkpoint folders and both Dropbox locations. Readable equality-test p-value annotations in all eight submitted marginal-effect panels agree with the 1,000-replication summaries. These are numerical annotation checks, not a claim that PNG hashes match: the stored and submitted images differ in titles or rendering.

Both checkpoint sets, their metadata, the original comparison CSVs, source snapshots, and hashes were preserved read-only under `.safety_backups/bootstrap-recovery-2026-11-05-hqdjmf6o/`. The former active `data/datasets/boot_me_between_model_tests.csv` was moved into the separate local reconciliation backup after its identity with the recovered 50-replication CSV was checked.

### Approved reconciliation

The author selected the existing 1,000-replication run rather than a new bootstrap. The changes are:

- Tables A.3/A.4 and all four comparison panels in Figures A.11/A.12 now use the guarded 1,000-replication output.
- Results, Conclusion, and responses R3/R4 distinguish the weaker separate-model AW–WA comparison from the unchanged pooled state-clustered result.
- Table notes, figure captions, and a Results footnote describe the actual comparison calculation: differences between model-specific median AMEs; each model's approximate SE inferred from its 95% percentile-interval width divided by 3.92; the two SEs combined in quadrature assuming independent samples; two-sided normal tests.
- The comparison plots now use fill-capable symbols. Their previously solid nonsignificant markers are hollow, implementing the captions' five-percent rule.
- Scripts 49 and 54 share a reader that checks requested and effective replication counts, variable/outcome coverage, source metadata, and all 48 numerical comparison rows. It rejects both a genuine 50-replication checkpoint set and a 1,000-replication set paired with the stale CSV. Numerical prose is generated with the tables.

Four comparisons lose five-percent significance; all four remain significant at ten percent:

| Comparison | Covariate and outcome | Former p-value | Reconciled p-value |
|---|---|---:|---:|
| AW minus WA | Bias, White only | 0.016 | 0.079 |
| One minus two Asian grandparents | Maternal college, White only | 0.028 | 0.059 |
| One minus three Asian grandparents | Female, Asian and White | 0.028 | 0.053 |
| One minus three Asian grandparents | Maternal college, Asian and White | 0.017 | 0.059 |

Two one-minus-two-grandparent comparisons also lose ten-percent significance: bias on Asian only and female on Asian and White. All changes, including changes at the one-percent threshold, are retained in `bootstrap-reconciliation.json`. The main pooled state-clustered result is unchanged: AW–WA White-only AME difference 15.1 pp, SE 4.2; clustered Wald χ²(2) = 10.48, p = 0.005.

`bootstrap-comparison-provenance.json` records checkpoint and metadata hashes, seeds, replication counts, and the reconstruction error. `bootstrap-reconciliation.json` records the former and reconciled values, threshold changes, plot checks, and preserved-file hashes. `correction-verification.json` describes the earlier note-only pass, before this numerical reconciliation; its unchanged-figure and unchanged-table checks do not describe the later pass.

### Remaining limits and recommendation

The RDS files retain medians, percentile bounds, and effective-replication counts, not replication-level draws or per-fit convergence histories. The reconstructed comparisons therefore reproduce the historical normal approximation; they are not direct percentile intervals for paired differences, a convergence audit, or a state-clustered bootstrap. The individual-resampling limitation remains explicit throughout the paper.

Review the changed significance statements and the disclosure in responses R3/R4 before submission. A new draw-retaining or state-clustered bootstrap would be new analysis, not recovery of a missing prerequisite for the requested editorial revisions. It is not needed merely to reconcile this package.

## 5. Exhibit map

| R2 exhibit | Revised destination |
|---|---|
| Figures 1 and 2 | Figures 1 and 2 |
| Figures 3 and 4 | Appendix Figures A.2 and A.3 |
| Figures 5 through 8 | Appendix Figures A.4 through A.7 |
| Figure 9 | Figure 3, panel a |
| Figure 10 | Appendix Figure A.8 |
| Figure 11 | Appendix Figure A.9 |
| Figure 12 | Figure 3, panel b |
| Figure 13 | Appendix Figure A.10 |
| Figure 14 | Table 8, Panel A |
| Figure 15 | Table 8, Panel B, including pooled adults |
| Figure 16 | Table 8, Panel C |
| Old Appendix Figure A.1 | Appendix Figure A.1 |
| Old Appendix Figures A.2 and A.3 | Appendix Figures A.11 and A.12; four comparison panels |
| Old Appendix Figure A.4 | Appendix Figure A.13; unchanged scatter plot |
| Tables 1 through 7 and A.1 through A.4 | Same numbers; some contents changed |
| No R2 counterpart | Table 8 and Appendix Table A.5 |

## 6. Using the tracked documents

- `manuscript_tracked.docx` includes the title, abstract, main text, notes, references, main tables, and main figures.
- `appendix_tracked.docx` includes the online appendix, its tables, figures, and references.
- `comparison-inputs/` preserves the baseline and revised clean comparison inputs.
- In Word, select **Review → All Markup** and show insertions and deletions. Changes use native Word revision elements, not merely colored text.
- Similar prose paragraphs are compared word by word; replaced paragraphs use whole-paragraph revisions where needed to preserve boundaries. Changed or moved exhibits appear as complete deleted old and inserted new blocks, including their captions and notes; tables are not cell-level edits. This avoids combining captions from unrelated figures.
- Tables and mathematical expressions are rendered from LaTeX to preserve their contents. Notes are collected at the end. Both versions use the same review layout and the repository's `apa.csl` citation formatter, so these files are review supplements rather than replicas of journal pagination or final production manuscripts.
- The revised comparison input contains both the corrections in §3 and the approved numerical reconciliation in §4. Changed comparison tables and figures appear as complete old/new exhibits; the eight marginal-effect panels and pooled state-clustered table retain their estimates.

Use this package rather than the pre-existing PDF-imported tracked Word files, which contain missing captions, broken table structure, and conversion noise. The older latexdiff PDF also suppresses important removed floats and table-value changes. Any later source edit requires rebuilding the comparison. The retained scripts and command sequence are in `tools/README.md`; the package is published only after the integrity and native-application checks pass.

## 7. Verification and limits

- Clean journal/preprint build results and numerical preservation checks are recorded in `bootstrap-reconciliation.json`. Appendix Table A.5 retains the smaller note font introduced in the earlier correction pass to keep its notes clear of the footer.
- Publication requires both tracked documents to pass exact Accept All and Reject All XML comparisons, including paragraph properties, run formatting, text, links, and image contents.
- Both must also pass native Accept All and Reject All in an isolated LibreOffice 7.5 process, with zero revisions remaining and a match to the corresponding clean input opened in the same application, including paragraph text, notes, and ordered graphic content. The saved DOCX outputs are checked independently before publication.
- Package checks include parsed XML, resolved internal relationships, content types, unique revision and drawing IDs, valid internal links, and unchanged source hashes during the build. `verification.json` records the executed checks and source, tool, and artifact hashes.
- Microsoft Word's live session is not automated. Word may paginate differently. The PDF previews show the tested rendering; the DOCX files contain the actual accept/reject revisions.
- Existing LaTeX margin warnings, an oversized appendix histogram float, and missing citation/footnote hyperlink-destination warnings remain. The same destination warnings are present in the pre-reconciliation build logs. The baseline also retains its duplicate `sub:lw-bias` label. This is not a comprehensive JPAM house-style check.
- These checks establish numerical consistency and comparison integrity, not the correctness of every empirical result. The comparison-run mismatch is resolved; the absence of raw draws and state-clustered bootstrap inference remains as described in §4.
