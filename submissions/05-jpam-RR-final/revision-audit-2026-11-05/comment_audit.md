# JPAM final-revision audit

Audit date: November 5, 2026. Manuscript: JPAM-2025-13688.R2.

## Verdict

The revised folder contains a direct response to every editorial instruction. The main paper has three figures rather than sixteen; the requested three-panel heterogeneity table and pooled adult estimates are present; the state-fixed-effects limitation, policy motivation, bootstrap explanation, and state-clustering correction are all present.

**All eleven manuscript and response corrections identified by the audit have now been applied.** The remaining empirical issue is provenance: the saved bootstrap checkpoints do not match the submitted comparison table, and the matching checkpoint set has not been recovered. Section 4 gives the evidence, searched locations, and recovery recommendation.

The final-round sources and table notes were corrected with the author's approval. The R2 sources, regression estimates, estimation samples, and submitted figure PNGs are unchanged. Descriptive tables were rebuilt and their counts and shares matched the prior manifests exactly. No model was re-estimated and no bootstrap was rerun.

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
| E4a | Clarify the bootstrap calculation at the end of Section 5 | Text and captions distinguish the average individual derivative from the median across replications, state the binary 0-to-1 calculation, resampling unit, strata, replication count, percentile interval, numerical step, and units. The unsupported uncertainty multiplier is removed. `manuscript_main.tex:121`; `manuscript_tab_fig.tex:149`. | Implemented; separate provenance limit in §4 |
| E4b | Check Table 7 clustering, explain if it was not a typo, and number Table 6/7 notes consecutively | The code uses state clustering. Table 7 bias SEs rise from 0.01 to 0.03; the three bias coefficients are no longer significant. The main text discloses that household clustering was real, not merely a note typo. Table 6 notes are 1–3 and Table 7 notes are 1–2. `tab41a-marriage.tex:12–13,26–27`; `tab46-iat-migration-tab.tex:33–37`; `manuscript_main.tex:147`; `programs/17-table-eight-endo-interethnic-reg.R:21–36`. | Implemented |

The saved Table 8 pooled-adult row reports female −0.012, SE 0.033; college graduate −0.023, SE 0.039; above-median income −0.020, SE 0.040. It uses 4,686 observations and 51 state clusters. The manifest identifies run `20261025-113240`. The table note explains that the adult interaction sample is restricted to ASEC respondents because it uses family income.

## 2. Referee comments required by the decision letter

| Comment | Response verified | Status after correction |
|---|---|---|
| R1: variation in the bias index | Section 3.2 reports the state-year SD decomposition and observation timing. One index unit is now distinguished from one empirical SD throughout the text, captions, and table notes. | Addressed |
| R2: identity distributions in Tables 4 and 5 | Tables report one count and shares in the MNL categories plus Other. Notes now enumerate the exact pooled CPS categories; the one-grandparent percentage is generated from Table 4. | Addressed; numeric cells unchanged |
| R3: bootstrap | Section 5 and captions explain the calculation. Retaining the median is disclosed, units are corrected, and clustered-versus-conventional SE ratios are not presented as bootstrap uncertainty multipliers. | Addressed; checkpoint provenance remains separate |
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
| 4. Sex differences | Text acknowledges the A.3 female AW–WA difference of 0.097, SE 0.028, on Asian and White, and A.4 one-versus-three-grandparent female differences of −0.118, SE 0.043, on White only and 0.087, SE 0.040, on Asian and White. These use individual-bootstrap inference, not state-clustered inference. | `manuscript_main.tex:131,133`; Tables A.3–A.4 |
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


## 4. Bootstrap provenance limitation

The saved comparison table used in the paper is not the same as the older comparison output stored beside the bootstrap checkpoints:

| Location | AW–WA bias contrast, White only | SE | p-value |
|---|---:|---:|---:|
| `data/datasets/boot_me_between_model_tests.csv`, matching the manuscript table | 0.106 | 0.044 | 0.016 |
| `data/raw/bootstrapping-data/boot_me_between_model_tests.csv` | 0.101 | 0.058 | 0.079 |

The saved RDS summaries beside the accessible checkpoints belong to the older output set. The submitted PNGs have different hashes from the PNGs there. This does not establish that the submitted figures are wrong: rendering settings can change PNG hashes. The CSVs also contain different component marginal effects, however, so their numerical difference is not merely a formatting change.

### Follow-up investigation

`notes/results.txt` records a completed 1,000-replication child and adult bootstrap on `Alis-Mac-mini`, in `/Users/alisaffouri/Downloads/asian-identity-attrition`. The transcript reports the older AW–WA White-only contrast, 0.1010 with p = 0.0790. Metadata beside the preserved checkpoints dates that run to April 29, 2026. This identifies the machine and path for the older run, not the origin of the manuscript-matching output.

The code uses two output locations. `programs/44-bootstrap-run.R` writes checkpoints and metadata to `git_mdir`, the checkout root. `programs/45-bootstrap-plot.R` reads checkpoints and writes figures and the comparison CSV under `bootstrap_results_path`, normally `data/raw/bootstrapping-data`. A successful run therefore required a transfer or a path override that the repository does not document. This is a possible reason that some checkpoints were not collected, not proof that the missing output was deleted.

The follow-up searched the current repository including ignored files, raw and derived data, output directories, submission rounds, notes and logs, `.safety_backups/20251015-220459/untracked.tar.gz`, and accessible Downloads and Git project locations. No matching additional checkpoint set was found. The recorded `/Users/alisaffouri/...` checkout is absent from this workstation, and no SSH hosts are configured.

### Remaining prerequisite and recommendation

Recover a read-only archive of the original checkout or its backup, searching both its root and `data/raw/bootstrapping-data`. Prioritize `boot_results_second_AW.rds`, `boot_results_second_WA.rds`, the remaining child and adult summaries, their metadata, and `boot_me_between_model_tests.csv`. The matching CSV's White-only contrast is 0.105634923179102, SE 0.0439472910134151, p = 0.01623127855465. Preserve the entire recovered run, not just that row, and reconcile its summaries to the submitted tables and figures.

Until those files are recovered, complete numerical provenance remains unverified. Do not regenerate the submitted figures from the older checkpoints or substitute their numbers. If recovery fails, the next decision is whether to run the bootstrap from frozen inputs to create a new, fully documented result set. Such a run would create new results, not recover the provenance of the submitted PNGs, and would require reconciling every affected table, figure, and claim before submission.

No new empirical run is necessary merely to answer the editor's requested revisions: the pooled adult model, pooled MNL comparison, state-clustered marriage estimates, and revised descriptive tables already have saved outputs. A state-clustered bootstrap or a state-FE mixed-race MNL would be additional analysis, not an unfulfilled instruction to rerun the existing bootstrap.

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
| Old Appendix Figures A.2 through A.4 | Appendix Figures A.11 through A.13 |
| Tables 1 through 7 and A.1 through A.4 | Same numbers; some contents changed |
| No R2 counterpart | Table 8 and Appendix Table A.5 |

## 6. Using the tracked documents

- `manuscript_tracked.docx` includes the title, abstract, main text, notes, references, main tables, and main figures.
- `appendix_tracked.docx` includes the online appendix, its tables, figures, and references.
- `comparison-inputs/` preserves the baseline and revised clean comparison inputs.
- In Word, select **Review → All Markup** and show insertions and deletions. Changes use native Word revision elements, not merely colored text.
- Similar prose paragraphs are compared word by word; replaced paragraphs use whole-paragraph revisions where needed to preserve boundaries. Changed or moved exhibits appear as complete deleted old and inserted new blocks, including their captions and notes; tables are not cell-level edits. This avoids combining captions from unrelated figures.
- Tables and mathematical expressions are rendered from LaTeX to preserve their contents. Notes are collected at the end. Both versions use the same review layout and the repository's `apa.csl` citation formatter, so these files are review supplements rather than replicas of journal pagination or final production manuscripts.
- The revised comparison input now contains the approved corrections in §3. It does not change the submitted estimates or figure PNGs or resolve their outstanding bootstrap provenance.

Use this package rather than the pre-existing PDF-imported tracked Word files, which contain missing captions, broken table structure, and conversion noise. The older latexdiff PDF also suppresses important removed floats and table-value changes. Any later source edit requires rebuilding the comparison. The retained scripts and command sequence are in `tools/README.md`; the package is published only after the integrity and native-application checks pass.

## 7. Verification and limits

- The corrected journal and preprint sources compiled in an isolated directory with no unresolved references or citations. The generated 7 percent statement, corrected table notes, Figure 2 caption, and response text were checked in the rendered journal PDF. Appendix Table A.5's note font was reduced to keep its text clear of the page footer.
- Both tracked documents passed exact Accept All and Reject All XML comparisons, including paragraph properties, run formatting, text, links, and image contents.
- Both also passed native Accept All and Reject All in an isolated LibreOffice 7.5 process. Each result had zero revisions remaining and matched the corresponding clean input opened in the same application, including paragraph text, notes, and ordered graphic content. Publication additionally requires an independent check of the saved DOCX outputs.
- Package checks include parsed XML, resolved internal relationships, content types, unique revision and drawing IDs, valid internal links, and unchanged source hashes during the build. `verification.json` records the executed checks and source, tool, and artifact hashes.
- Microsoft Word's live session is not automated. Word may paginate differently. The PDF previews show the tested rendering; the DOCX files contain the actual accept/reject revisions.
- Existing LaTeX margin warnings and an oversized appendix histogram float remain. The baseline also retains its duplicate `sub:lw-bias` label. This is not a comprehensive JPAM house-style check.
- These checks establish comparison integrity, not the correctness of every empirical result. Complete bootstrap provenance remains unverified as described in §4. Recovering the matching output set is the recommended next step before submission.
