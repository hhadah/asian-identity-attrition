# Final Word submission checklist

Prepared November 6, 2026. Manuscript: JPAM-2025-13688.R2.

**Status: planning only. No manuscript edits are recorded as completed.**

## How to use this checklist

Work through the sections in order. Every task has two boxes:

- **Edited:** the change has been made in the working Word document, with Track Changes where specified.
- **Verified:** the accepted-changes text, exhibit, or output has been checked against the listed source and the completion criterion. An edit is not finished until both boxes are checked.

Replace `☐` with `☑` as work is completed. Record the Word heading, caption, or distinctive opening words in the completion log at the end. Use page numbers only as a supplement because pagination will change. If an item is already correct, mark it only after verification and record “already correct” in the log. Do not silently skip an item or mark a whole section complete based on one spot check.

The files in `05-jpam-RR-final/` are the content sources, not the editing targets. Leave the LaTeX sources unchanged during this Word-only revision. Use their current corrected contents, including the 1,000-replication reconciliation. Do not use the older tracked exports in that folder as authoritative text.

### Files and sources

Paths below are relative to `submissions/`.

| Role | File |
|---|---|
| Submitted main manuscript baseline | [submit_journal.docx](submit_journal.docx) |
| Submitted appendix baseline | [online-app.docx](online-app.docx) |
| Submitted title/abstract baseline | [title-page.docx](title-page.docx) |
| Previous-round response, not the current response | [point-by-point-response.docx](point-by-point-response.docx) |
| Final main text | [manuscript_main.tex](05-jpam-RR-final/manuscript_main.tex) |
| Final title and abstract | [manuscript_title_etc.tex](05-jpam-RR-final/manuscript_title_etc.tex) |
| Final main exhibits and captions | [manuscript_tab_fig.tex](05-jpam-RR-final/manuscript_tab_fig.tex) |
| Final appendix and captions | [manuscript_online_appendix.tex](05-jpam-RR-final/manuscript_online_appendix.tex) |
| Current response and cover | [submit_journal.tex](05-jpam-RR-final/submit_journal.tex) |
| Complete change record | [CHANGES.md](05-jpam-RR-final/CHANGES.md) |
| Corrected audit and bootstrap reconciliation explanation | [comment_audit.md](05-jpam-RR-final/revision-audit-2026-11-05/comment_audit.md) |
| Editor's four instructions | [JPAM-2025-13688.R2-Comments.docx](05-jpam-RR-final/ref-reports/JPAM-2025-13688.R2-Comments.docx) |
| Decision letter and eight referee comments | [jpam-letter.pdf](05-jpam-RR-final/ref-reports/jpam-letter.pdf) |

In the tables below, `final/` means `05-jpam-RR-final/`. E1a–E1f identify the six exhibit instructions; E2 concerns state fixed effects; E3 concerns policy motivation; E4a concerns the bootstrap; E4b concerns clustering and table notes. R1–R8 identify the referee comments. “Correction” means an additional correction disclosed in the current response.

## 1. Preserve baselines and establish working copies

Recommended output directory: `word-final-submission/`, alongside this checklist. It is a proposed destination, not a claim that these outputs already exist.

| ID | Edited | Verified | Task and completion criterion |
|---|:---:|:---:|---|
| S01 | ☐ | ☐ | Preserve the four submitted DOCX files untouched. Record baseline checksums before editing so unchanged originals can be verified at the end. |
| S02 | ☐ | ☐ | Create working copies named `manuscript_final_tracked.docx`, `appendix_final_tracked.docx`, and `title-page_final_tracked.docx`. Enable Track Changes before the first edit and use a consistent revision author. |
| S03 | ☐ | ☐ | Inspect the working documents in Word before editing. The main baseline contains 36 text boxes; text extraction also shows split words and fragmented equations. Identify layout-sensitive content. Do not perform a blanket reformat or global hyphen removal. |

## 2. Revise the main manuscript text

Source for this section: `final/manuscript_main.tex`, unless otherwise stated. Match by section and opening words, not by baseline page number.

| ID | Edited | Verified | Word location and required change | Completion criterion | Addresses |
|---|:---:|:---:|---|---|---|
| M01 | ☐ | ☐ | Introduction, opening paragraph: transfer the revised policy motivation. | Connect self-reported population counts and disparity measures to policy. Distinguish hate-crime incident classification from population denominators; do not claim that nearly every policy depends on self-identification. | E3 |
| M02 | ☐ | ☐ | Introduction, results summary and policy-stakes paragraph. | Distinguish state-clustered pooled evidence from individual-bootstrap comparisons; qualify grandparent findings, possible state confounding, and the family-specific education-selection result. | E2, E3, R3, R7 |
| M03 | ☐ | ☐ | Section 3.2, bias-index interpretation and variation. | Explain IAT D-score units versus empirical SDs. Include SDs of 0.42 across state-years, 0.18 between states, 0.39 within states, and 0.26 across states within years; one index unit is about 2.4 state-year SDs. Include two-year blocks and unbalanced coverage. | R1 |
| M04 | ☐ | ☐ | Section 3.2, Figure 2 and map discussion. | Describe the different place sets in Figure 2's panels. Change map references to A.2–A.3. Match the corrected caption under F02. | E1a, Correction |
| M05 | ☐ | ☐ | Section 4, opening distribution paragraphs. | Replace the old binary Asian/not-only-Asian summary with the distributions in revised Tables 4–5. Check all numbers against the numerical control table below. Refer to moved histograms in an appendix footnote. | R2, E1b |
| M06 | ☐ | ☐ | Section 4 category footnote and Section 5 outcome definition. | “Asian and White” pools White–Asian, White–Hawaiian/Pacific Islander, Asian–Hawaiian/Pacific Islander, and all three components, CPS codes 803, 804, 809, 813. “Other” is excluded from the MNL. Do not imply category membership was changed by this clarification. | R2, Correction |
| M07 | ☐ | ☐ | End of Section 5, replace paragraph beginning “Since log odds coefficients…”. | State that bias AMEs average individual derivatives at each child's covariates, without weights. Binary covariates use 0-to-1 changes. Numerical step is one-tenth of the sample SD across children, not 0.1 index units; retain the endpoint-truncation qualification. An AME is a slope per index unit, not an exact finite one-unit change. | E4a, R3 |
| M08 | ☐ | ☐ | End of Section 5, bootstrap calculation and limitations. | Explain 1,000 individual resamples within region × year, re-estimation, medians across sample-average AMEs, and 2.5th/97.5th percentile intervals. These are not medians across children. State that individual resampling does not account for within-state dependence and point to the pooled state-clustered model. Do not import the unsupported threefold bootstrap-uncertainty multiplier. | E4a, R3 |
| M09 | ☐ | ☐ | Section 6 opening and second-generation pooled results. | Use Figure 3a/3b and Appendix Figures A.8–A.10. Correct bias-effect units. Add the pooled common-effect White-only AME of 14.6 pp per index unit, SE 5.7, and the offsetting declines of 12.3 pp for Asian only and 2.4 pp for Asian and White. | E1, R3 |
| M10 | ☐ | ☐ | Section 6, AW–WA comparison paragraphs and footnotes. | Transfer the current separate-model and pooled-model comparison discussion. Explain covariate-distribution and MNL-scale issues. Keep the reconciled 10.1 pp comparison distinct from the pooled state-clustered 15.1 pp comparison. Identify the LPM interaction −0.08, SE 0.06, as statistically inconclusive. | R4, R3 |
| M11 | ☐ | ☐ | Section 6, between-model test calculation and sex differences. | Explain approximate SEs from percentile-interval widths divided by 3.92, combination in quadrature under independence, and two-sided normal tests. These are not direct bootstrap percentile intervals for paired differences. Report the AW–WA female difference of 9.5 pp, SE 3.0, for Asian and White; qualify the individual-bootstrap inference. | R3, R4, Correction |
| M12 | ☐ | ☐ | Section 6, third-generation results. | Transfer the current comparison and significance statements, including weaker maternal-college comparisons. Remove “Gender effects never differ across groups.” Report the one-minus-three female differences of −11.6 pp, SE 4.6, p = 0.011, for White only and 8.2 pp, SE 4.2, p = 0.053, for Asian and White. State that these comparisons lack state-clustered inference. | R3, Correction |
| M13 | ☐ | ☐ | Interaction subsection, specification and results. | Replace Figures 14–16 with Table 8 Panels A–C. Transfer current coefficients, SEs, and significance statements. Describe adults' family income correctly. Check against N06–N10 below and the complete table. | E1f |
| M14 | ☐ | ☐ | Interaction subsection, selection interpretation. | Tie the supported maternal-college selection pattern to Asian-father families. Distinguish the WA null, pooled-child 10% result, pooled-adult null, and imprecise grandparent subgroup interactions. Do not generalize the result to every family type. | E3, Correction |
| M15 | ☐ | ☐ | Robustness, marriage paragraph and disclosure footnote. | Explain the correction from household to state clustering; none of the three bias coefficients is now statistically significant. Discuss non-marriage and co-resident-child selection. Include Asian–Asian and mixed-parentage couples. Attribute the 20,247 WA and 9,055 AW child counts to descriptive Table 4, not the Table 7 estimation sample. | E4b, R5, R6 |
| M16 | ☐ | ☐ | Robustness, migration paragraph. | Change the data source to ACS and the movers' estimate to 0.02 index units, SE 0.04, statistically indistinguishable from zero. Do not retain “0.06 standard deviations.” | Correction |
| M17 | ☐ | ☐ | Robustness, replace paragraph beginning “Moreover, the results are also robust to fixed state characteristics.” | Identify A.1 as an all-generation LPM with endogamous children and generation controls, not the headline mixed-race MNL. Explain −0.10 to −0.04 with the same rounded SE of 0.03; include the interval and variation discussion from the final text. Leave state-level confounding open. Do not attribute the loss of significance to a larger reported SE. | E2, R7 |
| M18 | ☐ | ☐ | Conclusion, results, selection, policy, and limitations. | Synchronize with M02 and M09–M17. Include both AW–WA comparisons with their different inferential bases, the grandparent caveat, family-specific selection, conditional disparity implications, and explicit state-FE limitation. | E2, E3, R3, R4, R7 |
| M19 | ☐ | ☐ | Main references; source `final/AttitudesPaper.bib`. | Add the cited OMB 2024 standards revision, COVID-19 Hate Crimes Act, and Waters integration volume. Verify author/year/title and in-text citation matches. Remove duplicate entries only if present; do not replace the entire bibliography unnecessarily. | E3, Correction |

## 3. Revise main tables

Transfer the entire current table and notes where values or structure changed. Prefer editable Word tables. For redesigned tables, use complete tracked old-table deletion and new-table insertion rather than an unreadable series of cell moves.

| ID | Edited | Verified | Task and source | Completion criterion | Addresses |
|---|:---:|:---:|---|---|---|
| T01 | ☐ | ☐ | Replace Table 4 from `final/tab02-observations-by-gen.tex`. | One total count per row; shares for Asian only, White only, Asian and White, Other; complete ancestry rows and corrected category notes. Check every numeric cell and row label. | R2 |
| T02 | ☐ | ☐ | Replace Table 5 from `final/tab54-observations-by-gen-adults.tex`. | Same distribution structure for adults, with correct sample and category notes. Check every numeric cell and row label. | R2 |
| T03 | ☐ | ☐ | Revise Table 6 notes from `final/tab46-iat-migration-tab.tex`. | Retain estimates; number notes 1–3; correct “a Asian country”; identify ACS/IPUMS USA rather than “Census Data.” | E4b, R6, Correction |
| T04 | ☐ | ☐ | Replace Table 7 from `final/tab41a-marriage.tex`. | Transfer every corrected SE and significance marker, not just the bias row or clustering note. Bias SEs are 0.03 in all three columns, with no significant bias coefficients. Notes say state clustering and are numbered 1–2. | E4b, R6 |
| T05 | ☐ | ☐ | Add Table 8 from `final/tab50-heterogeneity-interactions.tex`. | Title: “Heterogeneity in the Effect of Anti-Asian Bias on Asian Identification.” Three panels: mixed-race children, mixed-race adults, third generation. Include requested subgroups and pooled rows, coefficients, state-clustered SEs, N, and full notes. Adult interaction sample is ASEC because family income is used. | E1f, R8 |

## 4. Reorganize figures

Move captions and notes with the image panels. Across separate files, a move is a tracked deletion from the main manuscript plus a tracked insertion into the appendix. Check both ends before marking Verified.

| ID | Edited | Verified | Existing exhibit | Final destination and completion criterion | Addresses |
|---|:---:|:---:|---|---|---|
| F01 | ☐ | ☐ | Main Figure 1 | Retain unchanged; confirm diagram, caption, and reference remain intact. | Preservation |
| F02 | ☐ | ☐ | Main Figure 2 | Retain. Transfer corrected title/caption and index units. Panel a shows Hawaii, North Dakota, Tennessee, Vermont; panel b shows DC, North Dakota, South Dakota, Vermont. Do not describe matched state sets. Images are unchanged. | E1a, R1, Correction |
| F03 | ☐ | ☐ | Main Figures 3–4 | Move to Appendix Figures A.2–A.3, with current captions and notes. | E1a, R8 |
| F04 | ☐ | ☐ | Main Figures 5–8 | Move to Appendix Figures A.4–A.7, with captions and notes. | E1b, R8 |
| F05 | ☐ | ☐ | Main Figures 9 and 12 | Combine as main Figure 3, “Marginal Effects of Key Covariates on Racial Identity Choice.” Panel a: “Mixed-Race Second-Generation Asian Americans”; panel b: “Third-Generation Asian Americans.” Use the current AME/bootstrap caption from `final/manuscript_tab_fig.tex`. | E1c |
| F06 | ☐ | ☐ | Main Figure 10 | Move to Appendix Figure A.8 with current corrected caption. | E1d, R8 |
| F07 | ☐ | ☐ | Main Figure 11 | Move to Appendix Figure A.9 with current corrected caption. | E1e, R8 |
| F08 | ☐ | ☐ | Main Figure 13 | Move to Appendix Figure A.10 with current corrected caption. | E1d, R8 |
| F09 | ☐ | ☐ | Main Figures 14–16 | Delete, including obsolete captions and notes; Table 8 replaces them. Do not relocate these figures to the appendix. | E1f, R8 |

## 5. Revise the online appendix

Source: `final/manuscript_online_appendix.tex` and the table files listed below. Complete the incoming figure moves under F03–F08 as part of this document's revision.

| ID | Edited | Verified | Task and source | Completion criterion | Addresses |
|---|:---:|:---:|---|---|---|
| A01 | ☐ | ☐ | Preserve Table A.1 and original Figure A.1. | Numerical contents of A.1 and the IAT-example images remain unchanged. Corrected main-text interpretation points to the correct A.1 columns. | E2, R7, Preservation |
| A02 | ☐ | ☐ | Replace Table A.2 from `final/tab47-marriage.tex`. | Use the current state-clustered table displaying odds ratios. Do not retain old log-odds cells under an exponentiated-coefficient note. Verify every cell and note. | R6, Correction |
| A03 | ☐ | ☐ | Replace Tables A.3–A.4 from `final/tab-between-model-second-gen.tex` and `final/tab-between-model-third-gen.tex`. | Use the reconciled 1,000-replication results. Check all 48 comparison rows, SEs, p-values/stars, labels, and notes; do not update only values mentioned in prose. | R3, R4, Correction |
| A04 | ☐ | ☐ | Add Table A.5 from `final/tab51-pooled-mnl-bias-x-aw.tex`. | Include the pooled model, bias × AW terms, state-clustered inference, Wald and independence-based LR tests, common-covariate AMEs, and full notes. Keep notes legible and clear of the footer. | R4, R3 |
| A05 | ☐ | ☐ | Old Appendix Figures A.2–A.3 become A.11–A.12. | Replace all four comparison panels with current `between_model_second_AW_WA.png`, `between_model_third_1v2GP.png`, `between_model_third_1v3GP.png`, and `between_model_third_2v3GP.png` from `final/`. Check hollow/filled markers against the five-percent rule and current tables. | R3, R4, Correction |
| A06 | ☐ | ☐ | Old Appendix Figure A.4 becomes A.13. | Preserve the scatter plot; update its caption number and all references. | E1, Preservation |
| A07 | ☐ | ☐ | Appendix captions, table notes, and internal references. | Match current index units, AME definitions, bootstrap limits, and approximate comparison-test calculation. Keep the eight marginal-effect image panels unchanged. Check appendix citations/references and all moved exhibit references. | E4a, R3, R8 |

## 6. Title page, abstract, and current response

| ID | Edited | Verified | Task and source | Completion criterion |
|---|:---:|:---:|---|---|
| C01 | ☐ | ☐ | Revise the abstract in the title-page working copy from `final/manuscript_title_etc.tex`. | Transfer the qualifications on grandparent comparisons and selective identification. Ensure the abstract agrees with the revised main text, not the old unconditional selection claim. |
| C02 | ☐ | ☐ | Revise cover/submission description using `final/submit_journal.tex`. | Describe the final revision after conditional acceptance and the separate tracked review supplement. Preserve author details and other unchanged statements unless separately authorized. |
| C03 | ☐ | ☐ | Create a clean current-round `response_final.docx` from the current response section in `final/submit_journal.tex`. | Address E1a–E1f, E2, E3, E4a–E4b, and R1–R8. Do not submit the previous-round response as the current response. No tracked comparison of the old response is required. |
| C04 | ☐ | ☐ | Verify the current response's disclosures and quoted passages. | Explain the household-to-state clustering correction, A.2 scale correction, Figure 2 place-set clarification, and 50-to-1,000-replication reconciliation. Quotes match the final accepted Word text exactly. Describe the approximate tests honestly; do not claim raw bootstrap draws, a new bootstrap, or state-clustered bootstrap inference. |

## 7. Numerical control table

This is a transcription control, not a substitute for checking complete tables. The first five rows are shares for the specified identity definition, not interchangeable with the old binary outcome. Bias slopes and contrasts are per index unit. SEs are in the same units as estimates.

| Control | Quantity | Final value / inference | Verify in |
|---|---|---|---|
| N01 | Children reporting Asian only, first/second/third generation | 94% / 72% / 29% | M05, T01 |
| N02 | Second-generation children, two/one Asian-born parents, Asian only | 96% / 31% | M05, T01 |
| N03 | Second-generation adults, two/one Asian-born parents, Asian only | 93% / 33% | M05, T02 |
| N04 | Third generation, four/three/two/one Asian grandparents, Asian only | 93% / 74% / 24% / 7% | M05, T01 |
| N05 | One Asian grandparent, White only | 54% | M05, T01 |
| N06 | AW-child bias × maternal college | −0.23, SE 0.10 | M13, T05, M18 |
| N07 | AW-adult bias × own college | −0.08, SE 0.05; significant at 10% | M13, T05 |
| N08 | Pooled-child bias × maternal college | −0.07, SE 0.03; significant only at 10% | M13, T05 |
| N09 | Pooled-third-generation bias × maternal college | −0.07, SE 0.03 | M13, T05 |
| N10 | Table 8 pooled-adult interactions: female / college / family income | −0.012 (0.033) / −0.023 (0.039) / −0.020 (0.040); N = 4,686; 51 state clusters | T05 |
| N11 | Separate-model AW–WA bias contrast, White only | 10.1 pp, SE 5.8, p = 0.079; significant at 10%, not 5% | M10, M18, A03, A05 |
| N12 | Pooled state-clustered AW–WA White-only contrast | 15.1 pp, SE 4.2; joint clustered Wald χ²(2) = 10.48, p = 0.005 | M10, M18, A04 |
| N13 | Pooled common-effect White-only AME | 14.6 pp, SE 5.7 | M09, A04 |
| N14 | AW–WA female contrast, Asian and White | 9.5 pp, SE 3.0 | M11, A03 |
| N15 | One-minus-three female contrast, White only | −11.6 pp, SE 4.6, p = 0.011 | M12, A03 |
| N16 | One-minus-three female contrast, Asian and White | 8.2 pp, SE 4.2, p = 0.053; significant only at 10% | M12, A03, A05 |
| N17 | Table 7 bias SEs | 0.03 / 0.03 / 0.03; no significant bias coefficients | M15, T04 |
| N18 | Movers' bias difference | 0.02 index units, SE 0.04; statistically indistinguishable from zero | M16, T03 |
| N19 | State-FE check, A.1 columns 7 to 8 | −0.10 to −0.04; rounded SE remains 0.03 | M17, M18, A01 |

Important: N11 and N12 use different comparisons and inference. Do not substitute one for the other. The previous 10.6 pp, SE 4.4, p = 0.016 result is obsolete for the final comparison; it may appear only in tracked deletions or an explicit historical correction disclosure.

## 8. Final verification and submission outputs

Run searches on the **accepted-changes view or a clean copy**. Deleted text will legitimately retain superseded wording in All Markup.

| ID | Edited | Verified | Check | Passing condition |
|---|:---:|:---:|---|---|
| Q01 | ☐ | ☐ | Cross-document coverage | Every M, T, F, A, and C item has both boxes checked and a completion-log entry. The editor/referee crosswalk below has no unaddressed comment. |
| Q02 | ☐ | ☐ | Numerical reconciliation | Check N01–N19 everywhere each is reported. Check complete replacement table bodies and notes, not only rounded prose. All 48 between-model comparison rows match the current sources. |
| Q03 | ☐ | ☐ | Stale claims and values | Search for “robust to fixed state,” “robust to the inclusion,” “gender effects never,” “0.06,” “10.6,” “0.016,” “8%,” “0.20,” “0.04,” “0.05,” “household,” and “three.” Review each hit in context; these are flags, not global replacements. No obsolete substantive statement survives in accepted text. Valid values and historical disclosures remain. |
| Q04 | ☐ | ☐ | Units and definitions | Search “standard deviation,” “SD,” “one-unit,” “median,” “Asian and White,” “US-born White,” and “income.” Each use matches the relevant definition. Retain real SD decomposition values; do not mechanically replace every occurrence of “standard deviation.” |
| Q05 | ☐ | ☐ | Exhibit inventory and references | Accepted main manuscript has Tables 1–8 and Figures 1–3; accepted appendix has Tables A.1–A.5 and Figures A.1–A.13. No missing or duplicate captions. Every in-text reference points to the intended exhibit, including Figure 3a/3b. Old Figures 14–16 are absent from accepted files. |
| Q06 | ☐ | ☐ | Preserve unchanged content | Confirm untouched main tables, theoretical material, equations, images, citations, and other unchanged content were not accidentally altered. Recheck original-file checksums from S01. Any necessary conversion repair is explicitly logged and tracked. |
| Q07 | ☐ | ☐ | Review Word rendering | Open each working DOCX in Word. Inspect all changed pages and all equations, tables, captions, footnotes, text boxes, links, and page breaks. Check image resolution, table editability, clipping, and notes overlapping footers. Do not rely on extracted text alone. |
| Q08 | ☐ | ☐ | Verify native tracked changes | In disposable copies, Reject All restores baseline content and exhibits; Accept All produces the intended final content and exhibits. Check text boxes, tables, images, and notes as well as paragraphs. Do not accept or reject changes in the only tracked master. |
| Q09 | ☐ | ☐ | Produce clean copies | Create `manuscript_final_clean.docx`, `appendix_final_clean.docx`, and `title-page_final_clean.docx` by accepting changes in copies. Verify no revisions or internal drafting comments remain. Clean content matches the accepted tracked masters. |
| Q10 | ☐ | ☐ | Final response and journal style | Recheck response quotations after all edits. Consult current JPAM author instructions and submission fields. Ensure the clean documents meet required style; submit tracked versions as review supplements, not substitutes for clean manuscripts. Do not claim style compliance before checking it. |
| Q11 | ☐ | ☐ | Record final files and completion | Enter final output paths and verification date in the log. Keep original submissions, tracked masters, clean copies, and current response distinguishable. Upload only the required current files, not obsolete review exports or this internal checklist. |

### Comment coverage crosswalk

| Comment | Checklist items that must be complete |
|---|---|
| E1a: retain one bias figure, move two | M04, F02, F03, Q05 |
| E1b: move identity histograms | M05, F04, Q05 |
| E1c: combine Figures 9 and 12 | M09, F05, Q05 |
| E1d: move Figures 10 and 13 | F06, F08, Q05 |
| E1e: move Figure 11 | F07, Q05 |
| E1f: replace Figures 14–16 with three-panel table including pooled adults | M13, M14, T05, F09 |
| E2: state-FE limitation | M02, M17, M18, A01 |
| E3: policy motivation | M01, M02, M14, M18, M19 |
| E4a: bootstrap clarification | M07, M08, M11, A07 |
| E4b: clustering and consecutive notes | M15, T03, T04 |
| R1: variation in bias | M03, M04, F02 |
| R2: identity distributions | M05, M06, T01, T02 |
| R3: bootstrap | M07–M12, A03, A05, A07, C04 |
| R4: AW–WA comparison | M10, M11, M18, A03, A04, A05 |
| R5: marriage asymmetry | M15 |
| R6: clustering and table notes | M15, T03, T04, A02, C04 |
| R7: state fixed effects | M02, M17, M18, A01 |
| R8: exhibit consolidation | T05, F03–F09, A06, Q05 |
| Current-round response to every comment | C03, C04, Q10 |

## Completion log

Add one row per completed task ID. Record exceptions or unresolved questions here immediately. An unresolved item keeps its Verified box unchecked.

| Task ID | Working/output file | Word heading, caption, or opening words | Edit date | Verification performed and result | Outstanding issue |
|---|---|---|---|---|---|

### Final sign-off

- [ ] Every task has both Edited and Verified checked, with supporting log entries.
- [ ] Every editor and referee comment is addressed in both the documents and the current response.
- [ ] All tracked and clean deliverables passed the checks above.
- [ ] Original submitted files and final-folder LaTeX sources remain unchanged.
- [ ] No unresolved issue remains in the completion log.

Verified by: ____________________

Date: ____________________

Final output directory: ____________________
