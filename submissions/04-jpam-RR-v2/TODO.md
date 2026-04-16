# JPAM R&R v2 — To-Do List

**Manuscript:** JPAM-2025-13688.R1 — *"The Effect of Racial and Ethnic Attitudes on Asian Identity in the U.S."*
**Decision:** Major Revision (10-Mar-2026)
**Editor:** Alice Chen (Co-Editor)

---

## Editor's Note (Alice Chen)

- [ ] **Include the interaction between bias and income in the main text** (currently interactions with parental education are in the Appendix; per editor, move/add the income × bias interaction into the main text).
- [ ] If any Reviewer 2 suggestion is **not** incorporated, provide a clear rationale in the response memo.
- [ ] Prepare a response memo documenting how each concern was addressed.

---

## Reviewer 1 (minor)

- [ ] **Explicitly list the ANES survey items used to measure racial animus** (e.g., ethnocentrism, racial resentment, feeling thermometers, or a combination). A footnote or appendix is sufficient.

---

## Reviewer 2 (major, structural)

### Framing / narrative

- [ ] **Reframe the paper's setup early and clearly:**
  - Show quickly that when all measured ancestors are Asian, individuals almost always report themselves as Asian.
  - Show that when at least one parent or grandparent is non-Asian, a substantial fraction identify as something other than *solely* Asian.
  - **Avoid "non-Asian" phrasing** — the choice is between solely Asian vs. mixed identity (Kamala Harris identifies as *both* Black and Indian, not as non-Indian or non-Black).

### Sample / scope

- [ ] **Drop individuals born in (East) Asia or with two Asian parents** from the analysis — this group is ~96% Asian-identifying, so there is little variation to explain; including them produces implausibly large anti-Asian-sentiment effects that detract from the story.

### Model choice

- [ ] **Skip the binary choice models** and move straight to the **multinomial logit** (where all the interesting results are located). Streamline accordingly.

### Statistical significance

- [ ] **Be much more careful about significance claims.** "Significant vs. not significant" ≠ "significantly different from each other."
- [ ] **Test WA vs. AW differences directly.** Since few WA–AW differences are statistically significant:
  - Either report point estimates as "suggestive" if a coherent story is supported, **or**
  - State explicitly that WA–AW estimates were not statistically significantly different and **combine them into a single mixed-race category**.

### Cohesion / length

- [ ] **Streamline and shorten** the paper substantially given the changes above (drop two-Asian-parent group, drop binary models, collapse WA/AW where appropriate).

### Identity × income (consistency with stated motivation)

- [ ] **Resolve inconsistency between introduction and analysis.** The intro claims:
  - "if individuals respond to prejudice by avoiding Asian racial identification, conventional analyses of racial gaps may systematically underestimate disparities in the most prejudiced areas"
  - "identity choices may influence measured labor market trajectories among racial groups…"
- [ ] **Estimate the effect of income interacted with prejudice** on identity choice (direction is not knowable a priori; this is the object the intro asks about).
- [ ] **Quantify the magnitude of the effect on estimated income of Asians in the U.S.** — i.e., show how much identity attrition biases measured Asian–non-Asian income gaps. (Editor flags this as addressable via the bias × income interaction in the main text.)

### Lesser points

- [ ] **Confirm/state that other mixed-race forms (e.g., Black/Asian) were too small to include.** Add a brief note or footnote.
- [ ] **Figures 2A and 3 — address within-state changes:**
  - The large within-state changes appear implausible.
  - Attempt **state fixed-effects** specifications; if they render coefficients too imprecise, state this explicitly.

---

## Submission checklist

- [ ] Write response-to-reviewers memo (point-by-point).
- [ ] Update `submit_journal.tex` / `manuscript_main.tex` to reflect restructuring.
- [ ] Update tables/figures: drop 2-Asian-parent group; collapse WA/AW where warranted; add income × bias interaction to main text; add state-FE robustness for Figures 2A & 3.
- [ ] List ANES racial-animus items (footnote or appendix).
- [ ] Rebuild PDF and point-by-point response document.
- [ ] Submit revision via ScholarOne link (within one year of 10-Mar-2026).

---

## Detailed Suggestions for Addressing Each Comment

### Editor (Alice Chen)

#### 1. Include income × bias interaction in the main text

The editor specifically asks you to move/add the income × bias interaction into the main text (not just appendix). Currently the appendix has interaction plots for gender and parental education (Figures A.5–A.6), and the adult regressions already include inverse hyperbolic sine of income as a control. Concrete steps:

- [ ] **Run new regressions** that interact the bias measure with household income (or IHS of income) for the adult sample — at minimum for AW and WA second-generation adults, and ideally for third-generation as well.
- [ ] **Create a new figure or table** (analogous to Figures A.5–A.6) showing the interaction coefficients for bias × income, alongside bias × education for comparison.
- [ ] **Add a subsection or paragraph in Section 4.1 (Results)** discussing whether higher-income individuals are more or less responsive to bias. This directly addresses the intro's claim about income gaps being distorted — if higher-income Asians retain identity while lower-income Asians attrit, the measured Asian-White income gap is upward-biased.
- [ ] **Interpret the sign**: if bias × income is positive (higher income buffers against bias-induced attrition), this supports the paper's narrative that identity attrition selectively removes lower-SES Asians from the "Asian" category.

#### 2. Response memo

- [ ] Structure the memo point-by-point, mirroring each reviewer comment with a brief summary of the concern, what you did, and where in the paper the change appears. For any suggestion you decline, provide a clear, respectful rationale.

---

### Reviewer 1

#### 3. Explicitly list the ANES survey items measuring racial animus

Reviewer 1 is essentially asking for conditional acceptance — this is a quick win. Currently footnote 6 in Section 3.2 lists the ANES questions but somewhat generically. Steps:

- [ ] **Expand the existing footnote (or create a new one / short appendix subsection)** to explicitly enumerate each ANES item used, with the exact question wording or at minimum the variable labels. For example: (1) racial resentment items, (2) feeling thermometer toward Asians, (3) "conditions make it difficult for Blacks to succeed," etc.
- [ ] **Clarify the measure type** — state whether these are ethnocentrism measures, racial resentment scales, feeling thermometers, or a combination. The current footnote lists them but doesn't categorize them.
- [ ] **Add a brief note** explaining why anti-Black racial resentment items proxy for anti-Asian attitudes (you already have the Almasalkhi and Mora citations; just be more explicit about cross-group correlation of prejudice).

---

### Reviewer 2

#### 4. Reframe the paper's setup: show the "solely Asian vs. mixed identity" distinction early

The reviewer wants a cleaner narrative arc at the top of the paper. Currently Section 4 ("From the Data") does this but it comes after the theory section, and the framing uses "non-Asian" language in places.

- [ ] **Add a brief motivating subsection or paragraph at the start of Section 4** (or even at the end of the introduction) that presents the key descriptive fact: when all ancestors are Asian, ~96% identify as Asian; when ancestry is mixed, a substantial fraction identify as something other than solely Asian. Use Tables 2/A.8 numbers directly.
- [ ] **Replace "non-Asian" phrasing throughout** with "something other than solely Asian" or "mixed/multiracial identity." The reviewer's Kamala Harris example is pointed — people identify as *both*, not as *non-something*. Do a find-and-replace pass for "non-Asian" and "identify as White" and reword to emphasize the choice is between solely-Asian vs. multiracial or White-only identity.
- [ ] **Tighten the introduction** to preview this framing: "I show that while nearly all individuals with exclusively Asian ancestry report Asian identity, those with mixed ancestry face meaningful identity choices that respond to local prejudice conditions."

#### 5. Drop individuals born in (East) Asia or with two Asian parents

This is the most structurally consequential suggestion. The reviewer argues that since ~96% of first-gen and AA (two-Asian-parent) individuals identify as Asian, there's a near-ceiling effect that makes large bias coefficients look implausible.

- [ ] **Re-run all main specifications** excluding: (a) first-generation immigrants born in Asia, and (b) second-generation individuals with two Asian parents (AA category). This means the main analysis focuses on mixed-race second-gen (AW, WA) and third-gen with fewer than four Asian grandparents.
- [ ] **Keep the dropped group in a descriptive table** as motivation (the "96% identify as Asian" fact), but remove them from the regression analysis.
- [ ] **If you want to push back**, you could argue that including them is important for completeness and that the ceiling effect is itself informative. But note the editor said these suggestions are "reasonable" — so partial compliance is likely necessary. A compromise: present the main results on mixed-ancestry only, and relegate the full-sample results (including AA/first-gen) to an appendix robustness table.
- [ ] **Update the abstract and introduction** to reflect the narrowed focus on mixed-ancestry individuals.

#### 6. Skip binary choice models → go straight to multinomial logit

Currently Section 4.1 presents binary (LPM) results first, then Section 4.2 presents multinomial logit. The reviewer wants the multinomial to be the centerpiece.

- [ ] **Restructure the results section**: lead with the multinomial logit results as the main analysis. The multinomial is more informative because it distinguishes between "White only" vs. "Asian and White" vs. "Asian only."
- [ ] **Move the binary LPM results to an appendix** or present them very briefly as a simplified summary before diving into the multinomial. You can frame it as: "For completeness, Appendix Table X shows that the binary Asian/non-Asian specification yields qualitatively similar patterns."
- [ ] **This pairs naturally with dropping the AA/first-gen group** — with those groups removed, the binary model becomes even less interesting because the remaining sample is all mixed-ancestry, where the multinomial distinctions matter most.

#### 7. Be more careful about significance claims; test WA vs. AW differences

This is about the "significant vs. not significant ≠ significantly different" problem (Gelman & Stern, 2006). Several places in the manuscript describe WA and AW effects as if they differ when the difference hasn't been formally tested.

- [ ] **Run formal tests of WA = AW** for all key specifications. You can do this by pooling WA and AW into a single regression with an interaction term (bias × AW_indicator) and testing whether the interaction is significant.
- [ ] **Audit the language** in Sections 4.1 and 4.2. For example, the current text says the AW effect is 15pp and the WA effect is 10pp — but if the difference is not significant, don't frame these as meaningfully different. Instead say something like: "Both mixed-race family types show large negative effects of bias, with point estimates of 15pp (AW) and 10pp (WA); the difference between these estimates is not statistically significant (p = X.XX)."
- [ ] **Consider combining WA and AW** into a single "mixed-race" category if the WA–AW differences are not significant. This would simplify the paper considerably and address the reviewer's suggestion. You could still show AW/WA separately in an appendix.
- [ ] **Apply the same logic to multinomial results**: when discussing directional differences between AW and WA marginal effects, note when the differences are or aren't statistically distinguishable.

#### 8. Streamline and shorten the paper

This follows naturally from changes 5–7 above.

- [ ] **Cut the binary results section** from the main text (move to appendix).
- [ ] **Consolidate WA/AW** into a combined mixed-race group where differences aren't significant — this halves the number of panels in several figures.
- [ ] **Tighten the literature review** in the introduction. Currently paragraphs 7–11 (starting "This research contributes to multiple bodies…") are quite long. Consider cutting or condensing the behavioral economics and identity literature paragraphs — keep the most directly relevant cites (Akerlof & Kranton, Antman, Fouka) and trim the rest.
- [ ] **Shorten the theoretical framework** (Section 2) if possible — it's fairly standard Akerlof & Kranton extension and the reviewer seems focused on getting to the empirical results faster.
- [ ] **Aim for a net reduction of 5–8 pages** through these combined changes.

#### 9. Resolve the identity × income inconsistency (intro claims vs. analysis)

The reviewer identifies a real tension: the introduction makes strong claims about how identity attrition biases measured Asian-White income gaps, but the paper never quantifies this bias. Two approaches:

- [ ] **Option A (preferred, more feasible):** Add the bias × income interaction to the main text (per the editor's request) and discuss what the coefficient implies for measured income gaps. For example: "The positive interaction between income and Asian identity suggests that higher-earning individuals are X pp more likely to retain Asian identity per SD of bias, implying that measured Asian incomes in high-bias states are upward-biased by approximately $Y." You can do a rough back-of-the-envelope calculation.
- [ ] **Option B (ambitious):** Construct a simple decomposition or simulation showing how much identity attrition shifts the measured Asian-White income gap. E.g., compare mean income of all objectively-Asian individuals vs. mean income of only those who self-identify as Asian, across high- and low-bias states. This is a powerful exercise that could be done with CPS data you already have.
- [ ] **Option C (minimal):** Soften the intro language. Replace the strong causal claims with more hedged statements: "These patterns *suggest* that identity attrition could affect measured gaps, though quantifying the precise magnitude is beyond the scope of this analysis." The reviewer may find this disappointing but it's honest.
- [ ] **Whichever option you choose**, make sure the intro and conclusion match — currently both make claims that the analysis doesn't fully support.

#### 10. Confirm other mixed-race forms (e.g., Black/Asian) were too small to include

- [ ] **Add a footnote** in Section 3.1 (where you define the family type categories) noting: "Other mixed-race combinations, such as Black-Asian families, were present in the data but yielded sample sizes too small for reliable estimation (N = X). I therefore restrict the mixed-race analysis to Asian-White families, which constitute the vast majority of Asian interracial families in the CPS."
- [ ] **Report the actual count** if possible — this adds credibility.

#### 11. Figures 2A and 3: address within-state changes and state fixed effects

The reviewer finds the large within-state changes in bias implausible and suspects state FE would kill the coefficients. Currently you use region × year FE and note in footnote 8 that state FE are excluded due to insufficient within-state variation.

- [ ] **Run the main specifications with state fixed effects** as a robustness check. If the coefficients become imprecise (as the reviewer suspects), report them in an appendix table with a note like: "State fixed-effects specifications yield coefficients of similar sign but are imprecisely estimated (Table A.X), consistent with the limited within-state temporal variation in bias measures."
- [ ] **If state FE results are actually reasonable**, include them as a robustness table and note this strengthens the findings.
- [ ] **Add a brief discussion** of what drives within-state variation in Figures 2A and 3. Is it measurement noise in the bias index year-to-year, or genuine shifts (e.g., COVID-era spike in anti-Asian hate crimes)? A sentence or two contextualizing the variation would help.
- [ ] **Consider showing a version of the figures with state-demeaned bias** to illustrate that within-state variation, while noisier, still shows the expected pattern.

---

## Suggested Revision Sequence

A recommended order of operations to minimize rework:

1. **Data/estimation work first**: run bias × income interactions, state FE robustness, WA=AW formal tests, and re-estimate dropping first-gen/AA group. This determines which results you have before rewriting.
2. **Decide on WA/AW**: based on the formal test results, decide whether to combine into a single mixed-race category or keep them separate with appropriate caveats.
3. **Restructure the paper**: lead with descriptive facts (96% ceiling for mono-Asian), then multinomial logit as main results, binary LPM to appendix.
4. **Rewrite framing**: fix "non-Asian" language, add income × bias discussion, soften/support intro claims about income gaps.
5. **Quick wins**: ANES items footnote (R1), Black/Asian footnote (R2 lesser point 1).
6. **Trim**: cut lit review length, shorten theory section, consolidate figures.
7. **Write the response memo** last, once all changes are finalized.
