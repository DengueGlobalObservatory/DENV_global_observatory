# Global Dengue Observatory — Social Content Pack

**Date:** 5 May 2026

**Data version:** `Output/2026_05_05/` (96 countries; latest observed month: April 2026 for 19 countries, March 2026 for ~64 countries)

**Audience:** LinkedIn (professional / public-health / academic) and Bluesky (researchers, journalists, public-health community)

**Maintainer note:** Drafts only — please verify country-level numbers against the live dashboard at [https://www.globaldengueobservatory.org](https://www.globaldengueobservatory.org) before posting and tag the relevant agency handles for each platform.

---

## 1. Headline findings from the latest observatory run

Numbers below are computed from `Output/2026_05_05/DENV_cases_nowcast_output.csv` (cumulative observed cases for the latest observed month in 2026 ÷ cumulative seasonal expectation for the same month).

### Global picture (YTD vs seasonal expectation)

- **958,000 confirmed cases** across the 96 countries the observatory currently tracks for 2026, against an expected **2.13 million** for the same period — a YTD aggregate of **0.45× the seasonal baseline**.
- **But the modelled full-year trajectory is ~3.37 million cases vs 1.11M typically expected for the season — about 3× a normal year overall**, driven by countries that are starting their season early or hot.
- Translation: the *Americas* are coming off the 2024–25 megaseason and are running quieter so far in 2026, while *Asia and parts of Africa* are running ahead of schedule.

### Countries currently running well above their seasonal baseline (2026 YTD)


| Country     | Region                  | YTD ratio | Cumulative cases |
| ----------- | ----------------------- | --------- | ---------------- |
| Bangladesh  | South Asia              | 32×       | ~1,840           |
| Guyana      | South America           | 12×       | ~29,000          |
| Panama      | North & Central America | 11×       | ~3,000           |
| Maldives    | South Asia              | 7.6×      | ~1,640           |
| Nepal       | South Asia              | 6.5×      | ~470             |
| Timor-Leste | East & SE Asia          | 5.1×      | ~4,170           |
| Cambodia    | East & SE Asia          | 4.8×      | ~4,340           |
| Sri Lanka   | South Asia              | 2.5×      | ~19,600          |
| Puerto Rico | Caribbean               | 1.9×      | ~510             |
| Guatemala   | North & Central America | 1.7×      | ~5,400           |


> **Two flag-with-care signals:** Kenya (301× ratio, ~3,560 cumulative) and China (24×, 279 cases). Both have small expected baselines for this point in the year, so the *ratio* is unstable — but the absolute cases in Kenya in particular are well above any recent year-to-date for January–March, and the signal aligns with Kenya stepping up dengue preparedness in April 2026.

### Modelled full-year 2026 vs typical season (top 10)


| Country    | Predicted 2026 total | Typical seasonal total | Ratio |
| ---------- | -------------------- | ---------------------- | ----- |
| Brazil     | ~980,000             | ~120,600               | 8.1×  |
| Bangladesh | ~564,000             | ~46,600                | 12.1× |
| China      | ~342,000             | ~10,500                | 32×   |
| Kenya      | ~189,000             | ~1,200                 | 162×  |
| Sri Lanka  | ~130,000             | ~52,200                | 2.5×  |
| Guyana     | ~127,000             | ~11,300                | 11.2× |
| Panama     | ~94,000              | ~8,100                 | 11.6× |
| Cambodia   | ~90,000              | ~22,200                | 4.1×  |
| Bolivia    | ~59,000              | ~6,400                 | 9.2×  |
| Nepal      | ~49,000              | ~11,900                | 4.1×  |


### Regional cumulative ratio (sum of observed cum / sum of expected cum)


| Region                  | YTD ratio | Median country ratio                                                          |
| ----------------------- | --------- | ----------------------------------------------------------------------------- |
| East & Southeast Asia   | 1.29×     | 2.51×                                                                         |
| North & Central America | 1.18×     | 1.39×                                                                         |
| South America           | 0.46×     | 0.44×                                                                         |
| South Asia              | 0.25×     | 6.53× *(skewed; high case-count countries are early but absolute totals lag)* |
| Pacific Islands         | 0.24×     | 0.02×                                                                         |
| Sub-Saharan Africa      | 0.19×     | 0.19×                                                                         |
| Caribbean               | 0.13×     | 0.04×                                                                         |


---

## 2. External news overlap (cross-checks where the observatory has unique value)


| Observatory signal | Recent external reporting | Where the observatory adds value |
| --- | --- | --- |
| Americas YTD running well below 2024–25 (regional cum ratio 0.46 in S. America, 0.13 in Caribbean) | PAHO [Epidemiological Update — Dengue in the Americas, 18 Feb 2026][paho-feb18]; supporting [PAHO news release, 20 Feb 2026][paho-feb20]; weekly [PAHO Dengue sitrep EW07 2026][paho-ew07]. Headline: Americas down ~64% YTD vs 2025; Brazil down 75% vs same period. | Confirms PAHO at country level, while highlighting that 2026 is still on track to be a top-three Brazil year because of off-cycle DENV-3 risk. |
| Bangladesh 32× YTD, Sri Lanka, Maldives & Nepal all running above baseline | [Kathmandu Post (3 Mar 2026): "Dengue shows no sign of slowing in winter…"][nep-kp]; [WHO Western Pacific Dengue Situation Update #737 (8 Jan 2026)][wpro-737]; Bangladesh DGHS [Dengue Dashboard][bgd-dghs] and [BSS daily case bulletin][bgd-bss]. | Quantifies how *unusual* the off-season activity is by pegging it to each country's own typical seasonal pattern. |
| Kenya signal in Sub-Saharan Africa | [Dawan Africa (Apr 2026): "Kenya Strengthens Response to Dengue and Emerging Disease Threats"][ken-dawan]; historical context — [IFRC ReliefWeb EPoA, Kenya 2021 dengue outbreak][ken-ifrc]. | Independent corroboration that "something is happening" beyond what national bulletins show — useful framing for surveillance investment. |
| Brazil predicted 8× a typical year (~980k for our coverage) | Fiocruz / FGV projection coverage in [O Globo (24 Feb 2026)][bra-oglobo] and [FAPESP Na Mídia][bra-fapesp] — 1.8M cases projected, DENV-3 reintroduction after a 17-yr gap. Counterpoint: [Agência Brasil (Apr 2026): YTD cases down 75% vs 2025][bra-agencia]. | Two independent models pointing the same direction — useful for stakeholders who want triangulation. |
| Imported risk via SE Asia (Cambodia, Timor-Leste, Myanmar all flagged) | [China customs new dengue-import prevention measures, effective 10 Apr 2026 (ECNS)][chn-ecns]. | Validates the international-traveller risk window — observatory provides the upstream signal. |
| Pacific Islands and Caribbean running below average | PAHO regional sitreps (see EW07 link above); [ECDC monthly Dengue worldwide overview][ecdc-monthly] for travel-related case context. | Important context: not every dengue story this year is "outbreak" — the observatory is good at spotting *de-escalations* too. |
| Climate-driven expansion narrative (Nepal up to >2,000 m, off-season activity) | [BMJ Global Health (2025): "Mosquito and global dengue cases in a warming world"][bmj-gh]; [Scientific Reports (2025): India monsoon dengue projections][sci-rep]; [PMC review: long-term climate-change impacts on dengue transmission risk][pmc-review]. | Provides up-to-date, country-level evidence that the literature's projections are starting to show in real reporting streams. |
| Multilateral framing — "dengue is no longer just a tropical disease" | [PAHO news (8 Apr 2026): "PAHO Director urges global action… at One Health Forum in Lyon"][paho-lyon]. | Direct policy hook for any post that wants to anchor in a high-level WHO/PAHO message. |
| Panama outbreak nuance (observatory shows YTD ratio ~11×) | [Metro Libre (Apr 2026): hospitalisations down 32% in Panama in 2026][pan-metro]. | Useful tension to acknowledge: cases vs. hospitalisations don't always move together — observatory tracks reported cases. |
| Guyana running 12× YTD (observatory) | [News Room Guyana (6 Mar 2026): "No surge in cases but Health Ministry monitoring dengue, chikungunya situation"][guy-newsroom]. | Important to flag a *divergence*: the observatory's signal is louder than the national framing — do not over-claim Guyana in posts. |


**Macro narrative for posts:** *Globally, dengue isn't simply "up" or "down" in 2026 — it has shifted. The Americas are taking a breath after their record 2024–25 wave, while South Asia, parts of South-East Asia and East Africa are running early and ahead of normal. The Global Dengue Observatory (LSHTM) is built precisely for this kind of mixed picture: a country-by-country, near-real-time view tied to each country's own seasonal baseline.*

---

## 3. LinkedIn posts (drafts)

> Style: 1,200–1,800 character professional post; soft hook → 2–3 data points → context → CTA. Add 1 image (suggested: `Assets/Dynamic/world_plot.png` or the 8-region map screenshot from the home page).

### LinkedIn Post A — Headline / state-of-play (recommended primary post)

> **Hook:** Is the global dengue picture in 2026 really getting better? Look closer.

The latest run of the **Global Dengue Observatory** — our LSHTM-led near-real-time tracker that integrates national reports for 96 countries — paints a more nuanced picture than the headlines suggest.

🔻 **The Americas are quieter (so far).** Across the countries we cover, year-to-date cumulative cases are running at about 0.46× the seasonal baseline in South America and 0.13× in the Caribbean, consistent with [PAHO's 18 Feb 2026 epidemiological update][paho-feb18] reporting a ~64% YTD drop versus the record 2024–25 season.

🔺 **Asia and parts of Africa are running early and hot.**
• Bangladesh is already at ~32× its expected cumulative cases for this point in the year.
• The Maldives, Nepal, Sri Lanka, Timor-Leste and Cambodia are all 2.5×–7.6× their cumulative seasonal baselines.
• Kenya is showing a sustained signal that aligns with the government's April 2026 announcement of expanded surveillance.

🌍 **What our model expects for 2026 overall:** ~3.4M cases across the 96 countries we track — about 3× a typical season — driven less by a single mega-outbreak and more by simultaneous early-season activity in multiple regions, plus a likely DENV-3 resurgence in Brazil.

The point of the Observatory isn't to declare a "dengue year" — it's to give every country its own seasonal yardstick and show whether *their* current curve is on, ahead of, or behind schedule.

🔗 Explore the latest country and regional dashboards → globaldengueobservatory.org

#Dengue #PublicHealth #GlobalHealth #Epidemiology #LSHTM #VectorBorneDisease #OneHealth

---

### LinkedIn Post B — South Asia angle (good as a follow-up 3–5 days later)

> **Hook:** Dengue isn't supposed to be this loud in February and March.

For most of South Asia, dengue cases peak after the monsoon. But the latest update from the **Global Dengue Observatory** shows several countries running far above their typical early-year baselines:

📍 Bangladesh: ~32× the expected cumulative cases for the year-to-date ([DGHS dengue dashboard][bgd-dghs]).
📍 Maldives: ~7.6×, building on a rising trend through Nov–Dec 2025 ([WHO Western Pacific Dengue Situation Update #737][wpro-737]).
📍 Nepal: ~6.5×, with the [Kathmandu Post reporting 300+ cases in January and February alone][nep-kp] — and dengue now reaching elevations above 2,000 m.
📍 Sri Lanka: ~2.5×, with ~19,600 confirmed cumulative cases in 2026 already.
📍 Timor-Leste: ~5.1× — flagged by [WHO SEARO][wpro-737] earlier this year.

Why this matters:
1️⃣ Off-season activity is increasingly the new normal, consistent with climate-driven expansion documented in [BMJ Global Health][bmj-gh] and [Scientific Reports][sci-rep] over the past 12 months.
2️⃣ Each percentage above baseline now is a **planning lead-time** — vector control, hospital surge plans, and clinician awareness campaigns are most effective before the seasonal peak.
3️⃣ Country-by-country baselines matter. A "small" outbreak in absolute terms can still represent a major signal when set against that country's own seasonal pattern.

The Observatory pulls together WHO Global, WHO SEARO, and PAHO PLISA data, fills reporting gaps with statistical models, and updates weekly so policy teams can act on the freshest available picture.

🔗 globaldengueobservatory.org

#Dengue #SouthAsia #ClimateAndHealth #DiseaseSurveillance #LSHTM

---

### LinkedIn Post C — Brazil + DENV-3 angle (Americas / vaccine community)

> **Hook:** Dengue cases are down across the Americas in early 2026 — but a quieter year is not a safe year.

Two independent modelling efforts agree that 2026 is unlikely to be a low burden year for Brazil:

• A Fiocruz / FGV study published in February 2026 projects **~1.8M cases** for Brazil this year, with São Paulo and Minas Gerais carrying most of the load ([O Globo coverage][bra-oglobo]; [FAPESP Na Mídia][bra-fapesp]).
• Our **Global Dengue Observatory** at LSHTM, which integrates national surveillance with seasonal nowcasting, currently projects **~980,000 cases for 2026 in our covered countries' Brazil dataset — about 8× a typical seasonal total** for the included reporting stream.

The two estimates disagree on the headline number (different inputs and granularity), but they agree on the *direction*: above-average burden, even after a [reassuring 75% YTD drop reported by Brazil's Ministry of Health through mid-April][bra-agencia].

The likely driver: the reintroduction of **DENV-3** after almost two decades, leaving a large susceptible population.

When two independent systems converge on the same risk signal, that's the moment to pre-position vector control, vaccine deployment planning, and clinical readiness — not when the curve has already steepened.

🔗 globaldengueobservatory.org
🔗 Methods: globaldengueobservatory.org/pages/methods.html

#Dengue #Brazil #DENV3 #VaccinePolicy #PublicHealth #LSHTM

---

## 4. Bluesky posts (drafts)

> Style: 300-character limit per post; conversational; threads work well. Bluesky audiences over-index on researchers, journalists, climate-and-health folks. Hashtags help less than on LinkedIn — 1–2 max.

### Bluesky thread — primary launch (5 posts)

**1/5**
The 5 May update of the Global Dengue Observatory is live. 96 countries, near-real-time, each compared to its own seasonal baseline.
Headline: 2026 is *not* a single global story. Americas are quieter; South Asia, SE Asia and parts of Africa are running early.
🔗 globaldengueobservatory.org

**2/5**
Country signals running *above* their 2026 cumulative seasonal baseline (YTD):
🇧🇩 Bangladesh ~32×
🇬🇾 Guyana ~12×
🇵🇦 Panama ~11×
🇲🇻 Maldives ~7.6×
🇳🇵 Nepal ~6.5×
🇹🇱 Timor-Leste ~5.1×
🇰🇭 Cambodia ~4.8×
🇱🇰 Sri Lanka ~2.5×

**3/5**
Two flag-with-care signals: Kenya (small expected base, but absolute cases unusually high for Q1 — aligns with the govt's April 2026 surveillance scale-up) and China (early-year imported case activity matches new April 2026 border measures).

**4/5**
Americas in context: [PAHO reports a ~64% YTD drop][paho-feb18] vs the record 2024–25 wave. Our regional aggregate matches: ~0.46× baseline in S. America, ~0.13× in Caribbean. But a [Fiocruz/FGV model][bra-oglobo] + ours both still predict an above-average 2026 in Brazil — DENV-3 resurgence after a 17-yr gap.

**5/5**
The Observatory's added value isn't another single number. It's a country-by-country yardstick that flags *when each country starts to depart from its own normal*. Built at @lshtm.ac.uk, funded by the AXA Research Foundation.
Methods: globaldengueobservatory.org/pages/methods.html
#dengue

---

### Bluesky standalone — South Asia (good for re-share by SEARO / regional accounts)

> Off-season dengue, in numbers.
> Compared to each country's own seasonal baseline, the cumulative 2026 case count so far is:
> • Bangladesh ~32×
> • Maldives ~7.6×
> • Nepal ~6.5×
> • Sri Lanka ~2.5×
> The peak hasn't even started.
> 🔗 globaldengueobservatory.org

### Bluesky standalone — climate / one health framing

> ["Dengue is no longer just a tropical disease."][paho-lyon] — PAHO Director, April 2026.
> Our latest tracker shows why: cases are simultaneously rising off-season in South Asia, expanding upward in elevation in Nepal, and showing unusual Q1 activity in coastal East Africa. The Observatory keeps a country-level eye on it.
> 🔗 globaldengueobservatory.org

### Bluesky standalone — methods / data nerd post

> How does the Global Dengue Observatory turn patchy weekly bulletins into a global situational picture?
>
> 1. Daily auto-scrape of WHO Global, WHO SEARO and PAHO PLISA
> 2. Reporting-delay correction (backfilling) on PAHO data
> 3. Seasonal nowcasting using each country's own historic pattern
> Code is open.
> 🔗 github.com/DengueGlobalObservatory

---

## 5. Suggested visuals to attach


| Post                           | Suggested image                             | Where to find it                                                                                                        |
| ------------------------------ | ------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| LinkedIn A, Bluesky 1/5        | Global radial-plots-on-world-map screenshot | Top of `index.html` (or render `Assets/Dynamic/world_plot.png` + the 8 region radials in `Assets/Dynamic/region/*.png`) |
| LinkedIn B, Bluesky South Asia | South Asia regional radial plot             | `Assets/Dynamic/region/south-asia.png`                                                                                  |
| LinkedIn C                     | Brazil country card                         | `pages/country/bra.html` screenshot or country page from country-index                                                  |
| Bluesky methods post           | "How to read our plots" guide               | `Assets/Stable/how_to.png`                                                                                              |
| All                            | Logo lockup if needed                       | `Assets/Stable/logo_large.png`                                                                                          |


> Reminder: regional PNGs in `Assets/Dynamic/region/` are regenerated each pipeline run, so confirm the timestamp matches `Output/2026_05_05/` before exporting.

---

## 6. Tagging & handle suggestions

**LinkedIn (where appropriate):**

- London School of Hygiene & Tropical Medicine
- AXA Research Fund
- World Health Organization (WHO)
- Pan American Health Organization (PAHO)
- WHO South-East Asia Region (WHO SEARO)
- Centre for Mathematical Modelling of Infectious Diseases (CMMID)

**Bluesky handles to consider (verify before posting):**

- @lshtm.ac.uk (LSHTM) and CMMID handle
- @who.int / @paho.org if active
- Project leads' personal accounts
- Climate & health journalists who have covered prior posts (e.g., @thelancet.com, @bmj.com community accounts)

---

## 7. Posting cadence suggestion


| When              | Channel  | Post                                                          |
| ----------------- | -------- | ------------------------------------------------------------- |
| Day 0 (this week) | LinkedIn | Post A (headline)                                             |
| Day 0             | Bluesky  | 5-post thread                                                 |
| Day +2            | Bluesky  | Standalone South Asia post                                    |
| Day +3            | LinkedIn | Post B (South Asia deep dive)                                 |
| Day +5            | Bluesky  | Methods / data nerd post                                      |
| Day +7            | LinkedIn | Post C (Brazil + DENV-3)                                      |
| Day +7            | Bluesky  | Climate / One Health post (re-amplifying PAHO Director quote) |


---

## 8. Pre-publish checklist

- Re-run numbers against the live dashboard at the time of posting (data updates weekly).
- Cross-check Kenya and China figures against the latest WHO Global Dashboard before quoting them — both have small baselines and large ratios.
- Confirm `Assets/Dynamic/region/*.png` were regenerated on or after 5 May 2026.
- Add UTM tags to outbound links if running campaign analytics (e.g. `?utm_source=linkedin&utm_campaign=may2026_update`).
- If sharing the Brazil number, name both modelling sources (Fiocruz/FGV and the Observatory) to avoid implying a single estimate.
- Final read-through for plain-language phrasing (target: educated non-epidemiologist).

---

## 9. References & source links

Reference-style link definitions used throughout this document. All URLs verified to be reachable as of 5 May 2026; please re-check before publishing in case any have been moved or paywalled.

### Multilateral / official surveillance

- **PAHO — Epidemiological Update, Dengue in the Americas (18 Feb 2026):** <https://www.paho.org/en/documents/epidemiological-update-dengue-americas-region-18-february-2026>
- **PAHO — News release on Americas dengue update (20 Feb 2026):** <https://paho.org/en/news/20-2-2026-paho-updates-dengue-situation-americas-recommends-strengthened-surveillance-and>
- **PAHO — Weekly dengue sitrep, EW07 2026:** <https://paho.org/en/documents/dengue-epidemiological-situation-region-americas-epidemiological-week-06-2026>
- **PAHO — Director on rising arboviral threats, One Health Forum Lyon (8 Apr 2026):** <https://www.paho.org/en/news/8-4-2026-paho-director-urges-global-action-combat-rising-dengue-and-other-arboviral-threats>
- **WHO Western Pacific — Dengue Situation Update #737 (8 Jan 2026):** <https://www.who.int/westernpacific/publications/m/item/dengue-situation-update---737--08-january-2026>
- **ECDC — Dengue worldwide overview (monthly):** <https://www.ecdc.europa.eu/en/dengue-monthly>

### National sources

- **Bangladesh DGHS — Dengue Dashboard:** <https://dashboard.dghs.gov.bd/pages/heoc_dengue_v1.php>
- **BSS News (Bangladesh) — daily dengue case bulletin:** <https://www.bssnews.net/news/375769>
- **Kenya — Dawan Africa (Apr 2026): "Kenya Strengthens Response to Dengue and Emerging Disease Threats":** <https://www.dawan.africa/news/kenya-strengthens-response-to-dengue-and-emerging-disease-threats>
- **Kenya — IFRC ReliefWeb EPoA (2021 outbreak, historical context):** <https://reliefweb.int/report/kenya/kenya-dengue-fever-outbreak-emergency-plan-action-epoa-n-mdrke048>
- **Nepal — Kathmandu Post (3 Mar 2026): "Dengue shows no sign of slowing in winter…":** <https://kathmandupost.com/health/2026/03/03/dengue-shows-no-sign-of-slowing-in-winter-300-infected-in-january-and-february>
- **Brazil — O Globo (24 Feb 2026): Fiocruz/FGV 1.8M projection coverage:** <https://oglobo.globo.com/rio/bairros/noticia/2026/02/24/dengue-pesquisa-internacional-preve-18-milhao-de-casos-no-pais-em-2026-com-maior-impacto-no-rio.ghtml>
- **Brazil — FAPESP Na Mídia: Fiocruz/FGV projection summary:** <https://namidia.fapesp.br/dengue-deve-atingir-18-milhao-de-brasileiros-em-2026-metade-dos-casos-em-sao-paulo/660405>
- **Brazil — Agência Brasil (Apr 2026): YTD cases down 75% vs 2025:** <https://agenciabrasil.ebc.com.br/saude/noticia/2026-04/casos-de-dengue-no-brasil-caem-75-em-2026>
- **Panama — Metro Libre (Apr 2026): hospitalisations down 32% in 2026:** <https://www.metrolibre.com/nacionales/disminuyen-en-32-las-hospitalizaciones-por-dengue-en-panama-durante-2026-segun-el-minsa-EG21632879>
- **Guyana — News Room Guyana (6 Mar 2026): "No surge in cases…":** <https://newsroom.gy/2026/03/06/no-surge-in-cases-but-health-ministry-monitoring-dengue-chikungunya-situation/>
- **China — ECNS (9 Apr 2026): customs measures to prevent dengue import:** <http://www.ecns.cn/cns-wire/2026-04-09/detail-ihfcmemi3023885.shtml>

### Climate / scientific literature

- **BMJ Global Health (2025) — "Mosquito and global dengue cases in a warming world":** <https://gh.bmj.com/content/10/5/e014688>
- **Scientific Reports (2025) — Dengue dynamics and future increase under changing monsoon climate in India:** <https://www.nature.com/articles/s41598-025-85437-w>
- **PMC review — State-of-the-science long-term predictions of climate-change impacts on dengue transmission risk:** <https://pmc.ncbi.nlm.nih.gov/articles/PMC12083777/>

### Project resources (for CTAs in posts)

- **Global Dengue Observatory homepage:** <https://www.globaldengueobservatory.org>
- **Methods page:** <https://www.globaldengueobservatory.org/pages/methods.html>
- **About / team / funders:** <https://www.globaldengueobservatory.org/pages/about.html>
- **GitHub organisation:** <https://github.com/DengueGlobalObservatory>

<!-- Reference-style link definitions used in the document above -->
[paho-feb18]: https://www.paho.org/en/documents/epidemiological-update-dengue-americas-region-18-february-2026
[paho-feb20]: https://paho.org/en/news/20-2-2026-paho-updates-dengue-situation-americas-recommends-strengthened-surveillance-and
[paho-ew07]: https://paho.org/en/documents/dengue-epidemiological-situation-region-americas-epidemiological-week-06-2026
[paho-lyon]: https://www.paho.org/en/news/8-4-2026-paho-director-urges-global-action-combat-rising-dengue-and-other-arboviral-threats
[wpro-737]: https://www.who.int/westernpacific/publications/m/item/dengue-situation-update---737--08-january-2026
[ecdc-monthly]: https://www.ecdc.europa.eu/en/dengue-monthly
[bgd-dghs]: https://dashboard.dghs.gov.bd/pages/heoc_dengue_v1.php
[bgd-bss]: https://www.bssnews.net/news/375769
[ken-dawan]: https://www.dawan.africa/news/kenya-strengthens-response-to-dengue-and-emerging-disease-threats
[ken-ifrc]: https://reliefweb.int/report/kenya/kenya-dengue-fever-outbreak-emergency-plan-action-epoa-n-mdrke048
[nep-kp]: https://kathmandupost.com/health/2026/03/03/dengue-shows-no-sign-of-slowing-in-winter-300-infected-in-january-and-february
[bra-oglobo]: https://oglobo.globo.com/rio/bairros/noticia/2026/02/24/dengue-pesquisa-internacional-preve-18-milhao-de-casos-no-pais-em-2026-com-maior-impacto-no-rio.ghtml
[bra-fapesp]: https://namidia.fapesp.br/dengue-deve-atingir-18-milhao-de-brasileiros-em-2026-metade-dos-casos-em-sao-paulo/660405
[bra-agencia]: https://agenciabrasil.ebc.com.br/saude/noticia/2026-04/casos-de-dengue-no-brasil-caem-75-em-2026
[pan-metro]: https://www.metrolibre.com/nacionales/disminuyen-en-32-las-hospitalizaciones-por-dengue-en-panama-durante-2026-segun-el-minsa-EG21632879
[guy-newsroom]: https://newsroom.gy/2026/03/06/no-surge-in-cases-but-health-ministry-monitoring-dengue-chikungunya-situation/
[chn-ecns]: http://www.ecns.cn/cns-wire/2026-04-09/detail-ihfcmemi3023885.shtml
[bmj-gh]: https://gh.bmj.com/content/10/5/e014688
[sci-rep]: https://www.nature.com/articles/s41598-025-85437-w
[pmc-review]: https://pmc.ncbi.nlm.nih.gov/articles/PMC12083777/

