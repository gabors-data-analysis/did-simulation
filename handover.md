# DID Simulation App — Handover Notes

## Current State (2026-03-22)

The app **runs** on R 4.5.3 with renv. The UI has been restructured into two toggleable tabs. Most features work but there's one blocking bug with regression tables.

## What Works
- Data generation (simultaneous/staggered, uniform/heterogeneous effects, dynamic, reversal, noise, trends)
- Panel view heatmap + treatment effects time series plot
- TWFE transformation 3-panel plot (with integer x-axis labels)
- Event study transformation plots (4 panels for staggered)
- Event study coefficient plot (TWFE, Sun & Abraham, Callaway-Sant'Anna)
- Tabbed UI layout (Tab 1: Panel Models, Tab 2: Event Study)
- CS `cohort_numeric` fixed: uses `ifelse` not `if_else` to avoid `as.integer(Inf)` warning

## Blocking Bug: `etable()` HTML output

**The regression tables don't render.** The `etable(..., type = "html")` call from fixest fails when called via `do.call` because named args like `type = "html"` get mixed into `...` and interpreted as model objects.

**What was tried:**
1. `do.call(etable, c(model_list, list(type = "html")))` — FAILS: "element named 'type' is not valid: should be fixest object"
2. `etable(.list = model_list, type = "html")` — FAILS: `.list` is not a real etable parameter
3. Direct call `etable(m$twfe, m$fd, m$fd_cumul, type = "html")` — **status unknown, not yet confirmed working by user**

**Likely fix approaches:**
- Test `etable(m$twfe, m$fd, m$fd_cumul, type = "html")` in R console directly to see if it returns HTML
- Check `fixest` version: `packageVersion("fixest")` — `type = "html"` was added in fixest 0.11+
- If `type = "html"` isn't supported, use `etable(..., tex = FALSE)` and capture as text, or use `modelsummary` package instead
- Fallback: `verbatimTextOutput` + `renderPrint({ etable(m$twfe, m$fd, m$fd_cumul) })` — ugly but guaranteed to work

**Current code location:** `server.r` lines 155-164 (panel table) and lines 189-208 (event study table)

## Files

| File | Lines | Status |
|------|-------|--------|
| `global.r` | ~880 | Clean. Data generation, models, plots, transformations |
| `server.r` | ~351 | Clean. Reactives, renders, etable calls (broken) |
| `ui.r` | ~270 | Clean. Two-tab layout with tabsetPanel |
| `renv.lock` | — | Fresh for R 4.5.3. Packages: shiny, tidyverse, fixest, plotly, did |

## Architecture

```
ui.r
  ├── Header (description, version)
  ├── Overview plots: panel_view + did_plot
  ├── Settings (4 columns)
  └── tabsetPanel(id = "analysis_tabs", type = "pills")
        ├── Tab 1: "Panel Models (FE / FD)"
        │     ├── htmlOutput("model_results_panel")  ← BROKEN
        │     ├── textOutput("model_explanation_panel")
        │     ├── plotlyOutput("twfe_plot")
        │     └── textOutput("twfe_explanation")
        └── Tab 2: "Event Study"
              ├── htmlOutput("model_results_event")  ← BROKEN
              ├── textOutput("estimator_warnings")
              ├── plotlyOutput("event_coef_plot")
              ├── plotlyOutput("event_study_plot")
              └── textOutput("event_study_explanation")

server.r
  ├── Validation observers (year_fe, event window, dynamic values, reversal)
  ├── Reactives: data(), models(), twfe_data(), event_study_data()
  ├── Renders: did_plot, panel_view, treatment_explanation
  ├── model_results_panel → etable(twfe, fd, fd_cumul)
  ├── model_results_event → etable(event, sunab) + format_cs_output(cs)
  ├── twfe_plot, twfe_explanation
  ├── event_study_plot, event_coef_plot, event_study_explanation
  └── downloadData

global.r
  ├── Package loading (shiny, tidyverse, fixest, plotly, did)
  ├── compute_ylim() — dynamic y-axis limits
  ├── generate_data(input) — creates panel dataset
  ├── run_twfe_transform(data) — 3-step FE removal
  ├── create_did_plot(data) — main time series
  ├── run_models(data, input) — returns list(twfe, fd, fd_cumul, event, sunab, cs)
  ├── transform_event_study_data() — 4-panel event study data
  ├── create_event_study_plot() — plotly subplot
  ├── create_event_coef_plot() — coefficient plot with CIs
  ├── format_cs_output() — CS results as HTML table
  └── create_panel_view() — treatment heatmap
```

## CS Estimator Warnings (expected)
With only 6 units (4 treated, 2 control), Callaway-Sant'Anna may warn about:
- "No never-treated group available" — if cohort_numeric isn't 0 for controls
- "Last treated cohort coerced as never-treated" — when all units eventually treated
- Wide confidence intervals — expected with 1 unit per cohort

These are wrapped in `tryCatch` and should show graceful warnings via `textOutput("estimator_warnings")`.

## Plan file
Full plan history at: `.claude/plans/eager-mixing-lantern.md`
