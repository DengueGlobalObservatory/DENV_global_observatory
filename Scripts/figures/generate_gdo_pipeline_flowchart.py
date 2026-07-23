#!/usr/bin/env python3
"""
Generate publication-ready GDO pipeline flowchart (SVG + PNG).
Layout modelled on the OpenDengue flowchart.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import textwrap

# Publication canvas (approx. 180 mm wide at 300 dpi when exported at 2400 px)
W = 1500
MARGIN_L = 118
FONT = "Helvetica, Arial, sans-serif"

COLORS = {
    "who": "#4A90D9",
    "searo": "#7CB342",
    "paho": "#26A69A",
    "opendengue": "#E8B84A",
    "process": "#D4EDDA",
    "process_edge": "#2E7D32",
    "decision": "#FFF8E1",
    "decision_edge": "#F9A825",
    "stage_bg": "#F7F7F7",
    "text": "#1A1A1A",
    "muted": "#555555",
    "arrow": "#546E7A",
    "yes": "#2E7D32",
    "no": "#C62828",
    "db": "#A5D6A7",
    "db_edge": "#1B5E20",
    "title_bar": "#263238",
    "note": "#ECEFF1",
    "note_edge": "#90A4AE",
}


def esc(s: str) -> str:
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def wrap(text: str, width: int) -> list[str]:
    return textwrap.wrap(text, width=width) or [""]


def text_block(x, y, lines, size=10, weight="normal", anchor="middle", fill=None):
    fill = fill or COLORS["text"]
    lh = size * 1.22
    sy = y - (len(lines) - 1) * lh / 2
    return "\n".join(
        f'<text x="{x:.1f}" y="{sy + i * lh:.1f}" font-family="{FONT}" '
        f'font-size="{size}" font-weight="{weight}" text-anchor="{anchor}" fill="{fill}">'
        f"{esc(ln)}</text>"
        for i, ln in enumerate(lines)
    )


def rect(x, y, w, h, fill, stroke, rx=6, sw=1.6, dash=None):
    d = f' stroke-dasharray="{dash}"' if dash else ""
    return (
        f'<rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" rx="{rx}" '
        f'fill="{fill}" stroke="{stroke}" stroke-width="{sw}"{d}/>'
    )


def diamond(cx, cy, w, h, fill, stroke):
    pts = f"{cx},{cy - h / 2} {cx + w / 2},{cy} {cx},{cy + h / 2} {cx - w / 2},{cy}"
    return f'<polygon points="{pts}" fill="{fill}" stroke="{stroke}" stroke-width="1.6"/>'


def para(x, y, w, h, fill, stroke, skew=12):
    pts = f"{x + skew},{y} {x + w},{y} {x + w - skew},{y + h} {x},{y + h}"
    return f'<polygon points="{pts}" fill="{fill}" stroke="{stroke}" stroke-width="1.5"/>'


def arrow_line(x1, y1, x2, y2, color=None, dash=None, sw=1.7):
    color = color or COLORS["arrow"]
    d = f' stroke-dasharray="{dash}"' if dash else ""
    return (
        f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" '
        f'stroke="{color}" stroke-width="{sw}"{d} marker-end="url(#ah)"/>'
    )


def arrow_path(points, color=None, dash=None):
    color = color or COLORS["arrow"]
    d_attr = f' stroke-dasharray="{dash}"' if dash else ""
    pts = " ".join(f"{x:.1f},{y:.1f}" for x, y in points)
    return (
        f'<polyline points="{pts}" fill="none" stroke="{color}" '
        f'stroke-width="1.7"{d_attr} marker-end="url(#ah)"/>'
    )


def process_box(cx, top, w, title, body, title_size=11, body_size=9.2, pad=(14, 28, 16)):
    blines = wrap(body, int(w / 5.6))
    h = pad[0] + title_size + 8 + len(blines) * body_size * 1.22 + pad[2]
    x = cx - w / 2
    body_y = top + pad[0] + title_size + 10 + (len(blines) - 1) * body_size * 1.22 / 2
    parts = [
        rect(x, top, w, h, COLORS["process"], COLORS["process_edge"]),
        text_block(cx, top + pad[0] + title_size * 0.45, [title], size=title_size, weight="bold"),
        text_block(cx, body_y, blines, size=body_size),
    ]
    return "\n".join(parts), top + h


def decision_box(cx, cy, w, text, size=8.8):
    lines = wrap(text, int(w / 5.0))
    h = max(88, 24 + len(lines) * size * 1.2)
    return "\n".join([
        diamond(cx, cy, w, h, COLORS["decision"], COLORS["decision_edge"]),
        text_block(cx, cy, lines, size=size),
    ]), h


def source_column(cx, color, title_lines, subtitle, items, top=92, cw=270):
    x = cx - cw / 2
    bits = [
        rect(x, top, cw, 56, color, color, rx=8, sw=2),
        text_block(cx, top + 18, title_lines, size=11.5, weight="bold", fill="#FFFFFF"),
        text_block(cx, top + 44, [subtitle], size=8.5, fill="#FFFFFF"),
    ]
    y = top + 68
    for item in items:
        bits.append(para(x + 14, y, cw - 28, 34, "#FFFFFF", color))
        bits.append(text_block(cx, y + 18, wrap(item, 22), size=8.8))
        y += 42
    return "\n".join(bits), y


def database_cylinder(cx, cy, w, h, title, subtitle):
    x, y = cx - w / 2, cy - h / 2
    eh = 16
    return "\n".join([
        f'<ellipse cx="{cx:.1f}" cy="{y + eh:.1f}" rx="{w / 2:.1f}" ry="{eh:.1f}" '
        f'fill="{COLORS["db"]}" stroke="{COLORS["db_edge"]}" stroke-width="2"/>',
        rect(x, y + eh, w, h - 2 * eh, COLORS["db"], COLORS["db_edge"], rx=0),
        f'<ellipse cx="{cx:.1f}" cy="{y + h - eh:.1f}" rx="{w / 2:.1f}" ry="{eh:.1f}" '
        f'fill="{COLORS["db"]}" stroke="{COLORS["db_edge"]}" stroke-width="2"/>',
        text_block(cx, cy - 6, wrap(title, 28), size=12, weight="bold"),
        text_block(cx, cy + 16, wrap(subtitle, 32), size=9.2, fill=COLORS["muted"]),
    ])


def stage_band(label, y0, y1):
    return "\n".join([
        rect(12, y0, MARGIN_L - 24, y1 - y0, COLORS["stage_bg"], "#CCCCCC", rx=4),
        text_block(53, (y0 + y1) / 2, label.split("\n"), size=10.5, weight="bold"),
    ])


def build() -> tuple[str, int]:
    cx = (MARGIN_L + W) / 2
    bw = W - MARGIN_L - 50
    body: list[str] = []
    stages: list[tuple[str, float, float]] = []

    def stage_start(name: str, y: float):
        stages.append((name, y, y))

    def stage_end(y: float):
        stages[-1] = (stages[-1][0], stages[-1][1], y)

    # --- Data sources ---
    stage_start("Data\nSources", 72)
    cols = [235, 530, 825, 1120]
    meta = [
        (COLORS["who"], ["WHO Global", "Dengue Dashboard"], "Monthly · 96 countries · 2017–present",
         ["Monthly case counts", "Date-triggered .xlsx extraction"]),
        (COLORS["searo"], ["WHO SEARO", "Dengue Dashboard"], "Monthly · 10 countries · 2024–present",
         ["Country profile chart data", "Date-triggered .csv extraction"]),
        (COLORS["paho"], ["PAHO Dengue", "Database"], "Weekly cumulative · 51 countries · 2014–present",
         ["Epiweek cumulative counts", "Daily unconditional country-week scrape"]),
        (COLORS["opendengue"], ["OpenDengue v3.1", "(gap-filled)"], "Monthly · 143 countries · 1990–2024",
         ["Historical baseline time series", "Static release — no scraper"]),
    ]
    src_bottom = 0
    for c, m in zip(cols, meta):
        col_svg, bottom = source_column(c, *m)
        body.append(col_svg)
        src_bottom = max(src_bottom, bottom)
    stage_end(src_bottom + 12)

    # --- Data extraction (live sources) ---
    stage_start("Data\nExtraction", src_bottom + 20)
    merge_y = src_bottom + 36
    ext_top = merge_y + 28
    box_svg, ext_bottom = process_box(
        cx, ext_top, bw,
        "Automated daily extraction (08:00 UTC)",
        "GitHub Actions trigger Python scrapers (Selenium). Outputs committed to version-controlled "
        "repositories: WHO .xlsx archives, SEARO .csv profiles, PAHO date-stamped country-week files.",
        pad=(12, 24, 12),
    )
    body.append(box_svg)
    # Converge live sources: vertical drops, shared bus, single feed into extraction
    body.append(
        f'<line x1="{cols[0]:.1f}" y1="{merge_y:.1f}" x2="{cols[2]:.1f}" y2="{merge_y:.1f}" '
        f'stroke="{COLORS["arrow"]}" stroke-width="1.7"/>'
    )
    for c in cols[:3]:
        body.append(arrow_line(c, src_bottom + 4, c, merge_y))
    body.append(arrow_line(cx, merge_y, cx, ext_top))
    stage_end(ext_bottom + 8)

    # --- Data processing ---
    stage_start("Data\nProcessing", ext_bottom + 16)
    y = ext_bottom + 24
    steps = [
        ("1. Reporting delay correction (native time resolution)",
         "Empirical median reporting factors f(c,d) applied to WHO and PAHO at weekly/monthly resolution before harmonisation. "
         "Validated at 1-year lag; SEARO excluded (insufficient history)."),
        ("2. Standardise format and reporting timing",
         "PAHO: epiweek to calendar month (ISOweek; Supp. Table 1) and cumulative to incident counts. "
         "WHO/SEARO: retain monthly totals. Negative month-on-month differences set to NA."),
        ("3. Geographic source selection",
         "Where WHO Global overlaps PAHO or SEARO, regional dashboard counts are preferred."),
        ("4. Harmonise to country–month structure",
         "One row per country-month: ISO3, date, cases, source (PAHO/SEARO/WHO), Data Status (Observed/Unobserved)."),
    ]
    for i, (title, step_body) in enumerate(steps):
        box_svg, y = process_box(cx, y, bw, title, step_body)
        body.append(box_svg)
        if i < len(steps) - 1:
            body.append(arrow_line(cx, y, cx, y + 14))
            y += 14

    dcy = y + 58
    body.append(arrow_line(cx, y, cx, dcy - 50))
    dec_svg, dec_h = decision_box(
        cx, dcy, 340,
        "Country meets inclusion criteria? "
        "(in current source + OpenDengue + >=3 complete seasons + mean >5 cases/month)",
    )
    body.append(dec_svg)
    body.append(text_block(cx + 195, dcy, ["No → exclude from GDO"], size=8.8, anchor="start", fill=COLORS["no"]))
    body.append(arrow_line(cx + 172, dcy, cx + 310, dcy, color=COLORS["no"]))
    body.append(text_block(cx, dcy + dec_h / 2 + 10, ["Yes"], size=9, fill=COLORS["yes"]))
    stage_end(dcy + dec_h / 2 + 20)

    # OpenDengue → historic season profiles (parallel path)
    hist_top = dcy + dec_h / 2 + 48
    body.append(arrow_path(
        [(cols[3], src_bottom + 4), (cols[3], hist_top - 18), (340, hist_top - 18), (340, hist_top)],
        color=COLORS["opendengue"], dash="7,4",
    ))
    body.append(text_block(cols[3] + 10, (src_bottom + hist_top) / 2,
                           ["Historic\nbaseline"], size=8.5, anchor="start", fill=COLORS["opendengue"]))

    box_svg, hist_bottom = process_box(
        340, hist_top, 360,
        "Historic season profiles",
        "12-month dengue season from circular mean of lowest-case month. "
        "Mean monthly proportions P(m); exclude seasons with <5 cases/month or <60 cases/season.",
    )
    body.append(box_svg)

    # --- Nowcasting ---
    stage_start("Nowcasting", hist_top - 8)
    now_top = hist_bottom + 28
    body.append(arrow_line(cx, dcy + dec_h / 2, cx, now_top))
    body.append(arrow_line(340, hist_bottom, 340, now_top + 18))
    body.append(arrow_line(340, now_top + 18, cx, now_top + 18))
    body.append(arrow_line(cx, now_top + 18, cx, now_top))

    y = now_top
    now_steps = [
        ("Proportion-based nowcasting",
         "Estimate seasonal total C_s = C(<=k) / P(<=k); impute missing recent months "
         "C_m = C_s x P(m) using historic seasonal profile."),
        ("Uncertainty intervals (retrospective validation)",
         "Leave-one-season-out validation. APE quantiles by country, lead-time and season timing "
         "yield 50% and 95% multiplicative prediction intervals around point forecasts."),
        ("Season severity assessment",
         "Compare current season to historic baseline; generate country and regional observatory outputs."),
    ]
    for i, (title, step_body) in enumerate(now_steps):
        box_svg, y = process_box(cx, y, bw, title, step_body)
        body.append(box_svg)
        if i < len(now_steps) - 1:
            body.append(arrow_line(cx, y, cx, y + 14))
            y += 14
    stage_end(y + 8)

    # --- Data hosting ---
    stage_start("Data\nHosting", y + 16)
    db_cy = y + 88
    body.append(arrow_line(cx, y, cx, db_cy - 58))
    body.append(database_cylinder(cx, db_cy, 560, 104,
                                  "GLOBAL DENGUE OBSERVATORY",
                                  "globaldengueobservatory.org · updated twice monthly"))
    fb_top = db_cy + 72
    body.append(arrow_line(cx, db_cy + 56, cx, fb_top))
    box_svg, fb_bottom = process_box(
        cx, fb_top, bw,
        "Open repositories and feedback loop",
        "Scraper and pipeline code on GitHub (DengueGlobalObservatory). Version history of extractions and outputs.",
        pad=(12, 22, 12),
    )
    body.append(box_svg)
    body.append(arrow_line(cx, fb_top, cx, db_cy + 56, color=COLORS["process_edge"], dash="5,3"))
    stage_end(fb_bottom + 12)

    stage_svg = "\n".join(stage_band(name, y0, y1) for name, y0, y1 in stages)

    ly = fb_bottom + 42
    legend = [text_block(MARGIN_L, ly, ["Source colour key:"], size=9.5, weight="bold", anchor="start")]
    for i, (k, lab) in enumerate([
        ("who", "WHO Global"), ("searo", "SEARO"), ("paho", "PAHO"), ("opendengue", "OpenDengue")
    ]):
        lx = MARGIN_L + 125 + i * 160
        legend.append(rect(lx, ly - 10, 16, 13, COLORS[k], COLORS[k], rx=2))
        legend.append(text_block(lx + 24, ly, [lab], size=9, anchor="start"))

    canvas_h = int(ly + 36)
    header = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{canvas_h}" viewBox="0 0 {W} {canvas_h}">',
        "<defs>",
        '<marker id="ah" markerWidth="8" markerHeight="8" refX="6" refY="3" orient="auto">',
        f'<polygon points="0 0, 8 3, 0 6" fill="{COLORS["arrow"]}"/>',
        "</marker>",
        "</defs>",
        f'<rect width="{W}" height="{canvas_h}" fill="#FFFFFF"/>',
        rect(0, 0, W, 58, COLORS["title_bar"], COLORS["title_bar"], rx=0),
        text_block(W / 2, 26, ["Global Dengue Observatory — data pipeline and methods"],
                   size=15.5, weight="bold", fill="#FFFFFF"),
        text_block(W / 2, 46, ["Automated surveillance integration, harmonisation, nowcasting and publication"],
                   size=9.5, fill="#CFD8DC"),
    ]

    full = "\n".join(header + [stage_svg] + body + legend + ["</svg>"])
    return full, canvas_h


def export_png(svg_path: str, png_path: str) -> bool:
    """Best-effort PNG export without blocking."""
    try:
        import cairosvg  # type: ignore
        cairosvg.svg2png(url=svg_path, write_to=png_path, output_width=3000)
        return True
    except Exception:
        pass

    try:
        subprocess.run(
            ["qlmanage", "-t", "-s", "2400", "-o", os.path.dirname(svg_path), svg_path],
            check=False, capture_output=True, timeout=8,
        )
        alt = svg_path + ".png"
        if os.path.exists(alt):
            shutil.copy(alt, png_path)
            return True
    except (subprocess.TimeoutExpired, OSError):
        pass
    return False


def main():
    repo = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
    out = os.path.join(repo, "Assets", "Stable", "figures")
    os.makedirs(out, exist_ok=True)

    svg_path = os.path.join(out, "GDO_pipeline_flowchart.svg")
    png_path = os.path.join(out, "GDO_pipeline_flowchart.png")

    svg, height = build()
    with open(svg_path, "w", encoding="utf-8") as f:
        f.write(svg)
    print(f"Wrote {svg_path} ({W}x{height}px)")

    if export_png(svg_path, png_path):
        print(f"Wrote {png_path}")
    else:
        print("PNG export skipped (open SVG in browser or Illustrator to export)")


if __name__ == "__main__":
    main()
