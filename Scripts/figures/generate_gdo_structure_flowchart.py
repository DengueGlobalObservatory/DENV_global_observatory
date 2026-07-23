#!/usr/bin/env python3
"""
Generate a supplemental site-structure flowchart (SVG + PNG) for the
Global Dengue Observatory.

Shows page hierarchy and cross-page navigation as defined in _quarto.yml,
with dashed screenshot placeholder callouts for the maintainer to populate.

Visual language follows the live GDO site: minty/teal palette, logo in the
title bar, and the same card-and-arrow style as the pipeline flowchart.
"""

from __future__ import annotations

import base64
import os
import shutil
import subprocess
import textwrap

# --- Canvas ---------------------------------------------------------------
W = 1720
MARGIN_L = 118
HIER_X0 = 130
HIER_X1 = 1040
HIER_W = HIER_X1 - HIER_X0
HIER_CX = (HIER_X0 + HIER_X1) / 2
CALL_X0 = 1070
CALL_X1 = 1700
CALL_W = CALL_X1 - CALL_X0
FONT = "Helvetica, Arial, sans-serif"

# GDO minty / site palette (style.css + minty theme)
COLORS = {
    "landing": "#1f6f63",
    "collection": "#0d5c48",
    "detail": "#26A69A",
    "reference": "#E8B84A",
    "nav": "#13443c",
    "nav_chip": "#1f6f63",
    "item": "#FFFFFF",
    "item_edge": "#B0BEC5",
    "stage_bg": "#f7faf9",
    "stage_edge": "#c5ddd8",
    "text": "#1f2a37",
    "muted": "#5f6d7a",
    "arrow": "#546E7A",
    "title_bar": "#13443c",
    "title_accent": "#a8e6cf",
    "callout_bg": "#FAFAFA",
    "callout_edge": "#9E9E9E",
    "callout_text": "#56647a",
    "framework_bg": "#eef6f4",
    "framework_edge": "#1f6f63",
}


def esc(s: str) -> str:
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def wrap(text: str, width: int) -> list[str]:
    return textwrap.wrap(text, width=width) or [""]


def text_block(x, y, lines, size=10, weight="normal", anchor="middle", fill=None, style=None):
    fill = fill or COLORS["text"]
    lh = size * 1.24
    sy = y - (len(lines) - 1) * lh / 2
    st = f' font-style="{style}"' if style else ""
    return "\n".join(
        f'<text x="{x:.1f}" y="{sy + i * lh:.1f}" font-family="{FONT}" '
        f'font-size="{size}" font-weight="{weight}" text-anchor="{anchor}" fill="{fill}"{st}>'
        f"{esc(ln)}</text>"
        for i, ln in enumerate(lines)
    )


def rect(x, y, w, h, fill, stroke, rx=6, sw=1.6, dash=None):
    d = f' stroke-dasharray="{dash}"' if dash else ""
    return (
        f'<rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" rx="{rx}" '
        f'fill="{fill}" stroke="{stroke}" stroke-width="{sw}"{d}/>'
    )


def arrow_line(x1, y1, x2, y2, color=None, dash=None, sw=1.7, marker=True):
    color = color or COLORS["arrow"]
    d = f' stroke-dasharray="{dash}"' if dash else ""
    m = ' marker-end="url(#ah)"' if marker else ""
    return (
        f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" '
        f'stroke="{color}" stroke-width="{sw}"{d}{m}/>'
    )


def arrow_path(points, color=None, dash=None, marker=True):
    color = color or COLORS["arrow"]
    d_attr = f' stroke-dasharray="{dash}"' if dash else ""
    m = ' marker-end="url(#ah)"' if marker else ""
    pts = " ".join(f"{x:.1f},{y:.1f}" for x, y in points)
    return (
        f'<polyline points="{pts}" fill="none" stroke="{color}" '
        f'stroke-width="1.7"{d_attr}{m}/>'
    )


def stage_band(label, y0, y1):
    return "\n".join([
        rect(12, y0, MARGIN_L - 24, y1 - y0, COLORS["stage_bg"], COLORS["stage_edge"], rx=4),
        text_block(53, (y0 + y1) / 2, label.split("\n"), size=10.5, weight="bold"),
    ])


def item_chip(cx, top, w, text, size=9.0, pad_v=9):
    lines = wrap(text, int(w / 5.4))
    lh = size * 1.24
    h = pad_v * 2 + len(lines) * lh
    x = cx - w / 2
    bits = [
        rect(x, top, w, h, COLORS["item"], COLORS["item_edge"], rx=5, sw=1.2),
        f'<circle cx="{x + 12:.1f}" cy="{top + h / 2:.1f}" r="3" fill="{COLORS["landing"]}"/>',
        text_block(cx + 8, top + h / 2, lines, size=size, anchor="middle"),
    ]
    return "\n".join(bits), top + h


def page_card(cx, top, w, color, kicker, title, subtitle, items, badge=None):
    x = cx - w / 2
    header_h = 58
    bits = [rect(x, top, w, header_h, color, color, rx=8, sw=2)]
    bits.append(text_block(x + 16, top + 18, [kicker], size=8.6, weight="bold",
                            anchor="start", fill="#FFFFFF"))
    if badge:
        blines = wrap(badge, 26)
        bw = max(9 * len(blines[0]) + 20, 60)
        bx = x + w - bw - 14
        bits.append(rect(bx, top + 8, bw, 20, "#FFFFFF", "#FFFFFF", rx=10, sw=0))
        bits.append(text_block(bx + bw / 2, top + 18, blines, size=8.2, weight="bold",
                                fill=color))
    bits.append(text_block(cx, top + 34, [title], size=13.5, weight="bold", fill="#FFFFFF"))
    bits.append(text_block(cx, top + 50, wrap(subtitle, 62), size=8.8, fill="#FFFFFF"))
    y = top + header_h + 10
    for it in items:
        chip_svg, y2 = item_chip(cx, y, w - 20, it)
        bits.append(chip_svg)
        y = y2 + 7
    bottom = y + 3
    bits.insert(0, rect(x - 3, top - 3, w + 6, bottom - top + 6, "#FFFFFF", "#E0E0E0", rx=11, sw=1))
    return "\n".join(bits), top, bottom, top + header_h


def navbar(cx, top, w, items):
    h = 74
    x = cx - w / 2
    bits = [
        rect(x, top, w, h, COLORS["nav"], COLORS["nav"], rx=8, sw=0),
        text_block(cx, top + 17, ["Persistent top navigation — present on every page (_quarto.yml navbar)"],
                    size=10, weight="bold", fill="#FFFFFF"),
    ]
    n = len(items)
    gap = 8
    pad = 14
    avail = w - 2 * pad - gap * (n - 1)
    pw = avail / n
    px = x + pad
    py = top + 32
    ph = 30
    for label in items:
        bits.append(rect(px, py, pw, ph, COLORS["nav_chip"], "#2d8a7a", rx=15, sw=1))
        bits.append(text_block(px + pw / 2, py + ph / 2 + 1, wrap(label, 14), size=8.2,
                                weight="bold", fill="#FFFFFF"))
        px += pw + gap
    return "\n".join(bits), top, top + h


def framework_strip(cx, top, w):
    """Quarto / presentation layer note below the title bar."""
    h = 36
    x = cx - w / 2
    return "\n".join([
        rect(x, top, w, h, COLORS["framework_bg"], COLORS["framework_edge"], rx=6, sw=1.2),
        text_block(cx, top + h / 2, [
            "Quarto website  ·  minty theme  ·  full-width layout  ·  scrollama scrollytelling  ·  responsive CSS"
        ], size=9.2, fill=COLORS["nav"]),
    ]), top, top + h


def camera_icon(x, y, s=22):
    return "\n".join([
        f'<rect x="{x:.1f}" y="{y + s*0.28:.1f}" width="{s:.1f}" height="{s*0.62:.1f}" rx="3" '
        f'fill="none" stroke="{COLORS["callout_edge"]}" stroke-width="1.6"/>',
        f'<rect x="{x + s*0.28:.1f}" y="{y + s*0.10:.1f}" width="{s*0.40:.1f}" height="{s*0.20:.1f}" rx="2" '
        f'fill="none" stroke="{COLORS["callout_edge"]}" stroke-width="1.6"/>',
        f'<circle cx="{x + s/2:.1f}" cy="{y + s*0.60:.1f}" r="{s*0.20:.1f}" '
        f'fill="none" stroke="{COLORS["callout_edge"]}" stroke-width="1.6"/>',
    ])


def callout_box(cx, top, w, tag, desc, height=None):
    lines = wrap(desc, int(w / 5.6))
    lh = 9.6 * 1.28
    h = height or max(78, 52 + len(lines) * lh)
    x = cx - w / 2
    bits = [
        rect(x, top, w, h, COLORS["callout_bg"], COLORS["callout_edge"], rx=8, sw=1.6, dash="6,4"),
        rect(x + 8, top + 8, w - 16, h - 16, "#FFFFFF", "#ECEFF1", rx=4, sw=1, dash="4,3"),
        camera_icon(x + 18, top + 16),
        text_block(x + 52, top + 28, [f"INSERT SCREENSHOT — {tag}"], size=8.8,
                    weight="bold", anchor="start", fill=COLORS["callout_edge"]),
    ]
    ty = top + 44
    for ln in lines:
        bits.append(text_block(x + 52, ty, [ln], size=9.2, anchor="start", fill=COLORS["callout_text"]))
        ty += lh
    bits.append(text_block(cx, top + h - 12, ["[ placeholder ]"], size=8, fill="#B0BEC5"))
    return "\n".join(bits), top, top + h


def elbow(x1, y1, x2, y2, color=None):
    color = color or COLORS["callout_edge"]
    mid_x = x1 + (x2 - x1) * 0.42
    return arrow_path([(x1, y1), (mid_x, y1), (mid_x, y2), (x2, y2)], color=color,
                       dash="3,3", marker=False) + (
        f'\n<circle cx="{x1:.1f}" cy="{y1:.1f}" r="3" fill="{color}"/>'
    )


def logo_data_uri(repo: str) -> str | None:
    for name in ("logo_no_text.png", "logo_t.png", "logo.png"):
        path = os.path.join(repo, "Assets", "Stable", name)
        if os.path.isfile(path):
            with open(path, "rb") as f:
                b64 = base64.b64encode(f.read()).decode("ascii")
            return f"data:image/png;base64,{b64}"
    return None


def title_bar(logo_uri: str | None) -> list[str]:
    bits = [
        rect(0, 0, W, 68, COLORS["title_bar"], COLORS["title_bar"], rx=0),
    ]
    if logo_uri:
        bits.append(
            f'<image href="{logo_uri}" x="18" y="8" width="52" height="52" '
            f'preserveAspectRatio="xMidYMid meet"/>'
        )
        title_x = W / 2 + 20
    else:
        title_x = W / 2
    bits.extend([
        text_block(title_x, 28, ["Global Dengue Observatory — site structure & cross-page navigation"],
                   size=15.5, weight="bold", fill="#FFFFFF"),
        text_block(title_x, 50, [
            "Page hierarchy and navigation flow as defined in _quarto.yml  "
            "(companion to the data-pipeline flowchart)"
        ], size=9.4, fill=COLORS["title_accent"]),
    ])
    return bits


def build(repo: str) -> tuple[str, int]:
    body: list[str] = []
    stages: list[tuple[str, float, float]] = []
    callouts: list[str] = []
    call_cursor = 188

    def stage_start(name, y):
        stages.append((name, y, y))

    def stage_end(y):
        stages[-1] = (stages[-1][0], stages[-1][1], y)

    def add_callout(anchor_x, anchor_y, tag, desc, min_gap=16, height=None):
        nonlocal call_cursor
        top = max(call_cursor, anchor_y - 38)
        svg, ctop, cbottom = callout_box(
            CALL_X0 + CALL_W / 2, top, CALL_W - 10, tag, desc, height=height
        )
        callouts.append(svg)
        callouts.append(elbow(anchor_x, anchor_y, CALL_X0, (ctop + cbottom) / 2))
        call_cursor = cbottom + min_gap

    y = 82
    fw_svg, _, y = framework_strip(HIER_CX, y, HIER_W)
    body.append(fw_svg)

    # --- Navigation --------------------------------------------------
    stage_start("Global\nNavigation", y + 8)
    nav_svg, nav_top, nav_bottom = navbar(
        HIER_CX, y + 8, HIER_W,
        ["Home", "All Countries", "Regions ▾", "Methods", "Data", "About", "FAQ", "GitHub"],
    )
    body.append(nav_svg)
    add_callout(HIER_X1, nav_top + 38, "A · Navbar & branding",
                "Site header with GDO logo, page links and Regions dropdown menu.",
                height=88)
    stage_end(nav_bottom + 10)

    # --- Landing -------------------------------------------------------
    stage_start("Landing\nPage", nav_bottom + 18)
    home_top = nav_bottom + 44
    body.append(arrow_line(HIER_CX, nav_bottom, HIER_CX, home_top))
    home_svg, home_top, home_bottom, _ = page_card(
        HIER_CX, home_top, HIER_W, COLORS["landing"],
        "LANDING PAGE  ·  index.qmd",
        "Home",
        "Global entry point — situational overview and drill-down hub",
        [
            "Hero banner with GDO logo and project introduction",
            "Scrollytelling narrative with sticky global radial plot (current season vs. historic baseline)",
            "“How to read our plots” infographic (radial-plot conventions)",
            "Interactive world map with 8 clickable regional radial-plot overlays",
            "“High severity countries” spotlight with “Explore All Countries” call-to-action",
        ],
    )
    body.append(home_svg)
    add_callout(HIER_X1, home_top + 95, "B · Home scrolly + global map",
                "Scrolling narrative, sticky global radial plot and world map with regional overlays.",
                height=92)
    add_callout(HIER_X1, home_bottom - 65, "C · Plot legend + severity panel",
                "“How to read our plots” graphic and high-severity countries spotlight / CTA.",
                height=88)
    stage_end(home_bottom + 8)

    # --- Collection pages ----------------------------------------------
    stage_start("Collection\nPages", home_bottom + 20)
    col_gap = 44
    col_w = (HIER_W - col_gap) / 2
    cx_all = HIER_X0 + col_w / 2
    cx_reg = HIER_X1 - col_w / 2
    col_top = home_bottom + 52

    body.append(arrow_path([(HIER_CX, home_bottom), (HIER_CX, home_bottom + 24),
                             (cx_all, home_bottom + 24), (cx_all, col_top)]))
    body.append(text_block(cx_all - 6, home_bottom + 38, ["“Explore All Countries” button"],
                            size=8.2, anchor="end", fill=COLORS["muted"]))
    body.append(arrow_path([(HIER_CX, home_bottom), (HIER_CX, home_bottom + 24),
                             (cx_reg, home_bottom + 24), (cx_reg, col_top)]))
    body.append(text_block(cx_reg + 6, home_bottom + 38, ["Map click / “Regions” nav menu"],
                            size=8.2, anchor="start", fill=COLORS["muted"]))

    all_svg, all_top, all_bottom, _ = page_card(
        cx_all, col_top, col_w, COLORS["collection"],
        "COLLECTION PAGE",
        "All Countries",
        "pages/country-index.qmd",
        [
            "Search box (filter by country name)",
            "Region filter dropdown and sort controls",
            "Grid of country tiles — radial plot + severity blurb + region link per country",
        ],
    )
    body.append(all_svg)

    reg_svg, reg_top, reg_bottom, _ = page_card(
        cx_reg, col_top, col_w, COLORS["collection"],
        "COLLECTION PAGE  ·  × 8",
        "Regions",
        "8 region .qmd pages (navbar dropdown or map click)",
        [
            "Regional hero: region map, radial plot, summary text and season badge",
            "Region-scoped country tile grid (same layout as All Countries, filtered)",
        ],
        badge="8 regions",
    )
    body.append(reg_svg)

    add_callout(HIER_X1, all_top + 50, "D · All Countries grid",
                "Search / filter / sort controls and the full tile grid of country cards.",
                height=88)
    add_callout(HIER_X1, reg_top + 55, "E · Region page hero",
                "Regional map, radial plot and summary text for one example region.",
                height=88)

    col_bottom = max(all_bottom, reg_bottom)
    stage_end(col_bottom + 8)

    # --- Detail pages ---------------------------------------------------
    stage_start("Detail\nPages", col_bottom + 20)
    nat_top = col_bottom + 66
    body.append(arrow_path([(cx_all, all_bottom), (cx_all, nat_top - 26),
                             (HIER_CX, nat_top - 26), (HIER_CX, nat_top)]))
    body.append(arrow_path([(cx_reg, reg_bottom), (cx_reg, nat_top - 26),
                             (HIER_CX, nat_top - 26), (HIER_CX, nat_top)]))
    body.append(text_block(HIER_CX, nat_top - 36, ["Click a country tile"], size=8.6,
                            fill=COLORS["muted"]))

    nat_svg, nat_top, nat_bottom, _ = page_card(
        HIER_CX, nat_top, HIER_W, COLORS["detail"],
        "DETAIL PAGE  ·  templated  ·  × ~90 countries",
        "National Summary Page",
        "pages/country/*.qmd — _country-template.qmd + country-config.csv",
        [
            "Hero: country-in-region map, radial plot and season badge",
            "Auto-generated status sentence vs. seasonal baseline",
            "Interactive monthly time-series chart with 95% prediction-interval band",
            "“Further analysis” narrative (month-on-month change, YTD ratio, peak month)",
        ],
        badge="~90 pages",
    )
    body.append(nat_svg)
    add_callout(HIER_X1, nat_top + 95, "F · National page hero",
                "Country map, radial plot and season badge for one example country.",
                height=88)
    add_callout(HIER_X1, nat_bottom - 55, "G · Time-series chart",
                "Current-year line with uncertainty band and year-comparison toggle.",
                height=88)
    stage_end(nat_bottom + 8)

    # --- Reference pages ------------------------------------------------
    stage_start("Reference &\nSupport", nat_bottom + 20)
    ref_top = nat_bottom + 60
    ref_gap = 26
    ref_w = (HIER_W - 3 * ref_gap) / 4
    ref_titles = [
        ("Methods", "pages/methods.qmd", [
            "Data sources, backfilling and nowcasting explained",
            "Companion data-pipeline flowchart figure",
        ]),
        ("Data", "pages/data.qmd", [
            "Country-month data table (cases, source, status)",
            "CSV download buttons for data and metadata",
        ]),
        ("About", "pages/about.qmd", [
            "Project background, team and funder acknowledgement",
            "Contact details",
        ]),
        ("FAQ", "pages/faq.qmd", [
            "Data-collection sources and delay correction",
            "Dengue-season definition and nowcasting, plain language",
        ]),
    ]
    ref_bottoms = []
    ref_cxs = []
    x = HIER_X0 + ref_w / 2
    for title, path, items in ref_titles:
        ref_cxs.append(x)
        body.append(arrow_line(x, ref_top - 30, x, ref_top - 14, dash="4,3", sw=1.4))
        svg, rtop, rbottom, _ = page_card(
            x, ref_top, ref_w, COLORS["reference"],
            "REFERENCE PAGE",
            title,
            path,
            items,
        )
        body.append(svg)
        ref_bottoms.append(rbottom)
        x += ref_w + ref_gap

    body.append(text_block(HIER_CX, ref_top - 24,
                           ["Reached directly from persistent navigation — not part of the country drill-down path"],
                           size=8.8, fill=COLORS["muted"]))

    add_callout(ref_cxs[1] + ref_w / 2, ref_top + 55, "H · Data table + download",
                "Data table preview and CSV download buttons for data and metadata.",
                height=88)
    add_callout(ref_cxs[3] + ref_w / 2, ref_top + 75, "I · Methods / FAQ layout",
                "Plain-language explainer layout used on Methods and FAQ pages.",
                height=88)

    ref_bottom = max(ref_bottoms)
    stage_end(ref_bottom + 8)

    body_bottom = max(ref_bottom, call_cursor)

    stage_svg = "\n".join(stage_band(name, y0, y1) for name, y0, y1 in stages)

    ly = body_bottom + 42
    legend_items = [
        ("landing", "Landing page"),
        ("collection", "Collection page"),
        ("detail", "Detail page (templated)"),
        ("reference", "Reference / support page"),
    ]
    legend = [text_block(MARGIN_L, ly, ["Page-type colour key:"], size=9.5, weight="bold", anchor="start")]
    lx = MARGIN_L + 140
    for key, lab in legend_items:
        legend.append(rect(lx, ly - 10, 16, 13, COLORS[key], COLORS[key], rx=2))
        legend.append(text_block(lx + 24, ly, [lab], size=9, anchor="start"))
        lx += 18 + 9 * len(lab) + 34
    legend.append(rect(lx + 6, ly - 10, 16, 13, COLORS["callout_bg"], COLORS["callout_edge"], rx=2, dash="4,3"))
    legend.append(text_block(lx + 30, ly, ["Screenshot placeholder (A–I) — to be added by maintainer"],
                              size=9, anchor="start", fill=COLORS["callout_text"]))

    canvas_h = int(ly + 36)
    logo_uri = logo_data_uri(repo)
    header = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
        f'width="{W}" height="{canvas_h}" viewBox="0 0 {W} {canvas_h}">',
        "<defs>",
        '<marker id="ah" markerWidth="8" markerHeight="8" refX="6" refY="3" orient="auto">',
        f'<polygon points="0 0, 8 3, 0 6" fill="{COLORS["arrow"]}"/>',
        "</marker>",
        "</defs>",
        f'<rect width="{W}" height="{canvas_h}" fill="#FFFFFF"/>',
        *title_bar(logo_uri),
        f'<line x1="{CALL_X0 - 18:.1f}" y1="118" x2="{CALL_X0 - 18:.1f}" y2="{canvas_h - 20}" '
        f'stroke="#E0E0E0" stroke-width="1.4" stroke-dasharray="2,4"/>',
        text_block(CALL_X0 + CALL_W / 2, 108, ["SUPPLEMENTAL SCREENSHOTS"],
                   size=9.5, weight="bold", fill=COLORS["callout_text"]),
        text_block(CALL_X0 + CALL_W / 2, 122, ["Labelled placeholders A–I — select and insert screenshots"],
                   size=8.6, fill=COLORS["callout_text"]),
    ]

    full = "\n".join(header + [stage_svg] + body + callouts + legend + ["</svg>"])
    return full, canvas_h


CHROME_PATHS = [
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
]


def export_png(svg_path: str, png_path: str, width: int, height: int, scale: int = 2) -> bool:
    try:
        import cairosvg  # type: ignore
        cairosvg.svg2png(url=svg_path, write_to=png_path, output_width=width * scale)
        return True
    except Exception:
        pass

    for chrome in CHROME_PATHS:
        if not os.path.exists(chrome):
            continue
        try:
            subprocess.run(
                [
                    chrome, "--headless", "--disable-gpu", "--hide-scrollbars",
                    f"--force-device-scale-factor={scale}",
                    f"--window-size={width},{height}",
                    "--default-background-color=FFFFFFFF",
                    f"--screenshot={png_path}",
                    f"file://{os.path.abspath(svg_path)}",
                ],
                check=False, capture_output=True, timeout=25,
            )
            if os.path.exists(png_path) and os.path.getsize(png_path) > 0:
                return True
        except (subprocess.TimeoutExpired, OSError):
            continue

    try:
        subprocess.run(
            ["qlmanage", "-t", "-s", str(width * scale), "-o", os.path.dirname(svg_path), svg_path],
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

    svg_path = os.path.join(out, "GDO_structure_flowchart.svg")
    png_path = os.path.join(out, "GDO_structure_flowchart.png")

    svg, height = build(repo)
    with open(svg_path, "w", encoding="utf-8") as f:
        f.write(svg)
    print(f"Wrote {svg_path} ({W}x{height}px)")

    if export_png(svg_path, png_path, W, height):
        print(f"Wrote {png_path}")
    else:
        print("PNG export skipped (open SVG in browser or Illustrator to export)")

    docs_out = os.path.join(repo, "docs", "Assets", "Stable", "figures")
    os.makedirs(docs_out, exist_ok=True)
    for src, name in [(svg_path, "GDO_structure_flowchart.svg"), (png_path, "GDO_structure_flowchart.png")]:
        if os.path.isfile(src):
            shutil.copy2(src, os.path.join(docs_out, name))
            print(f"Copied to docs/Assets/Stable/figures/{name}")


if __name__ == "__main__":
    main()
