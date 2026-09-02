#!/usr/bin/env python3
"""
Generate the GDO website-structure figure (SVG + PNG): a simple
hierarchical box diagram of the page tree, with dashed placeholder
panels for the maintainer to drop screenshots into. Deliberately plain
— boxes and labels only, no descriptive copy — mirroring the
hand-drawn sketch this was built from.
"""

from __future__ import annotations

import base64
import os
import shutil
import subprocess

W = 1500
FONT = "Helvetica, Arial, sans-serif"

COLORS = {
    "page": "#1f6f63",
    "page_edge": "#13443c",
    "section": "#FFFFFF",
    "section_edge": "#1f6f63",
    "util": "#FFFFFF",
    "util_edge": "#5f6d7a",
    "util_text": "#3e4b58",
    "text": "#1f2a37",
    "arrow": "#546E7A",
    "title_bar": "#13443c",
    "title_accent": "#a8e6cf",
    "shot_bg": "#EFF4FA",
    "shot_edge": "#3B6FA0",
    "shot_text": "#2b4f73",
}


def esc(s: str) -> str:
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def text_block(x, y, lines, size=11, weight="normal", anchor="middle", fill=None, style=None):
    fill = fill or COLORS["text"]
    lh = size * 1.25
    sy = y - (len(lines) - 1) * lh / 2
    st = f' font-style="{style}"' if style else ""
    return "\n".join(
        f'<text x="{x:.1f}" y="{sy + i * lh:.1f}" font-family="{FONT}" '
        f'font-size="{size}" font-weight="{weight}" text-anchor="{anchor}" fill="{fill}"{st}>'
        f"{esc(ln)}</text>"
        for i, ln in enumerate(lines)
    )


def rect(x, y, w, h, fill, stroke, rx=8, sw=1.8, dash=None):
    d = f' stroke-dasharray="{dash}"' if dash else ""
    return (
        f'<rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" rx="{rx}" '
        f'fill="{fill}" stroke="{stroke}" stroke-width="{sw}"{d}/>'
    )


def arrow_line(x1, y1, x2, y2, color=None, dash=None, sw=2, marker=True):
    color = color or COLORS["arrow"]
    d = f' stroke-dasharray="{dash}"' if dash else ""
    m = ' marker-end="url(#ah)"' if marker else ""
    return (
        f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" '
        f'stroke="{color}" stroke-width="{sw}"{d}{m}/>'
    )


def arrow_path(points, color=None, dash=None, marker=True, sw=2):
    color = color or COLORS["arrow"]
    d_attr = f' stroke-dasharray="{dash}"' if dash else ""
    m = ' marker-end="url(#ah)"' if marker else ""
    pts = " ".join(f"{x:.1f},{y:.1f}" for x, y in points)
    return f'<polyline points="{pts}" fill="none" stroke="{color}" stroke-width="{sw}"{d_attr}{m}/>'


def curve(x1, y1, x2, y2, color=None, dash="5,4", sw=1.8, marker=True):
    """Single smooth S-curve connector (sketch-style) from a node to a screenshot panel."""
    color = color or COLORS["shot_edge"]
    d_attr = f' stroke-dasharray="{dash}"' if dash else ""
    m = ' marker-end="url(#ah2)"' if marker else ""
    mx = (x1 + x2) / 2
    return (
        f'<path d="M {x1:.1f} {y1:.1f} C {mx:.1f} {y1:.1f}, {mx:.1f} {y2:.1f}, {x2:.1f} {y2:.1f}" '
        f'fill="none" stroke="{color}" stroke-width="{sw}"{d_attr}{m}/>'
    )


def page_box(cx, top, w, h, label, sublabel=None, fill=None, edge=None, text_fill="#FFFFFF"):
    fill = fill or COLORS["page"]
    edge = edge or COLORS["page_edge"]
    bits = [rect(cx - w / 2, top, w, h, fill, edge, rx=10, sw=2)]
    if sublabel:
        bits.append(text_block(cx, top + h / 2 - 10, [label], size=15, weight="bold", fill=text_fill))
        bits.append(text_block(cx, top + h / 2 + 12, [sublabel], size=9, fill=text_fill))
    else:
        bits.append(text_block(cx, top + h / 2, [label], size=15, weight="bold", fill=text_fill))
    return "\n".join(bits)


def section_box(cx, top, w, h, label):
    return "\n".join([
        rect(cx - w / 2, top, w, h, COLORS["section"], COLORS["section_edge"], rx=8, sw=1.6, dash="5,4"),
        text_block(cx, top + h / 2, [label], size=10.5, weight="600", fill=COLORS["page_edge"]),
    ])


def util_box(cx, top, w, h, label):
    return "\n".join([
        rect(cx - w / 2, top, w, h, COLORS["util"], COLORS["util_edge"], rx=8, sw=1.6),
        text_block(cx, top + h / 2, [label], size=11, weight="600", fill=COLORS["util_text"]),
    ])


def shot_box(cx, top, w, h, label):
    return "\n".join([
        rect(cx - w / 2, top, w, h, COLORS["shot_bg"], COLORS["shot_edge"], rx=10, sw=2, dash="7,5"),
        text_block(cx, top + 24, ["SCREENSHOT"], size=10, weight="bold", fill=COLORS["shot_edge"]),
        text_block(cx, top + 44, [label], size=12, weight="bold", fill=COLORS["shot_text"]),
    ])


def logo_data_uri(repo: str) -> str | None:
    for name in ("logo_no_text.png", "logo_t.png", "logo.png"):
        path = os.path.join(repo, "Assets", "Stable", name)
        if os.path.isfile(path):
            with open(path, "rb") as f:
                b64 = base64.b64encode(f.read()).decode("ascii")
            return f"data:image/png;base64,{b64}"
    return None


def title_bar(logo_uri: str | None) -> list[str]:
    bits = [rect(0, 0, W, 60, COLORS["title_bar"], COLORS["title_bar"], rx=0)]
    if logo_uri:
        bits.append(f'<image href="{logo_uri}" x="14" y="6" width="48" height="48" preserveAspectRatio="xMidYMid meet"/>')
        title_x = W / 2 + 18
    else:
        title_x = W / 2
    bits.append(text_block(title_x, 34, ["Global Dengue Observatory — website structure"],
                            size=16, weight="bold", fill="#FFFFFF"))
    return bits


def build(repo: str) -> tuple[str, int]:
    body: list[str] = []

    HIER_CX = 400
    HOME_W = 460
    NODE_H = 60

    y = 110

    # --- Utility pages (peer of Home, not a child) — centred above Home,
    # kept clear of the screenshot column entirely.
    util_w, util_h, util_gap = 150, 44, 14
    util_titles = ["Methods", "Data", "About", "FAQ"]
    util_row_w = len(util_titles) * util_w + (len(util_titles) - 1) * util_gap
    ux = HIER_CX - util_row_w / 2
    uy = y
    for t in util_titles:
        body.append(util_box(ux + util_w / 2, uy, util_w, util_h, t))
        ux += util_w + util_gap
    body.append(text_block(HIER_CX, uy - 14,
                            ["Top navigation (every page)"], size=9.5, fill=COLORS["util_text"], style="italic"))

    # --- Home --------------------------------------------------------
    home_top = uy + util_h + 46
    body.append(page_box(HIER_CX, home_top, HOME_W, NODE_H, "Home"))
    home_cx_right = HIER_CX + HOME_W / 2
    home_cy = home_top + NODE_H / 2

    # High severity / global summary — sections within Home (dashed)
    sec_w, sec_h = 190, 46
    sec_gap = 24
    sec_y = home_top + NODE_H + 46
    sec1_cx = HIER_CX - sec_w / 2 - sec_gap / 2
    sec2_cx = HIER_CX + sec_w / 2 + sec_gap / 2
    body.append(section_box(sec1_cx, sec_y, sec_w, sec_h, "High Severity Countries"))
    body.append(section_box(sec2_cx, sec_y, sec_w, sec_h, "Global Summary"))
    body.append(arrow_line(HIER_CX - 30, home_top + NODE_H, sec1_cx, sec_y, dash="4,4", marker=False, sw=1.4))
    body.append(arrow_line(HIER_CX + 30, home_top + NODE_H, sec2_cx, sec_y, dash="4,4", marker=False, sw=1.4))

    branch_top = sec_y + sec_h + 60

    # --- Regions | All Countries -------------------------------------
    child_w = 300
    child_gap = 120
    cx_left = HIER_CX - child_gap / 2 - child_w / 2
    cx_right = HIER_CX + child_gap / 2 + child_w / 2

    body.append(arrow_path([(HIER_CX, sec_y + sec_h + 14), (HIER_CX, branch_top - 26),
                             (cx_left, branch_top - 26), (cx_left, branch_top)]))
    body.append(arrow_path([(HIER_CX, sec_y + sec_h + 14), (HIER_CX, branch_top - 26),
                             (cx_right, branch_top - 26), (cx_right, branch_top)]))

    body.append(page_box(cx_left, branch_top, child_w, NODE_H, "All Countries"))
    body.append(page_box(cx_right, branch_top, child_w, NODE_H, "Regions"))

    # --- National page (converges both) -------------------------------
    nat_top = branch_top + NODE_H + 70
    body.append(arrow_path([(cx_left, branch_top + NODE_H), (cx_left, nat_top - 26),
                             (HIER_CX, nat_top - 26), (HIER_CX, nat_top)]))
    body.append(arrow_path([(cx_right, branch_top + NODE_H), (cx_right, nat_top - 26),
                             (HIER_CX, nat_top - 26), (HIER_CX, nat_top)]))
    body.append(page_box(HIER_CX, nat_top, HOME_W, NODE_H, "National Page"))

    canvas_content_bottom = nat_top + NODE_H

    # ==================================================================
    # Screenshot placeholders — right column.
    # Home and National are lone full-width nodes, so a direct curve to
    # their placeholder never crosses anything. All Countries and Regions
    # sit side by side, so a direct line from All Countries would cross
    # straight through the Regions box — instead it escapes upward to a
    # lane above the row first, then drops into the screenshot column.
    # ==================================================================
    SHOT_X = 1020
    SHOT_W = 430
    row_safe_y = branch_top - 16

    cursor = 100

    def place_shot(label, h, connector_svg):
        nonlocal cursor
        top = cursor
        body.append(shot_box(SHOT_X + SHOT_W / 2, top, SHOT_W, h, label))
        body.append(connector_svg(top + h / 2))
        cursor = top + h + 30

    place_shot("Home Page", 220,
                lambda ty: curve(home_cx_right, home_cy, SHOT_X, ty))
    place_shot("All Countries Page", 190,
                lambda ty: arrow_path([
                    (cx_left + child_w / 2, branch_top + NODE_H / 2),
                    (cx_left + child_w / 2, row_safe_y),
                    (SHOT_X - 40, row_safe_y),
                    (SHOT_X - 40, ty),
                    (SHOT_X, ty),
                ], color=COLORS["shot_edge"], dash="5,4", sw=1.8))
    place_shot("Regional Page", 190,
                lambda ty: arrow_path([
                    (cx_right + child_w / 2, branch_top + NODE_H / 2),
                    (cx_right + child_w / 2, row_safe_y),
                    (SHOT_X - 20, row_safe_y),
                    (SHOT_X - 20, ty),
                    (SHOT_X, ty),
                ], color=COLORS["shot_edge"], dash="5,4", sw=1.8))
    place_shot("National Page", 220,
                lambda ty: curve(HIER_CX + HOME_W / 2, nat_top + NODE_H / 2, SHOT_X, ty))

    canvas_h = int(max(canvas_content_bottom, cursor) + 60)

    logo_uri = logo_data_uri(repo)
    header = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
        f'width="{W}" height="{canvas_h}" viewBox="0 0 {W} {canvas_h}">',
        "<defs>",
        '<marker id="ah" markerWidth="8" markerHeight="8" refX="6" refY="3" orient="auto">',
        f'<polygon points="0 0, 8 3, 0 6" fill="{COLORS["arrow"]}"/>',
        "</marker>",
        '<marker id="ah2" markerWidth="8" markerHeight="8" refX="6" refY="3" orient="auto">',
        f'<polygon points="0 0, 8 3, 0 6" fill="{COLORS["shot_edge"]}"/>',
        "</marker>",
        "</defs>",
        f'<rect width="{W}" height="{canvas_h}" fill="#FFFFFF"/>',
        *title_bar(logo_uri),
    ]

    full = "\n".join(header + body + ["</svg>"])
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
        print("PNG export skipped (open SVG in a browser or Illustrator to export)")


if __name__ == "__main__":
    main()
