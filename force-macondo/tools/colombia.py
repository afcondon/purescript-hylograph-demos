"""Draw the watermark map: Colombia's outline, its rivers (clipped to the
country), and Aracataca.

    python3 tools/colombia.py <ne_10m_admin_0_countries.geojson> <ne_10m_rivers_lake_centerlines.geojson> public/colombia.svg

Natural Earth (public domain). Equirectangular, scaled by the cosine of the
country's middle latitude; coordinates rounded to tenths, rings thinned.
"""
import json, math, sys

countries, rivers, out = sys.argv[1:4]
LAT0 = 4.5
K = math.cos(math.radians(LAT0))
ARACATACA = (-74.1917, 10.5917)
SCALE = 60.0  # SVG units per degree

def project(lon, lat):
    return (lon * K * SCALE, -lat * SCALE)

col = next(f for f in json.load(open(countries))["features"] if f["properties"].get("ADM0_A3") == "COL")
geom = col["geometry"]
polys = geom["coordinates"] if geom["type"] == "MultiPolygon" else [geom["coordinates"]]

def thin(points, tolerance):
    # keep a point when it is far enough from the last kept one
    kept = [points[0]]
    for p in points[1:]:
        if math.dist(p, kept[-1]) >= tolerance:
            kept.append(p)
    return kept

rings = []
for poly in polys:
    outer = [project(*p) for p in poly[0]]
    if len(outer) < 20:  # the smallest islets add nothing at this scale
        continue
    rings.append(thin(outer, 0.6))

xs = [x for r in rings for x, _ in r]; ys = [y for r in rings for _, y in r]
x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
pad = 20
minlon, maxlon, minlat, maxlat = -79.5, -66.5, -4.5, 13.0

def inside(lon, lat):
    return minlon <= lon <= maxlon and minlat <= lat <= maxlat

lines = []
for f in json.load(open(rivers))["features"]:
    if f["properties"].get("featurecla") != "River":
        continue
    g = f["geometry"]
    parts = g["coordinates"] if g["type"] == "MultiLineString" else [g["coordinates"]]
    for part in parts:
        if not any(inside(*p) for p in part):
            continue
        pts = [project(*p) for p in part if inside(*p)]
        if len(pts) > 2:
            lines.append((f["properties"].get("name") or "", thin(pts, 0.8)))

def d(points, close):
    s = "M" + " L".join(f"{x - x0 + pad:.1f} {y - y0 + pad:.1f}" for x, y in points)
    return s + (" Z" if close else "")

ax, ay = project(*ARACATACA)
ax, ay = ax - x0 + pad, ay - y0 + pad
w, h = x1 - x0 + 2 * pad, y1 - y0 + 2 * pad
svg = f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {w:.0f} {h:.0f}">
<!-- Colombia, its rivers, and Aracataca. Natural Earth 1:10m, public domain. -->
<defs><clipPath id="land"><path d="{" ".join(d(r, True) for r in rings)}"/></clipPath></defs>
<g fill="none" stroke="#5a4630" stroke-linejoin="round" stroke-linecap="round">
<path stroke-width="3.2" d="{" ".join(d(r, True) for r in rings)}"/>
<path stroke-width="1.6" opacity="0.8" clip-path="url(#land)" d="{" ".join(d(p, False) for _, p in lines)}"/>
</g>
<g fill="#5a4630">
<circle cx="{ax:.1f}" cy="{ay:.1f}" r="4.5"/>
<text x="{ax + 9:.1f}" y="{ay + 5:.1f}" font-family="'IM Fell English', Georgia, serif" font-style="italic" font-size="19">Aracataca</text>
</g>
</svg>
'''
open(out, "w").write(svg)
print(f"{len(rings)} rings, {len(lines)} rivers, {len(svg) // 1024} KB, viewBox {w:.0f}x{h:.0f}")
