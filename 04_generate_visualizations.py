"""
04_generate_visualizations.py
=============================
Generate all interactive visualizations as standalone HTML files.
These HTML files are embedded in the GitHub Pages website via <iframe>.

Reads from: data/processed/ (CSVs exported from Snowflake)
Writes to:  docs/charts/    (standalone HTML chart files)

Usage:
  pip install plotly pandas folium
  python 04_generate_visualizations.py

Note:
  Before running, export query results from Snowflake (Section 6 of
  02_snowflake_etl.sql) and save them as CSVs in data/processed/:
    - permits_trend.csv
    - processing_time.csv
    - neighborhood_activity.csv
    - change_of_use.csv
    - housing_types.csv
    - hpi_clean.csv
    - census_clean.csv
    - neighborhood_income_vs_wait.csv
"""

import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import folium
from folium.plugins import MarkerCluster
from pathlib import Path
import json

# ──────────────────────────────────────────────
# Configuration
# ──────────────────────────────────────────────
DATA_DIR = Path(__file__).parent.parent / "data" / "processed"
CHART_DIR = Path(__file__).parent.parent / "docs" / "charts"
CHART_DIR.mkdir(parents=True, exist_ok=True)

# Color palette (matches Marilyn's scheme but in Python)
PALETTE = ["#004777", "#A30000", "#FF7700", "#EFD28D", "#00AFB5"]
EXTENDED_PALETTE = [
    "#004777", "#A30000", "#FF7700", "#EFD28D", "#00AFB5",
    "#6A4C93", "#1982C4", "#8AC926", "#FFCA3A", "#FF595E"
]

CHART_TEMPLATE = "plotly_white"
FONT_FAMILY = "Inter, Arial, sans-serif"
TITLE_COLOR = "#004777"


def save_chart(fig, filename: str):
    """Save a Plotly figure as a standalone HTML file."""
    path = CHART_DIR / filename
    fig.write_html(
        str(path),
        include_plotlyjs="cdn",      # use CDN to keep file small
        full_html=True,
        config={
            "displayModeBar": True,
            "displaylogo": False,
            "modeBarButtonsToRemove": ["lasso2d", "select2d"]
        }
    )
    size_kb = path.stat().st_size / 1024
    print(f"  ✓ {filename} ({size_kb:.0f} KB)")


def style_fig(fig, title: str, height: int = 550):
    """Apply consistent styling to all figures."""
    fig.update_layout(
        title=dict(
            text=f"<b>{title}</b>",
            font=dict(family=FONT_FAMILY, size=18, color=TITLE_COLOR),
            x=0.02,
        ),
        font=dict(family=FONT_FAMILY, size=13),
        template=CHART_TEMPLATE,
        height=height,
        margin=dict(l=60, r=40, t=70, b=60),
        plot_bgcolor="white",
        paper_bgcolor="white",
    )
    return fig


# ──────────────────────────────────────────────
# Chart 1: Housing Types (Bar Chart)
# ──────────────────────────────────────────────
def chart_housing_types():
    print("\n📊 Chart 1: Housing Types")
    df = pd.read_csv(DATA_DIR / "housing_types.csv")
    df = df.sort_values("parcel_count", ascending=False)

    fig = px.bar(
        df, x="restype", y="parcel_count",
        color="restype",
        color_discrete_sequence=EXTENDED_PALETTE,
        text="parcel_count",
        labels={"restype": "Residential Type", "parcel_count": "Parcel Count"},
    )
    fig.update_traces(texttemplate="%{text:,}", textposition="outside")
    fig.update_layout(showlegend=False, xaxis_tickangle=-30)
    style_fig(fig, "San Francisco Land Parcels by Residential Type (2023)")
    save_chart(fig, "housing_types.html")


# ──────────────────────────────────────────────
# Chart 2: Permits Trend (Line Chart)
# ──────────────────────────────────────────────
def chart_permits_trend():
    print("\n📊 Chart 2: Permits Trend")
    df = pd.read_csv(DATA_DIR / "permits_trend.csv")

    # Get top 5 permit types by total count
    top_types = (
        df.groupby("permit_type")["permit_count"]
        .sum()
        .nlargest(5)
        .index.tolist()
    )
    df_top = df[df["permit_type"].isin(top_types)]

    fig = px.line(
        df_top, x="issued_year", y="permit_count",
        color="permit_type",
        markers=True,
        color_discrete_sequence=EXTENDED_PALETTE,
        labels={
            "issued_year": "Year",
            "permit_count": "Number of Permits",
            "permit_type": "Permit Type"
        },
    )
    fig.update_traces(line=dict(width=2.5))
    style_fig(fig, "Building Permits Issued by Type (2000–2024)")
    fig.update_layout(
        legend=dict(
            orientation="h", yanchor="bottom", y=-0.35, xanchor="center", x=0.5
        )
    )
    save_chart(fig, "permits_trend.html")


# ──────────────────────────────────────────────
# Chart 3: Change of Use (Sankey Diagram)
# ──────────────────────────────────────────────
def chart_change_of_use():
    print("\n📊 Chart 3: Change of Use (Sankey)")
    df = pd.read_csv(DATA_DIR / "change_of_use.csv")

    # Build node lists
    sources = df["existing_use"].unique().tolist()
    targets = df["proposed_use"].unique().tolist()
    all_nodes = sources + [t for t in targets if t not in sources]
    node_idx = {name: i for i, name in enumerate(all_nodes)}

    # Assign colors
    n = len(all_nodes)
    node_colors = (EXTENDED_PALETTE * ((n // len(EXTENDED_PALETTE)) + 1))[:n]
    link_colors = [
        node_colors[node_idx[row["existing_use"]]].replace(")", ", 0.4)").replace("rgb", "rgba")
        if "rgb" in node_colors[node_idx[row["existing_use"]]]
        else node_colors[node_idx[row["existing_use"]]] + "66"
        for _, row in df.iterrows()
    ]

    fig = go.Figure(go.Sankey(
        node=dict(
            pad=15, thickness=20,
            label=all_nodes,
            color=node_colors,
        ),
        link=dict(
            source=[node_idx[r] for r in df["existing_use"]],
            target=[node_idx[r] for r in df["proposed_use"]],
            value=df["flow_count"].tolist(),
            color=link_colors,
        ),
    ))
    style_fig(fig, "Top Changes in Building Use", height=600)
    fig.update_layout(
        annotations=[
            dict(x=0.01, y=1.08, text="<b>Existing Use</b>", showarrow=False,
                 font=dict(size=14, color="#666")),
            dict(x=0.99, y=1.08, text="<b>Proposed Use</b>", showarrow=False,
                 font=dict(size=14, color="#666")),
        ]
    )
    save_chart(fig, "change_of_use.html")


# ──────────────────────────────────────────────
# Chart 4: Processing Time (Box Plot)
# ──────────────────────────────────────────────
def chart_processing_time():
    print("\n📊 Chart 4: Processing Time")
    df = pd.read_csv(DATA_DIR / "processing_time.csv")

    # Get top types by count for readability
    type_counts = df["permit_type"].value_counts()
    top_types = type_counts.nlargest(6).index.tolist()
    df_top = df[df["permit_type"].isin(top_types)]

    fig = px.box(
        df_top, x="permit_type", y="processing_days",
        color="permit_type",
        color_discrete_sequence=EXTENDED_PALETTE,
        labels={
            "permit_type": "Permit Type",
            "processing_days": "Processing Time (Days)"
        },
    )
    fig.update_layout(showlegend=False)
    style_fig(fig, "Distribution of Permit Processing Times")
    fig.update_layout(
        xaxis_tickangle=-20,
        yaxis_title="Days from Filing to Issuance"
    )
    save_chart(fig, "processing_time.html")


# ──────────────────────────────────────────────
# Chart 5: Neighborhood Activity (Horizontal Bar)
# ──────────────────────────────────────────────
def chart_neighborhoods():
    print("\n📊 Chart 5: Neighborhood Activity")
    df = pd.read_csv(DATA_DIR / "neighborhood_activity.csv")
    df = df.sort_values("total_permits", ascending=True)

    fig = px.bar(
        df, x="total_permits", y="neighborhood",
        orientation="h",
        text="total_permits",
        color="total_permits",
        color_continuous_scale=["#EFD28D", "#FF7700", "#A30000"],
        labels={
            "total_permits": "Total Permits",
            "neighborhood": ""
        },
    )
    fig.update_traces(texttemplate="%{text:,}", textposition="outside")
    fig.update_layout(coloraxis_showscale=False)
    style_fig(fig, "Top 20 Neighborhoods by Permit Activity", height=650)
    save_chart(fig, "neighborhoods.html")


# ──────────────────────────────────────────────
# Chart 6: House Price Index (Line Chart)
# ──────────────────────────────────────────────
def chart_hpi():
    print("\n📊 Chart 6: House Price Index")
    df = pd.read_csv(DATA_DIR / "hpi_clean.csv")
    df["observation_date"] = pd.to_datetime(df["observation_date"])

    fig = px.line(
        df, x="observation_date", y="hpi_value",
        labels={
            "observation_date": "",
            "hpi_value": "Index Value"
        },
    )
    fig.update_traces(
        line=dict(color=PALETTE[2], width=2.5),
        hovertemplate="<b>%{x|%Y Q%q}</b><br>Index: %{y:,.1f}<extra></extra>"
    )
    style_fig(fig, "House Price Index: San Francisco MSA (1975=100)")
    fig.update_layout(
        annotations=[
            dict(
                x="2008-06-01", y=None, yref="paper", y0=0, y1=1,
                text="2008 Financial Crisis", showarrow=True,
                arrowhead=2, ax=0, ay=-40,
                font=dict(size=11, color="#A30000")
            ),
        ]
    )
    save_chart(fig, "hpi.html")


# ──────────────────────────────────────────────
# Chart 7: Income vs Processing Time (Scatter)
# ── YOUR UNIQUE ADDITION ─────────────────────
# ──────────────────────────────────────────────
def chart_income_vs_wait():
    print("\n📊 Chart 7: Income vs Processing Time (Cross-analysis)")
    df = pd.read_csv(DATA_DIR / "neighborhood_income_vs_wait.csv")

    fig = px.scatter(
        df, x="avg_median_income", y="avg_wait_days",
        size="permit_count", color="avg_median_rent",
        hover_name="neighborhood",
        color_continuous_scale=["#004777", "#00AFB5", "#FF7700", "#A30000"],
        size_max=40,
        labels={
            "avg_median_income": "Avg. Median Household Income ($)",
            "avg_wait_days": "Avg. Permit Processing Time (Days)",
            "permit_count": "Total Permits",
            "avg_median_rent": "Avg. Median Rent ($)"
        },
    )
    style_fig(fig, "Neighborhood Income vs. Permit Processing Time")
    fig.update_layout(
        coloraxis_colorbar=dict(title="Median<br>Rent ($)"),
        xaxis_tickformat="$,.0f",
    )
    save_chart(fig, "income_vs_wait.html")


# ──────────────────────────────────────────────
# Chart 8: Median Rent Map (Folium Choropleth)
# ──────────────────────────────────────────────
def chart_rent_map():
    """
    NOTE: This chart requires Census tract GeoJSON boundaries.
    Download from: https://www2.census.gov/geo/tiger/TIGER2023/TRACT/
    For the 9 Bay Area counties, or use the Census TIGERweb API.

    If you have a GeoJSON file, uncomment and adapt below.
    Otherwise, this creates a simpler point-based map.
    """
    print("\n📊 Chart 8: Median Rent Map")
    df = pd.read_csv(DATA_DIR / "census_clean.csv")
    df = df.dropna(subset=["median_rent"])

    # Simple approach: create a summary by county
    county_names = {
        "001": "Alameda", "013": "Contra Costa", "041": "Marin",
        "055": "Napa", "075": "San Francisco", "081": "San Mateo",
        "085": "Santa Clara", "095": "Solano", "097": "Sonoma"
    }

    # County center coordinates (approximate)
    county_coords = {
        "001": (37.6017, -121.7195),  # Alameda
        "013": (37.9535, -122.0311),   # Contra Costa
        "041": (38.0834, -122.7633),   # Marin
        "055": (38.5025, -122.2654),   # Napa
        "075": (37.7749, -122.4194),   # San Francisco
        "081": (37.4337, -122.4014),   # San Mateo
        "085": (37.3541, -121.9552),   # Santa Clara
        "095": (38.2494, -122.0400),   # Solano
        "097": (38.5110, -122.9750),   # Sonoma
    }

    county_stats = (
        df.groupby("county")
        .agg(
            median_rent=("median_rent", "median"),
            median_income=("median_income", "median"),
            pop_total=("pop_total", "sum"),
            tract_count=("tract", "count"),
        )
        .reset_index()
    )

    # Create Folium map
    m = folium.Map(
        location=[37.75, -122.25],
        zoom_start=9,
        tiles="CartoDB positron"
    )

    # Color scale
    import branca.colormap as cm
    colormap = cm.LinearColormap(
        colors=["#004777", "#72A1C6", "#EFD28D", "#FF7700", "#A30000"],
        vmin=df["median_rent"].quantile(0.1),
        vmax=df["median_rent"].quantile(0.9),
        caption="Median Rent ($)"
    )
    colormap.add_to(m)

    # Add county markers with rent info
    for _, row in county_stats.iterrows():
        fips = row["county"]
        if fips in county_coords:
            lat, lon = county_coords[fips]
            name = county_names.get(fips, fips)
            color = colormap(row["median_rent"])

            popup_html = f"""
            <div style="font-family: Arial; width: 200px;">
                <h4 style="color: #004777; margin: 0 0 8px 0;">{name} County</h4>
                <b>Median Rent:</b> ${row['median_rent']:,.0f}<br>
                <b>Median Income:</b> ${row['median_income']:,.0f}<br>
                <b>Population:</b> {row['pop_total']:,.0f}<br>
                <b>Census Tracts:</b> {row['tract_count']}
            </div>
            """

            folium.CircleMarker(
                location=[lat, lon],
                radius=max(8, row["pop_total"] / 100000),
                popup=folium.Popup(popup_html, max_width=250),
                color=color,
                fill=True,
                fill_color=color,
                fill_opacity=0.7,
                weight=2,
            ).add_to(m)

    # Save
    map_path = CHART_DIR / "rent_map.html"
    m.save(str(map_path))
    size_kb = map_path.stat().st_size / 1024
    print(f"  ✓ rent_map.html ({size_kb:.0f} KB)")

    # ── ADVANCED VERSION (with tract-level choropleth) ──
    # If you have tract GeoJSON, use this instead:
    #
    # geojson_path = DATA_DIR / "bay_area_tracts.geojson"
    # with open(geojson_path) as f:
    #     geojson = json.load(f)
    #
    # m = folium.Map(location=[37.75, -122.25], zoom_start=9, tiles="CartoDB positron")
    # folium.Choropleth(
    #     geo_data=geojson,
    #     data=df,
    #     columns=["geoid", "median_rent"],
    #     key_on="feature.properties.GEOID",
    #     fill_color="YlOrRd",
    #     fill_opacity=0.7,
    #     line_opacity=0.2,
    #     legend_name="Median Rent ($)",
    # ).add_to(m)
    # m.save(str(CHART_DIR / "rent_map.html"))


# ──────────────────────────────────────────────
# Chart 9: Permits vs HPI Dual-Axis (Unique)
# ── ANOTHER UNIQUE ADDITION ──────────────────
# ──────────────────────────────────────────────
def chart_permits_vs_hpi():
    print("\n📊 Chart 9: Permits vs HPI (Dual-axis)")

    # Load and prepare permits trend (annual total)
    permits = pd.read_csv(DATA_DIR / "permits_trend.csv")
    annual_total = (
        permits
        .groupby("issued_year")["permit_count"]
        .sum()
        .reset_index()
    )
    annual_total.columns = ["year", "total_permits"]

    # Load and prepare HPI (annual average)
    hpi = pd.read_csv(DATA_DIR / "hpi_clean.csv")
    hpi["observation_date"] = pd.to_datetime(hpi["observation_date"])
    hpi["year"] = hpi["observation_date"].dt.year
    annual_hpi = hpi.groupby("year")["hpi_value"].mean().reset_index()

    # Merge
    merged = annual_total.merge(annual_hpi, on="year", how="inner")
    merged = merged[(merged["year"] >= 2000) & (merged["year"] <= 2024)]

    # Dual-axis chart
    fig = go.Figure()

    fig.add_trace(go.Bar(
        x=merged["year"], y=merged["total_permits"],
        name="Total Permits Issued",
        marker_color=PALETTE[0],
        opacity=0.7,
        yaxis="y",
    ))

    fig.add_trace(go.Scatter(
        x=merged["year"], y=merged["hpi_value"],
        name="House Price Index",
        line=dict(color=PALETTE[2], width=3),
        mode="lines+markers",
        yaxis="y2",
    ))

    fig.update_layout(
        yaxis=dict(title="Permits Issued", side="left", showgrid=False),
        yaxis2=dict(
            title="House Price Index",
            side="right", overlaying="y",
            showgrid=False,
        ),
        legend=dict(orientation="h", yanchor="bottom", y=-0.25, x=0.5, xanchor="center"),
        barmode="overlay",
    )
    style_fig(fig, "Annual Permits Issued vs. House Price Index (2000–2024)")
    save_chart(fig, "permits_vs_hpi.html")


# ──────────────────────────────────────────────
# Main
# ──────────────────────────────────────────────
if __name__ == "__main__":
    print("=" * 60)
    print("  Generating Visualizations")
    print("=" * 60)

    chart_housing_types()
    chart_permits_trend()
    chart_change_of_use()
    chart_processing_time()
    chart_neighborhoods()
    chart_hpi()
    chart_income_vs_wait()
    chart_rent_map()
    chart_permits_vs_hpi()

    print("\n" + "=" * 60)
    print(f"  ✅  All charts saved to {CHART_DIR}/")
    print("=" * 60)
    print("\nGenerated files:")
    for f in sorted(CHART_DIR.glob("*.html")):
        print(f"  • {f.name}")
