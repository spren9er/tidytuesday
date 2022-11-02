# %%
from pathlib import Path

import altair as alt
import pandas as pd
import pycountry as pyc
import umap
from sklearn.preprocessing import StandardScaler

# fmt:off

# %%
df = pd.read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv"
)

# %%
data_path = Path(__file__).parent.absolute() / "data"
df_noc_regions = pd.read_csv(data_path / 'tidytuesday_202131_olympics_noc_regions.csv')

# %%
ioc_codes = {
    "ALG": "DZA",
    "ASA": "ASM",
    "ANG": "AGO",
    "ANT": "ATG",
    "ARU": "ABW",
    "BAH": "BHS",
    "BRN": "BHR",
    "BAN": "BGD",
    "BAR": "BRB",
    "BIZ": "BLZ",
    "BER": "BMU",
    "BHU": "BTN",
    "BOT": "BWA",
    "IVB": "VGB",
    "BRU": "BRN",
    "BUL": "BGR",
    "BUR": "BFA",
    "CAM": "KHM",
    "CAY": "CYM",
    "CHA": "TCD",
    "CHI": "CHL",
    "CGO": "COG",
    "CRC": "CRI",
    "CRO": "HRV",
    "DEN": "DNK",
    "ESA": "SLV",
    "GEQ": "GNQ",
    "FIJ": "FJI",
    "GAM": "GMB",
    "GER": "DEU",
    "GRE": "GRC",
    "GRN": "GRD",
    "GUA": "GTM",
    "GUI": "GIN",
    "GBS": "GNB",
    "HAI": "HTI",
    "HON": "HND",
    "INA": "IDN",
    "IRI": "IRN",
    "KUW": "KWT",
    "LAT": "LVA",
    "LIB": "LBN",
    "LES": "LSO",
    "LBA": "LBY",
    "MAD": "MDG",
    "MAW": "MWI",
    "MAS": "MYS",
    "MTN": "MRT",
    "MRI": "MUS",
    "MON": "MCO",
    "MGL": "MNG",
    "MYA": "MMR",
    "NEP": "NPL",
    "NED": "NLD",
    "NCA": "NIC",
    "NIG": "NER",
    "NGR": "NGA",
    "OMA": "OMN",
    "PLE": "PSE",
    "PAR": "PRY",
    "PHI": "PHL",
    "POR": "PRT",
    "PUR": "PRI",
    "SKN": "KNA",
    "VIN": "VCT",
    "SAM": "WSM",
    "KSA": "SAU",
    "SEY": "SYC",
    "SIN": "SGP",
    "SLO": "SVN",
    "SOL": "SLB",
    "RSA": "ZAF",
    "SRI": "LKA",
    "SUD": "SDN",
    "SUI": "CHE",
    "TPE": "TWN",
    "TAN": "TZA",
    "TOG": "TGO",
    "TGA": "TON",
    "TRI": "TTO",
    "UAE": "ARE",
    "ISV": "VIR",
    "URU": "URY",
    "VAN": "VUT",
    "VIE": "VNM",
    "ZAM": "ZMB",
    "ZIM": "ZWE",
}

# %%
def noc_to_country(noc):
    if noc == 'IOA':
        return 'Independent Olympic Athletes';

    if noc == 'KOS':
        return 'Kosovo';

    try:
        return pyc.countries.get(alpha_3=noc).name
    except:
        return pyc.countries.get(alpha_3=ioc_codes[noc]).name

# %% [markdown]
# ## Rio 2016 Medals

# %%
df_rio_medals = (
    df
    .query("year == 2016")
    .dropna(subset=["medal"])
    .filter(["event", "noc", "medal"])
    .drop_duplicates()
)
df_rio_medals["country"] = df_rio_medals["noc"].apply(noc_to_country)
df_rio_medals["sports"] = df_rio_medals["event"].str.split(" ").str[0]

# %%
df_medals_per_country = (
    df_rio_medals
    .value_counts(["country", "medal"])
    .rename('total')
    .reset_index()
    .pivot_table(index='country', columns='medal', values='total', fill_value=0)
)

# %%
(
    df_medals_per_country
    .sort_values(by=["Gold", "Silver", "Bronze"], ascending=False)
    .filter(["medal", "Gold", "Silver", "Bronze"])
)

# %%
(
    df_rio_medals
    .value_counts(["country", "sports"])
    .rename('total')
    .reset_index()
    .pivot_table(index='country', columns='sports', values='total', fill_value=0)
)

# %% [markdown]
# ## Clustering Athletes of Rio 2016 (age, weight, height)

# %%
reducer = umap.UMAP()

# %%
df_rio = df.query("year == 2016").dropna(subset=['age', 'height', 'weight'])
num_data = df_rio.filter(['age', 'height', 'weight'])
std_data = StandardScaler().fit_transform(num_data.values)
embedding = reducer.fit_transform(std_data)

# %%
df_rio_embedded = pd.concat(
    [
        df_rio.reset_index(drop=True),
        pd.DataFrame(embedding, columns=['x', 'y'])
    ],
    axis=1
)

# %%
(
    df_rio_embedded
    .pipe(alt.Chart)
    .mark_circle(opacity=0.1)
    .encode(
        x=alt.X("x:Q"),
        y=alt.Y("y:Q"),
        tooltip=['age', 'height', 'weight'],
    )
    .interactive()
)

# %% [markdown]
# ## Olympic Medals by Country

# %%
df_medals = df[df['medal'].notna()]
df_medals["country"] = df_medals["noc"].apply(noc_to_country)

# %%
(
    df_medals
    .value_counts(['country', 'medal'])
)
