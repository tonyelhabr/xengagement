#%%
from jupyter_dash import JupyterDash
import dash
import dash_core_components as dcc
import dash_html_components as html
# import plotly.express as px
import pandas as pd
# external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']
#%%
preds = pd.read_parquet('preds_favorites.parquet')

#%%
# app = dash.Dash()
external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']
app = JupyterDash(__name__, external_stylesheets=external_stylesheets)

#%%
app.layout = html.Div(
    [
        html.H1('Header'),
        dcc.Graph(
            id='fig1',
            figure={
                'data':
                    [
                        dict(
                            x=preds['favorite_count'],
                            y=preds['.pred'],
                            mode='markers'
                        )
                    ]
            }
        )
    ]
)
# app.run_server(mode='inline')
# app.server = server

# %%
