#%%
# from jupyter_dash import JupyterDash # only for local dev
import dash
import dash_core_components as dcc
import dash_html_components as html
import pandas as pd

#%%
# preds = pd.read_parquet('preds_favorites.parquet')
# preds.to_csv('preds_favorites.csv', index=False)
preds = pd.read_csv('preds_favorites.csv')
preds
#%%
# app = dash.Dash()
external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']
# app = JupyterDash(__name__, external_stylesheets=external_stylesheets) # local
app = dash.Dash(__name__, external_stylesheets=external_stylesheets)
# app.run_server(mode='inline') # local
server = app.server  # prod
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

# %%
if __name__ == "__main__":
    app.run_server()