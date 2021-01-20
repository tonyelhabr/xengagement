#%%
# from jupyter_dash import JupyterDash # only for local dev
import dash
import dash_core_components as dcc
import dash_html_components as html
import pandas as pd

#%%
# preds = pd.read_parquet('preds_favorites.parquet')
# preds.to_csv('preds_favorites.csv', index=False)
preds = pd.read_csv('xengagement.csv')
preds

#%%
external_stylesheets = [
    {
        'href':
            'https://fonts.googleapis.com/css2?'
            'family=Lato:wght@400;700&display=swap',
        'rel': 'stylesheet',
    },
]

#%%
# app = dash.Dash()
# app = JupyterDash(__name__, external_stylesheets=external_stylesheets) # local
app = dash.Dash(__name__, external_stylesheets=external_stylesheets)
# app.run_server(mode='inline') # local
server = app.server  # prod
#%%
app.layout = html.Div(
    [
        html.H1('An attempt was made'),
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
if __name__ == '__main__':
    app.run_server()