import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import dash_table
import datetime
import plotly.graph_objs as go
import pandas as pd
pd.options.mode.chained_assignment = None

# utils ------------------------------------------------------------------------
# import pandas as pd
# pd.options.mode.chained_assignment = None
# import dash_core_components as dcc
# import dash_html_components as html
# import dash_table
# # from functools import lru_cache

app_colors = {
    'blue': '#003f5c',
    'orange': '#ffa600',
    'purple': '#7a5193',
    'pink': '#ef5675',
    'grey50': '#7f7f7f',
    'grey80': '#cccccc'
}


# @lru_cache(maxsize=None)
def _import_path(stem):
    return pd.read_csv(
        f'https://raw.githubusercontent.com/tonyelhabr/xengagement/master/inst/extdata/{stem}.csv'
    )


def import_shap():
    return _import_path('shap')


# @lru_cache(maxsize=None)
def import_preds():
    preds = _import_path('preds')
    preds.sort_values('created_at', ascending=False, inplace=True)
    preds['date'] = pd.to_datetime(preds['created_at']).dt.date
    preds['total_diff_prnk'] = preds['total_diff_prnk'] * 100
    preds['link'] = (
        # '[\U0001f517](https://twitter.com/xGPhilosophy/status/' +
        '[Link](https://twitter.com/xGPhilosophy/status/' +
        preds['status_id'].astype(str) + ')'
    )
    return preds


def generate_about(preds=import_preds()):
    preds_sort = preds.sort_values('total_diff_prnk', ascending=False)
    pred_top = preds_sort.iloc[0]

    def _pull(col):
        return pred_top[[col]].astype(str).values[0]

    text_top = _pull('lab_text')
    date_top = _pull('date')
    link_top = _pull('link').replace('[Link]', '')
    about_md = dcc.Markdown(
        f'''
    
    [xGPhilosophy](https://twitter.com/xGPhilosophy) is a Twitter account that provides end-of-match xG summaries of major games across the football world, focusing mostly on the Premier League.

    This app pulls in the number of favorites (likes) and retweets that every xGPhilosphy end-of-match tweet has received. Models have been trained and used to make predictions for these numbers, providing "expected favorites" (xFavorites) and "expected retweets" (xRetweets) that can be viewed on the [__"Data & Predictions"__ page](/apps/data-and-predictions).

    The [__"Prediction Explanation"__ page](/apps/shap) describes the factors that go into the xFavorite and xRetweet numbers for each tweet using [SHAP values](https://proceedings.neurips.cc/paper/2017/hash/8a20a8621978632d76c43dfd28b67767-Abstract.html). A positive SHAP value indicates that a given feature (e.g. goals scored by the home team) contributed to making the prediction greater than the average prediction across all tweets, and visa versa for a negative SHAP value.

    The [__"Leaderboard"__ page](/apps/leaderboard) lists tweets in order of an "Engagement Over Expected (EOE)" metric that assigns each tweet a percentile based on the predicted and actual number of favorites and retweets, where 100 is the tweet receiving the most engagement compared to expectation. As of today, the tweet with the top EOE was the recap for [{text_top}]{link_top}.
    ```
    '''
    )
    return about_md


def generate_leaderboard(preds):
    # preds['total_diff_prnk'] = preds['total_diff_prnk'].map('{:,.1f}%'.format)
    preds_sort = preds.sort_values('total_diff_prnk', ascending=False)
    # preds.sort_values('created_at', ascending=False, inplace=True)
    preds_sort['link'] = (
        # '[\U0001f517](https://twitter.com/xGPhilosophy/status/' +
        '[Link](https://twitter.com/xGPhilosophy/status/' +
        preds_sort['status_id'].astype(str) + ')'
    )
    cols_table = [
        {
            'name': ['', 'Date'],
            'id': 'date',
            'type': 'datetime'
        }, {
            'name': ['Home', 'Team'],
            'id': 'tm_h',
            'type': 'lab_text'
        }, {
            'name': ['Home', 'G'],
            'id': 'g_h',
            'type': 'numeric'
        }, {
            'name': ['Home', 'xG'],
            'id': 'xg_h',
            'type': 'numeric'
        }, {
            'name': ['Away', 'Team'],
            'id': 'tm_a',
            'type': 'lab_text'
        }, {
            'name': ['Away', 'G'],
            'id': 'g_a',
            'type': 'numeric'
        }, {
            'name': ['Away', 'xG'],
            'id': 'xg_a',
            'type': 'numeric'
        }, {
            'name': ['Favorites', 'Actual'],
            'id': 'favorite_count',
            'type': 'numeric',
            'format': {
                'specifier': ',.0f'
            }
        }, {
            'name': ['Favorites', 'Predicted'],
            'id': 'favorite_pred',
            'type': 'numeric',
            'format': {
                'specifier': ',.0f'
            }
        }, {
            'name': ['Retweets', 'Actual'],
            'id': 'retweet_count',
            'type': 'numeric',
            'format': {
                'specifier': ',.0f'
            }
        }, {
            'name': ['Retweets', 'Predicted'],
            'id': 'retweet_pred',
            'type': 'numeric',
            'format': {
                'specifier': ',.0f'
            }
        }, {
            'name': ['', 'EOE'],
            'id': 'total_diff_prnk',
            'type': 'numeric',
            'format': {
                'specifier': ',.1f'
            }
        }, {
            'name': ['', 'Link'],
            'id': 'link',
            'type': 'lab_text',
            'presentation': 'markdown'
        }
    ]
    return dash_table.DataTable(
        id='leaderboard-table',
        style_header={
            'backgroundColor': 'transparent',
            'fontFamily': 'Karla',
            'fontWeight': 'bold',
            'font-size': '14px',
            'color': '#003f5c',
            # 'border': '1px solid #003f5c',
            'border': '0px transparent',
            # 'textAlign': 'center'
        },
        style_cell={
            # 'backgroundColor': 'transparent',
            'fontFamily': 'Karla',
            'font-size': '14px',
            # 'color': '#ffffff',
            'border': '1px solid #cccccc',
            # 'textAlign': 'center'
        },
        cell_selectable=False,
        column_selectable=False,
        columns=cols_table,
        data=preds_sort.to_dict('records'),
        # style_as_list_view=True,
        # style_cell={
        #     'font-family': 'Karla',
        #     'font-size': '12px',
        #     'padding': '5px'
        # },
        # style_header={
        #     # 'backgroundColor': '#003f5c',
        #     'fontWeight': 'bold',
        # },
        # style_data={
        #     'whiteSpace': 'normal',
        #     'height': 'auto',
        # },
        # style_table={'overflowX': 'auto'},
        filter_action='native',
        sort_action='native',
        # sort_mode='single',
        # column_selectable='single',
        merge_duplicate_headers=True,
        # page_action='native',
        page_current=0,
        page_size=10,
    )


# app ----
# import dash

app = dash.Dash(
    __name__,
    suppress_callback_exceptions=True,
    external_stylesheets=[
        {
            'href':
                'https://fonts.googleapis.com/css2?'
                'family=Karla:wght@400;700&display=swap',
            'rel': 'stylesheet',
        },
    ],
    meta_tags=[
        {
            'name': 'viewport',
            'content': 'width=device-width, initial-scale=1.0'
        }
    ]
)
server = app.server

# layouts ----------------------------------------------------------------------
# import dash_core_components as dcc
# import dash_html_components as html
# import datetime
# from app import app
# from utils import *

styles_current_page = {
    'text-decoration': 'underline',
    'text-decoration-color': '#ffffff',  # app_colors['blue'],
    'text-shadow': '0px 0px 1px #ffffff'
}


def _graph_wrapper(id):
    return dcc.Graph(
        id=id,
        config={
            'displayModeBar': True,
            'displaylogo': False,
            'modeBarButtonsToRemove': ['lasso2d']
        }
    )


def get_generic_col_classname():
    return 'col-lg-5 col-md-5 col-sm-10 col-xs-10'


def _div_graph_wrapper(id):
    return html.Div(
        [_graph_wrapper(id=id)], className=get_generic_col_classname()
    )


def row_div_graph_wrapper(suffix):
    id1 = 'favorites-' + suffix
    id2 = 'retweets-' + suffix
    return html.Div(
        [
            html.Div([], className='col-1'),
            _div_graph_wrapper(id1),
            _div_graph_wrapper(id2),
            html.Div([], className='col-1'),
        ],
        className='row'
    )


preds = import_preds()
leaderboard = generate_leaderboard(preds)
about = generate_about()
created_at_max = preds['created_at'].max()
initial_text = preds['lab_text'].iloc[0]
# min_date = preds['date'].min()
# max_date = preds['date'].max()
min_date = datetime.date(2020, 1, 1)
max_date = datetime.date.today()
init_date = datetime.date(2020, 6, 1)


def get_header():

    header = html.Div(
        [
            html.Div([], className='col-1'),
            html.Div(
                [
                    html.H1(
                        children='xGPhilosophy xEngagement',
                        style={
                            'textAlign': 'left',
                            'font-weight': 'bold',
                        }
                    )
                ],
                className='col-10',
                style={'padding-top': '1%'}
            ),
            html.Div([], className='col-1'),
        ],
        className='row',
        style={
            # 'height': '4%',
            'background-color': app_colors['blue']
        }
    )

    return header


def get_navbar_item(name, link, p):
    is_current = True if link == p else False
    if is_current:
        title = html.H4(children=name, style=styles_current_page)
    else:
        title = html.H4(children=name)

    return html.Div([dcc.Link(title, href='/apps/' + link)], className='col-2')


def get_navbar(p='about'):
    if p not in ['about', 'data-and-preds', 'shap', 'leaderboard']:
        return '404: Error'

    navbar = html.Div(
        [
            html.Div([], className='col-1'),
            get_navbar_item(name='About', link='about', p=p),
            get_navbar_item(
                name='Data & Predictions', link='data-and-preds', p=p
            ),
            get_navbar_item(name='Prediction Explanation', link='shap', p=p),
            get_navbar_item(name='Leaderboard', link='leaderboard', p=p),
        ],
        className='row',
        style={'background-color': app_colors['blue']}
    )
    return navbar


def get_blank_row(h='45px'):

    row = html.Div(
        [html.Div([html.Br()], className='col-12')],
        className='row',
        style={'height': h}
    )

    return row


def get_date_filter():
    res = html.Div(
        [
            html.Div(
                [
                    html.H5(
                        children='Date Range',
                        style={
                            'text-align': 'left',
                            'color': app_colors['blue']
                        }
                    ),
                    html.Div(
                        [
                            dcc.DatePickerRange(
                                id='date-filter',
                                min_date_allowed=min_date,
                                max_date_allowed=max_date,
                                start_date=init_date,
                                end_date=max_date,
                                start_date_placeholder_text='Start date',
                                # display_format='DD-MMM-YYYY',
                                # first_day_of_week=1,
                                end_date_placeholder_text='End date',
                                style={
                                    'font-size': '12px',
                                    'display': 'inline-block',
                                    'border-radius': '2px',
                                    'border': '1px solid #ffffff',
                                    'color': app_colors['blue'],
                                    'border-spacing': '0',
                                    'border-collapse': 'separate'
                                }
                            )
                        ],
                        style={'margin-top': '5px'}
                    )
                ],
                style={
                    'margin-top': '10px',
                    'margin-bottom': '5px',
                    'text-align': 'left',
                    'paddingLeft': 5
                }
            )
        ],
        className=get_generic_col_classname()
    )
    return res


def get_text_filter():
    res = html.Div(
        [
            html.Div(
                [
                    html.H5(
                        children='Select a Tweet to Emphasize',
                        style={
                            'text-align': 'left',
                            'color': app_colors['blue']
                        }
                    ),
                    html.Div(
                        [
                            dcc.Dropdown(
                                id='text-filter',
                                value=initial_text,
                                clearable=True,
                                className='dropdown',
                                style={
                                    'font-size': '12px',
                                    # 'display': 'inline-block',
                                    # 'border-radius': '2px',
                                    # 'border': '1px solid #ffffff',
                                    'border-spacing': '0',
                                    'border-collapse': 'separate'
                                }
                            )
                        ],
                        style={'margin-top': '5px'}
                    )
                ],
                style={
                    'margin-top': '10px',
                    'margin-bottom': '5px',
                    'text-align': 'left',
                    'paddingLeft': 5
                }
            )
        ],
        className=get_generic_col_classname()
    )
    return res


page_about = html.Div(
    [
        get_header(),
        get_navbar('about'),
        html.Div([html.Br()], className='row sticky-top'),
        html.Div(
            [
                html.Div([], className='col-1'),
                html.Div([about], className='col-10'),
                html.Div([], className='col-1'),
            ],
            className='row'
        )
    ]
)


def get_filter_row():
    return html.Div(
        [
            html.Div(
                [
                    html.Div(
                        [
                            html.Div([], className='col-1'),
                            get_date_filter(),
                            get_text_filter(),
                            html.Div([], className='col-1'),
                        ],
                        className='row'
                    )
                ],
                className='col-12'
            ),
        ],
        className='row sticky-top'
    )


page_data_and_preds = html.Div(
    [
        get_header(),
        get_navbar('data-and-preds'),
        # get_blank_row(h='45px'),
        get_filter_row(),
        get_blank_row(h='10px'),
        row_div_graph_wrapper('over-time'),
        row_div_graph_wrapper('v-pred')
    ]
)

page_shap = html.Div(
    [
        get_header(),
        get_navbar('shap'),
        # html.Div([html.Br()], className='row sticky-top'),
        get_filter_row(),
        get_blank_row(h='10px'),
        row_div_graph_wrapper('shap')
    ]
)

page_leaderboard = html.Div(
    [
        get_header(),
        get_navbar('leaderboard'),
        get_blank_row(h='20px'),
        html.Div(
            [
                html.Div([], className='col-1'),
                html.Div([leaderboard], className='col-10'),
                html.Div([], className='col-1'),
            ],
            className='row'
        )
    ]
)

# callbacks --------------------------------------------------------------------
# import dash_core_components as dcc
# import dash_html_components as html
# from dash.dependencies import Output, Input
# import plotly.graph_objs as go
# import pandas as pd
# pd.options.mode.chained_assignment = None
# # import dash
# import datetime
# from app import app
# from utils import *

shap = import_shap()
preds = import_preds()


def _convert_to_date(x):
    return datetime.datetime.strptime(x, '%Y-%m-%d').date()


def _filter_date_between(df, d1, d2):
    d1 = _convert_to_date(d1)
    d2 = _convert_to_date(d2)
    res = df.loc[(df['date'] >= d1) & (df['date'] <= d2), :]
    return res


@app.callback(
    Output('text-filter', 'options'),
    [Input('date-filter', 'start_date'),
     Input('date-filter', 'end_date')]
)
def update_text_dropdown(start_date, end_date):
    preds_filt = _filter_date_between(preds, start_date, end_date)
    # preds_filt = preds.loc[selected, :]
    # preds_filt = preds
    opts = [{'label': x, 'value': x} for x in preds_filt['lab_text']]
    return opts


def _split_df_by_text(df, text):
    selected = (df['lab_text'] == text)
    other = (df['lab_text'] != text)
    df_selected = df.loc[selected, :]
    df_other = df.loc[other, :]
    return df_selected, df_other


def _convert_stem_to_lab(stem):
    return f'{stem.capitalize()}s'


def _identify_stem_color(stem):
    return app_colors['orange'] if stem == 'retweet' else app_colors['blue']


def _update_common_layout_settings(fig, width=512, height=350):
    fig.update_layout(
        {
            'showlegend': False,
            'hoverlabel_align': 'right',
            'plot_bgcolor': '#ffffff',
            'autosize': True,
            'width': width,
            'height': height,
            'margin': {
                # 'l': 10,
                # 'r': 10,
                # 'b': 50,
                # 't': 50,
                'pad': 0
            },
            # 'clickmode': 'event+select',
            'font_family': 'Karla',
        }
    )
    return fig


def _update_common_xaxes_settings(fig, *args, **kwargs):
    return fig.update_xaxes(
        showgrid=True,
        gridwidth=1,
        gridcolor=app_colors['grey80'],
        *args,
        **kwargs
    )


def _update_common_yaxes_settings(fig, *args, **kwargs):
    return fig.update_yaxes(
        showgrid=True,
        gridwidth=1,
        gridcolor=app_colors['grey80'],
        *args,
        **kwargs
    )


def _plot_actual(df, stem, text, col_x, title_text, hovertemplate):
    df_selected, df_other = _split_df_by_text(df, text)
    col_y = f'{stem}_count'
    lab_stem = _convert_stem_to_lab(stem)
    lab_y = f'# of {lab_stem}'
    marker_color = _identify_stem_color(stem)

    def _plot(df, which='other'):
        o = 0.5 if which == 'other' else 1
        c = marker_color if which == 'other' else 'black'
        s = 5 if which == 'other' else 10
        return go.Scatter(
            x=df[col_x],
            y=df[col_y],
            mode='markers',
            # text=df['lab_text'],
            text=df['lab_hover'],
            opacity=o,
            hovertemplate=hovertemplate,
            marker={
                'size': s,
                'color': c
            }
        )

    fig = go.Figure(_plot(df_other, 'other'))
    fig.add_trace(_plot(df_selected, 'selected'))

    fig = _update_common_layout_settings(fig)
    fig.update_layout({
        'title_text': title_text,
        'yaxis_tickformat': ',.',
    })
    fig = _update_common_xaxes_settings(fig)
    fig = _update_common_yaxes_settings(fig, rangemode='tozero')
    return fig


def _plot_over_time(df, stem, text):
    col_x = 'created_at'
    lab_stem = _convert_stem_to_lab(stem)
    lab_y = f'# of {lab_stem}'
    title_text = f'{lab_stem} over Time'
    hovertemplate = '%{text}<br>' + lab_y + ': %{y:0,000}</br><extra></extra>'
    fig = _plot_actual(
        df,
        stem,
        text,
        col_x=col_x,
        title_text=title_text,
        hovertemplate=hovertemplate
    )
    return fig


def _plot_v_pred(df, stem, text):
    col_x = f'{stem}_pred'
    lab_stem = _convert_stem_to_lab(stem)
    lab_y = f'Actual'
    lab_x = f'Predicted'
    title_text = f'Actual vs. Predicted {lab_stem}'
    hovertemplate = '%{text}<br>' + lab_y + ': %{y:0,000}</br>' + lab_x + ': %{x:,.0f}<br></br><extra></extra>'
    fig = _plot_actual(
        df,
        stem,
        text,
        col_x=col_x,
        title_text=title_text,
        hovertemplate=hovertemplate
    )
    fig.update_layout({
        'xaxis_tickformat': ',.',
    })
    return fig


def _identify_sign_color(sign):
    if sign == 'neg':
        color = app_colors['purple']
    elif sign == 'pos':
        color = app_colors['pink']
    else:
        color = app_colors['grey50']
    return color


def _plot_shap(df, stem, text):
    selected = (df['lab_text'] == text)
    df_selected = df.loc[selected, :]

    if len(df_selected) == 0:
        return None

    col_y = 'lab'
    col_x = f'{stem}_shap_value'
    lab_stem = _convert_stem_to_lab(stem)
    # bar_app_colors = _identify_sign_color(df['sign'])
    hovertemplate = '%{y}<br>SHAP value: %{x:.2f}</br><extra></extra>'
    lab_y = f'# of {lab_stem}'
    title_text = f'{lab_stem} SHAP values'

    def _plot_bar(fig, sign):

        df_sign = df_selected.loc[df_selected[f'{stem}_sign'] == sign, :]
        if sign == 'pos':
            df_sign.sort_values(col_x, inplace=True, ascending=True)
        elif sign == 'neg':
            df_sign.sort_values(col_x, inplace=True, ascending=True)
        c = _identify_sign_color(sign)
        fig.add_trace(
            go.Bar(
                x=df_sign[col_x],
                y=df_sign[col_y],
                orientation='h',
                marker_color=c,
                hovertemplate=hovertemplate
            )
        )
        return fig

    fig = go.Figure()
    fig = _plot_bar(fig, 'neg')
    fig = _plot_bar(fig, 'pos')

    fig = _update_common_layout_settings(fig, height=700)
    fig.update_layout(
        {
            'title_text': title_text,
            # 'xaxis_text': 'SHAP value',
            'xaxis_tickformat': ',.',
        }
    )
    fig = _update_common_xaxes_settings(fig)
    # fig = _update_common_yaxes_settings(fig)
    fig.update_yaxes(showgrid=False)
    return fig


# idk why having all callbacks toegether use to work, but now it doesn't
@app.callback(
    [Output('favorites-v-pred', 'figure'),
     Output('retweets-v-pred', 'figure')],
    [
        Input('date-filter', 'start_date'),
        Input('date-filter', 'end_date'),
        Input('text-filter', 'value')
    ],
)
def update_charts(start_date, end_date, text):
    preds_filt = _filter_date_between(preds, start_date, end_date)
    favorites_v_pred = _plot_v_pred(preds_filt, 'favorite', text)
    retweets_v_pred = _plot_v_pred(preds_filt, 'retweet', text)
    return favorites_v_pred, retweets_v_pred  # , favorites_shap, retweets_shap


@app.callback(
    [Output('favorites-shap', 'figure'),
     Output('retweets-shap', 'figure')],
    [
        Input('date-filter', 'start_date'),
        Input('date-filter', 'end_date'),
        Input('text-filter', 'value')
    ],
)
def update_charts(start_date, end_date, text):
    favorites_shap = _plot_shap(shap, 'favorite', text)
    retweets_shap = _plot_shap(shap, 'retweet', text)
    return favorites_shap, retweets_shap


@app.callback(
    [
        Output('favorites-over-time', 'figure'),
        Output('retweets-over-time', 'figure')
    ],
    [
        Input('date-filter', 'start_date'),
        Input('date-filter', 'end_date'),
        Input('text-filter', 'value')
    ],
)
def update_charts(start_date, end_date, text):
    preds_filt = _filter_date_between(preds, start_date, end_date)
    favorites_over_time = _plot_over_time(preds_filt, 'favorite', text)
    retweets_over_time = _plot_over_time(preds_filt, 'retweet', text)
    return favorites_over_time, retweets_over_time


# index ------------------------------------------------------------------------
# import dash
# import dash_core_components as dcc
# import dash_html_components as html
# from dash.dependencies import Input, Output

# from app import app
# from app import server

# from layouts import page_about, page_data_and_preds, page_shap, page_leaderboard
# import callbacks

app.layout = html.Div(
    [dcc.Location(id='url', refresh=False),
     html.Div(id='page-content')]
)


@app.callback(
    dash.dependencies.Output('page-content', 'children'),
    [dash.dependencies.Input('url', 'pathname')]
)
def display_page(pathname):
    if pathname == '/apps/about':
        return page_about
    elif pathname == '/apps/data-and-preds':
        return page_data_and_preds
    elif pathname == '/apps/shap':
        return page_shap
    elif pathname == '/apps/leaderboard':
        return page_leaderboard
    else:
        return page_about


if __name__ == '__main__':
    app.run_server(debug=False)
