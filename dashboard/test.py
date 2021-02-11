#%%
import plotly.graph_objs as go
import plotly.express as px
import pandas as pd
pd.options.mode.chained_assignment = None


def _import_path(stem):
    return pd.read_csv(
        f'../inst/extdata/{stem}.csv'
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
#%%
preds = import_preds()
# %%
tms = preds[['tm_h']].drop_duplicates()
# %%
tms
# %%

app_colors = {
    'blue': '#003f5c',
    'orange': '#ffa600',
    'purple': '#7a5193',
    'pink': '#ef5675',
    'grey50': '#7f7f7f',
    'grey80': '#cccccc'
}


def _split_df_by_tm(df, tm):
    selected = (df['tm'] == tm)
    other = (df['tm'] != tm)
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


def _plot_actual(df, stem, tm, col_x, title_text, hovertemplate):
    df_selected, df_other = _split_df_by_tm(df, tm)
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
            # tm=df['lab_tm'],
            tm=df['lab_tm'],
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


def _plot_v_pred(df, stem, tm):
    col_x = f'{stem}_pred'
    lab_stem = _convert_stem_to_lab(stem)
    lab_y = f'Actual'
    lab_x = f'Predicted'
    title_text = f'Actual vs. Predicted {lab_stem}'
    hovertemplate = '%{tm}<br>' + lab_y + ': %{y:0,000}</br>' + lab_x + ': %{x:,.0f}<br></br><extra></extra>'
    fig = _plot_actual(
        df,
        stem,
        tm,
        col_x=col_x,
        title_text=title_text,
        hovertemplate=hovertemplate
    )
    fig.update_layout({
        'xaxis_tickformat': ',.',
    })
    return fig

#%%
px.scatter(
    fig = _plot_v_pred(preds, 'favorite', 'Arsenal'),
    config={
        'displayModeBar': True,
        'displaylogo': False,
        'modeBarButtonsToRemove': ['lasso2d']
    }
)
