#%%
import tweepy

# Authenticate to Twitter
auth = tweepy.OAuthHandler(
    'Rgs6v9QgHOoKnJXARl537FCgj',
    'bh5m9cy4vUPsISTr5dtPMTsSWpn9et1Tb0bJEaY1jPWg78wlKv'
)
auth.set_access_token(
    '3320523836-C4nbyP8zQS4ohQXQgabWf6HCG71Zww2RoWUwaCl',
    'JbScuEZk2OALD0g9B4BFphkKQ3HXwQFhnZR3FoYo5SeJu'
)
# Create API object
api = tweepy.API(auth)
# %%
timeline = api.user_timeline('xGPhilosophy', count=3200)

#%%
len(timeline)
#%%
for tweet in timeline:
    print(f'{tweet.text}')
# %%
user = api.get_user('xGPhilosophy')
# %%
