Use the following command when updating requirements to avoid weird `"file:///..."` paths that get generated with just `pip freeze > requirements.txt` (since this is a conda environment).

```bash
pip list --format=freeze > requirements.txt
```

Then, manually change the ansi2html version from 0.0.0 to 1.6.0. Also, remove the pywin32 line.

Note that I changed the data from a parquet to a CSV because python-snappy (needed for parquet) files wasn't compiling with the heroku deploy.
