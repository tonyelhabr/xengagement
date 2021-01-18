First, I had to update all of my libraries with the following commands, per this [SO answer](https://stackoverflow.com/questions/47304291/heroku-upload-could-not-find-a-version-that-satisfies-the-requirement-anaconda)

```bash
conda update -n base conda
conda update --all
```

This was because I was having requirements with 0.0.0 version, which heroku couldn't handle

Use the following command when updating requirements to avoid weird `"file:///..."` paths that get generated with just `pip freeze > requirements.txt` (since this is a conda environment).

```bash
pip list --format=freeze > requirements.txt
```
