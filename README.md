Use the following command when updating requirements to avoid weird `"file:///..."` paths that get generated with just `pip freeze > requirements.txt` (since this is a conda environment).

```
pip list --format=freeze > requirements.txt
```
