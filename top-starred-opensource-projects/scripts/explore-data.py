


## -----------------------------------------------------------------------------
## load modules and data
## -----------------------------------------------------------------------------

import pandas as pd
from collections import Counter
import matplotlib.pyplot as plt

github_repos = pd.read_csv("~/Documents/top-starred-opensource-projects/data/TopStaredRepositories.csv")

## -----------------------------------------------------------------------------
## explore data
## -----------------------------------------------------------------------------

# data size
print(github_repos.shape)

print(github_repos.columns)

# top list
print(github_repos.head())


# bottom list
print(github_repos.tail())

# data overall description
print(github_repos.info())

print(github_repos.describe())


# which are the most popular language?

# pop_lang = github_repos["Language"]
# pop_lang[pd.isnull(pop_lang)] = "Unknown"  # same as R 
# pop_lang = Counter(pop_lang)

# pop_lang = pd.DataFrame.from_dict(pop_lang, orient="index").reset_index()
# pop_lang = pop_lang.rename(columns={"index": "lang", 0: "n"})
# pop_lang = pop_lang.sort_values(["n"], ascending=False)

pop_lang = github_repos["Language"].value_counts(dropna=False)
print(pop_lang)


pop_user = github_repos["Username"].value_counts(dropna=False)
print(pop_user)

# which are the most popular tags?

# pop_tag = github_repos["Tags"]
# pop_tag[pd.isnull(pop_tag)] = "Unknown"

# tag_count = []
# [tag_count.extend(tag.split(",")) for tag in pop_tag]

# tag_count = Counter(tag_count)


