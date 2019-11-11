
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtweet.download

<!-- badges: start -->

<!-- badges: end -->

Robust tools for automating large and/or time-consuming tasks involving
the collection of Twitter data via [**{rtweet}**](https://rtweet.info).

## Installation

You can install the released version of **{rtweet.download}** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rtweet.download")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("mkearney/rtweet.download")
```

## Friends lists

Twitter’s `"friends/list"` API endpoint is rate limited to 15 requests
(or [15\*](#notes) friend lists) per 15 minutes. So while a single call
using `rtweet::get_friends()` can retrieve friend lists for up to 15
users, a single call using `rtweet.download::get_friends_download()` can
retrieve friend lists for hundreds or even thousandsusers\!

|                                         |                          |
| --------------------------------------- | ------------------------ |
| **API Feature**                         | **Value**                |
| <span> </span> Endpoint                 | `"friends/list"`         |
| <span> </span> Rate limit (per 15 min.) | `15`                     |
| <span> </span> Lists per request        | `1`[\*](#notes)          |
| **R Package**                           | **Function**             |
| <span> </span> {rtweet}                 | `get_friends()`          |
| <span> </span> {rtweet.download}        | `get_friends_download()` |

The example below uses `get_friends_download()` to automate the
collection of friends (accounts followed by) users on \[@Teradata's list
of data science
influencers\](<https://twitter.com/Teradata/lists/data-science-influencers/members>).

``` r
## get list of data science influencers
data_sci_influencers <- rtweet::lists_members(
  owner_user = "Teradata", slug = "data-science-influencers"
)

## download friend lists
fds <- get_friends_download(data_sci_influencers$screen_name)

## preview data
head(fds)
```

While a single call of `rtweet::get_friends()` can retrieve up to 15
friend lists, a single call of `get_friends_download()` can collect
hundreds or even thousands of friend lists.

## Users data

Twitter’s `"users/lookup"` API endpoint is rate limited to 900 requests
(or 90,000 users) per 15 minutes. So while a single call using
`rtweet::lookup_users()` can retrieve data on up to 90,000 users, a
single call using `rtweet.download::lookup_users_download()` can collect
data on hundreds of thousands or even millions of users\!

|                                         |                           |
| --------------------------------------- | ------------------------- |
| **API Feature**                         | **Value**                 |
| <span> </span> Endpoint                 | `"users/lookup"`          |
| <span> </span> Rate limit (per 15 min.) | `900`                     |
| <span> </span> Users per request        | `100`                     |
| **R Package**                           | **Function**              |
| <span> </span> {rtweet}                 | `lookup_users()`          |
| <span> </span> {rtweet.download}        | `lookup_users_download()` |

The example below uses `lookup_users_download()` to automate data
collection for the previously collected accounts followed by data
science influencers.

``` r
## download users data
fds_data <- lookup_users_download(fds$user_id)

## preview data
head(fds)
```

## Notes

\* The `"friends/list"` endpoint returns **a single list of up to 5,000
friends**, so 15 requests can only return 15 complete friend lists if
the 15 accounts all follow 5,000 or fewer accounts. To retrieve the
complete list for users following more than 5,000 accounts, multiple
requests (friends\_count / 5,000) are required.
