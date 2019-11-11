
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

## Friends IDs

Twitter’s `"friends/ids"` API endpoint is rate limited to 15 requests
(or the friend IDs of [15\*](#notes) accounts) per 15 minutes. So while
a single call using `rtweet::get_friends()` can retrieve the friend IDs
of up to 15 users, a single call using
`rtweet.download::get_friends_download()` can retrieve the friend IDs of
hundreds or even thousands of users\!

|                                         |                          |
| --------------------------------------- | ------------------------ |
| **API Feature**                         | **Value**                |
| <span> </span> Endpoint                 | `"friends/ids"`          |
| <span> </span> Rate limit (per 15 min.) | `15`                     |
| <span> </span> Lists per request        | `1`[\*](#notes)          |
| **R Package**                           | **Function**             |
| <span> </span> {rtweet}                 | `get_friends()`          |
| <span> </span> {rtweet.download}        | `get_friends_download()` |

The example below uses `get_friends_download()` to automate the
collection of friend (accounts followed by) IDs of users on
\[@Teradata's list of data science
influencers\](<https://twitter.com/Teradata/lists/data-science-influencers/members>).

``` r
## get members on data science influencers influence
data_sci_influencers <- rtweet::lists_members(
  owner_user = "Teradata", slug = "data-science-influencers"
)

## download friend IDs for each user
fds <- get_friends_download(data_sci_influencers$screen_name)

## preview data
head(fds)
```

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

\* The `"friends/ids"` endpoint returns the **up to 5,000 friend IDs of
a single user**, so 15 requests can only return all the friend IDs of 15
users if all 15 of those users follow 5,000 or fewer accounts. To
retrieve all the friend IDs for users following more than 5,000
accounts, multiple requests (friends\_count / 5,000) are required.
