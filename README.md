# spelunker

Scraping and data mining framework

I'm fond of Twitter and I'm grateful of being able to so easily learn from the world's greatest
hackers but I frequently get tired of having to wade through their less relevant tweets on
non-technical subjects.

Hence the intention here is to scrape news entries from a large number of sources (eg thousands of
Twitter handles' tweets) then use data mining techniques to sort the entries based on a dynamic,
user-defined interest.

The initial plan is to generate periodic emails (ie for use as a daily cronjob) then consider
building an interactive web application to more easily support modifying system parameters.

## Sources

Scraping is implemented using a custom Chrome DevTools interface.  System state is maintained by a
local SQLite database.

Sources that include images use inline, base64-encoded values rather than external links to avoid
link rot.

- [ ] [Twitter](https://twitter.com)
  - Supports handles, hashtags and searches
- [ ] [Hacker News](https://news.ycombinator.com)
- [ ] [Google Scholar](https://scholar.google.com)
- [ ] [Google News](https://news.google.com)
- [ ] [Reddit](https://reddit.com)
