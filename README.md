# A session explorer and calendar builder for NICAR 2025
A Shiny app that makes it easy to build your schedule for [NICAR 2025](https://www.ire.org/training/conferences/nicar-2025/)


<!--
Run the app locally:

```r
shiny::runGitHub("nicar-2025-calendar", "spcanelon", ref = "main")
```
-->

## Featured R packages used to build the app

- [shiny](https://shiny.rstudio.com) to build the app
- [bslib](https://rstudio.github.io/bslib/) to customize the user interface with Bootswatch
- [reactable](https://glin.github.io/reactable/) to produce the table of session that reacts to user inputs
- [calendar](https://github.com/ATFutures/calendar) to build the calendar file (.ics)
- [rvest](https://rstudio.github.io/rvest) to scrape the session links from the official schedule
- [renv](https://rstudio.github.io/renv/) to support reproducibility of the project

## Thanks
- IRE for the official [NICAR 2025 schedule](https://schedules.ire.org/nicar-2025/)!
- This app was adapted from one built by Garrick Aden-Buie for the [2021 rstudio::global conference](https://github.com/gadenbuie/rstudio-global-2021-calendar/). Thanks Garrick!
