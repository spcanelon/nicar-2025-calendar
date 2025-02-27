# load libraries
library(shiny)
library(bslib)
library(lubridate)
library(calendar)
library(reactable)
library(glue)
# source
source("R/data_cleaning.R")
source("R/stack.R")
source("R/cards.R")

Sys.setenv(TZ = "US/Central")

# define Bootstrap theme
theme <- bs_theme(
  version = 4, #TODO update to 5
  bootswatch = "lux",
  primary = "#4f3d63",
  secondary = "#6aae8a",
  success = "#113421",
  warning = "#bf7836",
  danger = "#666666",
  info = "#1f78b4",
  "font-size-base" = "1rem",
  base_font = font_collection(font_google("Atkinson Hyperlegible", local = FALSE), "Nunito", "sans-serif"),
  code_font = font_google("Atkinson Hyperlegible Mono", local = FALSE)
)

# import and wrangle schedule data
schedule <- readr::read_csv("data/schedule.csv") |> dplyr::arrange(start_datetime)
# create unique event ID
schedule$id <- seq_len(nrow(schedule))
# create line break between speakers
schedule$speakers <- gsub(schedule$speakers, pattern = ", ", replacement = "<br>")


ui <- navbarPage(
  'NICAR 2025 Calendar',
  theme = theme,
  collapsible = TRUE,
  tabPanel(
    title = "Schedule",
    id = "schedule",
    # skip link
    a(
      "If you're using a screen reader, you may find the official ",
      "NICAR 2025 conference website to be better suited. Do you want to go there now?",
      class = "screenreader-text",
      `tab-index` = 1,
      href = "https://schedules.ire.org/nicar-2025/"
    ),
    div(
      class = "container-fluid",
      style = "max-width: 1600px",
      div(
        class = "row",
        div(
          class = "col-lg-3 order-1 order-lg-2 sidebar",
          uiOutput("your_talks"),
          div(
            class = "row",
            div(
              class = "col-6 col-lg-12",
              textInput("sch_search", "Search", width = "100%"),
              radioButtons("sch_day", "Day",
                           c("Thursday" = "one", "Friday" = "two", "Saturday" = "three", "Sunday" = "four", "All" = "all"),
                           inline = TRUE, selected = c("all"), width = "100%"),
              sliderInput("sch_hours", "Hours", value = c(7, 17),
                          min = 0, max = 24, step = 1, post = ":00", width = "100%")
            ),
            div(
              class = "col-6 col-lg-12",
              selectizeInput(
                "sch_presenter", "Speakers",
                choices = sort(unique(schedule$speakers)),
                multiple = TRUE, width = "100%"),
              selectizeInput("sch_type", "Session Type",
                             choices = sort(unique(schedule$session_type)),
                             multiple = TRUE, width = "100%"),
              selectizeInput("sch_topic", "Skill Level",
                             choices = sort(unique(schedule$skill_level)),
                             multiple = TRUE, width = "100%")
            )
          )
        ),
        div(
          class = "col-lg-9 order-2 order-lg-1",
          reactable::reactableOutput("schedule"),
          helpText(
            class = "mt-3",
            tags$a(
              href = "https://www.ire.org/training/conferences/nicar-2025/",
              code("NICAR 2025")
            ),
            "will be held Thursday 2025-03-06 to Sunday 2025-03-09."
          ),
          htmltools::htmlDependency(
            name = "nicar-2025-calendar",
            version = "0.0.1",
            src = "www",
            script = "extra.js",
            stylesheet = "extra.css"
          )
        )
      )
    )
  ),
  # About tab
  tabPanel(
    title = "About",
    id = "about",
    div(
      class = "container-fluid",
      style = "max-width: 900px",
      h2(class = "text-monospace",
         HTML("Hi there!")
      ),
      p(HTML("My name is Silvia Canelón, and this is my first time attending NICAR!<br>If you see me around, please say hello. &#xFE0F;👋")
        ),
      p("You can also get in touch on",
        tags$a(href = "https://bsky.app/profile/silviacanelon.com", "Bluesky", .noWS = "after"),
        " or through",
        tags$a(href = "https://silviacanelon.com/contact", "my website.", .noWS = "after")
        ),
      h2(
        class = "text-monospace",
        "NICAR 2025"
      ),
      strong(HTML("March 6-9, 2025 &mdash; Minneapolis, MN<br><br>")),
      strong("From",
             tags$a(href = "https://www.ire.org/training/conferences/", "Investigative Reporters & Editors (IRE):", .noWS = "after")),
      p(
        HTML("<blockquote>",
        "Join us for our annual data journalism conference where you'll",
        "have access to trainings on some of the most powerful tools a journalist",
        "can have in their kit. Sessions will also feature the most tried and tested",
        "data analysis techniques and investigative skills out there.",
        "</blockquote>")
      ),
      p(
        a(
          "Register Now",
          href = "https://www.ire.org/training/conferences/nicar-2025/nicar25-registration/",
          class = "btn btn-primary"
        ),
        a(
          tags$a(
            href = "https://schedules.ire.org/nicar-2025/",
            class = "btn btn-success",
            "Official Schedule"
          )
        )
      ),
      tags$hr(class = "my-4"),
      h2("About this app", class = "text-monospace"),
      p(
        HTML("This app was built with &#x2665; by"),
        tags$a(href = "https://silviacanelon.com", "Silvia Canelón", .noWS = "after"),
        " using the packages below, and adapted from an app created by",
        tags$a(href = "https://github.com/gadenbuie/rstudio-global-2021-calendar", "Garrick Aden-Buie."),
        HTML("You can find"),
        tags$a(href = "https://github.com/spcanelon/nicar-2025-calendar", "the full source code"),
        "on GitHub."
      ),
      div(
        class = "d-flex flex-wrap align-items-stretch justify-content-between",
        card(
          "shiny",
          posit_hex("shiny"),
          "https://shiny.rstudio.com",
          "Shiny is an R package that makes it easy to build interactive web apps straight from R."
        ),
        card(
          "renv",
          posit_hex("renv"),
          "https://rstudio.github.io/renv",
          "The renv package helps you create reproducible environments for your R projects. Use renv to make your R projects more: isolated, portable, and reproducible."
        ),
        card(
          "bslib",
          posit_hex("bslib"),
          "https://rstudio.github.io/bslib/",
          "Tools for creating custom Bootstrap themes, making it easier to style Shiny apps & R Markdown documents directly from R without writing unruly CSS and HTML."
        ),
        card(
          "R6",
          posit_hex("R6"),
          "https://r6.r-lib.org/",
          "Encapsulated object-oriented programming for R."
        ),
        card(
          "glue",
          posit_hex("glue"),
          "https://glue.tidyverse.org",
          "Glue strings to data in R. Small, fast, dependency free interpreted string literals."
        ),
        card(
          "lubridate",
          posit_hex("lubridate"),
          "https://lubridate.tidyverse.org",
          "Make working with dates in R just that little bit easier."
        ),
        card(
          "calendar",
          NULL,
          "https://github.com/ATFutures/calendar",
          "Create, read, write, and work with iCalendar (.ics, .ical or similar) files in R."
        ),
        card(
          "reactable",
          NULL,
          "https://glin.github.io/reactable/index.html",
          "Interactive data tables for R, based on the React Table library and made with reactR."
        ),
        card(
          "prettyunits",
          NULL,
          "https://github.com/r-lib/prettyunits",
          "Pretty, human readable formatting of quantities."
        ),
      )
    )
  )
)


server <- function(input, output, session) {
  selected_talks <- Stack$new()
  selected_in_current_view <- reactiveVal()

  # select conference day
  schedule_view <- reactive({
    if (isTruthy(input$sch_day)) {
      if (input$sch_day == "one") {
        schedule <- schedule[schedule$day == "Thursday",]
      } else if (input$sch_day == "two") {
        schedule <- schedule[schedule$day == "Friday",]
      } else if (input$sch_day == "three") {
        schedule <- schedule[schedule$day == "Saturday"]
      } else if (input$sch_day == "four") {
        schedule <- schedule[schedule$day == "Sunday"]
      }
    }
    schedule$time <- with_tz(schedule$start_datetime, tzone = "US/Central")
    # select hours
    if (isTruthy(input$sch_hours)) {
      schedule <- schedule[
        hour(schedule$time) >= input$sch_hours[1] & hour(schedule$time) <= input$sch_hours[2],
      ]
    }
    # use keyword Search
    if (shiny::isTruthy(input$sch_search)) {
      schedule <- schedule[
        grepl(input$sch_search, tolower(paste(schedule$session_title, schedule$session_description, schedule$speakers))),
      ]
    }
    # select/search Presenter
    if (isTruthy(input$sch_presenter)) {
      schedule <- schedule[schedule$speakers %in% input$sch_presenter, ]
    }
    # select/search Talk Type
    if (isTruthy(input$sch_type)) {
      schedule <- schedule[schedule$session_type %in% input$sch_type, ]
    }
    # select/search Talk Topic
    if (isTruthy(input$sch_topic)) {
      schedule <- schedule[schedule$skill_level %in% input$sch_topic, ]
    }
    schedule$info <- schedule$session_id
    common_vars <- c(
      "id", "info", "session_id", "session_type", "session_title",
      "start_datetime", "duration_formatted", "skill_level", "location", "url"
    )
    schedule <- schedule[, common_vars]
    schedule
  })

  selected_by_user_current_view <- reactive(getReactableState("schedule", "selected"))

  observeEvent(selected_by_user_current_view(), {
    current <- selected_talks$stack()
    on.exit(ignore_schedule_change(FALSE))
    if (!is.null(current) && is.null(selected_by_user_current_view()) && ignore_schedule_change()) {
      return()
    }
    in_view <- intersect(current, schedule_view()$id)

    if (is.null(selected_by_user_current_view()) && length(in_view)) {
      selected_talks$remove(in_view)
      return()
    }

    selected <- schedule_view()$id[selected_by_user_current_view()]

    talks_to_add <- setdiff(selected, current)
    talks_to_drop <- setdiff(in_view, selected)

    if (length(talks_to_add)) {
      selected_talks$add(talks_to_add)
    }
    if (length(talks_to_drop)) {
      selected_talks$remove(talks_to_drop)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$your_talks <- renderUI({
    req(selected_talks$stack())
    tagList(
      downloadButton(
        "download_calendar",
        class = "d-block mb-3 btn-primary",
        glue(
          "Download Calendar ({n} talk{s})",
          n = length(selected_talks$stack()),
          s = if (length(selected_talks$stack()) == 1) "" else "s"
        )
      ),
      p(class = "text-right", actionLink("reset", "Reset Selection"))
    )
  })

  output$download_calendar <- downloadHandler(
    filename = "nicar-2025-sessions.ics",
    content = function(file) {
      talks <- schedule[schedule$id %in% selected_talks$stack(), ]
      talks$start_time <- with_tz(talks$start_datetime, tzone = "US/Central")
      talks$end_time <- with_tz(talks$end_datetime, tzone = "US/Central")
      talk_events <- lapply(seq_len(nrow(talks)), function(idx) {
        desc <- paste0("Speakers: ", talks$speakers[[idx]], "\n\n", talks$session_description[[idx]])
        desc <- gsub("\n", "\\n", desc, fixed = TRUE)
        desc <- strwrap(desc, 75)
        desc <- paste(desc, collapse = " \n ")
        desc <- gsub(",", "\\,", desc)
        ev <- calendar::ic_event(
          start_time = talks$start_time[[idx]],
          end_time = talks$end_time[[idx]],
          summary = talks$session_title[[idx]],
          more_properties = TRUE,
          event_properties = c(
            DESCRIPTION = desc,
            URL = talks$url[[idx]],
            LOCATION = talks$location[[idx]]
          )
        )
        ev
      })
      calendar::ic_write(do.call(rbind, talk_events), file)
    }
  )

  observeEvent(input$reset, {
    selected_talks$update(NULL)
    reactable::updateReactable(
      "schedule",
      selected = NA
    )
  })

  ignore_schedule_change <- reactiveVal(FALSE)

  output$schedule <- reactable::renderReactable({
    ignore_schedule_change(TRUE)
    reactable(
      schedule_view(),
      selection = "multiple",
      defaultSelected = which(schedule_view()$id %in% isolate(selected_talks$stack())),
      highlight = TRUE,
      borderless = TRUE,
      columns = list(
        session_id = colDef(show = FALSE),
        id = colDef(show = FALSE),
        url = colDef(show = FALSE),
        start_datetime = colDef(
          name = "Time",
          minWidth = 100,
          html = TRUE,
          cell = function(value) {
            strftime(
              value,
              format = '<span class="white-space:pre;">%a</span> %I:%M %p',
              tz = "US/Central"
            )
          }
        ),
        duration_formatted = colDef(
          name = "Length",
          minWidth = 75,
        ),
        session_type = colDef(
          name = "Type",
          html = TRUE,
          align = "left",
          cell = function(value) {
            value <- paste(value)
            glue(
              '<span class="badge badge-pill badge-{type}">{value}</span>',
              type = switch(
                value,
                Networking = "info",
                Panel = "secondary",
                Demo = "success",
                Special = "danger",
                Commons = "success",
                "Hands-on" = "warning",
                "light"
              ),
              value = paste0(toupper(substr(value, 1, 1)), substr(value, 2, nchar(value)))
            )
          }
        ),
        location = colDef(
          name = "Location",
          html = TRUE,
          minWidth = 120,
          align = "right"
        ),
        skill_level = colDef(
          name = "Skill Level",
          html = TRUE,
          minWidth = 120,
          align = "center",
          cell = function(value) {
            if (!is.na(value)) {
              glue(
                '<span class="badge badge-pill badge-{type}">{value}</span>',
                type = switch(
                  paste(value),
                  Beginner = "info",
                  Intermediate = "secondary",
                  Advanced = "success",
                  "light"
                )
              )
            }
          }
        ),
        session_title = colDef(
          name = "Title",
          minWidth = 150,
          html = TRUE,
          cell = JS("
            function(cellInfo) {
              var url = cellInfo.row['url']
              return url ?
                '<a href=\"' + url + '\" target=\"_blank\" title=\"Go to Official Talk Page\">' + cellInfo.value + '<a>' :
                cellInfo.value
            }
          ")
        ),
       info = colDef(
          name = "",
          html = TRUE,
          minWidth = 50,
          sortable = FALSE,
          class = "cell-info-button",
          cell = function(value) {
            if (!isTruthy(value)) return()
            tags$button(
              class = "btn btn-light btn-talk-more-info",
              `data-value` = value,
              title = "More info...",
              icon("info-circle")
            )
          },
          style = list(
            position = "sticky",
            left = 30,
            background = "#fff",
            zIndex = 1,
            borderRight = "2px solid #eee"
          ),
          headerStyle = list(
            position = "sticky",
            left = 30,
            background = "#fff",
            zIndex = 1,
            borderRight = "2px solid #eee"
          )
        ),
        .selection = colDef(
          width = 30,
          style = list(
            cursor = "pointer",
            position = "sticky",
            left = 0,
            background = "#fff",
            zIndex = 1
          ),
          headerStyle = list(
            cursor = "pointer",
            position = "sticky",
            left = 0,
            background = "#fff",
            zIndex = 1
          )
        )
      )
    )
  })

  observeEvent(input$talk_more_info, {
    talk <- schedule[!is.na(schedule$session_id) & schedule$session_id == as.numeric(input$talk_more_info), ]
    req(nrow(talk))
    
    showModal(
      modalDialog(
        size = "l",
        easyClose = TRUE,
        title = talk$session_title[[1]],
        h2("Description"),
        HTML(talk$session_description[[1]]),
        h2("Speakers"),
        HTML(talk$speakers[[1]]),
        footer = list(
          tags$a(
            href = talk$url[[1]],
            class = "btn btn-success",
            target = "_blank",
            "Go To Talk Page"
          ),
          modalButton("OK")
        )
      )
    )
  })

}

shinyApp(ui = ui, server = server)
