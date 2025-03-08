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

# setup ----
Sys.setenv(TZ = "US/Central")

# define Bootstrap theme
theme <- bs_theme(
  version = 5,
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
schedule <- readr::read_rds("data/schedule.rds") |> dplyr::arrange(start_datetime)
# create unique event ID
schedule$id <- seq_len(nrow(schedule))
# create line break between speakers
schedule$speakers_html <- gsub(schedule$speakers, pattern = ", ", replacement = "<br>")

# add directory of static resources to Shiny's web server
addResourcePath(prefix = "img", directoryPath = "img")

# define your Shiny UI (sometimes ui.R) ----
ui <- navbarPage(
  
  # add metadata to app
  header = metathis::meta_social(
    title = "NICAR 2025 Calendar",
    description = "An Shiny app that makes it easy to build and export your custom NICAR 2025 calendar.",
    url = "http://spcanelon.shinyapps.io/nicar-2025-calendar",
    image = "http://spcanelon.shinyapps.io/nicar-2025-calendar/social-card.png",
    image_alt = "Shiny app table of NICAR 2025 sessions, displaying fields including session type, time, length, skill level, location, and whether they were recorded. The interactive options to filter the sessions include a keyword search bar, a radio button for the day of the conference, a slider for the time of day, a search bar for speakers, session type, and skill level, and lastly, a radio button for whether the session will be recorded. Official conference schedule is posted online at https://schedules.ire.org/nicar-2025/index.html",
    twitter_creator = "@spcanelon",
    twitter_card_type = "summary"
  ),
  
  title = 'NICAR 2025 calendar',
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
    p(paste("Last updated:", "Sat, Mar 08 @ 9:50 AM" 
            # format(lubridate::now(tzone = "US/Central"), "%a, %B %d @ %I:%M:%S %p")
            )),
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
            # input fields
            div(
              class = "col-6 col-lg-12",
              textInput("sch_search", 
                        label = "Search", width = "100%"),
              radioButtons("sch_day", "Day",
                           c("Thursday" = "one", "Friday" = "two", 
                             "Saturday" = "three", "Sunday" = "four", "All" = "all"),
                           inline = TRUE, selected = c("all"), width = "100%"),
              sliderInput("sch_hours", "Hours", value = c(6, 18),
                          min = 0, max = 24, step = 1, post = ":00", width = "100%")
            ),
            div(
              class = "col-6 col-lg-12",
              selectizeInput("sch_presenter", "Speakers",
                             choices = sort(unique(schedule$speaker_names)),
                             multiple = TRUE, width = "100%"),
              selectizeInput("sch_type", "Session Type",
                             choices = sort(unique(schedule$session_type)),
                             multiple = TRUE, width = "100%"),
              selectizeInput("sch_topic", "Skill Level",
                             choices = sort(unique(schedule$skill_level)),
                             multiple = TRUE, width = "100%"),
              radioButtons("sch_recorded", "Recorded Session (Audio)",
                           c("Yes" = "yes", "No" = "no", "All" = "all"),
                           inline = TRUE, selected = c("all"), width = "100%")
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
      div(
      # class = "image-cropper",
        style = "margin-top: 20px;",
        img(src = "https://silviacanelon.com/about/sidebar/avatar.png",
            alt = "Smiling woman with a tan complexion, dark eyes, and dark long wavy hair styled to one side",
            class = "image-cropper rounded"
        ),
        p("My name is",
          tags$a(href = "https://silviacanelon.com/project/2025-02-28-nicar-calendar/", "Silvia Canelón", .noWS = "after"),
          HTML(" and this is my first time attending NICAR. &#xFE0F;👋")
        ),
        p("If you see me around, please say hello!"
          ),
        p("You can also get in touch on",
          tags$a(href = "https://bsky.app/profile/silviacanelon.com", "Bluesky", .noWS = "after"),
          " or through",
          tags$a(href = "https://silviacanelon.com/contact", "my website.", .noWS = "after")
        ),
      ),
      p(
        HTML("<br>This app was built with &#x2665;"),
        " using the official NICAR 2025 schedule, Shiny, and the R packages below.",
        HTML("You can find"),
        tags$a(href = "https://github.com/spcanelon/nicar-2025-calendar", "the full source code for this app"),
        "on GitHub (adapted from an app created by",
        tags$a(href = "https://github.com/gadenbuie/rstudio-global-2021-calendar", "Garrick Aden-Buie"),
        ")."
      ),
      tags$hr(class = "my-4"),
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
      h2("Packages", class = "text-monospace"),
      div(
        class = "row row-cols-2 row-cols-lg-3 g-2 g-lg-3",
        card(
          "shiny",
          posit_hex("shiny"),
          "https://shiny.rstudio.com",
          "Shiny is an R package that makes it easy to build interactive web apps straight from R."
        ),
        card(
          "bslib",
          posit_hex("bslib"),
          "https://rstudio.github.io/bslib/",
          "Tools for creating custom Bootstrap themes, making it easier to style Shiny apps & R Markdown documents directly from R without writing unruly CSS and HTML."
        ),
        card(
          "rvest",
          posit_hex("rvest"),
          "https://rstudio.github.io/rvest",
          "The rvest package helps you scrape (or harvest) data from web pages."
        ),
        card(
          "lubridate",
          posit_hex("lubridate"),
          "https://lubridate.tidyverse.org",
          "Make working with dates in R just that little bit easier."
        ),
        card(
          "glue",
          posit_hex("glue"),
          "https://glue.tidyverse.org",
          "Glue strings to data in R. Small, fast, dependency free interpreted string literals."
        ),
        card(
          "R6",
          posit_hex("R6"),
          "https://r6.r-lib.org/",
          "Encapsulated object-oriented programming for R."
        ),
        card(
          "renv",
          posit_hex("renv"),
          "https://rstudio.github.io/renv",
          "The renv package helps you create reproducible environments for your R projects. Use renv to make your R projects more: isolated, portable, and reproducible."
        ),
        card(
          "calendar",
          placeholder_hex("calendar"),
          "https://github.com/ATFutures/calendar",
          "Create, read, write, and work with iCalendar (.ics, .ical or similar) files in R."
        ),
        card(
          "reactable",
          placeholder_hex("calendar"),
          "https://glin.github.io/reactable/index.html",
          "Interactive data tables for R, based on the React Table library and made with reactR."
        ),
      )
    )
  )
)

# define Shiny server logic (sometimes server.R) ----
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
        schedule <- schedule[schedule$day == "Saturday",]
      } else if (input$sch_day == "four") {
      # } else {
        schedule <- schedule[schedule$day == "Sunday",]
      }
    }
    
    schedule$time <- with_tz(schedule$start_datetime, tzone = "US/Central")
    
    # use keyword Search
    if (shiny::isTruthy(input$sch_search)) {
      schedule <- schedule[
        grepl(tolower(input$sch_search), tolower(paste(schedule$session_id, schedule$session_title, schedule$session_description, schedule$speakers))),
      ]
    }
    # select hours
    if (isTruthy(input$sch_hours)) {
      schedule <- schedule[
        hour(schedule$time) >= input$sch_hours[1] & hour(schedule$time) <= input$sch_hours[2],
      ]
    }
    # select/search Presenter
    if (isTruthy(input$sch_presenter)) {
      schedule <- schedule[schedule$speaker_names %in% input$sch_presenter, ]
    }
    # select/search Talk Type
    if (isTruthy(input$sch_type)) {
      schedule <- schedule[schedule$session_type %in% input$sch_type, ]
    }
    # select/search Talk Topic
    if (isTruthy(input$sch_topic)) {
      schedule <- schedule[schedule$skill_level %in% input$sch_topic, ]
    }
    # select whether the sessions are recorded
    if (isTruthy(input$sch_recorded)) {
      if (input$sch_recorded == "yes") {
        schedule <- schedule[schedule$recorded == "Yes",]
      } else if (input$sch_recorded == "no") {
        schedule <- schedule[schedule$recorded == "No",]
      }
    }
    
    schedule$info <- schedule$session_id
    
    common_vars <- c(
      "id", "info", "session_id", "session_type", "session_title",
      "start_datetime", "duration_formatted", "skill_level", "location", "url",
      "recorded"
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
        desc <- paste0(
          "Session link: ", talks$url[[idx]], "\n\n",
          "Skill level: ", talks$skill_level[[idx]], "\n\n",
          "Recorded (audio): ", talks$recorded[[idx]], "\n\n",
          "Speakers: ", talks$speakers[[idx]], "\n\n", 
          talks$session_description[[idx]])
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
  
  icon_yes <- '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-mic-fill" viewBox="0 0 16 16">
  <path d="M5 3a3 3 0 0 1 6 0v5a3 3 0 0 1-6 0z"/>
  <path d="M3.5 6.5A.5.5 0 0 1 4 7v1a4 4 0 0 0 8 0V7a.5.5 0 0 1 1 0v1a5 5 0 0 1-4.5 4.975V15h3a.5.5 0 0 1 0 1h-7a.5.5 0 0 1 0-1h3v-2.025A5 5 0 0 1 3 8V7a.5.5 0 0 1 .5-.5"/>
</svg>'
  icon_no <- '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-mic-mute" viewBox="0 0 16 16">
  <path d="M13 8c0 .564-.094 1.107-.266 1.613l-.814-.814A4 4 0 0 0 12 8V7a.5.5 0 0 1 1 0zm-5 4c.818 0 1.578-.245 2.212-.667l.718.719a5 5 0 0 1-2.43.923V15h3a.5.5 0 0 1 0 1h-7a.5.5 0 0 1 0-1h3v-2.025A5 5 0 0 1 3 8V7a.5.5 0 0 1 1 0v1a4 4 0 0 0 4 4m3-9v4.879l-1-1V3a2 2 0 0 0-3.997-.118l-.845-.845A3.001 3.001 0 0 1 11 3"/>
  <path d="m9.486 10.607-.748-.748A2 2 0 0 1 6 8v-.878l-1-1V8a3 3 0 0 0 4.486 2.607m-7.84-9.253 12 12 .708-.708-12-12z"/>
</svg>'
  
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
          minWidth = 150,
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
        ),
        session_type = colDef(
          name = "Type",
          minWidth = 110,
          html = TRUE,
          align = "left",
          cell = function(value) {
            value <- paste(value)
            glue(
              '<span class="badge rounded-pill bg-{type}">{value}</span>',
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
        recorded = colDef(
          name = "Recorded",
          html = TRUE,
          minWidth = 100,
          align = "center",
          cell = function(value) {
            if (value == "Yes") icon_yes else icon_no
          }
        ),
        location = colDef(
          name = "Location",
          html = TRUE,
          minWidth = 150,
          align = "right"
        ),
        skill_level = colDef(
          name = "Skill Level",
          html = TRUE,
          minWidth = 110,
          align = "center",
          cell = function(value) {
            if (!is.na(value)) {
              glue(
                '<span class="badge rounded-pill bg-{type}">{value}</span>',
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
          minWidth = 250,
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

  # pop-up window with more event info
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
        # line break
        p(),
        h2("Speakers"),
        HTML(talk$speakers_html[[1]]),
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

# create and launch the Shiny app ----
shinyApp(ui = ui, server = server)
