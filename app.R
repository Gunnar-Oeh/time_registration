library(shiny)
library(stringr)
library(plyr)
library(reshape2)
library(chron)

file_data <- "time_registration"

### Load .RDS data if already saved
if(file_data %in% list.files()) {
  ze_df <- readRDS(paste(file_data, ".RDS", sep = ""))
} else { # else create empty df
  ze_df <- data.frame(
    dat = as.Date(character(), format = "%Y-%m-%d"),
    time_from = times(character(), format = "h:m:s"),
    time_to = times(character(), format = "h:m:s"),
    pause = times(character(), format = "h:m:s"),
    type_day = character(),
    perc_work = integer(),
    hours_day = times(character(), format = "h:m:s"),
    weekday = character(),
    month = character(),
    stringsAsFactors = FALSE
  )
}
###########################################################
### Non reactioniary functions used within the server logic
###########################################################

### Function to save entered data after various actions
save_data <- function(new_data) {
  saveRDS(new_data, file = paste(file_data, ".RDS", sep = ""))
}

### Function to fill in missing data for in-between days
inbetween <- function(last_day, input = input) {
  today <- input$dat
  
  dat <- seq(from = (last_day + 1), to = (today - 1), by = "days") # days since the last entry
  
  nr_days <- length(dat) # length of days without entry
  # assuming there was no work on in-between days, start and stop are equally 00:00
  time_from <- chron(times. = rep("00:00:00", nr_days)) 
  time_to <- time_from
  
  pause <- time_from # No work = no pause
  
  perc_work <- rep(input$perc_work, nr_days) # relates to the percentag of hours per week
  hours_day <- time_to - time_from - pause # worked hours inbetween
  weekday <- weekdays(dat) # weekdays inbetween
  
  type_day <- sapply(weekday, function(x) { # assigns a type of day (0,1,2,3)
    if (!(x %in% c("Saturday", "Sunday"))) {
      input$type_day_ibtw
    } else {
      0
    }
  })
  
  ### Return the values to be stored
  return( 
    data.frame(
      dat,
      time_from,
      time_to,
      pause,
      type_day,
      perc_work,
      hours_day,
      weekday,
      month = paste(years(dat), months(dat), sep = "_"),
      stringsAsFactors = FALSE
    )
  )
}

### Function to calculate target hours for workdays based on percentage of full time (40h)
target_hours <- function(df) {
  sum(daply(df, "month", function(df) {
    IND <- !(df$weekday %in% c("Saturday", "Sunday")) & df$type_day == 0
    nr_workdays <- sum(IND)
    weeks <- nr_workdays / 5
    work_hours_month <- weeks * unique(df$perc_work) / 100 * 40
    
    work_hours_month
  }))
}

### Function to calculate actual hours
actual_hours <- function(x) {
  sum(as.numeric(x) * 24)
}

###############################
### UI definition
###############################

ui <- fluidPage(
  # header = logo of company and name of the App
  titlePanel(
    div(
      img(src = "logo.jpg", height = "100"),
      "Zeiterfassung - G. Oehmichen"
    )
  ),
  # Sidebare = input-fields
  sidebarLayout(
    sidebarPanel(
      h4("Eingabe Arbeitszeiten"), # Title
      
      # Date 
      dateInput("dat", label = "Datum", format = "dd.mm.yyyy"), # Date input
      
      # Type of day (Workday, Sick-day, vacation, national-holiday)
      selectInput(
        "type_day",
        "Besonderheit zum Tag (Urlaub etc.)",
        choices = list(" " = 0, "Krankheit" = 1, "Urlaub" = 2, "Feiertag" = 3),
        selected = 0
      ),
      
      # Start-Time
      textInput("time_from", "Beginn in hh:mm:ss", value = "09:00:00"), # Start time input
      
      # Stop-Time
      textInput("time_to", "Ende in hh:mm:ss", value = "17:30:00"), # End time input
      
      # Pause-Time
      textInput("pause", "Pausenzeit in hh:mm:ss", value = "00:30:00"), # Pause time input
      
      # Percentage of full-time
      numericInput("perc_work", "Wochenarbeitszeit [%]", value = 100),
      
      selectInput(
        "type_day_ibtw",
        "Tage seit letzter Eingabe - Urlaub etc.?",
        choices = list(" " = 0, "Krankheit" = 1, "Urlaub" = 2, "Feiertag" = 3),
        selected = 0
      ),
      
      # Submit / Save input for one day
      actionButton("submit", "Speichern"),
      
      # If mistakes were made, restare Button enables the user to set back the saved values by X days
      numericInput("restore_days", "Zurücksetzen um X Tage", value = 1),
      
      actionButton("restore", "Zurücksetzen")
    ),
    
    # Main Panel displaying a table of the last week 
    mainPanel(
      h4("Darstellung Zeitkontingent"),
      tableOutput("ze_new"),
      textOutput("contingent")
    )
  )
)

##############################################
### Define Server logic
##############################################

server <- function(input, output, session) {
  
  # Reactive table with new input. Displays the days of the last week
  # Reactive table with new input. Displays the days of the last week
  ze_new <- reactive({
    
    # Nr of Days since last entry of work time
    diff_days <- input$dat - ze_df$dat[length(ze_df$dat)] 
    
    time_from <- chron (times. = input$time_from) 
    time_to <- chron (times. = input$time_to) 
    pause <- chron (times. = input$pause)
    hours_day <-  time_to - time_from - pause
    
    # Create new row with input data
    new_row <- data.frame(
      dat = input$dat,
      time_from = times(input$time_from),
      time_to = times(input$time_to),
      pause = times(input$pause),
      type_day = as.character(input$type_day),
      perc_work = input$perc_work,
      hours_day = hours_day,
      weekday = weekdays(input$dat),
      month = paste(years(input$dat), months(input$dat), sep = "_"),
      stringsAsFactors = FALSE
    )
    
    if (nrow(ze_df) > 0) {
      # Fill in missing days if necessary with inbetween function
      if (diff_days > 1) {
        ze_ibtw <- inbetween(last_day = ze_df$dat[length(ze_df$dat)], input = input)
        ze_df_2 <- rbind(ze_df, ze_ibtw)
      } else {
        # If last day is yesterday, adopt saved data
        ze_df_2 <- ze_df 
      }
    } else {
      # Create a dew_df to directly display if the dataframe is empty
      ze_df_2 <- new_row
    }
    
    # Combine new row with existing data
    ze_df_new <- rbind(ze_df_2, new_row)
    
    # Save the updated data
    save_data(ze_df_new)
    
    # Return the last 7 days of data. Less if shorter
    nr_df <- nrow(ze_df_new)
    if(nr_df >= 6){nr_displ <- 6} else {nr_displ <- 0}
    ze_df_new[(nr_df-nr_displ):nr_df, ]
  })
  
  
  # Render Table with the correct order of columns and their names
  output$ze_new <- renderTable({
    ze_display <- ze_new()[, c(8, 1:7)]
    names(ze_display) <- c(
      "Tag",
      "Datum",
      "Start",
      "Ende",
      "Pause",
      "Art.Tag",
      "Anteil [%]",
      "Stunden"
    )
    # transform to correct format
    ze_display$Datum <- as.character(format(ze_display$Datum, "%d.%m.%Y"))
    ze_display$Start <- as.character(ze_display$Start)
    ze_display$Ende <- as.character(ze_display$Ende)
    ze_display$Pause <- as.character(ze_display$Pause)
    ze_display$Art.Tag <- factor(
      ze_display$Art.Tag,
      levels = c(0:3),
      labels = c(" ", "Krankheit", "Urlaub", "Feiertag")
    )
    ze_display$Stunden <- ze_display$Stunden * 24
    
    tail(ze_display)
  })
  
  # Calculate contingent (over/undertime?)
  contingent <- reactive({
    plus_hours <- actual_hours(ze_new()$hours_day) - target_hours(ze_new())
    plus_days <- plus_hours / (ze_new()$perc_work[nrow(ze_new())] / 100 * 40 / 5)
    
    if (plus_hours > 0) {
      type_ <- "Über"
    } else {
      type_ <- "Unter"
    }
    
    type_hours <- paste(type_, "stunden", sep = "")
    type_days <- paste(type_, "tage", sep = "")
    
    list(round(plus_hours, 2), type_hours, ":", round(plus_days, 2), type_days)
  })
  # Output of Over/undertime with text
  output$contingent <- renderText({
    paste(contingent()[1:2], contingent()[3:4], collapse = "")
  })
  
  # Save button
  observeEvent(input$submit, {
    save_data(ze_new())
  })
  
  # Restore button
  observeEvent(input$restore, {
    ze_df_restored <- ze_df[1:(nrow(ze_df) - input$restore_days), ]
    save_data(ze_df_restored)
  })
}

#######################
# Run the app
#######################

shinyApp(ui = ui, server = server)