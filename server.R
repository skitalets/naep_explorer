shinyServer(
        function(input, output) {
                genderChoice <- reactive({
                        switch(input$gender,
                               "All" = "all",
                               "Male" = "male",
                               "Female" = "female")
                })
                naep.by.gender <- reactive({
                        naep[gender == genderChoice() &
                                     grade == input$grade, ]
                })
                output$view <- renderGvis( # set xlim and ylim consistently
                        # set xlabel and ylabel
                        gvisMotionChart(naep.by.gender(), "state", "year",
                                        "scale.score.math",
                                        "scale.score.reading", "region",
                                        options = list(
                                                title = "Here",
                                                vAxis = "{minValue: 190}",
                                                showAdvancedPanel = FALSE,
                                                showXMetricPicker = TRUE,
                                                showYMetricPicker = TRUE,
                                                showXScalePicker = FALSE,
                                                showYScalePicker = FALSE,
                                                showSidePanel = TRUE)))
                        }
)
