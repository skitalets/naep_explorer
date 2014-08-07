shinyServer(
        function(input, output) {
                genderChoice <- reactive({
                        switch(input$gender,
                               "All" = "all",
                               "Male" = "male",
                               "Female" = "female")
                })
                naep.by.gender <- reactive({
                        naep[gender == genderChoice(), ]
                })
                output$view <- renderGvis( # set xlim and ylim consistently
                        # set xlabel and ylabel
                        gvisMotionChart(naep.by.gender(), "state", "year",
                                        "scale.score.math",
                                        "scale.score.reading",
                                        options = list(
                                                showAdvancedPanel = FALSE,
                                                showXMetricPicker = FALSE,
                                                showYMetricPicker = FALSE,
                                                showXScalePicker = FALSE,
                                                showYScalePicker = FALSE,
                                                showSidePanel = FALSE)))
        }
)