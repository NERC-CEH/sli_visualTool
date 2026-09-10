timeline_plotly <- function(product_choice, events_column, product_choice2 = "") {

  product_list <- sales_long[sales_long$Product == product_choice, ]
  product_list2 <- sales_long[sales_long$Product == product_choice2, ]

  # need to do something in the case of total_sum_sales - do all events?? but need to label which one is related to which chemical
  if ("total_sum_sales" %in% c(product_choice, product_choice2)) {
    product_events <- events_long
    product_events2 <- ""
  }

  chemical <- sub("_.*$", "", product_choice)
  product_events <- events_long[events_long$Chemical == chemical, ]

  chemical2 <- sub("_.*$", "", product_choice2)
  product_events2 <- events_long[events_long$Chemical == chemical2, ]

  coloury1 = "#0066FF"
  coloury2 = '#FF6600'
  color_bar1 = "#99c2ff"
  color_bar2 = "#ffc299"
  color_intersect_y1 = "#0047b3"
  color_intersect_y2 = "#b34700"

  # Build all vertical lines
  shapes <- lapply(seq_len(nrow(product_events)), function(i) {
    list(
      type = "rect",
      x0 = product_events$Year[i] - 0.2,
      x1 = product_events$Year[i] + 0.2,
      y0 = 0,
      y1 = 1,
      yref = "paper",  # span full height
      fillcolor = color_bar1,
      opacity = 0.2,
      line = list(width = 0)
      #line = list(color = "red", dash = "dash")
    )
  })

  shapes2 <- lapply(seq_len(nrow(product_events2)), function(i) {
    list(
      type = "rect",
      x0 = product_events2$Year[i] - 0.2,
      x1 = product_events2$Year[i] + 0.2,
      y0 = 0,
      y1 = 1,
      yref = "paper",  # span full height
      fillcolor = color_bar2,
      opacity = 0.2,
      line = list(width = 0)
      #line = list(color = "red", dash = "dash")
    )
  })

  p <- plot_ly()

  # y-axis 1
  p <- p |> add_trace(
    data = product_list,
    name = product_choice,
    x = ~year,
    y = ~Value,
    type = "scatter",
    mode = "lines",
    line = list(
      color = coloury1,
      width = 3
      )
    )

  # y-axis 2
  p <- p |> add_trace(
    data = product_list2,
    name = product_choice2,
    x = ~year,
    y = ~Value,
    type = "scatter",
    mode = "lines",
    yaxis = "y2",
    line = list(
      color = coloury2,
      width = 3
      )
    )

  p <- p |>
    layout(shapes = c(shapes, shapes2),
           title = "Vet Meds timeline",
           xaxis = list(title = "Year",
                        showgrid = FALSE,
                        showline = TRUE,
                        mirror = FALSE,
                        ticks = "inside",
                        ticklen = 5,
                        margin = list(b = 50)


                        ),
           yaxis = list(
             title = list(
                        text = paste0(product_choice, " sales (kg)"),
                        font=list(size=15, family='Courier', color=coloury1)),
             showgrid = FALSE,
             showline = TRUE,
             linecolor = "black",
             zeroline = FALSE
             ),

        
           # temporary disable

           yaxis2 = list(
             title = list(
                        text = paste0(product_choice2, ""),
                        #text = paste0(product_choice2, " sales (kg)"),
                        font=list(size=15, family='Courier', color=coloury2),
                        standoff = 15
                        ),
             showgrid = FALSE,
             showline = TRUE,
             linecolor = "black",
             automargin = TRUE,
             zeroline = FALSE,

             overlaying = "y",
             side = "right"
             ),

           legend = list(
             font = list(
               family = "sans-serif",
               size = 12,
               color = "#000"),
             bgcolor = "rgba(0,0,0,0)",
             bordercolor = "rgba(0,0,0,0)",
             orientation = "h",
             x = 0,
             y = 1.1

             )

           ) |>


    # intersection points
    add_trace(
      data = product_events,
      x = ~Year,
      y = {
        vals <- approx(product_list$year,
                 product_list$Value,
                 xout = product_events$Year)$y
        replace(vals, is.na(vals), 0)

        },
      type = "scatter",
      mode = "markers",
      inherit = FALSE,
      marker = list(
        color = color_intersect_y1,
        size = 8,
        line = list(color = "white", width = 1.5)
      ),
      # text = c(~get(events_column), ~get(Year), ~get(value)),
      # hoverinfo = "text",

      text = ~get(events_column),
      hovertemplate = paste0(
        "%{text}<br>",
        "Year: %{x}<br>",
        "Value: %{y}<extra></extra>"
      ),

      showlegend = FALSE
    ) #|>

  # temporary disable
  
  
    # add_trace(
    #   data = product_events2,
    #   x = ~Year,
    #   y = {
    #     vals <- approx(product_list2$year,
    #                    product_list2$Value,
    #                    xout = product_events2$Year)$y
    #     replace(vals, is.na(vals), 0)
    # 
    #   },
    #   yaxis = "y2",
    #   type = "scatter",
    #   mode = "markers",
    #   inherit = FALSE,
    #   marker = list(
    #     color = color_intersect_y2,
    #     size = 8,
    #     line = list(color = "white", width = 2)
    #   ),
    #   text = ~get(events_column),
    #   hovertemplate = paste0(
    #     "%{text}<br>",
    #     "Year: %{x}<br>",
    #     "Value: %{y}<extra></extra>"
    #   ),
    #   showlegend = FALSE
    # )


  p
}