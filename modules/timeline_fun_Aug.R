timeline_plotly <- function(product_choice, chem_choice = "") {
  
  # helper to wrap y axis label
  wrap_title <- function(x, width = 20) {
    paste(strwrap(x, width = width), collapse = "<br>")
  }

  product_list <- combined_sales_prescription_data[combined_sales_prescription_data$product == product_choice, ]
  chem_list <- df_chem_data[df_chem_data$Compound_Name == chem_choice, ]


  chemical <- sub("_.*$", "", product_choice)
  product_events <- events_long[events_long$Chemical == chemical, ]
  events_column <- product_events$Event

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
      xref = "x",
      x0 = as.POSIXct(paste0(product_events$Year[i], "-01-01")) - days(90),
      x1 = as.POSIXct(paste0(product_events$Year[i], "-01-01")) + days(90),
      y0 = 0,
      y1 = 1,
      yref = "paper",  # span full height
      fillcolor = color_bar1,
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
    y = ~value,
    type = "scatter",
    mode = "lines",
    line = list(
      color = coloury1,
      width = 3
    )
  )

  # y-axis 2
  p <- p |> add_trace(
    data = chem_list,
    name = chem_choice,
    x = ~Sample_datetime,
    y = ~Concentration,
    type = "scatter",
    mode = "markers",
    yaxis = "y2"
    # line = list(
    #   color = coloury2,
    #   width = 3
    # )
  )
  
  product_choice_spaces <- str_replace_all(product_choice, "_", " ")
  product_title_wrapped <- wrap_title(paste0(product_choice_spaces, " sales (kg)"), width = 25)
  

  p <- p |>
    layout(shapes = shapes,
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
             text = product_title_wrapped,
             font=list(size=15, family='Courier', color=coloury1)),
             showgrid = FALSE,
             showline = TRUE,
             linecolor = "black",
             zeroline = FALSE
           ),


           yaxis2 = list(
             title = list(
               text = paste0(chem_choice, " ug/l"),
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
      x = ~as.POSIXct(paste0(Year, "-01-01")),
      y = {
        vals <- approx(as.numeric(as.POSIXct(product_list$year)),
                       product_list$value,
                       xout = as.numeric(as.POSIXct(paste0(product_events$Year, "-01-01"))))$y
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

      text = ~Event,
      hovertemplate = paste0(
        "%{text}<br>",
        "Year: %{x}<br>",
        "Value: %{y}<extra></extra>"
      ),

      showlegend = FALSE
    )

  p
}