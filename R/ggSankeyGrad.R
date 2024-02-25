# gg_sankey_grad
#
#
#
#

ggSankeyGrad <- function(
    c1, c2, col1 = "gray", col2 = "gray", values,
    padding = 2, alpha = 0.4,
    label = FALSE, label_color = TRUE,
    label_fontface = 'bold', label_size = 10,
    color_steps = 100) {

  stopifnot(requireNamespace("tidyr"))
  stopifnot(requireNamespace("dplyr"))
  stopifnot(requireNamespace("ggplot2"))
  stopifnot(requireNamespace("grid"))

  stopifnot(packageVersion("ggplot2") >= 3.5 )

  df <- data.frame(c1, c2, values)

  # calculate beginning bottom and top of flow ribbons
  d1 <- df |>
    group_by(c1, c2) |>
    summarise(lengths = sum(values)) |>
    ungroup() |>
    mutate(b1 = lag(cumsum(lengths +
                             ifelse(row_number() < n() &
                                      c1 == lead(c1),
                                    0, padding )))) |>
    mutate(b1 = ifelse(is.na(b1), 0, b1)) |>
    mutate(t1 = b1 + lengths) |>
    select(-lengths)

  # calculate ending bottom and top of flow ribbons
  d2 <- df |>
    group_by(c2, c1) |>
    summarise(lengths = sum(values)) |>
    ungroup() |>
    mutate(b2 = lag(cumsum(lengths +
                             ifelse(row_number() < n() &
                                      c2 == lead(c2),
                                    0, padding )))) |>
    mutate(b2 = ifelse(is.na(b2), 0, b2)) |>
    mutate(t2 = b2 + lengths) |>
    select(-lengths)

  # combine calculations with beginning and ending colors for each flow
  d <- left_join(d1, d2, by = c("c1", "c2"))
  d$col1 <- col1
  d$col2 <- col2

  # create a factor for each flow
  d <- d |> mutate(cat = as.integer(factor(paste0(c1, c2), levels = paste0(c1, c2)) ) )

  # x value for each vertical line
  x <- seq(0, 1, length = color_steps + 1)

  # calculate bottom and top y value for each line
  bez <- data.frame()
  for(i in seq(nrow(d))) {
    bot <- with(d[i,],
                matrix(c(0,b1, 1,b1, 3,b2, 4,b2),
                       nrow = 4, ncol = 2, byrow = TRUE))
    top <- with(d[i,],
                matrix(c(0,t1, 1,t1, 3,t2, 4,t2),
                       nrow = 4, ncol = 2, byrow = TRUE))
    col <- with(d[i,],
                colorRampPalette(c(as.character(col1),
                                   as.character(col2)))( length(x) ))
    bez <- bind_rows(bez,
                     data.frame(cat = i,
                                x = x,
                                col = col,
                                bez_b = bezier::bezier(t = x, p = bot)[,2],
                                bez_t = bezier::bezier(t = x, p = top)[,2],
                                stringsAsFactors = FALSE) )
  }

  # create base plot with lines
  pl <- ggplot(data = bez) +
    scale_x_continuous(limits = c(-0.2, 1.2))

  # add each flow ribbon to plot
  for(i in seq(length(col1))) {
    pl <- pl +
      geom_ribbon(
        data = filter(bez, cat == i),
        mapping = aes(
          x = x,
          ymin = bez_b, ymax = bez_t, group = cat
        ),
        fill = linearGradient(colours = c(d$col1[i], d$col2[i]), y2 = unit(0, 'npc')),
        alpha = alpha
      )
  }


  # add labels for beginning (left) and ending (right) categories
  if(label == TRUE) {
    loc <- d |>
      group_by(c1) |>
      slice(n()) |>
      ungroup() |>
      mutate(y = ifelse(row_number() == 1, t1/2 ,
                        (t1 + lag(t1) + padding)/2) )

    for(i in seq(nrow(loc))) {
      pl <- pl + annotate('text', x = -0.01, y = loc$y[i],
                          label = as.character(loc$c1[i]),
                          fontface = label_fontface,
                          hjust = 1, size = label_size/.pt, color = ifelse(label_color, loc$col1[i], '#000000') )
    }

    loc <- d |>
      group_by(c2) |>
      slice(n()) |>
      ungroup() |>
      mutate(y = ifelse(row_number() == 1, t2/2 ,
                        (t2 + lag(t2) + padding)/2) )


    for(i in seq(nrow(loc))) {
      pl <- pl + annotate('text', x = 1.01, y = loc$y[i],
                          label = as.character(loc$c2[i]),
                          fontface = label_fontface,
                          hjust = 0, size = label_size/.pt, color = ifelse(label_color, loc$col2[i], '#000000') )
    }


  }

  # remove all theme elements
  pl <- pl + theme_void()

  # return ggplot object
  return(pl)
}

