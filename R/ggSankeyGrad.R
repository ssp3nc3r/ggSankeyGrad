# gg_sankey_grad
#
#
#
#

ggSankeyGrad <- function(
    c1, c2, col1, col2, values,
    padding = 2, alpha = 0.4,
    label = FALSE, label_color = TRUE,
    label_fontface = 'bold', label_size = 10) {

  stopifnot(requireNamespace("tidyr"))
  stopifnot(requireNamespace("dplyr"))
  stopifnot(requireNamespace("ggplot2"))
  stopifnot(requireNamespace("grid"))

  stopifnot(packageVersion("ggplot2") >= 3.5 )



  # calculate beginning bottom and top of flow ribbons
  d1 <- data.frame(c1,
                   values) |>
    mutate(p = ifelse(c1 == lag(c1) | is.na(lag(c1)),0,padding)) |>
    mutate(csp = cumsum(p)) |>
    mutate(csv = cumsum(values)) |>
    mutate(t1 = csp + csv) |>
    mutate(b1 = t1 - values) |>
    select(c1,b1,t1)

  # calculate ending bottom and top of flow ribbons
  d2 <- data.frame(c2 ,
                   values) |>
    mutate(row = row_number()) |>
    arrange(factor(c2, levels = unique(c(c1,c2)))) |>
    mutate(p = ifelse(c2 == lag(c2) | is.na(lag(c2)),0,padding)) |>
    mutate(csp = cumsum(p)) |>
    mutate(csv = cumsum(values)) |>
    mutate(t2 = csp + csv) |>
    mutate(b2 = t2 - values) |>
    arrange(row) |>
    select(c2,b2,t2)



  # combine calculations with beginning and ending colors for each flow
  d <- bind_cols(d1, d2)
  d$col1 <- col1
  d$col2 <- col2

  # create a factor for each flow
  d <- d |> mutate(cat = as.integer(factor(paste0(c1, c2), levels = paste0(c1, c2)) ) )

  # x value for each vertical line
  x <- seq(0, 1, length = 101)

  # calculate bottom and top y value along bezier
  bez <- data.frame()
  for(i in seq(nrow(d))) {
    bot <- with(d[i,],
                matrix(c(0,b1, 1,b1, 3,b2, 4,b2),
                       nrow = 4, ncol = 2, byrow = TRUE))
    top <- with(d[i,],
                matrix(c(0,t1, 1,t1, 3,t2, 4,t2),
                       nrow = 4, ncol = 2, byrow = TRUE))
    bez <- bind_rows(bez,
      data.frame(cat = i,
        x = x,
        bez_b = bezier::bezier(t = x, p = bot)[,2],
        bez_t = bezier::bezier(t = x, p = top)[,2],
        stringsAsFactors = FALSE)
      )
  }

  bez <- bez |> filter(!is.na(bez_b))

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
      summarise(y = 0.5*max(t1) + 0.5*min(b1),
                col1 = col1[t1 == max(t1)]) |>
      ungroup() |>
      filter(!is.na(y))

    for(i in seq(nrow(loc))) {
      pl <- pl + annotate('text', x = -0.01, y = loc$y[i],
                          label = as.character(loc$c1[i]),
                          fontface = label_fontface,
                          hjust = 1, size = label_size/.pt, color = ifelse(label_color, loc$col1[i], '#000000') )
    }

    loc <- d |>
      group_by(c2) |>
      summarise(y = 0.5*max(t2) + 0.5*min(b2),
                col2 = col2[t2 == max(t2)]) |>
      ungroup() |>
      filter(!is.na(y))


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

