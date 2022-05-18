##################################################
########## Modified Waterfall Functions ##########
##################################################





# Create waterfall 2 function
waterfall_2 <- function(.data = NULL, values, labels, rect_text_labels = values, 
                        rect_text_size = 1, rect_text_labels_anchor = "centre", 
                        put_rect_text_outside_when_value_below = 0.05 * (max(cumsum(values)) - 
                                                                           min(cumsum(values))), calc_total = FALSE, total_axis_text = "Total", 
                        total_rect_text = sum(values), total_rect_color = "black", 
                        total_rect_text_color = "white", fill_colours = NULL, 
                        fill_by_sign = TRUE, rect_width = 0.7, 
                        rect_border = "white", # Change border to white
                        draw_lines = TRUE, lines_anchors = c("right", "left"), 
                        linetype = "dashed", draw_axis.x = "behind", 
                        theme_text_family = "", scale_y_to_waterfall = TRUE, 
                        print_plot = FALSE, ggplot_object_name = "mywaterfall") 
{
  if (!is.null(.data)) {
    if (!is.data.frame(.data)) {
      stop("`.data` was a ", class(.data)[1], ", but must be a data.frame.")
    }
    if (ncol(.data) < 2L) {
      stop("`.data` had fewer than two columns, yet two are required: labels and values.")
    }
    dat <- as.data.frame(.data)
    char_cols <- vapply(dat, is.character, FALSE)
    factor_cols <- vapply(dat, is.factor, FALSE)
    num_cols <- vapply(dat, is.numeric, FALSE)
    if (!xor(num_cols[1], num_cols[2]) || sum(char_cols[1:2], 
                                              factor_cols[1:2], num_cols[1:2]) != 2L) {
      const_width_name <- function(noms) {
        if (is.data.frame(noms)) {
          noms <- names(noms)
        }
        max_width <- max(nchar(noms))
        formatC(noms, width = max_width)
      }
      stop("`.data` did not contain exactly one numeric column and exactly one character or factor ", 
           "column in its first two columns.\n\t", 
           "1st column: '", const_width_name(dat)[1], 
           "'\t", sapply(dat, class)[1], "\n\t", 
           "2nd column: '", const_width_name(dat)[2], 
           "'\t", sapply(dat, class)[2])
    }
    if (num_cols[1L]) {
      .data_values <- .subset2(dat, 1L)
      .data_labels <- .subset2(dat, 2L)
    }
    else {
      .data_values <- .subset2(dat, 2L)
      .data_labels <- .subset2(dat, 1L)
    }
    if (!missing(values) && !missing(labels)) {
      warning(".data and values and labels supplied, .data ignored")
    }
    else {
      values <- .data_values
      labels <- as.character(.data_labels)
    }
  }
  if (!(length(values) == length(labels) && length(values) == 
        length(rect_text_labels))) {
    stop("values, labels, fill_colours, and rect_text_labels must all have same length")
  }
  if (rect_width > 1) 
    warning("rect_Width > 1, your chart may look terrible")
  number_of_rectangles <- length(values)
  north_edge <- cumsum(values)
  south_edge <- c(0, cumsum(values)[-length(values)])
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    grDevices::hcl(h = hues, l = 65, c = 100)[seq_len(n)]
  }
  if (fill_by_sign) {
    if (!is.null(fill_colours)) {
      warning("fill_colours is given but fill_by_sign is TRUE so fill_colours will be ignored.")
    }
    # Change colors to blue and red
    fill_colours <- ifelse(values >= 0, "red", 
                           "blue")
  }
  else {
    if (is.null(fill_colours)) {
      fill_colours <- gg_color_hue(number_of_rectangles)
    }
  }
  if (!(grepl("^[lrc]", lines_anchors[1]) && grepl("^[lrc]", 
                                                   lines_anchors[2]))) 
    stop("lines_anchors must be a pair of any of the following: left, right, centre")
  if (grepl("^l", lines_anchors[1])) 
    anchor_left <- rect_width/2
  if (grepl("^c", lines_anchors[1])) 
    anchor_left <- 0
  if (grepl("^r", lines_anchors[1])) 
    anchor_left <- -1 * rect_width/2
  if (grepl("^l", lines_anchors[2])) 
    anchor_right <- -1 * rect_width/2
  if (grepl("^c", lines_anchors[2])) 
    anchor_right <- 0
  if (grepl("^r", lines_anchors[2])) 
    anchor_right <- rect_width/2
  if (!calc_total) {
    p <- if (scale_y_to_waterfall) {
      ggplot2::ggplot(data.frame(x = c(labels, labels), 
                                 y = c(south_edge, north_edge)), ggplot2::aes_string(x = "x", 
                                                                                     y = "y"))
    }
    else {
      ggplot2::ggplot(data.frame(x = labels, y = values), 
                      ggplot2::aes_string(x = "x", y = "y"))
    }
    p <- p + ggplot2::geom_blank() + ggplot2::theme(axis.title = ggplot2::element_blank())
  }
  else {
    p <- if (scale_y_to_waterfall) {
      ggplot2::ggplot(data.frame(x = c(labels, total_axis_text, 
                                       labels, total_axis_text), y = c(south_edge, north_edge, 
                                                                       south_edge[number_of_rectangles], north_edge[number_of_rectangles])), 
                      ggplot2::aes_string(x = "x", y = "y"))
    }
    else {
      ggplot2::ggplot(data.frame(x = c(labels, total_axis_text), 
                                 y = c(values, north_edge[number_of_rectangles])), 
                      ggplot2::aes_string(x = "x", y = "y"))
    }
    p <- p + ggplot2::geom_blank() + ggplot2::theme(axis.title = ggplot2::element_blank())
  }
  if (grepl("behind", draw_axis.x)) {
    p <- p + ggplot2::geom_hline(yintercept = 0)
  }
  for (i in seq_along(values)) {
    # Add alpha to plot
    p <- p + ggplot2::annotate("rect", xmin = i - rect_width/2, 
                               xmax = i + rect_width/2, ymin = south_edge[i], ymax = north_edge[i], 
                               colour = rect_border, fill = fill_colours[i], alpha = 0.8)
    if (i > 1 && draw_lines) {
      # Add alpha to plot
      p <- p + ggplot2::annotate("segment", x = i - 
                                   1 - anchor_left, xend = i + anchor_right, linetype = linetype, 
                                 y = south_edge[i], yend = south_edge[i], alpha = 0.8)
    }
  }
  for (i in seq_along(values)) {
    if (abs(values[i]) > put_rect_text_outside_when_value_below) {
      p <- p + ggplot2::annotate("text", x = i, y = 0.5 * 
                                   (north_edge[i] + south_edge[i]), family = theme_text_family, 
                                 label = ifelse(rect_text_labels[i] == values[i], 
                                                ifelse(values[i] < 0, paste0("???", -1 * 
                                                                               values[i]), values[i]), rect_text_labels[i]), 
                                 size = rect_text_size/(5/14))
    }
    else {
      p <- p + ggplot2::annotate("text", x = i, y = north_edge[i], 
                                 family = theme_text_family, label = ifelse(rect_text_labels[i] == 
                                                                              values[i], ifelse(values[i] < 0, paste0("???", 
                                                                                                                      -1 * values[i]), values[i]), rect_text_labels[i]), 
                                 vjust = ifelse(values[i] >= 0, -0.2, 1.2), size = rect_text_size/(5/14))
    }
  }
  if (calc_total) {
    p <- p + ggplot2::annotate("rect", xmin = number_of_rectangles + 
                                 1 - rect_width/2, xmax = number_of_rectangles + 1 + 
                                 rect_width/2, ymin = 0, ymax = north_edge[number_of_rectangles], 
                               colour = rect_border, fill = total_rect_color) + 
      ggplot2::annotate("text", x = number_of_rectangles + 
                          1, y = 0.5 * north_edge[number_of_rectangles], 
                        family = theme_text_family, label = ifelse(total_rect_text == 
                                                                     sum(values), ifelse(north_edge[number_of_rectangles] < 
                                                                                           0, paste0("???", -1 * north_edge[number_of_rectangles]), 
                                                                                         north_edge[number_of_rectangles]), total_rect_text), 
                        color = total_rect_text_color, size = rect_text_size/(5/14)) + 
      ggplot2::scale_x_discrete(labels = c(labels, total_axis_text))
    if (draw_lines) {
      p <- p + ggplot2::annotate("segment", x = number_of_rectangles - 
                                   anchor_left, xend = number_of_rectangles + 1 + 
                                   anchor_right, y = north_edge[number_of_rectangles], 
                                 yend = north_edge[number_of_rectangles], linetype = linetype)
    }
  }
  else {
    p <- p + ggplot2::scale_x_discrete(labels = labels)
  }
  if (grepl("front", draw_axis.x)) {
    p <- p + ggplot2::geom_hline(yintercept = 0)
  }
  if (print_plot) {
    if (ggplot_object_name %in% ls(.GlobalEnv)) 
      warning("Overwriting ", ggplot_object_name, 
              " in global environment.")
    assign(ggplot_object_name, p, inherits = TRUE)
    print(p)
  }
  else {
    return(p)
  }
}

# Change showwaterfall function
showWaterfall_2 <- function (xgb.model, explainer, DMatrix, data.matrix, idx, type = "binary", 
                             threshold = 1e-04, limits = c(NA, NA)) 
{
  breakdown = explainPredictions(xgb.model, explainer, slice(DMatrix, 
                                                             as.integer(idx)))
  weight = rowSums(breakdown)
  if (type == "regression") {
    pred = weight
  }
  else {
    pred = 1/(1 + exp(-weight))
  }
  breakdown_summary = as.matrix(breakdown)[1, ]
  data_for_label = data.matrix[idx, ]
  # Change order to decreasing
  i = order(abs(breakdown_summary), decreasing = FALSE)
  breakdown_summary = breakdown_summary[i]
  data_for_label = data_for_label[i]
  intercept = breakdown_summary[names(breakdown_summary) == 
                                  "intercept"]
  data_for_label = round(data_for_label[names(breakdown_summary) != 
                                          "intercept"],2)
  breakdown_summary = breakdown_summary[names(breakdown_summary) != 
                                          "intercept"]
  i_other = which(abs(breakdown_summary) < threshold)
  other_impact = 0
  if (length(i_other > 0)) {
    other_impact = sum(breakdown_summary[i_other])
    names(other_impact) = "other"
    breakdown_summary = breakdown_summary[-i_other]
    data_for_label = data_for_label[-i_other]
  }
  if (abs(other_impact) > 0) {
    # Change ordering so other follows intercept
    breakdown_summary = c(intercept, other_impact, breakdown_summary)
    data_for_label = c("", "", data_for_label)
    labels = paste0(names(breakdown_summary), " = ", 
                    data_for_label)
    # Set labels for intercept and other
    labels[1] = "intercept"
    labels[2] = "other"
  }
  else {
    breakdown_summary = c(intercept, breakdown_summary)
    data_for_label = c("", data_for_label)
    labels = paste0(names(breakdown_summary), " = ", 
                    data_for_label)
    labels[1] = "intercept"
  }
  if (!is.null(getinfo(DMatrix, "label"))) {
    cat("\nActual: ", getinfo(slice(DMatrix, as.integer(idx)), 
                              "label"))
  }
  cat("\nPrediction: ", pred)
  cat("\nWeight: ", weight)
  cat("\nBreakdown")
  cat("\n")
  print(breakdown_summary)
  if (type == "regression") {
    waterfalls::waterfall(values = breakdown_summary, rect_text_labels = round(breakdown_summary, 
                                                                               2), labels = labels, total_rect_text = round(weight, 
                                                                                                                            2), calc_total = TRUE, total_axis_text = "Prediction") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  else {
    inverse_logit_trans <- scales::trans_new("inverse logit", 
                                             transform = plogis, inverse = qlogis)
    inverse_logit_labels = function(x) {
      return(1/(1 + exp(-x)))
    }
    logit = function(x) {
      return(log(x/(1 - x)))
    }
    ybreaks <- logit(seq(2, 98, 2)/100)
    # Change function call to waterfall_2
    waterfall_2(values = breakdown_summary, rect_text_labels = round(breakdown_summary, 
                                                                     2), labels = labels, total_rect_text = round(weight, 
                                                                                                                  2), calc_total = TRUE, total_axis_text = "Prediction") + 
      scale_y_continuous(labels = inverse_logit_labels, 
                         breaks = ybreaks, limits = limits) + 
      dark_theme_bw() + # Add dark mode
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(), # Remove grid
            panel.grid.minor = element_blank(), # Remove grid
            panel.border = element_blank(), # Remove grid
            panel.background = element_blank()) +
      coord_flip()  + # Flip coordinates
      # Add labels
      labs(x = "Variables", y = "SHAP Contribution")
  }
}

