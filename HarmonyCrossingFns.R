


d_addNA <- function(d, col1 = "Answer", col2 = "Number") {
  table(d, useNA = "ifany") |>
    {\(.x) {tibble::tibble(!!col1 := UU::`%|%`(names(.x), "NA"), !!col2 := as.numeric(.x))}}()
}
d_add_color <- function(d) {
  n_col <- nrow(d)
  na <- list(
    logical = is.na(d[[1]]),
    character = d[[1]] == "NA"
  )
  col_d <- if (any(c(na$logical, na$character))) {
    n_col <- n_col - 1
    c(UU::color_interpolate(n = n_col), "#D3D3D3")
  } else
    UU::color_interpolate(n = n_col)
  tibble::add_column(d, colors = col_d)
}

#' Combine a list of factor vectors into a single factor vector
#' @param factor_list A list of factor vectors, some of which may contain NA values
#' @return A factor vector preserving original levels and NA values
#' @examples
#' factors <- list(factor(c("A", "B", NA)), factor(c("B", "C", "A")))
#' combine_factors_preserve_na(factors)
combine_factors_preserve_na <- function(factor_list) {
  # Check if all elements are factors or NULL/NA
  if (!all(sapply(factor_list, function(x) is.factor(x) || is.null(x) || all(is.na(x))))) {
    stop("All elements in the list must be factor vectors, NULL, or entirely NA.")
  }

  # Extract unique levels from all factor vectors
  unique_levels <- unique(unlist(lapply(factor_list, function(x) if (!is.null(x)) levels(x))))

  # Flatten the list while preserving NA values
  combined_values <- unlist(lapply(factor_list, function(x) if (is.null(x)) NA else as.character(x)))

  # Create a factor with all unique levels, ensuring NA values are included
  factor(combined_values, levels = unique_levels, exclude = NULL)
}



d_likert_table <- function(d, col1 = "Answer", col2 = "Number", l) {
  l <- addNA(l)
  munged <- purrr::map(d[[1]], \(.x) {
    x <- if (!is.na(as.numeric(.x)))
      l[as.numeric(.x)]
    else if (is.na(.x))
      .x
    else
      l[l == .x]
    x
  })

  combine_factors_preserve_na(munged) |>
    d_addNA(col1 = col1, col2 = col2)
}

d_overall_rank <- function(d, sep = ", ") {
  nm <- names(d)
  num_na <- sum(is.na(d))


  out <- na.omit(d)
  l_obs <- nrow(out)
  l_items <- out[[1]][1] |>
    stringr::str_split(sep) |>
    unlist() |>
    length()
  out <- tidyr::separate_rows(out, 1, sep = sep) |>
    dplyr::mutate(Position = rep(1:l_obs, each = l_items)) |> # Because there are 13 valid obs with 8 items ranked in each
    dplyr::group_by(Position) |>
    dplyr::group_map(\(...) {
      dplyr::mutate(..1, Position = dplyr::row_number())
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(!!rlang::sym(nm)) |>
    dplyr::summarise(Total = sum(Position), .groups = "drop") |>
    dplyr::arrange(Total) |>
    dplyr::mutate(`Overall Rank` = dplyr::min_rank(Total), Total = NULL)

    return(tagList(htmltools::tags$div(
      class = "text-center",
    knitr::kable(out, format= "html") |>
      htmltools::HTML(),
    htmltools::tags$em(class = "text-center", num_na, " respondents did not answer."))
    ))
}

q_hist <- function(d, x_lab = "Days Attended", y_lab = "Frequency") {
  column_title <- names(d)
  col_sym <- rlang::sym(column_title)
  # Count NA values
  na_count <- sum(is.na(d[[1]]))

  # Create histogram
  rlang::quo({
    d |>
      dplyr::filter(!is.na(.data[[column_title]])) |>
      e_charts() |>
      e_histogram(!!col_sym, name = "Days") |>
      e_title(column_title, subtext = glue::glue("{na_count} NA removed")) |>
      e_x_axis(name = x_lab) |>
      e_y_axis(name = y_lab) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE)
  }) |>
    rlang::eval_tidy()

}
t_text_response <- function(d) {
  tagList(htmltools::tags$div(
    class = "text-center",
    na.omit(d) |>
            knitr::kable(format= "html") |>
            htmltools::HTML(),
          htmltools::tags$em(class = "text-center", sum(is.na(d)), " respondents did not answer."))
  )
}

#' Create a bar graph of ordinal question response data
#'
#' @param d \code{df/tbl} two column data with response label, and frequency as the respective columns
#' @param text \code{chr} The intended title of the graph
#' @param subtext \code{chr/NULL} The subtitle
#'
#' @returns \code{echart}
#' @export
#'

q_bar <- function(d, text = NULL, subtext = NULL) {
  nms <- names(d)
  x <-  rlang::sym(nms[1])
  y <- rlang::sym(nms[2])

  e <- rlang::quo({
    e_charts(d, !!x) |>
      e_bar(!!y) |>
      e_title(text, subtext) |>
      e_x_axis(name = nms[1], axisLabel = list(rotate = 45)) |>
      e_y_axis(name = nms[2]) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE)
  }) |>
    rlang::eval_tidy()

  if (any(names(d) == "colors"))
    e <- e_add_nested(e, "itemStyle", color = colors)
  e

}
