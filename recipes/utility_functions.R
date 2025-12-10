#' @title Format Ingredient Table
#'@description Standardize the ingredient table for each recipe. 
#' @param ing_list Named list of ingredients required for the recipe
#' @return GT table

make_ingredient_tab <- function(ing_list) {
  ing_df <- enframe(ing_list, name = " ", value = "Quantity") |> 
    gt() |> 
    cols_align(align = "left", columns = everything()) |> 
    # making the text in the whole table large
    tab_style(
      style = cell_text(size = "medium"), locations = cells_body()
    ) |>
    # making the column label bold
    tab_style(
      style = cell_text(weight = "bold"), locations = cells_column_labels()
    ) |> 
    opt_stylize(style = 1, color = "gray", add_row_striping = TRUE) |> 
    tab_options(table.align = "left") 
  
  return(ing_df)
}