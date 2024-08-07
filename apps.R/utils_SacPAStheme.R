#' SacPAStheme
#'
#' @description A utils function setting adjusting the default shinydashboard theme
#'
#' @return The return value, will reflect the CBR website color theme
#'
#' @noRd

SacPAStheme<- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#45585E", #status = "primary"
    aqua = "#5E7880", #status = "info"
    green = "#c3d7e2", #status = "success"
    teal = "#c3d7e2",
    gray_lte = "#e0d6b0",
    blue = "#5E7880"
  ),
  fresh::adminlte_sidebar(
    width = "300px",
    dark_bg = "#F8F8F8",
    dark_color = "black",
    dark_hover_bg = "#c3d7e2", #none of the colors seem to do anything other than bg//switch to css option for more piecemeal customization
    dark_hover_color = "#e0d6b0",
    dark_submenu_bg = "#F8F8F8",
    dark_submenu_color = "black",
    dark_submenu_hover_color = "#c3d7e2",
    # light_bg = "#e0d6b0",
    # light_hover_bg = "#c3d7e2",
    # light_color = "black",
    # light_hover_color = "#e0d6b0",
    # light_submenu_bg = "#F8F8F8",
    # light_submenu_color = "black",
    # light_submenu_hover_color = "#c3d7e2"
  ),
  fresh::adminlte_global(
    content_bg = "#FFF",
    box_bg = "#F8F8F8",
    info_box_bg = "#c3d7e2"
  )
)

#c3d7e2
#e0d6b0
#bcb18b
#04224e

