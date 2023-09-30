fresh::create_theme(
  theme = "default",
  fresh::bs_vars_wells(
    bg = "#a1d99b",
    border = "#3f2d54"
  ),
  fresh::bs_vars_global(
    body_bg = "#e5f5e0", 
    text_color = "#3a1e04",
    link_color = "#7570b3",
    link_hover_color = "#fdae61"
    ),
  output_file = "www/freshtheme.css"
)

