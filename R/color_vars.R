
parts <- unlist(strsplit(getwd(), .Platform$file.sep))
dir <- do.call(file.path, as.list(parts[1:length(parts) - 1])) |> paste0("/VIBRANT-99-utils/")

site_colors <- readr::read_csv(paste0(dir,"site_colors.csv"), show_col_types = FALSE)
site_colors <- site_colors$color |> magrittr::set_names(site_colors$site)

strain_origin_colors <- readr::read_csv(paste0(dir,"LBP_strain_origin_colors.csv"), show_col_types = FALSE)
strain_origin_colors <- strain_origin_colors$color |> magrittr::set_names(strain_origin_colors$strain_origin)

arm_colors <- readr::read_csv(paste0(dir,"arm_colors.csv"), show_col_types = FALSE)
arm_colors <- arm_colors$color |> magrittr::set_names(arm_colors$arm)


visit_colors <- readr::read_csv(paste0(dir,"visit_colors.csv"), show_col_types = FALSE)
visit_colors <- visit_colors$color |> magrittr::set_names(visit_colors$visit)

rm(parts, dir)
