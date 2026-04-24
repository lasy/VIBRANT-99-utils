
parts <- unlist(strsplit(getwd(), .Platform$file.sep))
dir <- do.call(file.path, as.list(parts[1:length(parts) - 1])) |> paste0("/VIBRANT-99-utils/")

VIRGO_dict <- readr::read_csv(paste0(dir,"VIRGO2_taxaKey_v2_tmp.csv"), show_col_types = FALSE)
rm(parts, dir)

VIRGO_dict <- VIRGO_dict |> janitor::clean_names()
VIRGO_dict <-
  VIRGO_dict |>
  dplyr::mutate(
    regex_pattern =
      dplyr::case_when(
        taxa == "Lactobacillus_mulieris" ~ "[Ll]acto.mulieris",
        taxa == "Lactobactillus_other" ~ "[Ll]acto.*[Oo]ther|[Oo]ther.*[Ll]acto",
        taxa == "UBA629_sp005465875" ~ "UBA629.sp005465875|BVAB1",
        taxa == "other" ~ "[Oo]ther",
        taxa == "other_non_Lacto" ~ "[Oo]ther.*[Nn]on.*[Ll]acto|[Nn]on.*[Ll]acto.*[Oo]ther",
        TRUE ~ taxa |>
          stringr::str_replace("_",".") |>
          stringr::str_remove("Lactobacillus") |>
          stringr::str_replace("Gardnerella", "ardnerella") |>
          stringr::str_replace("Prevotella", "revotella") |>
          stringr::str_replace("Aerococcus", "erococcus") |>
          stringr::str_replace("Fannyhessea", "annyhessea") |>
          stringr::str_replace("Berryella", "erryella")
      )
  ) |> 
  dplyr::mutate(
    label = case_when(
      text == "Ca. L. vaginae (BVAB1)" ~ "*Ca.* L. vaginae (BVAB1)",
      text == "other Lacto." ~ "Other *Lacto.*",
      text == "Other" ~ "Other",
      text == "Other non-Lacto." ~ "Other non-*Lacto.*",
      text == "P. sp000758925" ~ "*P.* sp000758925",
      text == "P. spNov33" ~ "*P.* spNov33",
      text == "N. sp004136275" ~ "*N.* sp004136275",
      text == "D. sp001553355" ~ "*D.* sp001553355",
      text == "B. sp001552935" ~ "*B.* sp001552935",
      text == "G. sp003585845" ~ "*G.* sp003585845",
      TRUE ~ 
        text |> str_replace(
          "(^|\\s)([A-Z])\\.\\s+([a-z][a-z0-9_-]*)(\\b)",
          "\\1*\\2.* *\\3*\\4"
        ) |>
        str_replace(
          "^([A-Z][a-z]{2,})$",
          "*\\1*"
        )
    )
  )


additional_taxa_dict <- 
  bind_rows(
    tibble(taxa = "Sneathia_sanguinegens", color = "#9070ff"),
    tibble(taxa = "Sneathia", color = "#5563ff"),
    tibble(taxa = "Nanoperiomorbus", color = "#503110"),
    tibble(taxa = "Megasphaera", color = "#7675bb"),
    tibble(taxa = "Streptococcus", color = "#eeaadd"),
    tibble(taxa = "Berryella", color = "#ee00c9"),
    tibble(taxa = "Dialister", color = "#035040"),
    tibble(taxa = "Veillonella_montpellierensis", color = "deepskyblue4"),
    tibble(taxa = "Veillonella", color = "deepskyblue3"),
    tibble(taxa = "Aerococcus", color = "#6B8019"),
    tibble(taxa = "Peptostreptococcus_anaerobius", color = "salmon2"),
    tibble(taxa = "Peptostreptococcus", color = "salmon3"),
    tibble(taxa = "Metamycoplasma_hominis", color = "red")
  ) |> 
  mutate(
    regex_pattern = taxa |> str_replace_all("_", "\\."),
    label = str_c("*", taxa, "*") |> str_replace_all("Sneathia_", "S. ")
  )

extended_taxa_dict <- 
  bind_rows(
    VIRGO_dict |> select(taxa, regex_pattern, color, label),
    additional_taxa_dict
  ) |> 
  mutate(
    color = 
      case_when(
        taxa == "Aerococcus_christensenii" ~ "#7B9029",
        TRUE ~ color
      )
  )


match_taxa_to_dict <- function(taxa, dict = extended_taxa_dict){
  if (is.null(taxa)) return(NULL)
  
  matched_taxa <-
    tibble(
      original_taxa = taxa
    ) |>
    mutate(
      taxa =
        case_when(
          str_detect(original_taxa, "[Ll]acto") &
            str_detect(original_taxa, "[Oo]ther") &
            str_detect(original_taxa, "[Nn]on") ~ "other_non_Lacto",
          
          original_taxa == "< 50% Lactobacillus" ~ "other_non_Lacto",
          
          str_detect(original_taxa, "[Ll]acto") &
            str_detect(original_taxa, "[Oo]ther") ~ "Lactobactillus_other",
          
          str_detect(original_taxa, "other spp") ~ original_taxa |> str_remove(" \\(.*\\)"),
          
          str_detect(original_taxa, "[Oo]ther") ~ "other",
          
          str_detect(original_taxa, "crispatus") ~ "Lactobacillus_crispatus",
          
          original_taxa == "LBP" ~ "C0175A1",
          str_detect(original_taxa, "LBP") ~ original_taxa |> str_remove("LBP") |> str_remove("strain") |> str_remove_all("\\(|\\)") |> str_remove_all(" "),
          str_detect(original_taxa, "iner") ~ "Lactobacillus_iners",
          str_detect(original_taxa, "BVAB1") | str_detect(original_taxa, "Ca.*L.*vaginae") | str_detect(original_taxa, "UBA629_sp005465875") ~ "UBA629_sp005465875",
          TRUE ~ original_taxa |> str_replace_all(" ", "_")
        )
    ) |>
    dplyr::left_join(dict |> dplyr::select(taxa) |> mutate(match = TRUE), by = join_by(taxa)) |>
    mutate(match = match |> replace_na(FALSE))
  
  if (any(!matched_taxa$match)) {
    warning(
      stringr::str_c(
        "The following taxa were not matched to the VIRGO dictionary: ",
        paste(matched_taxa$original_taxa[!matched_taxa$match], collapse = ", ")
      )
    )
  }
  
  matched_taxa |> pull(taxa)
}


match_taxa_to_VIRGO_dict <- function(taxa){
  if (is.null(taxa)) return(NULL)
  
  matched_taxa <-
    tibble(
      original_taxa = taxa
    ) |>
    mutate(
      taxa =
        case_when(
          str_detect(original_taxa, "[Ll]acto") &
            str_detect(original_taxa, "[Oo]ther") &
            str_detect(original_taxa, "[Nn]on") ~ "other_non_Lacto",
          
          original_taxa == "< 50% Lactobacillus" ~ "other_non_Lacto",
          
          str_detect(original_taxa, "[Ll]acto") &
            str_detect(original_taxa, "[Oo]ther") ~ "Lactobactillus_other",
          
          str_detect(original_taxa, "other spp") ~ original_taxa |> str_remove(" \\(.*\\)"),
          
          str_detect(original_taxa, "[Oo]ther") ~ "other",
          
          str_detect(original_taxa, "crispatus") ~ "Lactobacillus_crispatus",
          
          original_taxa == "LBP" ~ "C0175A1",
          str_detect(original_taxa, "LBP") ~ original_taxa |> str_remove("LBP") |> str_remove("strain") |> str_remove_all("\\(|\\)") |> str_remove_all(" "),
          str_detect(original_taxa, "iner") ~ "Lactobacillus_iners",
          str_detect(original_taxa, "BVAB1") | str_detect(original_taxa, "Ca.*L.*vaginae") | str_detect(original_taxa, "UBA629_sp005465875") ~ "UBA629_sp005465875",
          TRUE ~ original_taxa |> str_replace_all(" ", "_")
        )
    ) |>
    dplyr::left_join(VIRGO_dict |> dplyr::select(taxa) |> mutate(match = TRUE), by = join_by(taxa)) |>
    mutate(match = match |> replace_na(FALSE))
  
  if (any(!matched_taxa$match)) {
    warning(
      stringr::str_c(
        "The following taxa were not matched to the VIRGO dictionary: ",
        paste(matched_taxa$original_taxa[!matched_taxa$match], collapse = ", ")
      )
    )
  }
  
  matched_taxa |> pull(taxa)
}

get_taxa_colors <- function(taxa){

  tibble(
    original_taxa = taxa,
    taxa = match_taxa_to_dict(taxa)
  ) |>
    dplyr::left_join(extended_taxa_dict |> dplyr::select(taxa, color), by = "taxa") |>
    group_by(is.na(color)) |> 
    mutate(
      color = # color |> replace_na("black")
        case_when(
          !is.na(color) ~ color,
          is.na(color) ~ hsv(h = row_number()/n(), s = 0.8, v = 0.5)
        )
      ) |>
    ungroup() |> 
    pull(color)

}



get_taxa_labels <- function(taxa){

  tibble(
    original_taxa = taxa,
    taxa = match_taxa_to_dict(taxa)
  ) |>
    left_join(extended_taxa_dict |> dplyr::select(taxa, label), by = "taxa") |>
    mutate(label = case_when(is.na(label) ~ original_taxa |> str_replace("^([^()]+?)(\\s*\\(.*\\))?$", "*\\1*\\2"), TRUE ~ label)) |>
    pull(label)

}



get_taxa_colors_v1 <- function(taxa) {
  tibble(
    taxa = taxa
  ) |>
    mutate(
      cat =
        case_when(
          str_detect(taxa, "[Ll]acto") & str_detect(taxa, "[Oo]ther") & str_detect(taxa, "[Nn]on") ~ "Other non-Lacto",
          str_detect(taxa, "[Ll]acto") & str_detect(taxa, "[Oo]ther") ~ "Other Lacto",
          taxa == "Other" ~ "Other",
          str_detect(taxa, "Other") ~ "Other",
          str_detect(taxa, "crispatus") ~ "crispatus",
          str_detect(taxa, "LBP") ~ "LBP",
          str_detect(taxa, "iner") ~ "iners",
          str_detect(taxa, "revotella") ~ "prevotella",
          str_detect(taxa, "ardnerella") ~ "gardnerella",
          TRUE ~ "non lacto"
        )
    ) |>
    group_by(cat) |>
    mutate(
      color =
        case_when(
          cat == "Other" ~ "gray80",
          cat == "Other non-Lacto" ~ "gray80", # "lightblue3"
          cat == "Other Lacto" ~ "gold3",
          str_detect(taxa, "crispatus") ~ "#FDCC45",

          # "#ff3c00"
          # "#631700"
          # "#771c00"

          str_detect(taxa, "C0022A1") ~ "#F7931E", # LC106-US
          str_detect(taxa, "C0059E1") ~ "#F6881F", # LC106-US
          str_detect(taxa, "C0175A1") ~ "#F57C20", # LC106-US
          str_detect(taxa, "FF00018") ~ "#F37122", #LC106-SA
          str_detect(taxa, "FF00051") ~ "#F26523", #LC106-SA
          str_detect(taxa, "UC101"  ) ~ "#F15A24",  #LC106-SA

          str_detect(taxa, "C0006A1") ~ "#D63200", #LC115-US
          str_detect(taxa, "C0028A1") ~ "#C83002", #LC115-US
          str_detect(taxa, "C0112A1") ~ "#B92E03", #LC115-US
          str_detect(taxa, "FF00004") ~ "#AB2B05", #LC115-SA
          str_detect(taxa, "FF00064") ~ "#9D2906", #LC115-SA
          str_detect(taxa, "FF00072") ~ "#8E2708", #LC115-SA
          str_detect(taxa, "UC119"  ) ~ "#802509", #LC115-SA
          str_detect(taxa, "122010" ) ~ "#71220B", #LC115-SA
          str_detect(taxa, "185329") ~ "#63200C", #LC115-SA

          str_detect(taxa, "LBP") ~ "#F6881F",
          str_detect(taxa, "iner") ~ "green3",
          str_detect(taxa, "jensenii") ~ "green4",
          str_detect(taxa, "BVAB1") ~ "orchid2",
          str_detect(taxa, "ardnerella") ~ colorRampPalette(c("dodgerblue1", "dodgerblue4"))(n()),
          str_detect(taxa, "revotella") ~ colorRampPalette(c("purple1", "purple4"))(n()),
          TRUE ~ colorRampPalette(c("turquoise2", "black"))(n())
        )
    ) |>
    pull(color)
}



get_taxa_colors_deprecated <- function(taxa) {
  tibble(
    taxa = taxa
  ) |>
    mutate(
      cat =
        case_when(
          taxa == "Other" ~ "Other",
          str_detect(taxa, "crispatus") ~ "crispatus",
          str_detect(taxa, "LBP") ~ "LBP",
          str_detect(taxa, "iner") ~ "iners",
          str_detect(taxa, "revotella") ~ "prevotella",
          str_detect(taxa, "ardnerella") ~ "gardnerella",
          TRUE ~ "non lacto"
        )
    ) |>
    group_by(cat) |>
    mutate(
      color =
        case_when(
          taxa == "Other" ~ "gray80",
          str_detect(taxa, "crispatus") ~ "orange",
          str_detect(taxa, "LBP") ~ colorRampPalette(c("orangered1", "red4"))(n()),
          str_detect(taxa, "iner") ~ "green3",
          str_detect(taxa, "jensenii") ~ "green4",
          str_detect(taxa, "ardnerella") ~ colorRampPalette(c("dodgerblue1", "dodgerblue4"))(n()),
          str_detect(taxa, "revotella") ~ colorRampPalette(c("purple1", "purple4"))(n()),
          TRUE ~ colorRampPalette(c("turquoise3", "black"))(n())
        )
    ) |>
    pull(color)
}




