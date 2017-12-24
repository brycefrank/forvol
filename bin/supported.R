# Creates supported equations markdown tables for the wiki
library(knitr)
library(devtools)
library(dplyr)

load_all("../../forvol")

a <- test_config_all_sp(gather_frame = FALSE)


text_file <- ""

for (col in 2:ncol(a)) {
  species_df <- a[c(1,col)]
  species <- filter(species_df, species_df[[2]] == FALSE)
  species$spcd <- as.numeric(species$spcd)
  species <- sort(species$spcd)

  # Convert comma separated character vector
  species <- paste(as.character(species), sep="' '", collapse =", ")

  # Trim the sides
  print(species)
  species <- sub("c[(]", "", species)
  species <- sub("[)]", "", species)



  # Convert to mark down character vector
  md <- species

  # Create tags for line 1
  text_sub <- append(md, sprintf("<details><summary>%s</summary><p>", names(species_df)[2]), 0)

  # And tags for the end
  text_sub <- append(text_sub, "</p></details>")

  # Then add to text_file
  text_file <- append(text_file, text_sub)

}


write(text_file, "../../forvol.wiki/test_table2.md")
