
# Load libraries
library(ggtree)
library(ape)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtreeExtra)
library(ggnewscale)



# Load the tree file
tree <- read.tree("cgmlst.nhx")

# Load the annotation file
annotation_data <- read.csv("meta_data.csv")



# Merge tree tip labels with annotation data
tip_labels <- data.frame(id = tree$tip.label)
merged_data <- tip_labels %>%
  left_join(annotation_data, by = "id")

# Check the merged data
head(merged_data)


#Plot the tree
p <- ggtree(tree, size = 0.7, layout = "circular", branch.length = "none", open.angle = 10) + 
  geom_tiplab(size = 3, fontface = "bold", offset = 0.3)

# Add annotation data to the tree
p <- p %<+% merged_data


p <- p +
  geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = merged_data$Source),
    width = 1 ,  # Width of the Host strip
    offset = 0.7  # Distance from the tree
  ) +
  scale_fill_manual(
    name = "(1) Source",  # Legend title for Host
    values = c(
      "Human" = "#0000FF", 
      "Live animals" = "#00FF00", 
      "Live chicken" = "#FF0000",
      "Chicken meat" = "#800080", 
      "Isolates in this study" = "#FFA500"
    ), guide = guide_legend(order = 1) 
  )
# Add a new fill scale for collection_date
p <- p +
  new_scale_fill() +  # Reset the fill scale
  geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = Collection_date),
    width = 1 ,  # Width of the collection_date strip
    offset = 0.03  # Distance from the Host strip
  ) +
  scale_fill_manual(
    name = "(2) Collection date",  # Legend title for collection_date
    values = c(
      "2017-2018" = "#FFD700", 
      "2020-2021" = "#FF69B4", 
      "2022" = "#00CED1",
      "2023" = "#8A2BE2", 
      "2024" = "#FF4500"
    ),guide = guide_legend(order = 2) 
  )
# Add a new fill scale for MLST
p <- p +
  new_scale_fill() +  # Reset the fill scale
  geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = Location),
    width = 1 ,  # Width of the collection_date strip
    offset = 0.03  # Distance from the Host strip
  ) +
  scale_fill_manual(
    name = "(3) Location",  # Legend title for collection_date
    values = c(
      "KSA" = "blue", 
      "Qatar" = "red", 
      "UAE" = "green"
     
    ),guide = guide_legend(order = 3) 
  )

# Add a new fill scale for cgMLST
p <- p +
  new_scale_fill() +  # Reset the fill scale
  geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = cgMLST),
    width = 1,
    offset = 0.03  # Distance from the previous strip
  ) +  
  scale_fill_manual(
    name = "(4) cgMLST",  # Legend title
    values = c(
      "2390" = "#1f77b4",
      "2396" = "#ff7f0e",
      "2373" = "#2ca02c",
      "2321" = "#d62728",
      "2394" = "#9467bd",
      "2389" = "#8c564b",
      "2399" = "#e377c2",
      "2402" = "#7f7f7f",
      "2403" = "#bcbd22",
      "2401" = "#17becf",
      "2397" = "#aec7e8",
      "2395" = "#ffbb78",
      "2400" = "#98df8a",
      "2393" = "#ff9896",
      "2341" = "#c5b0d5",
      "2364" = "#c49c94",
      "2196" = "#f7b6d2",
      "2310" = "#dbdb8d",
      "2315" = "#9edae5",
      "2312" = "#393b79",
      "2361" = "#637939",
      "2329" = "#8c6d31",
      "2343" = "#843c39",
      "2347" = "#7b4173",
      "2350" = "#5254a3",
      "2223" = "#6b6ecf",
      "2300" = "#9c9ede",
      "2330" = "#b5cf6b",
      "2088" = "#cedb9c",
      "2265" = "#8ca252",
      "Others" = "#cccccc"  # Light gray for "Others"
    ),
    guide = guide_legend(order = 4, ncol = 3)  # Adjust ncol as needed
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10)
    )# Smaller text for many items

print(p)


# Save as PNG
ggsave("core_genome_tree_.png", p, width = 12, height = 8, dpi = 600)

# Save as PDF
ggsave("annotated_tree.pdf", p, width = 12, height = 8)