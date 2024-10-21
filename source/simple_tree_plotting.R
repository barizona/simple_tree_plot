library(tidyverse)
library(magrittr) # %<>%
library(ape) # read.tree
library(treeio) # as_tibble, as.treedata
library(ggtree) # ggtree


# AIM: Plot a phylogenetic tree and colour the tip branches

#xxxxxxxxxxxxxxxxx
# Input data --------------------------------------------------------------
#xxxxxxxxxxxxxxxxx

# read the newick tree to phy object
tree <- read.tree("input/ST167.nwk")

# convert the phy object to tibble
tree_tibble <- as_tibble(tree)

# read the data tab for the tree to plot
data_tab <- read_tsv("input/ST167_isolation_sources_clean.tsv")

# join the tree tibble with the data tab
tree_tibble %<>%
  left_join(., data_tab)

#xxxxxxxxxxxxxxxxx
# Plotting --------------------------------------------------------------
#xxxxxxxxxxxxxxxxx

# 15 colours + NA gray
# colours <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(15)
# from: https://mokole.com/palette.html
colours <- c("#2f4f4f",
             "#a0522d",
             "#006400",
             "#000080",
             "#ff0000",
             "#ffa500",
             "#ffff00",
             "#00ff00",
             "#00fa9a",
             "#00bfff",
             "#0000ff",
             "#ff00ff",
             "#eee8aa",
             "#dda0dd",
             "#ff1493")

# assign colours to Isolation sources
names(colours) <- pull(tree_tibble, Isolation_source_clean) %>%
  unlist() %>%
  unique() %>%
  # remove NA
  discard(is.na) %>%
  # oreder alphabetically
  sort()

#xxxxxxxxxx
## Plot single tree --------
#xxxxxxxxxx
tree_tibble %>%
  treeio::as.treedata() %>%
  # other layouts: https://yulab-smu.top/treedata-book/chapter4.html
  ggtree(aes(color = Isolation_source_clean), layout = "circular", size = 0.2) +
  # add tip labels
  geom_tiplab(size = 2) +
  scale_color_manual(values = colours, na.value = "gray",
                     name = "Isolation source") +
  # add title
  ggtitle("Tree") +
  # increase legend linewidth
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  # decrease legend size
  theme(legend.key.size = unit(0.1, "cm"))

ggsave("output/ST167_tree_plot.pdf", width = 20, height = 20, units = "cm")
