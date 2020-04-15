# Google trends
source("setup.R")

# Function for simplicity
visualise_googletrends_csv <- function(csv_file,
                                       search_term,
                                       location = "Worldwide") {
  read.csv(csv_file, 
           stringsAsFactors = FALSE, 
           skip=2) %>%
  rename("Week"=1, 
         "Index"=2) %>%
  mutate(Index = as.numeric(Index),
         Week = ymd(Week)) %>%
  replace_na(list(Index=0)) %>%
    ggplot() +
    geom_line(aes(x = Week, y = Index), 
              color = "blue") +
    labs(x = "Date", y = "Google Search Popularity (Index)") +
    ggtitle(label = toupper(search_term),
            subtitle = paste0(location, " Google searches for '", search_term, "' over the last 12 months")) +
    theme_hc()
}

# The virus -----
# Searches for coronavirus
cv <- visualise_googletrends_csv("google_trends_data/coronavirus-worldwide-year.csv",
                                 "coronavirus")
ggsave("gt_plots/gt-worldwide-coronavirus.png", 
       plot = cv,
       width = 8, height = 4)

# Panic buying -----
# Searches for 'toilet paper'
tp <- visualise_googletrends_csv("google_trends_data/toiletpaper-worldwide-year.csv",
                                 "toilet paper")
ggsave("gt_plots/gt-worldwide-toiletpaper.png",
       plot = tp,
       width = 8, height = 4)

# 'pasta'
pasta <- visualise_googletrends_csv("google_trends_data/pasta-worldwide-year.csv",
                                    "pasta")
ggsave("gt_plots/gt-worldwide-pasta.png",
       plot = pasta,
       width = 8, height = 4)

# 'bread'
bread <- visualise_googletrends_csv("google_trends_data/bread-worldwide-year.csv",
                                    "bread")
ggsave("gt_plots/gt-worldwide-bread.png",
       plot = bread,
       width = 8, height = 4)

pasta_bread <- gridExtra::grid.arrange(pasta, bread, ncol = 2)
ggsave("gt_plots/pasta_bread.png", 
       plot = pasta_bread,
       width = 12, height = 4)

# 'delivery'
del <- visualise_googletrends_csv("google_trends_data/delivery-worldwide-year.csv",
                                  "delivery")
ggsave("gt_plots/gt-worldwide-delivery.png",
       plot = del,
       width = 8, height = 4)