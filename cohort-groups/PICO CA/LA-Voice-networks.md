library(networkD3)

## FOR 6/4 CHECK-IN
df <- read.csv("/Users/lizmckenna/Dropbox/apis/210420 LA Voice Data Updated shared 420.csv") # NEW VARIABLE: Core Leader

yesleader <- subset(df, df$Core.Leader == "Yes")

yesleader$TeamParticipation <- ifelse(yesleader$Teams == 0, "No", "Yes")


# Network chart of leaders to see their spaces and experiences

networkdata <- yesleader %>%
  count(yesleader$event_name, yesleader$Full.name, sort = TRUE)

#colnames(networkdata)[1] <- "destination" # originally source
#colnames(networkdata)[2] <- "source" # originally destination
#colnames(networkdata)[3] <- "count"
colnames(networkdata)[1] <- "source" # originally source
colnames(networkdata)[2] <- "destination" # originally destination
colnames(networkdata)[3] <- "count"

sources <- networkdata %>%
  distinct(source) %>%
  rename(label = source)

destinations <- networkdata %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by = "label")

nodes <- nodes %>% rowid_to_column("id")

per_route <- networkdata %>%
  group_by(source, destination) %>%
  summarise(weight = count) %>% # Changed from summarise(weight = n()) %>%
  ungroup()

edges <- per_route %>%
  left_join(nodes, by = c("source" = "label")) %>%
  rename(from = id)

edges <- edges %>%
  left_join(nodes, by = c("destination" = "label")) %>%
  rename(to = id)

edges <- select(edges, from, to, weight)

edges <- mutate(edges, width = weight/5 + 1)
#visNetwork(nodes, edges) %>%
#visIgraphLayout(layout = "layout_with_fr") %>%
#visEdges(arrows = "from")
nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

# networkD3 package in R
forceNetwork(Links = edges_d3,
             Nodes = nodes_d3,
             Source = "from",
             Target = "to",
             NodeID = "label",
             Group = "id",
             Value = "weight",
             opacity = 1,
             fontSize = 16,
             zoom = TRUE)
             
# How to interpret: How dense is the plot? Which nodes are at the center?
# Where are people scattered?

# Alternative visualization with different package

library(visNetwork)
visNetwork(nodes, edges)
