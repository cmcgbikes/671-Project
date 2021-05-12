# ===========================     Final Project     ============================
# =========================     Catherine McGowan     ==========================

# install patentsview package
# install.packages("patentsview")

# I used this dev install because the cran version seems to fail at times
# if (!"devtools" %in% rownames(installed.packages())) 
#  install.packages("devtools")

# devtools::install_github("ropensci/patentsview")


library(patentsview)
library(dplyr)
library(DT)
library(knitr)
library(stringr)
library(statnet)
library(ggplot2)
library(tidyr)


# Write query for "geospatial" and "geolocation"
query <- with_qfuns(
  or(
    text_phrase(patent_title = "geospatial"),
    text_phrase(patent_abstract = "geospatial"),
    text_phrase(patent_abstract = "geolocation")
  )
)


query <- with_qfuns(
  or(
    text_phrase(patent_title = "geospatial"),
    text_phrase(patent_abstract = "geospatial")
  )
)

query

# Create a list of the fields we'll need for the analysis
fields <- c(
  "patent_number",
  "patent_title",
  "assignee_organization",
  "patent_num_cited_by_us_patents",
  "app_date",
  "patent_date",
  "assignee_total_num_patents",
  "cited_patent_number", # Which patents do these patents cite?
  "citedby_patent_number", # Which patents cite them?
  "cpc_category", # Top Level Cooperative Patent Classification (CPC) category (http://www.cooperativepatentclassification.org/index.html)
  "cpc_subsection_id",
  "cpc_subsection_title",
  "cpc_group_id"
)




# Send an HTTP request to the PatentsView API to get the data
pv_out <- search_pv(query, fields = fields, all_pages = TRUE)

# Unnest the data frames that are stored in the assignees data frame
pvresults <- unnest_pv_data(data = pv_out$data, pk = "patent_number")
pvresults



# ==============================================================================


# Save the unnested pvresults to dfs
output_patents <- pvresults$patents
output_assignees <- pvresults$assignees
output_cited_patents <- pvresults$cited_patents
output_citedby_patents <- pvresults$citedby_patents
output_cpcs <- pvresults$cpcs
patents<- output_patents
assignees<- output_assignees
cited_patents <- output_cited_patents
citedby_patents <- output_citedby_patents
cpcs <- output_cpcs
cpcs_sub_id <- cpcs$cpc_subsection_id


cpcs
cpcs_sub_id
patents
cited_patents
citedby_patents
assignees


# Write these dfs to csv files so I don't have to re-pull data...
write.csv(x=cpcs, file="patents_cpcs_df.csv")
write.csv(x=cpcs_sub_id, file="patents_cpcs_sub_id_df.csv")
write.csv(x=patents, file="patents_df.csv")
write.csv(x=cited_patents, file="cited_patents_df.csv")
write.csv(x=citedby_patents, file="citedby_patents_df.csv")
write.csv(x=assignees, file="assignees_df.csv")



# Make tables for patents and cpcs
patent_tbl <- as_tibble(patents)
patent_tbl

citedby_patents_tbl <- as_tibble(citedby_patents)
citedby_patents_tbl

cpcs_tbl <- as_tibble(cpcs)
cpcs_tbl

cpcs_clean_tbl <- select(cpcs_tbl, -cpc_group_id, -cpc_category)
cpcs_clean_tbl <- distinct(cpcs_clean_tbl)
cpcs_clean_tbl


# ==============================================================================
# ==============================================================================


# THESE NEXT FEW SECTIONS CREATE BAR CHARTS TO AID IN IDENTIFYING THE FINAL 
# NETWORKS TO VISUALIZE AND LATER CONDUCT ANALYSIS


# This is to find the top cited patents overall within all geospatial patents
citedby_patents_tbl <- na.omit(citedby_patents_tbl)

top_cited_counts <- count(citedby_patents_tbl, patent_number, sort=TRUE)
top_cited_counts

top_cited<- top_cited_counts[1:10,]
top_cited

# Reorder the patents for plotting (based on n rather than alphabetical)
top_cited$citedby_patent_number <- reorder(top_cited$patent_number, top_cited$n)
top_cited


top_patent_overall_bar <- ggplot(top_cited, aes(x = n, y = citedby_patent_number)) + 
  labs(title = "Top Cited Geospatial Patents", x="Citation Count", y="Patent Number") +
  geom_col(fill="purple") + 
  geom_text(aes(label=n), hjust = -.1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

top_patent_overall_bar


# ==============================================================================


# This section creates a table of the top CPC subsections with the most patents


# Get the top 10 cpc categories with the most patents assigned to it
cpc_counts <- count(cpcs_clean_tbl, cpc_subsection_id, sort=TRUE)
cpc_counts

top_cpcs <- cpc_counts[1:10,]
top_cpcs

# Reorder the cpcs for plotting (based on n rather than alphabetical)
top_cpcs$cpc_subsection_id <- reorder(top_cpcs$cpc_subsection_id, top_cpcs$n)
top_cpcs

top_cpc_bar <- ggplot(top_cpcs, aes(x = n, y = cpc_subsection_id)) + 
  labs(title = "Top CPC Subsections: Total Assigned Patent Counts", x="Patent Count", y="CPC Subsection ID") +
  geom_col(fill="sky blue") + 
  geom_text(aes(label=n), hjust = -.1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

top_cpc_bar


# ==============================================================================


# Create a list of the top CPC category ids to filter full list of patents down
# to just the list of patents that are assigned to the top CPCs

cpcs_clean_tbl

cpcs_clean_tbl_df <- na.omit(cpcs_clean_tbl)
summary(cpcs_clean_tbl_df)


# top cpc ids to use...
# "G06", "H04", "G01", "G08", "G09", "Y02", "G05", "Y10", "B60", "G07"

# Filter list of patents to just the top cpc ids
top_cpcs_ids <- c("G06", "H04", "G01", "G08", "G09", "Y02", "G05", "Y10", "B60", "G07")
clean_left_df_top_cpcs <- filter(cpcs_clean_tbl, cpc_subsection_id %in% top_cpcs_ids)
clean_left_df_top_cpcs


# ==============================================================================


# In the top CPC subsection (which is G06) find the citation counts for each 
# patent in G06

citedby_leftJoinDf <- left_join(clean_left_df_top_cpcs,citedby_patents_tbl,by="patent_number")
citedby_leftJoinDf

citedby_leftJoinDf <- na.omit(citedby_leftJoinDf)
citedby_leftJoinDf

# Filter down to ONLY CPC subsection G06
citedby_leftJoinDf <- filter(citedby_leftJoinDf, cpc_subsection_id=="G06")
citedby_leftJoinDf

# Get the citation counts for patents ONLY in CPC subsection G06
citation_cpc_counts <- count(citedby_leftJoinDf, patent_number, sort=TRUE)
citation_cpc_counts

top_citation_cpc <- citation_cpc_counts[1:10,]
top_citation_cpc

# Reorder the cpcs for plotting (based on n rather than alphabetical)
top_citation_cpc$citedby_patent_number <- reorder(top_citation_cpc$patent_number, top_citation_cpc$n)
top_citation_cpc

top_patent_cpc_bar <- ggplot(top_citation_cpc, aes(x = n, y = citedby_patent_number)) + 
  labs(title = "CPC Subsection G06: Top Cited Patents", x="Citation Count", y="Patent Number") +
  geom_col(fill="orange") + 
  geom_text(aes(label=n), hjust = -.1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

top_patent_cpc_bar



# ==============================================================================
# ==============================================================================


# THESE NEXT FEW SECTIONS CREATE TABLES TO SUMMARIZE THE DESCRIPTIONS OF SOME OF
# THE KEY FEATURES OF THE GEOSPATIAL PATENT CITATION NETWORK


# THIS SECTION IS TO MAKE A DATA TABLE SUMMARZING PATENT TOTALS BY ORG


# Merge patents and assignees
assignees_full <- left_join(patents, assignees, by="patent_number")
assignees_full

# Merge assignees_full and citedby_patents_tbl
assignees_full <- left_join(assignees_full, citedby_patents_tbl, by="patent_number")
assignees_full

# Select patent number, title, assignee org, and assignee total patents
assignee_fullDf <- select(assignees_full, patent_number, patent_title, assignee_organization, assignee_total_num_patents)
assignee_fullDf

# Drop duplicate rows of patent numbers
assignee_fullDf <- distinct(assignee_fullDf)
assignee_fullDf

# Get the total geospatial patent counts for each assignee organization
assignee_counts <- count(assignee_fullDf, assignee_organization, sort=TRUE)
assignee_counts

# Drop na's
assignee_counts <- na.omit(assignee_counts)

# Reorder the assignees for plotting (based on n rather than alphabetical)
assignee_counts$assignee_organization<- reorder(assignee_counts$assignee_organization, assignee_counts$n)
assignee_counts


# Merge the assignee counts column into the assignees df
assignee_fullDf <- left_join(assignees_full,assignee_counts,by="assignee_organization")
assignee_fullDf

assignee_fullDf <- select(assignee_fullDf, assignee_organization, n, assignee_total_num_patents)
assignee_fullDf

assignee_fullDf <- na.omit(assignee_fullDf)
assignee_fullDf <- distinct(assignee_fullDf)
assignee_fullDf

# Reorder the assignees for plotting (based on n rather than alphabetical)
assignee_fullDf <- arrange(assignee_fullDf, desc(n))
assignee_fullDf



# Create datatable
patents_by_org_table <- datatable(
  data = assignee_fullDf,
  rownames = FALSE,
  colnames = c(
    "Assignee", "Geo patents","Total patents"),
  caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: center; font-style: italic;',
    "Geospatial Patent Counts by Assignee"
  ),
  options = list(pageLength = 10)
)

patents_by_org_table


# ==============================================================================


# THIS SECTION IS TO MAKE A DATA TABLE SUMMARZING PATENT CITATION COUNTS BY
# PATENT NUMBER WITH ORG ASSIGNEE NAME
# THIS TABLE SHOULD BE PATENT NUM, TITLE, ORG, CITATION TOTALS


# Count total citations by patent
assignee_citation_counts <- count(assignees_full, patent_number, sort=TRUE)
assignee_citation_counts

# Drop na's
assignee_citation_counts <- na.omit(assignee_citation_counts)


# Merge the assignee_citation_counts column into assignees_full
topcited_assignee_fullDf <- left_join(assignee_citation_counts, assignees_full, by="patent_number")
topcited_assignee_fullDf


# Select the final columns patent num, title, assignee, and total citation count
topcited_fullDf <- select(topcited_assignee_fullDf, patent_number, patent_title, assignee_organization, n)
topcited_fullDf

# Remove na's & drop dups
topcited_fullDf <- na.omit(topcited_fullDf)
topcited_fullDf <- distinct(topcited_fullDf)
topcited_fullDf


# Reorder the assignees for plotting (based on n rather than alphabetical)
topcited_fullDf <- arrange(topcited_fullDf, desc(n))
topcited_fullDf


# Create datatable
patent_citation_counts_table <- datatable(
  data = topcited_fullDf,
  rownames = FALSE,
  colnames = c(
    "Patent Number", "Patent Title", "Assignee","Total Citations"),
  caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: center; font-style: italic;',
    "Geospatial Patent Citation Counts"
  ),
  options = list(pageLength = 10)
)

patent_citation_counts_table


# ==============================================================================


# THIS SECTION IS TO MAKE A DATA TABLE SUMMARZING TOP PATENTS BY ORG IN THE TOP
# CPC SUBSECTION (G06); ADD IN THE CPC GROUP ID TO SEE WHERE THESE LAND WITHIN
# THE CPC SUBSECTION SCHEME (PULL FROM THE CPCS_TBL)
# THIS TABLE SHOULD BE PATENT NUM, CPC GROUP ID, TITLE, ORG, CITATION TOTALS
# NEED TO MERGE TOP_CITATION_CPC WITH ASSIGNEE AND PATENTS...


# Merge top cited patents counts in CPC subsection G06 with assignee org
topcited_cpcDf <- left_join(top_citation_cpc,assignees,by="patent_number")
topcited_cpcDf

# Merge the patent title column into the top cited df
topcited_cpcDf <- left_join(topcited_cpcDf,patents,by="patent_number")
topcited_cpcDf

# Merge the cpc subsection id column from cpcs_tbl into the top cited df
topcited_cpcDf <- left_join(topcited_cpcDf, cpcs_tbl,by="patent_number")
topcited_cpcDf

# Select the final columns patent num, title, assignee, and total citation count
topcited_cpcDf <- select(topcited_cpcDf, patent_number, cpc_group_id, patent_title, assignee_organization, n)
topcited_cpcDf

# Drop the dups
topcited_cpcDf <- distinct(topcited_cpcDf)
topcited_cpcDf

# Collapse cpc_group_id multiple rows into list by patent in one column
topcited_cpcDf_collapsed <- topcited_cpcDf %>% group_by(patent_number) %>% summarise(cpc_group_id = paste(cpc_group_id, collapse=", "))
topcited_cpcDf_collapsed

# Merge the collapsed cpc subsection id column into the top cited df
topcited_cpcDf <- left_join(topcited_cpcDf, topcited_cpcDf_collapsed,by="patent_number")
topcited_cpcDf

# Drop the original cpc_group_id column
# Select the final columns patent num, title, assignee, and total citation count
topcited_cpcDf <- select(topcited_cpcDf, patent_number, cpc_group_id.y, patent_title, assignee_organization, n)
topcited_cpcDf

# Drop the dups
topcited_cpcDf <- distinct(topcited_cpcDf)
topcited_cpcDf

# Reorder the assignees for plotting (based on n rather than alphabetical)
topcited_cpcDf <- arrange(topcited_cpcDf, desc(n))
topcited_cpcDf



# Create datatable
top_patent_cpc_table <- datatable(
  data = topcited_cpcDf,
  rownames = FALSE,
  colnames = c(
    "Patent Number", "CPC Group ID", "Patent Title", "Assignee","Total Citations"),
  caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: center; font-style: italic;',
    "Top 10 Most Cited Geospatial Patents in G06 CPC Subsection"
  ),
  options = list(pageLength = 10)
)

top_patent_cpc_table


# ==============================================================================


# MAKE A TABLE THAT LISTS THE PATENTS THAT CITE THE TOP MOST CITED PATENT BY
# PALANTIR, INCLUDE ASSIGNEE ORG NAME TO SEE WHICH COMPANIES ARE INFLUENCED.
# ARE ANY OF THESE ORGS BIG NAMES?

# Top most cited patent with it's citations; take a look to see what to join
clean_top_most_cited_patent

# Isolate the list of patent numbers that cite the top most cited patent
top_patent_citations <- select(clean_top_most_cited_patent, to)
top_patent_citations

# Rename the "to" column to patent_number to pull in assignee org name
top_patent_citations <- rename_all(top_patent_citations, recode, "to" = "patent_number")
top_patent_citations

# Join with assignees_full
top_patent_citations <- left_join(top_patent_citations, assignees, by="patent_number")
top_patent_citations

# Select patent_number and assignee_org columns
top_patent_citations <- select(top_patent_citations, patent_number, assignee_organization)
top_patent_citations

# Reorder the assignees for plotting (based on n rather than alphabetical)
top_patent_citations <- arrange(top_patent_citations, desc(assignee_organization))
top_patent_citations

# Drop na's >> they are assigned to individuals, just looking at the companies
top_patent_citations <- na.omit(top_patent_citations)
top_patent_citations

# Join with patents for patent title
top_patent_citations <- left_join(top_patent_citations, patents, by="patent_number")
top_patent_citations

# Select patent_number, patent_title and assignee_org columns
top_patent_citations <- select(top_patent_citations, patent_number, patent_title, assignee_organization)
top_patent_citations




# Create datatable
orgs_cite_top_cited_patent <- datatable(
  data = top_patent_citations,
  rownames = FALSE,
  colnames = c(
    "Patent Number", "Patent Title", "Assignee Organization"),
  caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: center; font-style: italic;',
    "Assignee Organizations That Cite Palantir Patent #8,799,799"
  ),
  options = list(pageLength = 10)
)

orgs_cite_top_cited_patent


# ==============================================================================
# ==============================================================================


# THIS NEXT SECTION PLOTS GEOSPATIAL PATENT ASSIGNMENTS OVER TIME TO VISUALIZE
# THE INCREASING TECHNOLOGICAL TREND

# PLOT GEOSPATIAL PATENT ASSGINMENTS OVER TIME AND ADD VERTICAL LINES TO MARK
# THE YEARS WHERE GEOFENCE WARRANT CASES WERE PUBLICLY REPORTED

# Select columns for the time
time_fullDf <- select(assignees_full, patent_number, patent_title, assignee_organization, patent_date)
time_fullDf

# Drop na's and duplications of patent numbers
time_fullDf <- na.omit(time_fullDf)
time_fullDf <- distinct(time_fullDf)
time_fullDf

time_fullDf <- separate(time_fullDf, "patent_date", c("Year", "Month", "Day"), sep = "-")
time_fullDf

# Get the total geospatial patent counts for each date
date_counts <- count(time_fullDf, Year, sort=TRUE)
date_counts

# Reorder the assignees for plotting (based on n rather than alphabetical)
date_counts <- arrange(date_counts, Year)
date_counts

# Make a table of geofence warrant cases to drop in vertical markers
geofence_dates <- data.frame( Year=c("2019", "2019", "2018"),
                              Case=c("Chatrie v United States", "Zachary McCoy Wrongful Arrest", "Jorge Molina Wrongful Arrest"),
                              Case_Count=c(2,2,1))
geofence_dates


# Plot counts by date
time_plot <- ggplot(date_counts, aes(x=Year)) +
  geom_line(aes(y =n, group=1), color="tomato") +
  labs(title = "Geospatial Patent Counts by Year",
       subtitle = "(2002-2020)",
       caption = "Vertical lines indicate widely reported cases for wrongful arrests and constitutionality of geofence warrants: \nChatrie v. United States Case and Zachary McCoy Wrongful Arrest (2019), and \nJorge Molina Wrongful Arrest (2018).",
       x = "Year",
       y = "Total Number of Assigned Geospatial Patents",
       color = "Legend") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_vline(xintercept = geofence_dates$Year, linetype="dashed", 
             color = "steelblue", size=1) +
  xlab("")


time_plot


# ==============================================================================
# ==============================================================================


# THESE NEXT FEW SECTIONS VISUALIZE THE NETWORKS
# Make a network from the full list of patents

edges1 <- pvresults$cited_patents
edges2 <- pvresults$citedby_patents

colnames(edges1) <- c("from", "to")
colnames(edges2) <- c("to", "from")

edges <- rbind(edges1, edges2)

edges_df <- na.omit(edges)
summary(edges_df)

edge_net=as.network.matrix(edges_df)
summary(edge_net)

plot(edge_net,
     main="Geospatial Patent Citation Network")


# ==============================================================================


# Create edgelist for all patents in the top cpc category G06, then make it
# a network.

# Use this dataframe to create your edgelist, take a look at it again for a reminder
# of the patent information within it
citedby_leftJoinDf

# Create CPC edgelist by selecting patent_number and citedby_patent_number columns
# in the citedby_leftJoinDf that was previously filtered down to G06 patents only
cpcedges <- citedby_leftJoinDf %>% select(patent_number, citedby_patent_number)
cpcedges


cpcedge_net=as.network.matrix(cpcedges)
summary(cpcedge_net)

plot(cpcedge_net,
     main="Patent Citations in Top CPC Subsection G06 \n(COMPUTING; CALCULATING; COUNTING)")


# ==============================================================================


# Make a network out of the top 10 cited patents in the top CPC subsection category.

# This is the list of the top 10 most cited patents in the top CPC subsection G06
top_citation_cpc


# Create an object that has just the top 10 cited patents in G06
top_cpc_patents <- c("8799799", "7577522", "8230333", "8385964", "9021384",
                     "8290942", "8494215", "7933929", "7746343", "6985929")
top_cpc_patents

# Find all patent citations for the top 10 most cited patents in G06 by filtering
# down the original edgelist for the full patent network
clean_top_cpc_patents <- filter(edges, from %in% top_cpc_patents)
clean_top_cpc_patents


# Create citation network for 10 top most cited patents in G06
clean_top_cpc_patents_net=as.network.matrix(clean_top_cpc_patents)
summary(clean_top_cpc_patents_net)

clean_top_cpc_patents_net


plot(clean_top_cpc_patents_net,
     main="Top 10 Most Cited Patents in Top CPC Subsection G06 \n(COMPUTING; CALCULATING; COUNTING)")


# ==============================================================================


# Make a network out of the top most cited patent.

# This is the list of the top 10 most cited patents in the top CPC subsection G06
top_citation_cpc


# Create an object that has just the top 10 cited patents in G06
most_cited_patent <- c("8799799")
most_cited_patent

# Find all patent citations for the top most cited patent by filtering
# down the original edgelist for the full patent network
clean_top_most_cited_patent <- filter(edges, from %in% most_cited_patent)
clean_top_most_cited_patent


# Create citation network for 10 top most cited patents in G06
clean_top_most_cited_patent_net=as.network.matrix(clean_top_most_cited_patent)
summary(clean_top_most_cited_patent_net)

clean_top_most_cited_patent_net


plot(clean_top_most_cited_patent_net,
     main="Top Most Cited Patent")


# ==============================================================================


# THESE NEXT SECTIONS IMPROVE THE NETWORK VISUALIZATIONS TO ADD A NODE ATTRIBUTE
# THAT CALCULATES A WEIGHT FOR EACH NODE BASED ON THE QUANTITY OF CPC SUBSECTION
# ASSIGNMENTS IT HAS. THIS ATTRIBUTE WILL BE USED LATER TO TEST HOW THIS PARAMETER
# IMPACTS THE DEGREE OF NODEMATCHES OR THE LIKELIHOOD OF BEING CONNECTED/CITED


# Calculate cpc assignment totals to create a node attribute for cpc_weight
# Drop cpc_group_id & category to remove dups of subsection ids to get a more accurate count
cpcs_tbl_nogroup <- select(cpcs_tbl, -cpc_group_id, -cpc_category)
cpcs_tbl_nogroup

# Drop dups
cpcs_tbl_nogroup <- distinct(cpcs_tbl_nogroup)
cpcs_tbl_nogroup

# Count total citations by patent
cpc_cat_counts <- count(cpcs_tbl_nogroup, patent_number, sort=TRUE)
cpc_cat_counts

# Drop na's
cpc_cat_counts <- na.omit(cpc_cat_counts)
cpc_cat_counts

# Rename the "to" column to patent_number to pull in assignee org name
cpc_cat_counts <- rename_all(cpc_cat_counts, recode, "n" = "cpc_weight")
cpc_cat_counts


# ==============================================================================


# This section creates new plots of networks adding the cpc_weight to the nodes

# Full patent citation network
edge_net %v% 'cpc_weight' <- unlist(cpc_cat_counts)
edge_net %v% 'cpc_weight'
plot(edge_net, vertex.col="cpc_weight", main="Geospatial Patent Citation Network")
edge_net


# Top CPC Subsection G06 Patent Citation Network
cpcedge_net %v% 'cpc_weight' <- unlist(cpc_cat_counts)
cpcedge_net %v% 'cpc_weight'
plot(cpcedge_net, vertex.col="cpc_weight", main="Patent Citations in Top CPC Subsection G06 \n(COMPUTING; CALCULATING; COUNTING)")
cpcedge_net


# Top 10 Cited Patents Within CPC Subsection G06 Network
clean_top_cpc_patents_net %v% 'cpc_weight' <- unlist(cpc_cat_counts)
clean_top_cpc_patents_net %v% 'cpc_weight'
plot(clean_top_cpc_patents_net, vertex.col="cpc_weight", 
     main="Top 10 Most Cited Patents in Top CPC Subsection G06 \n(COMPUTING; CALCULATING; COUNTING)")
clean_top_cpc_patents_net


# Most Cited Patent Within CPC Subsection G06 Network
clean_top_most_cited_patent_net %v% 'cpc_weight' <- unlist(cpc_cat_counts)
clean_top_most_cited_patent_net %v% 'cpc_weight'
plot(clean_top_most_cited_patent_net, vertex.col="cpc_weight", 
     main="Top Most Cited Patent")
clean_top_most_cited_patent_net


# ==============================================================================
# ==============================================================================


# THESE NEXT SECTIONS FIRST TEST FOR VARIOUS NETWORK PARAMETERS


# Network density
# mode is 'digraph' for directed, 'graph' for undirected networks.
gden(edge_net,  mode="digraph")
gden(cpcedge_net,  mode="digraph")
gden(clean_top_cpc_patents_net,  mode="digraph")
gden(clean_top_most_cited_patent_net,  mode="digraph")


# Network transitivity 
# For directed graph, proportion of triads A->B->C where A->C
# For undirected graph, proportion closed triangles
gtrans(edge_net, mode="digraph") # network too large to execute
gtrans(cpcedge_net, mode="digraph") # network too large to execute
gtrans(clean_top_cpc_patents_net, mode="digraph") # network too large to execute
gtrans(clean_top_most_cited_patent_net, mode="digraph")


# Network reciprocity 
# edgewise: proportion edges that are reciprocated from all edges that are present
grecip(edge_net, measure="edgewise")
grecip(cpcedge_net, measure="edgewise")
grecip(clean_top_cpc_patents_net, measure="edgewise")
grecip(clean_top_most_cited_patent_net, measure="edgewise")


# ==============================================================================


# Degree centrality 
# gmode is 'digraph' or 'graph' for directed or undirected networks/
# cmode is 'indegree', 'outdegree', or 'freeman' (total degree)
degree(edge_net, gmode="digraph", cmode="indegree")  # indegree
degree(edge_net, gmode="digraph", cmode="outdegree") # outdegree
degree(edge_net, gmode="digraph", cmode="freeman")   # total

degree(cpcedge_net, gmode="digraph", cmode="indegree")  # indegree
degree(cpcedge_net, gmode="digraph", cmode="outdegree") # outdegree
degree(cpcedge_net, gmode="digraph", cmode="freeman")   # total

degree(clean_top_cpc_patents_net, gmode="digraph", cmode="indegree")  # indegree
degree(clean_top_cpc_patents_net, gmode="digraph", cmode="outdegree") # outdegree
degree(clean_top_cpc_patents_net, gmode="digraph", cmode="freeman")   # total

degree(clean_top_most_cited_patent_net, gmode="digraph", cmode="indegree")  # indegree
degree(clean_top_most_cited_patent_net, gmode="digraph", cmode="outdegree") # outdegree
degree(clean_top_most_cited_patent_net, gmode="digraph", cmode="freeman")   # total

# Betweenness centrality 
betweenness(edge_net, gmode="graph")
betweenness(cpcedge_net, gmode="graph")
betweenness(clean_top_cpc_patents_net, gmode="graph")
betweenness(clean_top_most_cited_patent_net, gmode="graph")


# Centralization
# Centralization is calculated as a single function with FUN indicating which
# type of measure is to be used (e.g. closeness, degree, betweenness, etc.)
centralization(edge_net, FUN=degree, mode="graph")
centralization(edge_net, FUN=degree, mode="digraph", cmode="outdegree")
centralization(edge_net, FUN=betweenness, mode="graph")
centralization(edge_net, FUN=closeness, mode="graph") # crashes r

centralization(cpcedge_net, FUN=degree, mode="graph")
centralization(cpcedge_net, FUN=degree, mode="digraph", cmode="outdegree")
centralization(cpcedge_net, FUN=betweenness, mode="graph")
centralization(cpcedge_net, FUN=closeness, mode="graph")

centralization(clean_top_cpc_patents_net, FUN=degree, mode="graph")
centralization(clean_top_cpc_patents_net, FUN=degree, mode="digraph", cmode="outdegree")
centralization(clean_top_cpc_patents_net, FUN=betweenness, mode="graph")
centralization(clean_top_cpc_patents_net, FUN=closeness, mode="graph")

centralization(clean_top_most_cited_patent_net, FUN=degree, mode="graph")
centralization(clean_top_most_cited_patent_net, FUN=degree, mode="digraph", cmode="outdegree")
centralization(clean_top_most_cited_patent_net, FUN=betweenness, mode="graph")
centralization(clean_top_most_cited_patent_net, FUN=closeness, mode="graph")

# ==============================================================================



# Calculate the out-degree network centralization of the networks.
# Conduct a permutation test conditioning on the number of edges
# in the networks.


# Calculate out-degree
degree(edge_net, gmode="digraph", cmode="outdegree")
degree(cpcedge_net, gmode="digraph", cmode="outdegree")
degree(clean_top_cpc_patents_net, gmode="digraph", cmode="outdegree")
degree(clean_top_most_cited_patent_net, gmode="digraph", cmode="outdegree")


# Create function
my_fun <- function(my_net) { centralization(my_net, FUN=degree, mode="digraph", cmode="indegree") }
centralization(edge_net, FUN=degree, mode="digraph", cmode="indegree")

# these work
my_fun(edge_net)
my_fun(cpcedge_net)
my_fun(clean_top_cpc_patents_net)
my_fun(clean_top_most_cited_patent_net)



# Conditioning on number of edges for cpcedge_net only--R would not process it for
# the full edge_net.

# this one works!
ct2 <- cug.test(cpcedge_net, FUN=my_fun , mode="digraph", cmode="edges", reps = 1000)
ct2
plot(ct2)



# PERMUTATION TEST ANALYSIS:
# The observed value for ct2 is 0.0028. The probability that this reciprocity value could
# be randomly created is 0 which is less than p (0.05). This means that
# the observed value of reciprocity is significantly different from the random
# graphs.


# ==============================================================================
# ==============================================================================



# THIS SECTION IS FOR ERGMS


# Build and estimate an exponential random graph model for
# each patent citation network

# This section builds models and tests GOF for each network


# This is for the edge_net
# Build model with node match, is same attribute level likely to be connected
cpcedge.mod.5 <- ergm(edge_net ~ edges + gwesp(0.2, fixed=TRUE) + nodematch("cpc_weight"))
summary(cpcedge.mod.5)

# Goodness of fit with regard to the model parameters:
gof(cpcedge.mod.5)

# Goodness of fit with regard to degree distribution
cpcedge.mod.5.gof2 <- gof(cpcedge.mod.5 ~ idegree + odegree)
cpcedge.mod.5.gof2
plot(cpcedge.mod.5.gof2)





# This is for the cpcedge_net
# Build model with node match, is same attribute level likely to be connected
cpc.mod.5 <- ergm(cpcedge_net ~ edges + gwesp(0.2, fixed=TRUE) + nodematch("cpc_weight"))
summary(cpc.mod.5)

# Goodness of fit with regard to the model parameters:
gof(cpc.mod.5)

# Goodness of fit with regard to degree distribution
cpc.mod.5.gof2 <- gof(cpc.mod.5 ~ idegree + odegree)
cpc.mod.5.gof2
plot(cpc.mod.5.gof2)





# This is for the clean_top_cpc_patents_net
# Build model with node match, is same attribute level likely to be connected
topcpcedge.mod.5 <- ergm(clean_top_cpc_patents_net ~ edges + gwesp(0.2, fixed=TRUE) + nodematch("cpc_weight"))
summary(topcpcedge.mod.5)

# Goodness of fit with regard to the model parameters:
gof(topcpcedge.mod.5)

# Goodness of fit with regard to degree distribution
topcpcedge.mod.5.gof2 <- gof(topcpcedge.mod.5 ~ idegree + odegree)
topcpcedge.mod.5.gof2
plot(topcpcedge.mod.5.gof2)




# This is for the clean_top_most_cited_patent_net
# Build model with node match, is same attribute level likely to be connected
toppatentedge.mod.5 <- ergm(clean_top_most_cited_patent_net ~ edges + gwesp(0.2, fixed=TRUE) + nodematch("cpc_weight"))
summary(toppatentedge.mod.5)

# Goodness of fit with regard to the model parameters:
gof(toppatentedge.mod.5)

# Goodness of fit with regard to degree distribution
toppatentedge.mod.5.gof2 <- gof(toppatentedge.mod.5 ~ idegree + odegree)
toppatentedge.mod.5.gof2
plot(toppatentedge.mod.5.gof2)




# GOF Reports:
# cpcedge.mod.5.gof2 has a p-value of 0 for each parameter, which is less than
# 0.05, so it is not significant. The box plot also confirms this observation where
# the line of fit is not within the confidnce interval for any of the parameters.

# cpc.mod.5.gof2 has a p-value of 0.7 for the edges parameter, which is higher
# than 0.05, so it is significant. The box plot also confirms this observation
# where the line of fit is within the confidence interval for the edges parameter,
# but it is not for both the mutual and gwesp parameters.

# topcpcedge.mod.5.gof2 has a p-value of 0 for each parameter, which is less than
# 0.05, so it is not significant. The box plot also confirms this observation where
# the line of fit is not within the confidnce interval for any of the parameters.

# toppatentedge.mod.5.gof2 has a p-value of 0 for the edges parameter, which is less
# than 0.05, so it is not significant. The box plot also confirms this observation
# where the line of fit is not within the confidence interval for any of the parameters.






