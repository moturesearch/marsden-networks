# ANALYSIS.R
#
# This script populates figures/ and tables/.
#
# Ben Davies
# July 2020



# Initialisation ----

library(alpaca)
library(conflicted)
library(dplyr)
library(ggplot2)
library(knitr)
library(margins)
library(MASS)
library(Matrix)
library(purrr)
library(readr)
library(sandwich)
library(scales)
library(sessioninfo)
library(texreg)
library(tidyr)
library(widyr)

# Resolve conflicts
conflict_prefer('filter', 'dplyr')
conflict_prefer('lag', 'dplyr')
conflict_prefer('select', 'dplyr')

# Define function for importing data
import_data <- function(f) {
  paths <- yaml::read_yaml('data/paths.yaml')
  if (f %in% names(paths)) {
    if (grepl('[.]feather', paths[[f]])) {
      feather::read_feather(paste0(paths$base, paths[[f]]))
    }
  } else (
    stop('Unknown file')
  )
}

# Import data
asjc_crosswalk <- import_data('asjc_crosswalk')
authorships <- import_data('authorships')
authorships_full <- import_data('authorships_full')
countries <- import_data('countries')
matches <- import_data('matches')
mean_citations <- import_data('mean_citations')
people <- import_data('people')
proposals <- import_data('proposals')
proposals_full <- import_data('proposals_full')
publications <- import_data('publications')
publications_full <- import_data('publications_full')
publication_fields <- import_data('publication_fields')
teams <- import_data('teams')

# Set horizon
horizon <- 10

# Set tolerance for testing float equalities
tol <- 1e-8

# Identify years in data
publication_years <- sort(unique(publications$pub_year))
proposal_years <- sort(unique(proposals$prop_year))
years <- sort(unique(c(publication_years, proposal_years)))

# Specify panel years
panel_years <- proposal_years[horizon]:max(years)



# Publication and citation counts ----

# Count publications in full data by whether they have an NZ-affiliated author
# (according to scopus)
publications_full %>%
  mutate(has_nzl_auth = pub_n_nzl_auth > 0) %>%
  count(has_nzl_auth)
# # A tibble: 2 x 2
#   has_nzl_auth       n
#   <lgl>          <int>
# 1 FALSE        7854938
# 2 TRUE          230730

# Count Scopus author IDs in full data
publications_full %>%
  select(pub) %>%
  left_join(authorships_full) %>%
  {n_distinct(.$auth)}  # 7,141,834

# Count publications prior to 2000
nrow(filter(publications_full, pub_year < 2000))  # 751,981
nrow(filter(publications, pub_year < 2000))  # 61,085

# Compute publication MNCS values
publication_mncs <- publication_fields %>%
  filter(!is.na(field)) %>%
  group_by(pub) %>%
  mutate(wt = 1 / n()) %>%
  ungroup() %>%
  left_join(publications) %>%
  left_join(mean_citations) %>%
  mutate(value = if_else(mean_n_citations == 0, 0, pub_n_citations / mean_n_citations)) %>%
  group_by(pub) %>%
  summarise(pub_mncs = sum(value * wt) / sum(wt)) %>%
  ungroup()

# Count publications and mean-normalised citations by researcher and year
researcher_publications <- people %>%
  left_join(authorships) %>%
  distinct(prsn, pub) %>%
  left_join(publications) %>%
  left_join(publication_mncs) %>%
  group_by(prsn, pub_year) %>%
  summarise(n_pub = n(),
            n_pub_frac = sum(1 / pub_n_auth),
            mncs = sum(pub_mncs, na.rm = T),  # Pubs with no ASJC fields contribute zero cites
            mncs_frac = sum(pub_mncs / pub_n_auth, na.rm = T)) %>%
  ungroup()

# Count publications and mean-normalised citations by pair and year
pair_publications <- authorships %>%
  left_join(publications) %>%
  left_join(publication_mncs) %>%
  mutate(pub_mncs = ifelse(is.na(pub_mncs), 0, pub_mncs)) %>%  # Some pubs have missing ASJC fields
  mutate(n_pub = 1,
         n_pub_frac = 1 / pub_n_auth,
         mncs = pub_mncs,
         mncs_frac = pub_mncs / pub_n_auth) %>%
  select(prsn, pub, pub_year, n_pub, n_pub_frac, mncs, mncs_frac) %>%
  gather(key, value, -prsn, -pub, -pub_year) %>%
  group_by(key, pub_year) %>%
  pairwise_count(prsn, pub, wt = value) %>%
  ungroup() %>%
  spread(key, n, fill = 0)



# Proposal counts ----

# Count proposals in full data
nrow(proposals_full)  # 18,811

# Count proposals by researcher, year, round, and type
researcher_proposals <- teams %>%
  add_count(prop, round) %>%
  right_join(people) %>%
  left_join(proposals) %>%
  distinct(prsn, prop, prop_year, round, prop_is_fast_start, n) %>%
  group_by(prsn, prop_year, round = round + 1, type = 2 - prop_is_fast_start) %>%
  summarise(n_prop = n(),
            n_prop_frac = sum(1 / n)) %>%
  ungroup()



# Annual counts ----

inner_join(
  authorships %>%
    distinct(prsn, pub) %>%
    left_join(publications) %>%
    group_by(Year = pub_year) %>%
    summarise(Publications = n_distinct(pub),
              Authors = n_distinct(prsn)),
  people %>%
    left_join(teams) %>%
    distinct(prsn, prop) %>%
    left_join(proposals) %>%
    group_by(Year = prop_year) %>%
    summarise(Proposals = n_distinct(prop),
              Applicants = n_distinct(prsn))
) %>%
  mutate_if(is.integer, comma, accuracy = 1) %>%
  kable(align = 'lrrrr', booktabs = T, format = 'latex', linesep = '') %>%
  {strsplit(., '\n')[[1]]} %>%
  {c(.[2:24],
     '\\midrule',
     sprintf('Total & %s & & %s & \\\\', comma(nrow(filter(publications, pub_year >= 2000)), accuracy = 1), comma(nrow(proposals)), accuracy = 1),
     .[25:26])} %>%
  write_lines('tables/counts.tex')



# Co-authorship network trends ----

# Identify nodes by year
nodes <- researcher_publications %>%
  distinct(prsn, pub_year) %>%
  crossing(year = years) %>%
  filter(year >= pub_year & year < pub_year + horizon) %>%
  distinct(prsn, year)

# Count co-authorships by pair and publication year
coauthorships <- authorships %>%
  distinct(prsn, pub) %>%
  left_join(publications) %>%
  group_by(pub_year) %>%
  pairwise_count(prsn, pub) %>%
  ungroup()

# Identify edges by year
edges <- coauthorships %>%
  crossing(year = years) %>%
  filter(year >= pub_year & year < pub_year + horizon) %>%
  distinct(year, item1, item2)

# Compute node degrees
degrees <- edges %>%
  count(prsn = item1, year, name = 'degree')

# Define partitions
parts <- c('No proposals', 'First round', 'Second round', 'Funded')
partitions <- researcher_proposals %>%
  group_by(prsn, prop_year) %>%
  summarise(max_round = max(round)) %>%
  ungroup() %>%
  crossing(year = proposal_years) %>%
  filter(year >= prop_year & year < prop_year + horizon) %>%
  group_by(prsn, year) %>%
  summarise(part = max(max_round)) %>%
  ungroup() %>%
  right_join(nodes) %>%
  mutate(part = replace(part, is.na(part), 0),
         part = factor(parts[part + 1], levels = parts))

# Compute part properties
part_properties <- partitions %>%
  filter(year %in% panel_years) %>%
  add_count(year, part, name = 'part_size') %>%
  add_count(year, name = 'order') %>%
  mutate(part_share = part_size / order) %>%
  left_join(degrees) %>%
  mutate(degree = replace(degree, is.na(degree), 0)) %>%
  group_by(Year = year, Part = part) %>%
  summarise(Size = n(),
            `Mean degree` = mean(degree)) %>%
  ungroup() %>%
  add_count(Year, wt = Size, name = 'n_nodes') %>%
  mutate(Share = 100 * Size / n_nodes,
         `Normalised mean degree` = 100 * `Mean degree` / (n_nodes - 1)) %>%
  select(Year, Part, Size, Share, contains('degree'), -n_nodes)

# Export part properties
part_properties %>%
  mutate_if(is.double, round, 3) %>%
  write_csv('tables/parts.csv')

# Plot proportion of nodes in each part
part_properties %>%
  ggplot(aes(Year, Share)) +
  geom_line(aes(lty = Part)) +
  coord_cartesian(clip = 'off') +
  labs(y = 'Proportion of nodes in part (%)') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'dotdash')) +
  theme_classic() +
  theme(legend.position = 'bottom')
ggsave('figures/part-shares.pdf', width = 6, height = 4, dpi = 100)

# Plot normalised mean degrees among researchers in each part
part_properties %>%
  ggplot(aes(Year, `Normalised mean degree`)) +
  geom_line(aes(lty = Part)) +
  coord_cartesian(clip = 'off') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'dotdash')) +
  theme_classic() +
  theme(legend.position = 'bottom')
ggsave('figures/part-mean-degrees.pdf', width = 6, height = 4, dpi = 100)



# Panel construction ----

# Identify active years by researcher
researcher_active_years <- crossing(prsn = people$prsn, year = years) %>%
  left_join(researcher_publications, by = c('prsn', 'year' = 'pub_year')) %>%
  left_join(researcher_proposals, by = c('prsn', 'year' = 'prop_year')) %>%
  select(prsn, year, n_pub, n_prop) %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
  group_by(prsn) %>%
  arrange(year) %>%
  mutate(pub_first = min(year[which(n_pub > 0)]),
         pub_last = max(year[which(n_pub > 0)]),
         pub = year >= pub_first & year <= pub_last,
         prop_first = min(year[which(n_prop > 0)]),
         prop_last = max(year[which(n_prop > 0)]),
         prop = year >= prop_first & year <= prop_last) %>%
  ungroup() %>%
  filter(pub | prop)

# Identify "boundary" years by researcher
researcher_boundary_years <- researcher_active_years %>%
  select(prsn, ends_with('first'), ends_with('last')) %>%
  distinct() %>%
  mutate(first = pmin(pub_first, prop_first),
         last = pmin(pub_last, prop_last)) %>%
  select(prsn, first, last)

# Identify NZers
nzers <- authorships %>%
  semi_join(filter(countries, aff_country_tag == 'NZL')) %>%
  distinct(prsn) %>%
  mutate(prsn_has_nzl_aff = T) %>%
  full_join(people) %>%
  filter(prsn_has_nzl_aff | prsn_is_nzl_based == 1) %>%
  {unique(.$prsn)}

# Count proposal team co-memberships by pair, year, round, and type
pair_proposals <- people %>%
  left_join(teams) %>%
  left_join(proposals) %>%
  distinct(prsn, round, prop_is_fast_start, prop, prop_year) %>%
  group_by(prop_year, round = round + 1, type = 2 - prop_is_fast_start) %>%
  pairwise_count(prsn, prop) %>%
  ungroup() %>%
  rename(n_prop = n)

# Identify pairs of researchers who ever collaborate during period of study
collab_pairs <- bind_rows(
  mutate(select(pair_publications, item1, item2, year = pub_year), type = 'Co-authors'),
  mutate(select(pair_proposals, item1, item2, year = prop_year), type = 'Co-members')
) %>%
  filter(year %in% proposal_years) %>%
  filter(item1 < item2) %>%
  distinct(item1, item2, type)

# Count pairs who ever collaborate
n_collab_pairs <- collab_pairs %>%
  distinct(item1, item2) %>%
  nrow()
n_collab_pairs  # 105,287

# How many pairs of NZers?
collab_pairs %>%
  filter(item1 %in% nzers & item2 %in% nzers) %>%
  distinct(item1, item2) %>%
  nrow()  # 67,569

# Count researchers among pairs who ever collaborate
collab_pairs %>%
  select(-type) %>%
  gather(key, prsn) %>%
  distinct(prsn) %>%
  nrow()  # 13,011

# How many NZers?
collab_pairs %>%
  filter(item1 %in% nzers & item2 %in% nzers) %>%
  select(-type) %>%
  gather(key, prsn) %>%
  distinct(prsn) %>%
  nrow()  # 8,131

# Count collaborating pairs by collaboration type
collab_pairs %>%
  mutate(value = T) %>%
  spread(type, value, fill = F) %>%
  count(`Co-authors`, `Co-members`)
# # A tibble: 3 x 3
#   `Co-authors` `Co-members`     n
#   <lgl>        <lgl>        <int>
# 1 FALSE        TRUE         23047
# 2 TRUE         FALSE        64746
# 3 TRUE         TRUE         17494

# Count first collaboration types among pairs who collaborate
bind_rows(
  mutate(select(pair_publications, item1, item2, year = pub_year), type = 'coauth'),
  mutate(select(pair_proposals, item1, item2, year = prop_year), type = 'comemb')
) %>%
  filter(year %in% proposal_years) %>%
  group_by(item1, item2, type) %>%
  summarise(year = min(year)) %>%
  ungroup() %>%
  spread(type, year) %>%
  drop_na() %>%
  semi_join(collab_pairs) %>%
  mutate(first = case_when(coauth < comemb ~ 'coauth',
                           coauth > comemb ~ 'comemb',
                           T ~ 'same')) %>%
  count(first) %>%
  mutate(p = n / sum(n))
# # A tibble: 3 x 3
#   first      n     p
#   <chr>  <int> <dbl>
# 1 coauth  7122 0.407
# 2 comemb  8037 0.459
# 3 same    2335 0.133

# Generate random sample of pairs who were concurrently active
# during period of study
set.seed(0)
random_pairs <- crossing(item1 = nzers, item2 = nzers) %>%
  filter(item1 < item2) %>%
  left_join(researcher_boundary_years, by = c('item1' = 'prsn')) %>%
  left_join(researcher_boundary_years, by = c('item2' = 'prsn')) %>%
  filter(last.x >= first.y & first.x <= last.y) %>%
  filter(pmin(first.x, first.y) >= min(proposal_years)) %>%
  select(item1, item2) %>%
  sample_n(n_collab_pairs)

# Initialise pair panel observations
pair_panel_obs <- bind_rows(
  mutate(distinct(collab_pairs, item1, item2), panel = 'Collaborators'),
  mutate(random_pairs, panel = 'Random')
) %>%
  crossing(pub_year = publication_years) %>%
  left_join(pair_publications) %>%
  mutate(Y = 1 * !is.na(n_pub)) %>%
  select(item1, item2, panel, year = pub_year, Y) %>%
  filter(year %in% panel_years) %>%  # Excludes pairs with early mutual active periods
  left_join(researcher_boundary_years, by = c('item1' = 'prsn')) %>%
  left_join(researcher_boundary_years, by = c('item2' = 'prsn')) %>%
  filter(year >= pmax(first.x, first.y) & year <= pmin(last.x, last.y)) %>%
  select(item1, item2, panel, year, Y) %>%
  arrange(item1, item2, year)

# Compute proposal dummies
pair_proposal_dummies <- pair_proposals %>%
  mutate(round = c('first', 'second', 'funded')[round],
         type = c('fs', 'std')[type]) %>%
  unite(key, c('round', 'type')) %>%
  group_by(item1, item2, key) %>%
  summarise(first = min(prop_year)) %>%
  ungroup() %>%
  crossing(year = proposal_years) %>%
  group_by(item1, item2, key) %>%
  arrange(year) %>%
  mutate(value = 1 * (cumsum(year >= first) > ifelse(year <= proposal_years[horizon], 0, lag(cumsum(year >= first), horizon)))) %>%
  ungroup() %>%
  select(-first) %>%
  spread(key, value) %>%
  arrange(item1, item2, year)

# Compute sorting covariates
pair_sorting_covariates <- pair_panel_obs %>%
  distinct(item1, item2) %>%
  crossing(pub_year = publication_years) %>%
  left_join(researcher_publications, by = c('item1' = 'prsn', 'pub_year')) %>%
  left_join(researcher_publications, by = c('item2' = 'prsn','pub_year')) %>%
  filter(!(is.na(n_pub.x) & is.na(n_pub.y))) %>%
  crossing(year = years) %>%
  filter(year >= pub_year & year < pub_year + horizon) %>%
  group_by(item1, item2, year) %>%
  summarise(mncs.x = sum(mncs_frac.x, na.rm = T),
            mncs.y = sum(mncs_frac.y, na.rm = T)) %>%
  ungroup() %>%
  right_join(distinct(pair_panel_obs, item1, item2, year)) %>%
  mutate_if(is.double, function(x) ifelse(is.na(x), 0, x)) %>%  # Some researchers have no pubs in rolling window
  left_join(degrees, by = c('item1' = 'prsn', 'year')) %>%
  left_join(degrees, by = c('item2' = 'prsn', 'year')) %>%
  left_join(mutate(edges, adjacent = 1)) %>%
  mutate_at(c('degree.x', 'degree.y', 'adjacent'), function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(degree_mean = (degree.x + degree.y) / 2 - adjacent,
         degree_diff = abs(degree.x - degree.y),
         mncs_mean = (mncs.x + mncs.y) / 2,
         mncs_diff = abs(mncs.x - mncs.y)) %>%
  select(-ends_with('.x'), -ends_with('.y'))

# Compute ASJC field weights by researcher
researcher_field_weights <- publications %>%
  filter(pub_year %in% proposal_years) %>%  # Since defining other covariates from 2000 onwards
  mutate(auth_wt = 1 / pub_n_auth) %>%  # Some authors not in data
  left_join(authorships) %>%
  select(prsn, pub, auth_wt) %>%
  left_join(publication_fields) %>%
  left_join(asjc_crosswalk) %>%
  filter(!is.na(field)) %>%
  add_count(prsn, pub) %>%
  mutate(fwt = 1 / n) %>%
  count(prsn, field, wt = auth_wt * fwt) %>%
  add_count(prsn, wt = n, name = 'n_tot') %>%
  mutate(wt = n / n_tot)

# Fractionally count publications (between 2000 and 2018) by ASJC field
field_counts <- publications %>%
  filter(pub_year %in% proposal_years) %>%
  left_join(publication_fields) %>%
  left_join(asjc_crosswalk) %>%
  distinct(pub, field) %>%
  add_count(pub) %>%
  count(field, wt = 1 / n)

# How many publications?
sum(field_counts$n)  # 613,889

# How many with missing fields?
sum(field_counts$n[is.na(field_counts$field)])  # 380

# Export distribution of publications across field groups
field_counts %>%
  left_join(asjc_crosswalk) %>%
  count(field_group_desc, wt = n, sort = T) %>%
  filter(!is.na(field_group_desc)) %>%
  mutate(n = n / 1e3,
         wt = 100 * n / sum(n)) %>%
  {bind_rows(., tibble(field_group_desc = 'Total', n = sum(.$n), wt = 100))} %>%
  `names<-`(c('Field group', 'Publications (000)', 'Share (\\%)')) %>%
  kable(booktabs = T, digits = 3, escape = F, format = 'latex', linesep = '') %>%
  {strsplit(., '\n')[[1]]} %>%
  {c(.[1:32], '\\midrule', .[33:35])} %>%
  write_lines('tables/field-groups.tex')

# Compute ASJC field "overlaps" by pair
# Note: using left/inner joins removes pair-field tuples for which either researcher
# in pair has no pubs in field. These tuples don't contribute to the numerator
# or denominator when computing Cosine seimilarities, so can be removed safely.
tmp <- researcher_field_weights %>%
  add_count(prsn, wt = wt ^ 2, name = 'divisor') %>%
  mutate(divisor = sqrt(divisor)) %>%
  select(prsn, field, wt, divisor)
pair_field_overlaps <- pair_panel_obs %>%
  distinct(item1, item2) %>%
  left_join(tmp, by = c('item1' = 'prsn')) %>%
  filter(!is.na(field)) %>%
  inner_join(tmp, by = c('item2' = 'prsn', 'field')) %>%
  group_by(item1, item2) %>%
  summarise(overlap = sum(wt.x * wt.y) / mean(divisor.x * divisor.y)) %>%
  ungroup()

# Combine panel data
pair_panels <- pair_panel_obs %>%
  left_join(pair_proposal_dummies) %>%
  mutate_if(is.double, function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(first = pmin(first_fs + first_std, 1),
         second = pmin(second_fs + second_std, 1),
         funded = pmin(funded_fs + funded_std, 1)) %>%
  left_join(pair_sorting_covariates) %>%
  left_join(pair_field_overlaps) %>%
  mutate(overlap = ifelse(is.na(overlap), 0, overlap)) %>%
  # Lag independent variables
  group_by(item1, item2, panel) %>%
  arrange(year) %>%
  mutate_at(names(.)[grepl('first|second|funded|adj|_mean|_diff', names(.))], lag) %>%
  slice(-1) %>%
  ungroup()

# Add panel of collaborating NZers
pair_panels <- bind_rows(
  pair_panels,
  pair_panels %>%
    filter(panel == 'Collaborators' & item1 %in% nzers & item2 %in% nzers) %>%
    mutate(panel = 'NZ collaborators')
)

# Count obs, pairs and researchers by panel
pair_panel_counts <- pair_panels %>%
  select(panel, item1, item2, year) %>%
  add_count(panel, name = 'n_obs') %>%
  distinct(panel, n_obs, item1, item2) %>%
  add_count(panel, name = 'n_pairs') %>%
  gather(key, value, item1, item2) %>%
  select(-key) %>%
  distinct() %>%
  count(panel, n_obs, n_pairs, name = 'n_researchers')
pair_panel_counts
# # A tibble: 3 x 4
#   panel             n_obs n_pairs n_researchers
#   <chr>             <int>   <int>         <int>
# 1 Collaborators    355911   68367          8997
# 2 NZ collaborators 247110   46052          5823
# 3 Random           312279   74529          3398

# Compare panels
pair_panels %>%
  select(-item1, -item2, -year) %>%
  gather(key, x, -panel) %>%
  group_by(panel, key) %>%
  summarise(x = sprintf('%.3f (%.3f)', mean(x), sd(x))) %>%
  ungroup() %>%
  spread(key, x) %>%
  left_join(pair_panel_counts) %>%
  mutate_if(is.integer, comma) %>%
  select(panel,
         Observations = n_obs,
         Pairs = n_pairs,
         Researchers = n_researchers,
         `Co-authored in year $t$ ($\\mathtt{coauth}$)` = Y,
         `First round co-members ($\\mathtt{first}$)` = first,
         `Second round co-members ($\\mathtt{second}$)` = second,
         `Funded co-members ($\\mathtt{funded}$)` = funded,
         `Adjacent in co-auth. network ($\\mathtt{adjacent}$)` = adjacent,
         `Mean degree ($\\overline{\\mathtt{degree}}$)` = degree_mean,
         `Diff. in degrees ($\\Delta\\mathtt{degree}$)` = degree_diff,
         `Mean citation impact ($\\overline{\\mathtt{MNCS}}$)` = mncs_mean,
         `Diff. in citation impacts ($\\Delta\\mathtt{MNCS}$)` = mncs_diff,
         `Research field overlap ($\\mathtt{overlap}$)` = overlap) %>%
  gather(Variable, value, -panel) %>%
  spread(panel, value) %>%
  select(Variable, `Random pairs` = Random, Collaborators, `NZ collaborators`) %>%
  slice(c(2, 5, 13, 6, 1, 8, 4, 7, 3, 11, 9, 10, 12)) %>%
  kable(align = 'lccc', booktabs = T, escape = F, format = 'latex', linesep = '') %>%
  {strsplit(., '\n')[[1]]} %>%
  {c(.[1:15], '\\midrule', .[16:20])} %>%
  write_lines('tables/panels.tex')



# Dyadic regressions ----

# Prepare regression data
reg_data <- pair_panels %>%
  gather(key, value, degree_mean, degree_diff, mncs_mean, mncs_diff) %>%
  mutate(zero = 1 * (abs(value) < tol),
         log = ifelse(abs(value) < tol, 0, log(value))) %>%
  gather(suffix, value, value, zero, log) %>%
  unite(key, c('key', 'suffix')) %>%
  mutate(key = sub('_value$', '', key)) %>%
  spread(key, value) %>%
  arrange(panel, item1, item2, year) %>%
  group_by(panel, item1, item2) %>%
  mutate(id = row_number() == 1) %>%
  ungroup() %>%
  mutate(id = cumsum(id))

# Select "full" regression data
reg_data_full <- reg_data %>%
  filter(panel == 'NZ collaborators') %>%
  select(-panel)

# How many observations?
nrow(reg_data_full)  # 247,110

# How many pairs?
n_distinct(reg_data_full$id)  # 46,052

# Check for duplicates
stopifnot(nrow(filter(count(reg_data_full, id, year), n > 1)) == 0)

# Do regressions
model_specs <- list(
  Y ~ first + second + funded,
  Y ~ first + second + funded + adjacent + degree_mean_zero + degree_mean_log + degree_diff_zero + degree_diff_log + mncs_mean_zero + mncs_mean_log + mncs_diff_zero + mncs_diff_log + overlap + I(overlap ^ 2)
)
system.time(models_full <- lapply(model_specs, glm, data = reg_data_full, family = 'binomial'))

# Anticipate FE inclusion by removing pairs with no variation in outcome variable
reg_data_rest <- reg_data_full %>%
  group_by(id) %>%
  filter(sd(Y) > 0) %>%
  ungroup()

# How many observations?
nrow(reg_data_rest)  # 124,619

# How many pairs?
n_distinct(reg_data_rest$id)  # 19,091

# How many researchers?
n_distinct(c(reg_data_rest$item1, reg_data_rest$item2))  # 4,553

# How many excluded pairs never co-authored?
reg_data_full %>%
  group_by(id) %>%
  filter(sd(Y) == 0 | n() == 1) %>%
  summarise(never = mean(Y) == 0) %>%
  ungroup() %>%
  count(never)
# # A tibble: 2 x 2
#   never     n
#   <lgl> <int>
# 1 FALSE  1625
# 2 TRUE  25336

# How many of the "always co-authored" pairs were active concurrently for one year only?
reg_data_full %>%
  group_by(id) %>%
  filter(sd(Y) == 0 | n() == 1) %>%
  filter(sum(Y == 1) == n()) %>%
  summarise(obs = n()) %>%
  count(obs == 1)
# # A tibble: 2 x 2
#   `obs == 1`     n
#   <lgl>      <int>
# 1 FALSE        899
# 2 TRUE         726

# Export descriptives
bind_rows(
  mutate(reg_data_full, key = 'full'),
  mutate(reg_data_rest, key = 'rest')
) %>%
  select(key,
         `Co-authored in year $t$ ($\\mathtt{coauth}$)` = Y,
         `First round co-members ($\\mathtt{first}$)` = first,
         `Second round co-members ($\\mathtt{second}$)` = second,
         `Funded co-members ($\\mathtt{funded}$)` = funded,
         `Adjacent in co-auth. network ($\\mathtt{adjacent}$)` = adjacent,
         `Log mean degree ($\\psi(\\overline{\\mathtt{degree}})$)` = degree_mean_log,
         `Log diff. in degrees ($\\psi(\\Delta\\mathtt{degree})$)` = degree_diff_log,
         `Log mean citation impact ($\\psi(\\overline{\\mathtt{MNCS}})$)` = mncs_mean_log,
         `Log diff. in citation impacts ($\\psi(\\Delta\\mathtt{MNCS})$)` = mncs_diff_log,
         `Research field overlap ($\\mathtt{overlap}$)` = overlap) %>%
  gather(Variable, x, -key) %>%
  group_by(Variable, key) %>%
  summarise(mean_sd = sprintf('%.3f (%.3f)', mean(x), sd(x)),
            zero = sprintf('%.3f', 100 * mean(abs(x) < tol))) %>%
  ungroup() %>%
  gather(stat, value, -Variable, -key) %>%
  unite(key, key, stat) %>%
  spread(key, value) %>%
  slice(c(2, 3, 10, 4, 1, 8, 6, 7, 5, 9)) %>%
  kable(align = 'lcccc', booktabs = T, escape = F, format = 'latex', linesep = '') %>%
  {strsplit(., '\n')[[1]]} %>%
  {c(.[2:3],
    'Variable & \\multicolumn{2}{c}{Full panel} & \\multicolumn{2}{c}{Restricted panel} \\\\',
    '\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}',
    '& Mean (s.d.) & \\% zero & Mean (s.d.) & \\% zero \\\\',
    .[5:15],
    '\\midrule',
    paste0('Observations & \\multicolumn{2}{c}{', comma(nrow(reg_data_full)), '} & \\multicolumn{2}{c}{', comma(nrow(reg_data_rest)), '} \\\\'),
    paste0('Pairs & \\multicolumn{2}{c}{', comma(n_distinct(reg_data_full$id)), '} & \\multicolumn{2}{c}{', comma(n_distinct(reg_data_rest$id)), '} \\\\'),
    .[16:17])} %>%
  write_lines('tables/descriptives.tex')

# Do regressions
system.time(models_rest <- lapply(model_specs, glm, data = reg_data_rest, family = 'binomial'))

# Estimate FE model
fe_model_spec <- Y ~ first + second + funded + adjacent + degree_mean_zero + degree_mean_log + degree_diff_zero + degree_diff_log + mncs_mean_zero + mncs_mean_log + mncs_diff_zero + mncs_diff_log | id + year
models_rest[[3]] <- feglm(
  fe_model_spec,
  data = reg_data_rest
) %>%
  biasCorr()  # Adjust for incidental parameter bias

# Define function for computing log-likelihoods
# See https://stats.stackexchange.com/a/113022/233792
get_logLik <- function(fit) {
  null_fit <- glm(Y ~ 1, data = fit$data, family = 'binomial')
  logLik(null_fit)[1] - 0.5 * (fit$deviance - fit$null.deviance)
}

# Combine models and compute log-likelihoods
models <- c(models_full, models_rest)
for (i in seq_along(models)) {
  models[[i]]$logLik <- get_logLik(models[[i]])
}

# Prepare for column of APEs
models[[6]] <- models[[5]]
model_summs <- map(models, ~coef(summary(.)))
model_summs[[6]] <- coef(summary(getAPEs(models[[5]])))

# Tabulate estimates
coef_map <- list(
  'first' = 'First round co-members ($\\mathtt{first}$)',
  'second' = 'Second round co-members ($\\mathtt{second}$)',
  'funded' = 'Funded co-members ($\\mathtt{funded}$)',
  'adjacent' = 'Adjacent in co-auth. network ($\\mathtt{adjacent}$)',
  'degree_mean_log' = 'Log mean degree ($\\psi(\\overline{\\mathtt{degree}})$)',
  'degree_diff_log' = 'Log diff. in degrees ($\\psi(\\Delta \\mathtt{degree})$)',
  'mncs_mean_log' = 'Log mean citation impact ($\\psi(\\overline{\\mathtt{MNCS}})$)',
  'mncs_diff_log' = 'Log diff. in citation impacts ($\\psi(\\Delta\\mathtt{MNCS})$)',
  'overlap' = 'Research field overlap ($\\mathtt{overlap}$)',
  'I(overlap^2)' = 'Squared research field overlap ($\\mathtt{overlap}^2$)'
)
texreg(
  models,
  booktabs = T,
  custom.coef.map = coef_map,
  custom.gof.rows = list('Pair fixed effects' = c(rep('', 4), rep('Yes', 2)),
                         'Year fixed effects' = c(rep('', 4), rep('Yes', 2)),
                         'Observations' = comma(map_dbl(models, ~nobs(.)[1])),
                         '\\quad Pairs' = comma(c(rep(n_distinct(reg_data_full$id), 2), rep(n_distinct(reg_data_rest$id), 4))),
                         'Log-likelihood' = comma(map_dbl(models, ~.$logLik), 0.001)),
  custom.model.names = c(paste0('(', 1:4, ')'), '(5a)', '(5b)'),
  digits = 3,
  no.margin = F,
  override.coef = map(model_summs, ~.[, 1]),
  override.se = map(model_summs, ~.[, 2]),
  override.pvalues = map(model_summs, ~.[, 4]),
  table = F,
  use.packages = F
) %>%
  {strsplit(., '\n')[[1]]} %>%
  {c(.[2:3],
     '\\multicolumn{7}{l}{Dependent variable: Co-authored in year $t$ ($\\mathtt{coauth}_{ijt}$)} \\\\',
     '& \\multicolumn{2}{c}{All pairs (coefficients)} & \\multicolumn{4}{c}{Pairs with variation in $\\mathtt{coauth}$} \\\\',
     '\\cmidrule(lr){2-3} \\cmidrule(lr){4-7}',
     '& & & \\multicolumn{3}{c}{Coefficients} & APEs \\\\',
     '\\cmidrule(lr){4-6}',
     .[4:25], .[27:28], '\\midrule', .[29:31], .[39], .[41]
  )} %>%
  {gsub('\\s+', ' ', .)} %>%
  trimws() %>%
  write_lines('tables/estimates.tex')

# Estimate APEs in model (1)
margins(models[[1]], data = reg_data_full) %>%
  summary() %>%
  as_tibble() %>%
  mutate_if(is.double, round, 3)
# # A tibble: 3 x 7
#   factor   AME    SE     z     p lower upper
#   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 first  0.032 0.002 14.6      0 0.027 0.036
# 2 funded 0.081 0.004 20.4      0 0.073 0.088
# 3 second 0.017 0.004  4.74     0 0.01  0.025

# Estimate APEs in model (2)
margins(models[[2]], data = reg_data_full) %>%
  summary() %>%
  as_tibble() %>%
  mutate_if(is.double, round, 3)
# # A tibble: 13 x 7
#    factor              AME    SE      z     p  lower  upper
#    <chr>             <dbl> <dbl>  <dbl> <dbl>  <dbl>  <dbl>
#  1 adjacent          0.107 0.001 71.9   0      0.104  0.11 
#  2 degree_diff_log   0.002 0.001  2.32  0.02   0      0.004
#  3 degree_diff_zero  0.004 0.005  0.932 0.351 -0.005  0.013
#  4 degree_mean_log   0.008 0.001  6.73  0      0.005  0.01 
#  5 degree_mean_zero -0.048 0.013 -3.81  0     -0.073 -0.023
#  6 first             0.027 0.002 12.6   0      0.023  0.031
#  7 funded            0.05  0.004 12.7   0      0.042  0.057
#  8 mncs_diff_log     0.004 0.001  5.19  0      0.002  0.005
#  9 mncs_diff_zero    0.127 0.051  2.49  0.013  0.027  0.228
# 10 mncs_mean_log     0.004 0.001  3.33  0.001  0.002  0.006
# 11 mncs_mean_zero   -0.097 0.069 -1.41  0.157 -0.232  0.037
# 12 overlap           0.152 0.003 54.8   0      0.146  0.157
# 13 second            0.002 0.004  0.449 0.653 -0.005  0.009



# Varying publication lags ----

# Create regression data
lag_reg_data <- vector('list', 3)
for (i in seq_along(lag_reg_data)) {
  lag_reg_data[[i]] <- reg_data %>%
    group_by(panel, id) %>%
    arrange(year) %>%
    mutate_at(names(.)[grepl('first|second|funded|adj|_mean|_diff', names(.))], lag, i) %>%
    slice(-seq_len(i)) %>%
    ungroup()
}

# Estimate models
system.time(lag_models <- map(lag_reg_data, ~feglm(fe_model_spec, data = filter(., panel == 'NZ collaborators'))))
for (i in seq_along(lag_models)) {
  lag_models[[i]] <- biasCorr(lag_models[[i]])
  lag_models[[i]]$logLik <- get_logLik(lag_models[[i]])
}

# Summarise estimates
lag_models_comb <- c(models[5], lag_models)
texreg(
  lag_models_comb,
  booktabs = T,
  custom.coef.map = coef_map[1:8],
  custom.gof.rows = list('Observations' = comma(map_dbl(lag_models_comb, ~nrow(.$data))),
                         '\\quad Pairs' = comma(map_dbl(lag_models_comb, ~.$lvls.k[1])),
                         '\\quad Years' = comma(map_dbl(lag_models_comb, ~.$lvls.k[2]), 1),
                         'Log-likelihood' = comma(map_dbl(lag_models_comb, ~.$logLik), 0.001)),
  custom.model.names = c('One', 'Two', 'Three', 'Four'),
  digits = 3,
  no.margin = F,
  table = F,
  use.packages = F
) %>%
  {strsplit(., '\n')[[1]]} %>%
  {c(.[2:3], '\\multicolumn{5}{l}{Dependent variable: Co-authored in year $t$ ($\\mathtt{coauth}_{ijt}$)} \\\\', '& \\multicolumn{4}{c}{Publication lag (years)} \\\\', '\\cmidrule(lr){2-5}', .[4:26], .[31], .[33])} %>%
  {gsub('\\s+', ' ', .)} %>%
  trimws() %>%
  write_lines('tables/estimates-lags.tex')

# Check that our results aren't due to dropping pairs with no variation in
# outcome variable when introducing each lag
lag_reg_data %>%
  map(~filter(., panel == 'NZ collaborators')) %>%
  map(~ungroup(filter(group_by(., id), sd(Y) > 0))) %>%
  map(~distinct(., id)) %>%
  map(~left_join(., reg_data_full)) %>%
  map(~feglm(fe_model_spec, data = .)) %>%
  screenreg()  # Same patterns as spec with one year lag



# Robustness test: dyadic clustering ----

fit <- lag_models[[3]]

# Identify linked observations
linked_obs <- lag_reg_data[[3]] %>%
  filter(id %in% unique(fit$data$id)) %>%
  mutate(obs = row_number()) %>%
  select(obs, item1, item2) %>%
  gather(key, prsn, -obs) %>%
  distinct(obs, prsn) %>%
  pairwise_count(obs, prsn) %>%
  filter(item1 <= item2)

# Construct cluster-robust error VCov estimate
n <- nrow(fit$data)
p <- fitted.values(fit)
u <- fit$data$Y - p  # residuals
X <- as.matrix(fit$data[, 2:13])  # model matrix, excluding FEs
k <- ncol(X)
Xid <- fit$data %>%
  select(id) %>%
  mutate(row = row_number(),
         col = dense_rank(id)) %>%
  {sparseMatrix(.$row, .$col)}
Xyear <- fit$data %>%
  as_tibble() %>%
  mutate(row = row_number(),
         col = dense_rank(year)) %>%
  {sparseMatrix(.$row, .$col)}
Xfe <- cbind(X, Xid, Xyear)  # model matrix, including FEs
bread <- solve(crossprod(Xfe, p * (1 - p) * Xfe))
P <- sparseMatrix(linked_obs$item1, linked_obs$item2, symmetric = T)
diag(P) <- 1
meat <- crossprod(Xfe, t(t(P * u) * u) %*% Xfe)
system.time(vcov <- bread %*% meat %*% bread)
#    user  system elapsed
# 1898.36    2.36 1900.92

# What are the dimensions of Xfe?
dim(Xfe)  # 51,841 x 11,426

# Tabulate robust SEs and corresponding p-values
tibble(
  term = factor(colnames(X), levels = colnames(X)),
  coef = as.numeric(coef(summary(fit))[, 1]),
  se_default = as.numeric(coef(summary(fit))[, 2]),
  se_robust = sqrt(diag(vcov[1:k,1:k]))
) %>%
  gather(type, se, se_default, se_robust) %>%
  mutate(p = 2 * pt(-abs(coef / se), n - k),
         stars = case_when(p < 0.001 ~ '$^{***}$',
                           p < 0.01 ~ '$^{**}$',
                           p < 0.05 ~ '$^*$',
                           T ~ ''),
         coef = sub('-', '$-$', sprintf('%.3f', coef)),
         se = sprintf('%.3f%s', se, stars)) %>%
  select(-p, -stars) %>%
  spread(type, se) %>%
  filter(!grepl('_zero', term)) %>%
  mutate(term = map_chr(as.character(term), ~coef_map[[.]])) %>%
  kable(col.names = c('Covariate', 'Coefficients', 'Unadjusted', 'Adjusted'),
        align = 'lccccc', booktabs = T, digits = 3, escape = F, format = 'latex', linesep = '') %>%
  {strsplit(., '\n')[[1]]} %>%
  {c(
    .[2:3],
    '& & \\multicolumn{2}{c}{Standard errors}\\\\',
    '\\cmidrule(lr){3-4}',
    .[4:15]
  )} %>%
  write_lines('tables/dyadic-clustering.tex')



# Robustness test: relaxing selection criteria ----

# Create panel of all collaborating pairs (not just NZers)
reg_data_collab <- lag_reg_data[[3]] %>%
  filter(panel == 'Collaborators') %>%
  select(-panel)

# Check for duplicates
stopifnot(nrow(filter(count(reg_data_collab, id, year), n > 1)) == 0)

# Create panel of random or collaborating pairs
reg_data_pooled <- lag_reg_data[[3]] %>%
  group_by(item1, item2, year) %>%
  arrange(panel) %>%
  filter(row_number() == n()) %>%
  ungroup()

# Check for duplicates
stopifnot(nrow(filter(count(reg_data_pooled, id, year), n > 1)) == 0)

# Combine data
selection_reg_data <- list(filter(lag_reg_data[[3]], panel == 'NZ collaborators'), reg_data_collab, reg_data_pooled)
selection_reg_data[[4]] <- reg_data_collab %>%
  group_by(id) %>%
  filter(sd(Y) > 0) %>%
  ungroup()

# Estimate models
system.time(selection_models <- map(selection_reg_data[1:3], ~glm(model_specs[[2]], data = ., family = 'binomial')))
selection_models[[4]] <- feglm(fe_model_spec, data = selection_reg_data[[4]]) %>%
  biasCorr()

# Compute log-likelihoods
for (i in seq_along(selection_models)) {
  selection_models[[i]]$logLik <- get_logLik(selection_models[[i]])
}

# Summarise estimates
selection_models_comb <- c(selection_models[1], lag_models[3], selection_models[c(2, 4)], selection_models[3])
texreg(
  selection_models_comb,
  booktabs = T,
  custom.coef.map = coef_map,
  custom.gof.rows = list('Pair fixed effects' = c('', 'Yes', '', 'Yes', ''),
                         'Year fixed effects' = c('', 'Yes', '', 'Yes', ''),
                         'Observations' = comma(map_dbl(selection_models_comb, ~nobs(.)[1])),
                         '\\quad Pairs' = comma(map_dbl(selection_models_comb, ~n_distinct(.$data$id))),
                         '\\quad Years' = comma(map_dbl(selection_models_comb, ~n_distinct(.$data$year))),
                         'Log-likelihood' = comma(map_dbl(selection_models_comb, ~.$logLik), 0.001)),
  custom.model.names = paste0('(', 1:5, ')'),
  digits = 3,
  no.margin = F,
  table = F,
  use.packages = F
) %>%
  {strsplit(., '\n')[[1]]} %>%
  {c(.[2:3],
     '\\multicolumn{5}{l}{Dependent variable: Co-authored in year $t$ ($\\mathtt{coauth}_{ijt}$)} \\\\',
     '& \\multicolumn{2}{c}{NZ collaborators} & \\multicolumn{2}{c}{Collaborators} & Pooled pairs \\\\',
     '\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}', 
     .[4],
     .[5:25],
     .[27:28],
     '\\midrule',
     .[29:32],
     .[40],
     .[42])} %>%
  {gsub('\\s+', ' ', .)} %>%
  trimws() %>%
  write_lines('tables/estimates-selection.tex')



# Save session info ----

options(width = 80)
write_lines(capture.output(session_info()), 'logs/analysis.txt')
