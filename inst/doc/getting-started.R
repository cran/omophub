## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----install------------------------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("omophub/omophub-R")

## ----auth-env-----------------------------------------------------------------
# Sys.setenv(OMOPHUB_API_KEY = "your_api_key_here")

## ----client-------------------------------------------------------------------
# library(omophub)
# 
# # Create client (reads API key from environment)
# client <- OMOPHubClient$new()
# 
# # Or provide API key explicitly
# client <- OMOPHubClient$new(api_key = "your_api_key")
# 
# # With additional options
# client <- OMOPHubClient$new(
#   api_key = "your_api_key",
#   timeout = 30,
#   max_retries = 3,
#   vocab_version = "2025.1"
# )

## ----get-concept--------------------------------------------------------------
# concept <- client$concepts$get(201826)
# print(concept$concept_name)
# # [1] "Type 2 diabetes mellitus"

## ----get-by-code--------------------------------------------------------------
# concept <- client$concepts$get_by_code("SNOMED", "44054006")
# print(concept$concept_name)
# # [1] "Type 2 diabetes mellitus"

## ----batch--------------------------------------------------------------------
# result <- client$concepts$batch(c(201826, 4329847, 1112807))
# for (concept in result$concepts) {
#   cat(sprintf("%s: %s\n", concept$concept_id, concept$concept_name))
# }

## ----search-basic-------------------------------------------------------------
# results <- client$search$basic("diabetes mellitus", page_size = 10)
# for (concept in results$data) {
#   cat(sprintf("%s: %s\n", concept$concept_id, concept$concept_name))
# }

## ----search-filters-----------------------------------------------------------
# results <- client$search$basic(
#   "heart attack",
#   vocabulary_ids = "SNOMED",
#   domain_ids = "Condition",
#   page_size = 20
# )

## ----semantic-search----------------------------------------------------------
# # Natural language search - understands clinical intent
# results <- client$search$semantic("high blood sugar levels")
# for (r in results$data$results) {
#   cat(sprintf("%s (similarity: %.2f)\n", r$concept_name, r$similarity_score))
# }

## ----semantic-filtered--------------------------------------------------------
# results <- client$search$semantic(
#   "heart attack",
#   vocabulary_ids = "SNOMED",
#   domain_ids = "Condition",
#   threshold = 0.5
# )

## ----semantic-all-------------------------------------------------------------
# all_results <- client$search$semantic_all(
#   "chronic kidney disease",
#   page_size = 50,
#   max_pages = 5,
#   progress = TRUE
# )
# print(nrow(all_results))

## ----similar-by-id------------------------------------------------------------
# # Find concepts similar to Type 2 diabetes mellitus
# similar <- client$search$similar(concept_id = 201826)
# for (s in similar$similar_concepts) {
#   cat(sprintf("%s (score: %.2f)\n", s$concept_name, s$similarity_score))
# }

## ----similar-by-query---------------------------------------------------------
# # Semantic similarity (neural embeddings)
# similar <- client$search$similar(
#   query = "high blood pressure",
#   algorithm = "semantic"
# )
# 
# # Lexical similarity (string matching)
# similar <- client$search$similar(
#   query = "high blood pressure",
#   algorithm = "lexical"
# )
# 
# # Hybrid (combined - default)
# similar <- client$search$similar(
#   query = "high blood pressure",
#   algorithm = "hybrid",
#   include_scores = TRUE,
#   include_explanations = TRUE
# )

## ----autocomplete-------------------------------------------------------------
# suggestions <- client$concepts$suggest("diab", page_size = 5)
# for (s in suggestions$suggestions) {
#   print(s$suggestion)
# }

## ----pagination-manual--------------------------------------------------------
# # First page
# results <- client$search$basic("diabetes", page = 1, page_size = 50)
# 
# # Check pagination info
# print(results$meta$total_items)
# print(results$meta$has_next)
# 
# # Get next page if available
# if (isTRUE(results$meta$has_next)) {
#   page2 <- client$search$basic("diabetes", page = 2, page_size = 50)
# }

## ----pagination-auto----------------------------------------------------------
# all_results <- client$search$basic_all(
#   "diabetes",
#   page_size = 100,
#   max_pages = 5,
#   progress = TRUE
# )
# 
# # Results are a tibble
# print(nrow(all_results))
# print(names(all_results))

## ----ancestors----------------------------------------------------------------
# result <- client$hierarchy$ancestors(201826, max_levels = 3)
# for (ancestor in result$ancestors) {
#   print(ancestor$concept_name)
# }

## ----descendants--------------------------------------------------------------
# result <- client$hierarchy$descendants(201826, max_levels = 2)
# for (descendant in result$descendants) {
#   print(descendant$concept_name)
# }

## ----mappings-----------------------------------------------------------------
# result <- client$mappings$get(201826)
# for (mapping in result$mappings) {
#   cat(sprintf("%s: %s\n",
#               mapping$target_vocabulary_id,
#               mapping$target_concept_name))
# }

## ----mappings-filter----------------------------------------------------------
# result <- client$mappings$get(
#   201826,
#   target_vocabulary = "ICD10CM"
# )

## ----error-handling-----------------------------------------------------------
# tryCatch(
#   {
#     concept <- client$concepts$get(999999999)
#   },
#   omophub_not_found = function(e) {
#     message("Concept not found: ", e$message)
#   },
#   omophub_api_error = function(e) {
#     message("API error: ", e$message)
#   }
# )

