library(jsonlite)

# build_environment <- function(data) {
#   env_list <- list()
#   
#   for (elm in data$elms) {
#     elm_name <- elm$elm
#     elm_qty  <- elm$qty
#     elm_instances <- vector("list", elm_qty)
#     
#     for (i in seq_len(elm_qty)) {
#       instance <- list()
#       
#       if (!is.null(elm$sub_elms)) {
#         resource_vector <- c()
#         
#         for (sub in elm$sub_elms) {
#           if (is.null(sub$sub_elm)) {
#             for (res in sub$resources) {
#               res_type <- res$type
#               res_name <- if ("name" %in% names(res)) res$name else res$resource
#               for (j in seq_len(res$qty)) {
#                 res_id <- paste("c", elm_name, res_type, res_name, j, paste0("t", i), sep = "_")
#                 resource_vector <- c(resource_vector, res_id)
#               }
#             }
#           } else {
#             sub_name <- sub$sub_elm
#             sub_vector <- c()
#             for (res in sub$resources) {
#               res_name <- if ("name" %in% names(res)) res$name else res$resource
#               for (j in seq_len(res$qty)) {
#                 res_id <- paste("c", elm_name, sub_name, res_name, j, paste0("t", i), sep = "_")
#                 sub_vector <- c(sub_vector, res_id)
#               }
#             }
#             instance[[sub_name]] <- sub_vector
#           }
#         }
#         
#         if (elm_name == "r1") {
#           instance <- resource_vector
#         }
#       }
#       
#       if (!is.null(elm$beds)) {
#         for (bed in elm$beds) {
#           bed_type <- bed$name
#           bed_ids <- paste0("b_", elm_name, "_", bed_type, "_", seq_len(bed$qty), "_t", i)
#           instance[[paste0(bed_type, "_bed")]] <- bed_ids
#         }
#       }
#       
#       elm_instances[[i]] <- instance
#     }
#     
#     env_list[[elm_name]] <- elm_instances
#   }
#   
#   # ── Build transports list ──
#   transports_list <- list()
#   if (!is.null(data$transports)) {
#     for (vehicle in data$transports) {
#       transports_list[[vehicle$name]] <- paste0("t_", vehicle$name, "_", seq_len(vehicle$qty))
#     }
#   }
#   
#   # ── Process pops into a named list ──
#   pops_list <- list()
#   if (!is.null(data$pops)) {
#     pops_list <- setNames(
#       lapply(data$pops, function(p) p$count),
#       sapply(data$pops, function(p) p$name)
#     )
#   }
#   
#   return(list(
#     pops = pops_list,
#     elms = env_list,
#     transports = transports_list
#   ))
# }

build_environment <- function(data) {
  env_list <- list()
  
  for (elm in data$elms) {
    elm_name <- elm$elm
    elm_qty  <- elm$qty
    elm_instances <- vector("list", elm_qty)
    
    for (i in seq_len(elm_qty)) {
      instance <- list()
      
      if (!is.null(elm$sub_elms)) {
        resource_vector <- c()
        
        for (sub in elm$sub_elms) {
          # ── No sub_elm name: flat resources for e.g. R1 teams ──
          if (is.null(sub$sub_elm)) {
            for (res in sub$resources) {
              res_type <- res$type
              res_name <- if ("name" %in% names(res)) res$name else res$resource
              for (j in seq_len(res$qty)) {
                res_id <- paste("c", elm_name, res_type, res_name, j, paste0("t", i), sep = "_")
                resource_vector <- c(resource_vector, res_id)
              }
            }
          } else {
            # ── Named sub_elm: supports multiple teams ──
            sub_name <- sub$sub_elm
            sub_qty <- if (!is.null(sub$qty)) sub$qty else 1
            
            sub_teams <- vector("list", sub_qty)
            
            for (team_index in seq_len(sub_qty)) {
              sub_vector <- c()
              for (res in sub$resources) {
                res_name <- if ("name" %in% names(res)) res$name else res$resource
                for (j in seq_len(res$qty)) {
                  res_id <- paste("c", elm_name, sub_name, team_index, res_name, j, paste0("t", i), sep = "_")
                  sub_vector <- c(sub_vector, res_id)
                }
              }
              sub_teams[[team_index]] <- sub_vector
            }
            
            instance[[sub_name]] <- sub_teams
          }
        }
        
        if (elm_name == "r1") {
          instance <- resource_vector
        }
      }
      
      # ── Beds ──
      if (!is.null(elm$beds)) {
        for (bed in elm$beds) {
          bed_type <- bed$name
          bed_ids <- paste0("b_", elm_name, "_", bed_type, "_", seq_len(bed$qty), "_t", i)
          instance[[paste0(bed_type, "_bed")]] <- bed_ids
        }
      }
      
      elm_instances[[i]] <- instance
    }
    
    env_list[[elm_name]] <- elm_instances
  }
  
  # ── Build transports list ──
  transports_list <- list()
  if (!is.null(data$transports)) {
    for (vehicle in data$transports) {
      transports_list[[vehicle$name]] <- paste0("t_", vehicle$name, "_", seq_len(vehicle$qty))
    }
  }
  
  # ── Process pops into a named list ──
  pops_list <- list()
  if (!is.null(data$pops)) {
    pops_list <- setNames(
      lapply(data$pops, function(p) p$count),
      sapply(data$pops, function(p) p$name)
    )
  }
  
  return(list(
    pops = pops_list,
    elms = env_list,
    transports = transports_list
  ))
}

# ── Load elms from file ──
load_elms <- function(path) {
  json_data <- fromJSON(path, simplifyVector = FALSE)
  build_environment(json_data)
}