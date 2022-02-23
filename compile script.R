library(rjson)

# load the various csv files we need
infos <- read.csv("info.csv", stringsAsFactors = FALSE)
nodes <- read.csv("node.csv", stringsAsFactors = FALSE)
questions <- read.csv("question.csv", stringsAsFactors = FALSE)

# subset the infos table
final_decisions <- infos[infos$type == "face_answer_2",]
initial_decisions <- infos[infos$type == "face_answer_1",]

# empty data vectors we will fill
chose_face_1 <- initially_chose_face_1 <- ppt_id <- score <- vector(length=nrow(final_decisions))
failed <- n_dems <- vector(length=nrow(final_decisions))
orientation <- sex <- gender <- preference <- age <- birth_country <- current_country <- rep(-666, nrow(final_decisions))

d1f1 <- d2f1 <- d3f1 <- d4f1 <- d5f1 <- d6f1 <- d7f1 <- d8f1 <- d9f1 <- rep(666, nrow(final_decisions))
d1s <- d2s <- d3s <- d4s <- d5s <- d6s <- d7s <- d8s <- d9s <- rep(0, nrow(final_decisions))

# go through every final decision
for (i in 1:nrow(final_decisions)) {
  final_decision <- final_decisions[i,]
  
  # get its network and node, and use these to find the corresponding initial decision
  net <- final_decision$network_id
  node <- final_decision$origin_id
  possible_initial_decisions <- initial_decisions[
    initial_decisions$origin_id == node &
    initial_decisions$network_id == net &
    initial_decisions$id < final_decision$id,
  ]
  initial_decision <- possible_initial_decisions[
    possible_initial_decisions$id == max(possible_initial_decisions$id),
  ]
  
  # from this, idenrtify face 1 and face 2
  initial_details <- fromJSON(initial_decision$details)
  face1 <- initial_details$face1
  face2 <- initial_details$face2
  
  # now record whether the final decision was for face 1 or 2
  if (final_decision$contents == face1) {
    chose_face_1[i] <- 1
  } else if (final_decision$contents == face2) {
    chose_face_1[i] <- 0
  } else {
    chose_face_1[i] <- -666
  }
  
  # now record whether the initial decision was for face 1 or face 2
  if (initial_decision$contents == face1) {
    initially_chose_face_1[i] <- 1
  } else if (initial_decision$contents == face2) {
    initially_chose_face_1[i] <- 0
  } else {
    initially_chose_face_1[i] <- -666
  }
  
  # now record the participants id
  ppt_id[i] <- nodes[nodes$id == node,]$participant_id
  
  # use ppt_id to get their preference
  pref <- questions[
    questions$question=="preference" &
    questions$participant_id == ppt_id[i],
    ]$response
  # for some reason some people have duplicate preferences
  # this makes sure they are at least unique (they are)
  pref <- unique(pref)
  if (pref == "women") {
    preference[i] <- 0
  } else if (pref == "men") {
    preference[i] <- 1
  } else if (pref == "both") {
    preference[i] <- 2
  } else {
    preference[i] <- -666
  }
  
  # now let's get their other questionnaire responses
  survey <- questions[
    questions$question=="questionnaire" &
    questions$participant_id == ppt_id[i],
  ]$response
  
  if (length(survey) > 0) {
    survey <- fromJSON(survey)
    age[i] <- as.numeric(survey$age)
    current_country[i] <- as.character(survey$current_country)
    birth_country[i] <- as.character(survey$birth_country)
    if (survey$sex == "Female") {
      sex[i] <- 0
    } else if (survey$sex == "Male") {
      sex[i] <- 1
    } else {
      sex[i] <- 2
    }
    if (survey$gender == "Female") {
      gender[i] <- 0
    } else if (survey$gender == "Male") {
      gender[i] <- 1
    } else {
      gender[i] <- 2
    }
    if (survey$orientation == "Heterosexual") {
      orientation[i] <- 0
    } else {
      orientation[i] <- 1
    }
  }

  # and their score
  score[i] <- fromJSON(nodes[nodes$id == node,]$details)$score
  
  # whether or not the node finished
  failed[i] <- (final_decision$failed == "t")*1
  
  # now on to the social information
  # first get everyone in the group
  group <- fromJSON(final_decision$details)
  # but remove the node themselves, to get the demonstrators
  index_of_self <- -666
  for (j in 1:length(group)) {
    if (group[[j]]$id == node) {
      index_of_self <- j
    }
  }
  dems <- group[-index_of_self]
  
  # from this the number of demonstrators is easy
  n_dems[i] <- length(dems)
  
  # we can also get the decision and score of each demonstrator
  if (n_dems[i] >= 1) {
    if (dems[[1]]$face == face1) {
      d1f1[i] <- 1
    } else if (dems[[1]]$face == face2) {
      d1f1[i] <- 0
    } else {
      d1f1[i] <- -666
    }
    d1s[i] <- dems[[1]]$score
  }
  if (n_dems[i] >= 2) {
    if (dems[[2]]$face == face1) {
      d2f1[i] <- 1
    } else if (dems[[2]]$face == face2) {
      d2f1[i] <- 0
    } else {
      d2f1[i] <- -666
    }
    d2s[i] <- dems[[2]]$score
  }
  if (n_dems[i] >= 3) {
    if (dems[[3]]$face == face1) {
      d3f1[i] <- 1
    } else if (dems[[3]]$face == face2) {
      d3f1[i] <- 0
    } else {
      d3f1[i] <- -666
    }
    d3s[i] <- dems[[3]]$score
  }
  if (n_dems[i] >= 4) {
    if (dems[[4]]$face == face1) {
      d4f1[i] <- 1
    } else if (dems[[4]]$face == face2) {
      d4f1[i] <- 0
    } else {
      d4f1[i] <- -666
    }
    d4s[i] <- dems[[4]]$score
  }
  if (n_dems[i] >= 5) {
    if (dems[[5]]$face == face1) {
      d5f1[i] <- 1
    } else if (dems[[5]]$face == face2) {
      d5f1[i] <- 0
    } else {
      d5f1[i] <- -666
    }
    d5s[i] <- dems[[5]]$score
  }
  if (n_dems[i] >= 6) {
    if (dems[[6]]$face == face1) {
      d6f1[i] <- 1
    } else if (dems[[6]]$face == face2) {
      d6f1[i] <- 0
    } else {
      d6f1[i] <- -666
    }
    d6s[i] <- dems[[6]]$score
  }
  if (n_dems[i] >= 7) {
    if (dems[[7]]$face == face1) {
      d7f1[i] <- 1
    } else if (dems[[7]]$face == face2) {
      d7f1[i] <- 0
    } else {
      d7f1[i] <- -666
    }
    d7s[i] <- dems[[7]]$score
  }
  if (n_dems[i] >= 8) {
    if (dems[[8]]$face == face1) {
      d8f1[i] <- 1
    } else if (dems[[8]]$face == face2) {
      d8f1[i] <- 0
    } else {
      d8f1[i] <- -666
    }
    d8s[i] <- dems[[8]]$score
  }
  if (n_dems[i] >= 9) {
    if (dems[[9]]$face == face1) {
      d9f1[i] <- 1
    } else if (dems[[9]]$face == face2) {
      d9f1[i] <- 0
    } else {
      d9f1[i] <- -666
    }
    d9s[i] <- dems[[9]]$score
  }
}

# relabel ppt_ids to avoid missing participants, ensures numbering is continuous
ppt_id <- match(ppt_id, unique(ppt_id))
