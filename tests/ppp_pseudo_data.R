# Script to generate data for testing the PPP.  
# Generate 10 species each with 100 actions each.  

a.data <- data.frame(
  action_id = 1:1000,
  species_id = rep(1:10, each = 100),
  TaxaCodeText = rep(LETTERS[1:10], each = 100),
  TaxaCode = rep(21:30, each = 100),
  ProjectText = rep(letters[1:10], each = 100),
  ProjectCode = rep(-c(1:10), each = 100),
  "Method" = "Method",
  Method1Text = "Method1Text",
  Method2Text = "Method2Text",
  Method3Text = "Method3Text",
  Method4Text = "Method4Text",
  Period1Cost = 1,
  Period1Sequence = paste(1:50, sep = "", collapse = ","),
  Period2Cost = 0,
  Period2Sequence = paste(1:50, sep = "", collapse = ","),
  Period3Cost = 0,
  Period3Sequence = paste(1:50, sep = "", collapse = ","),
  ManagementSiteInputSuccess = 100,
  ManagementSiteOutputSuccess = 100,
  ActionObjectiveSuccessProbability = 100
)

b.data <- data.frame(
  species_id = 1:10,
  TaxaText = LETTERS[1:10],
  TaxaCode = 21:30,
  ProjectText = letters[1:10],
  sciname = do.call(paste, list(LETTERS[1:10], letters[1:10], sep = "")),
  bene_with_action = 100,
  bene_no_action = 0,
  ProjectCode = -c(1:10)
)

o.data <- data.frame(
action_id = 1:10,
overlap_id = 1:10
)

w.data <- data.frame(
species_id = 1:10,
species_type = "Bat",
sciname = do.call(paste, list(LETTERS[1:10], letters[1:10], sep = "")),
genus = do.call(paste, list("Genus", letters[1:10], sep = "_")),
Bs = 1,
Ts = 1,
family = do.call(paste, list("Family", letters[1:10], sep = "_")),
Bg = 1,
Tf = 1,
order = do.call(paste, list("Order", letters[1:10], sep = "_")),
Bf = 1,
Tf = 1,
Endem.spp = 0,
Endem.gen = 0,
Endem.fam = 0,
weight_value = 1
)


all.costs <- data.frame(cbind("action_id" = 1:1000, as.data.frame(matrix(1, 1000, T))))  # (No. actions x T) True/false for if an action is costed
names(all.costs)[2:(T+1)] <- do.call(paste, list("Y", 1:50, sep = ""))
