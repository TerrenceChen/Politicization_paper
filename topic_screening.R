library(dplyr)

topic <- readRDS("unique_terms_350.rds")

length(unique(topic$topic))

## ---- 1. Drop topics with 5 or fewer unique terms ----
## Too few terms to judge whether the topic is coherent/usable.
topic_n_excluded <-
  topic %>%
  group_by(topic) %>%
  summarise(count = n()) %>%
  filter(count <= 5) %>%
  pull(topic)

## ---- 2. Drop topics reviewed and rejected by topic, grouped by reason ----
names_topics <- c(3, 11, 19, 22, 27, 31, 32, 36, 44, 47, 50, 68, 75, 86, 90,
  91, 98, 105, 110, 114, 121, 125, 127, 129, 130, 140, 142, 159, 161, 163,
  180, 183, 191, 211, 237, 245, 246, 255, 256, 257, 263, 266, 269, 275, 284,
  286, 297, 301, 312, 324, 329, 336, 341, 349)        ## mostly proper names, no thematic content
incoherent_topics <- c(18, 34, 41, 61, 65, 67, 93, 96, 103, 124, 138, 139, 145, 151, 154,
  157, 166, 187, 200, 202, 223, 226, 234, 240, 242, 244, 247, 252, 262, 264,
  267, 273, 287, 288, 290, 293, 294, 299, 309, 315, 327, 330, 323, 333, 338,
  340)  ## incoherent, too generic, or not substantively relevant (e.g. jargon-heavy/procedural clusters)
newspaper_topics <- c(38, 126, 220)  ## newspaper structural sections (corrections, letters, etc.)
recent_topics <- c(62, 79, 279, 334, 343)  ## dominated by very recent (post-2020) events

manually_dropped_topics <- unique(c(names_topics, incoherent_topics, newspaper_topics, recent_topics))

topic <-
  topic %>%
  filter(!topic %in% topic_n_excluded) %>%
  filter(!topic %in% manually_dropped_topics)

length(unique(topic$topic))  ## 136 topics remain

## ---- 3. Drop topics later found to have too few usable terms overall ----
topics_too_few_terms <- c(24, 314, 342)  ## Regulation, GovernmentAid, Generational

topic <-
  topic %>%
  filter(!topic %in% topics_too_few_terms)

length(unique(topic$topic))  ## 133 topics remain

## ---- 4. Attach a thematic label to each surviving topic ----
## topic_labels.csv already carries the final, reviewed label for every topic.
topic_labels <- read.csv("input_files/topic_labels.csv")

topic <-
  topic %>%
  left_join(topic_labels, by = "topic")

## ---- 5. Mark individual off-topic/noisy/proper-noun terms as "drop" ----
## (junk rows, terms too generic to be topic-specific, and stray proper
## nouns within an otherwise coherent topic are all treated the same way:
## marked "drop" rather than physically removed, so the file keeps a full
## record of every screening decision)
dropped_terms <- c("#metoo", "1-0", "1-year", "10-year-treasury-note", "10-yr", "2-0",
  "20s", "239-6200", "3-2", "30-year-mortgage-commitments", "30-yr", "30s",
  "3m", "40s", "5-10", "50s", "6-1", "6-12",
  "6-2", "6-3", "6-4", "60s", "7-5", "7-6",
  "70s", "a.i.g", "a.t", "abbas", "abdul-jabbar", "abu",
  "aca", "acceptances", "accidents", "accommodations", "accusers", "ackman",
  "acquaintance", "acres", "acres-zoned", "adidas", "adj", "adjustable",
  "administrators", "aegis", "affleck", "affordable-care-act", "afghans", "agassi",
  "aguirre", "aig", "airbnb", "airbus", "aires", "airstrikes",
  "al-assad", "albright", "alcohol-tobacco", "alibaba", "alito", "all-star",
  "allege", "alleged", "allegedly", "allegheny", "alleging", "allergan",
  "allergies", "altria", "alvarez", "alzheimer", "ama", "amnesty",
  "among-incidents-reported", "amr", "amusement", "anaheim", "analysts-expectations", "anchorage",
  "andersen", "andrews", "angioplasty", "anheuser-busch", "ankara", "annualized",
  "annuities", "annuity", "antenna", "anthrax", "antiques", "antitrust",
  "antonin", "ants", "arbitration", "area-theaters", "area-thefts-break-ins", "armco",
  "arpaio", "arteries", "artery", "arthur-andersen", "arthur-ashe", "ashe",
  "ashore", "assad", "assaulted", "astros", "athlete", "athletes",
  "atlantic-city", "atoms", "attorney-office", "audited", "augusta", "autism",
  "averaged-points", "avon", "backstrom", "bader", "baggage", "bailey",
  "bailout", "baldwin", "balkans", "baltic", "bankers-association", "bankers-trust",
  "baquet", "barcelona", "barclay", "barclays", "barnes-noble", "bartender",
  "basel", "bashar", "basketball-team", "bassist", "bathgate", "baths",
  "baucus", "bayer", "bcci", "beal", "bear-stearns", "becerra",
  "beck", "becker", "beckett", "beckham", "belgrade", "belichick",
  "bellies", "ben-bernanke", "bernanke", "bethlehem", "bettman", "billion-yen",
  "binance", "black-tie", "blade", "bleach", "bledsoe", "blizzard",
  "bloomingdale", "blue-devils", "blue-spruce", "bluth", "boating", "boesky",
  "bombings", "bondholders", "bookscan", "booster", "border-crossings", "borg",
  "bork", "borrow", "bosnia", "bosnian", "bourne", "bradlee",
  "brady", "brand-name", "brando", "braniff", "breach", "breaches",
  "breast", "breland", "brendan-hoffman", "brewers", "breyer", "brezhnev",
  "bribes", "brill", "bristol-myers", "bristol-myers-squibb", "brius", "brotherhood",
  "bryant", "builders", "bull-market", "bullets", "burger-king", "bushel",
  "c-o", "cable-wireless", "cadets", "caesars", "cakes", "calipari",
  "campaign-rally", "canseco", "capital-beltway", "capote", "carbide", "cardinals",
  "cardiologist", "cargill", "carnival", "cartel", "cartels", "castile",
  "casualty", "cell", "cftc", "champlain", "chaplin", "charitable",
  "charity", "chemical-weapons", "chen", "cher", "chernobyl", "chess",
  "china’s", "chinatown", "choir", "choreographer", "choreographers", "chorus",
  "citrus", "civilian", "civilians", "clark", "clemens", "clinics",
  "cloning", "clorox", "closing-price", "clyburn", "cnbc", "coast-guard",
  "cocktails", "coin", "coins", "colbert", "coldwell", "coliseum",
  "collective-bargaining-agreement", "columbine", "comics", "commerce-committee", "communications-inc", "complaint",
  "composite-index", "composite-trading", "conagra", "conan", "condoms", "confirmation-hearings",
  "connors", "conoco", "conseco", "contestant", "contestants", "continental-illinois",
  "controllers", "convertible", "cookware", "coors", "coppola", "cornyn",
  "coronary", "corporate-bonds", "cosmetics", "cosmos", "cost-of-living", "coty",
  "coughlin", "counteroffensive", "counterterrorism", "covid-19-vaccine", "coyotes", "crash",
  "crashes", "crater", "creamer", "creditor", "creditors", "crimes",
  "criminal-complaint", "croatia", "cronkite", "crossword", "cruelty", "csfb",
  "csi", "cubic-feet", "cubs", "customs", "cvs", "cw",
  "cyber", "d-calif", "d-md", "d-w.va", "d'antoni", "daimler",
  "daimlerchrysler", "dancer", "dances", "dargis", "darman", "darwin",
  "date-appearing", "deaf", "debt-load", "debts", "decliners", "dees",
  "default", "deferral", "dejoy", "delhi", "delinquent", "delivery-within-days",
  "demille", "democratic-presidential-nomination", "denomination", "dental", "depositary", "depreciation",
  "destinations", "detergent", "devos", "dial", "diary", "diller",
  "dimon", "dingell", "directv", "disneyland", "disorder", "disqualification",
  "dissent", "djokovic", "docks", "dodd", "dodgers", "domenici",
  "domino", "donaldson", "dorrance", "doses", "doug-mills", "dough",
  "downed", "dr-gridlock", "dreamworks", "drexel", "driesell", "drugstore",
  "drummer", "drunk", "du", "duct", "dumps", "duplex",
  "duran", "durant", "e-mail", "e-mails", "e85", "earmarks",
  "earners", "eastwood", "ebola", "ec", "ecb", "echostar",
  "eclipse", "edmonton", "efficacy", "egbert", "egging", "eisner",
  "el-paso", "electors", "elliman", "ellington", "ellison", "els",
  "embryo", "emerging-market", "emerging-markets", "emi", "ended-sept", "english-subtitles",
  "enrollment", "entergy", "entry-level", "equifax", "erdogan", "ernst-young",
  "eruption", "esposito", "estee-lauder", "ethanol", "european-central-bank", "eurozone",
  "ev", "evers", "evert", "examiners", "excise", "explosion",
  "exporter", "extremist", "f1", "face-value", "falco", "falwell",
  "fares", "farrar", "farris", "farrow", "fax", "federalism",
  "federalist", "federated", "federer", "fehr", "fell-cents", "fenway",
  "fertility", "fertilization", "fey", "fielder", "fighters", "fights",
  "filers", "financial-institutions", "five-bedroom", "fixed-income", "fletcher", "fliers",
  "floyd", "fluoride", "football", "football-league", "forecasters", "forgiveness",
  "fort-myers", "four-bedroom", "fox-news", "fragrance", "fragrances", "franchisees",
  "fraser", "frazier", "free-trade", "freight", "friedgen", "frist",
  "front-door", "frye", "fslic", "full-year", "funk", "fusion",
  "fx", "gainers", "gallaudet", "gallon", "gallons", "gangster",
  "garnett", "gawker", "geico", "geithner", "gen", "general-electric",
  "generic", "geneva", "gensler", "geopolitical", "george-floyd", "geriatric",
  "gershkovich", "getty", "giamatti", "gibbs", "girardi", "giroux",
  "glaxo", "glimmers", "global-crossing", "globalization", "goldwyn", "gomez",
  "goodell", "gorbachev", "gorman", "gorsuch", "grader", "graders",
  "graf", "granite", "grasso", "grazing", "great-depression", "greenberg",
  "greyhound", "groceries", "grocery", "grubhub", "gruden", "gte",
  "guantanamo", "guests", "guggenheim", "guilty-plea", "gulf", "gulf-coast",
  "gunman", "gunpoint", "guo", "hacker", "hackers", "hacking",
  "haines", "haiti", "halliburton", "hamlet", "han", "hanoi",
  "hardaway", "hardcover", "harpercollins", "harrah", "haskel", "hastert",
  "hawley", "hayden", "haze", "hazing", "hazy", "hca",
  "healthcare", "healthcare.gov", "hearst", "heineken", "heinz", "helene",
  "heller", "hepburn", "hernandez", "herrmann", "hertz", "heterosexual",
  "high-grade-unsecured-notes", "hillbilly", "hiltzik", "hinckley", "hindu", "hingis",
  "historic-preservation", "hitters", "hollings", "holly", "holmes", "holston",
  "holtby", "home-loan-mortgage-corp", "homicide", "homicides", "hornets", "hors",
  "hospice", "hospitalization", "hospitalizations", "hostages", "houghton", "hour-minutes",
  "house-speaker-nancy-pelosi", "housewives", "howe", "hoyer", "hu", "huang",
  "huawei", "huber", "hudson", "hugo", "hulu", "hunters",
  "hunting", "hunts", "hurricane-katrina", "hurricane-milton", "huskies", "hussein",
  "i-66", "i-95", "iac", "ideological", "iger", "illicit",
  "illustrations", "impeachment-trial", "implants", "import", "imported", "improper",
  "imus", "incest", "incitement", "index-fell", "india", "industrial-average",
  "industries-inc", "infertility", "inflation-reduction", "informant", "information-call", "inman",
  "insolvent", "insurrection", "interest-payments", "interior-department", "interior-secretary", "interns",
  "internship", "internships", "interrogation", "intraday", "inventors", "inversely",
  "investigation", "iridium", "isherwood", "islamist", "itt", "ivf",
  "j.p-morgan-chase", "janet-yellen", "japan-switzerland-britain", "jealousy", "jefferson", "jerseys",
  "jetblue", "jeter", "jin", "jinping", "joe-biden-delivers", "joe-manchin",
  "johnson-johnson", "joint-session", "jorge", "jose", "jpmorgan-chase", "judge-alito",
  "junk-bonds", "juul", "kaepernick", "kagan", "kaiser", "kashmir",
  "katrina", "kb", "kellogg", "kemper", "kenny", "kerkorian",
  "kgb", "khrushchev", "kidd", "kidder", "kili", "kimmel",
  "kite", "kizer", "knots", "kobe", "kohlberg", "kolzig",
  "komen", "kong", "kosher", "kosovo", "kozlowski", "kraft",
  "kramer", "kronor", "krulwich", "ku", "kuchar", "labor-statistics",
  "lampert", "landau", "landowners", "landry", "laptop-computer", "latimer",
  "latter-day", "lauder", "laundering", "lavelle", "lawsuit", "layden",
  "leagues", "lebow", "lebron", "legal-fees", "legalization", "lego",
  "lehman", "lendl", "lennar", "leno", "leonard", "leonsis",
  "lesher", "letterman", "leveraged", "levitt", "li", "liabilities",
  "librarians", "lice", "liens", "life-insurance", "liggett", "lighthizer",
  "lilco", "lilly", "limbaugh", "lincoln-memorial", "liquidation", "listeners",
  "listing-agent", "liu", "liv", "liverpool", "lives-matter", "lockout",
  "longnecker", "lopez", "lópez", "low-tonight", "ls", "lsu",
  "ltv", "lu", "lundqvist", "lung", "lungs", "lvmh",
  "lyft", "lyle", "lyon", "machine-tool", "mack", "madison-square-garden",
  "maduro", "magnum", "mahomes", "mail-order", "mailer", "maj",
  "major-corporations", "majors", "malaria", "malaysia", "malaysian", "male",
  "male-pedestrian", "males", "malone", "malpractice", "mammals", "manchester",
  "manchin", "manfred", "manuel", "manville", "mao", "marketer",
  "marketplaces", "marlins", "marquez", "marshals", "marvis", "masonry",
  "matches", "mayorkas", "maytag", "mayweather", "mca", "mcconnell",
  "mccormick", "mcenroe", "mcilroy", "mckinney", "mcmullen", "mdma",
  "mead", "median", "mediate", "medtronic", "mei", "mellon",
  "memorials", "meng", "merc", "mesa", "messages", "messiah",
  "messner", "metlife", "mf", "mgm", "mgm-grand", "mgm-ua",
  "michael-jordan", "mickelson", "mid", "mid-50s", "mid-60s", "middle-income",
  "miers", "mifflin", "miguel", "mikhail-gorbachev", "militant", "militants",
  "militia", "militias", "milky", "milligrams", "million-tons", "milosevic",
  "ministers", "minutes-contains", "missile", "missionary", "moderate-income", "moderna",
  "modi", "monday-saturday", "moneymaker", "monticello", "moonves", "morales",
  "mormon", "morris", "moslem", "mower", "mph", "ms-pelosi",
  "msg", "mubarak", "mud", "muhammad", "multiples", "murdoch",
  "musharraf", "mustard", "mx", "mystics", "nabokov", "nadal",
  "nasd", "nasdaq-stock-market", "nast", "nationals", "nato", "natural-history",
  "navalny", "navratilova", "needles", "negotiable", "neiman-marcus", "nestle",
  "net-loss", "netanyahu", "netflix", "neurons", "nevada", "new-home",
  "ng", "nichols", "nicklaus", "nicotine", "nike", "nikkei",
  "nitrogen", "nomination", "nominee", "nominees", "nonfiction", "nonperforming",
  "nonprofits", "nonviolent", "noriega", "north-atlantic-treaty", "northbound", "ntsb",
  "nudity", "nursing-home", "nursing-homes", "o'keeffe", "o'neal", "o'rourke",
  "obamacare", "obrador", "oct-property", "odessa", "office-buildings", "olds",
  "olsen", "omicron", "one-bedroom", "open-daily", "opened-fire", "organizers",
  "osha", "ounce", "ounces", "outfielder", "outflows", "outs",
  "ovechkin", "owed", "p-g", "p.s", "padilla", "painewebber",
  "paperback", "paramedics", "parcells", "pardon", "pareles", "parked",
  "particle", "passengers", "passport", "patrick-ewing", "patriots", "paulson",
  "payday", "peat", "pedestrian", "pedro", "pelosi", "penguin",
  "pensacola", "pension-plan", "pension-plans", "pensions", "pequot", "performance-enhancing",
  "period-scoring", "perle", "peterson", "petraeus", "pg-13", "pg-13-minutes",
  "pg-e", "philanthropic", "philanthropy", "philip-morris", "phone-number", "pickens",
  "pickleball", "pills", "pillsbury", "pirate", "pitino", "playoff",
  "plea", "plutonium", "police-brutality", "politico", "pollin", "pond",
  "pont", "porch", "porn", "posner", "postseason", "pound",
  "prebon", "premier-league", "prenatal", "preservationists", "president-donald-trump", "president-vladimir-putin",
  "president-vladimir-v-putin", "presser", "previously-announced", "prewar", "procter", "profanity",
  "profit-fell", "profit-taking", "prom", "promoter", "pronounced-dead", "prostate-cancer",
  "prostitute", "prostitutes", "prostitution", "protestant", "province", "proxmire",
  "prying", "pryor", "psychedelics", "ptcldv", "ptcldy", "public-offering",
  "purdue", "purple-line", "putin", "puzzles", "qatar", "quad",
  "quake", "quaranta", "quarterly-dividend", "quattrone", "quotations", "qvc",
  "radiation", "radioactive", "rainfall", "rajaratnam", "ramos", "rampage",
  "ranch", "ranchers", "ransomware", "rapp", "raptors", "raskin",
  "rath", "rcldy", "realty", "rear-door", "rear-window", "rebates",
  "rebel", "rebels", "receiver", "recruiters", "redstone", "regulatory-commission",
  "rehnquist", "reid", "remington", "remodeling", "renault", "reno",
  "rep", "repay", "repeal", "reported-missing", "represent-actual-transactions", "reproductions",
  "republican-presidential-nominee", "resale", "respiratory", "restructure", "retirees", "retirement-benefits",
  "revco", "revenue-rose", "reynolds", "rf", "rfk-stadium", "rhodes",
  "rica", "richfield", "richmond-hwy", "riders", "ring", "rio-grande",
  "riot", "rises", "rite-aid", "rjr", "roach", "robber",
  "robbery", "robins", "rockies", "roddick", "roofs", "rookie",
  "rooney", "rose-cents", "rostenkowski", "royals", "rtc", "rubble",
  "ruckelshaus", "rugby", "rugs", "ryanair", "sac", "sackler",
  "safeway", "sail", "sailed", "sailor", "sailors", "sails",
  "saints", "saks", "sallie", "salomon", "salomon-smith-barney", "samberg",
  "sampras", "sanchez", "sands", "sang", "sanofi", "santa-fe",
  "scams", "schaff", "schapiro", "schering-plough", "schneider", "schneiderman",
  "schumer", "scooter", "scored-twice", "scotch", "scruggs", "sdi",
  "seagram", "seasonally-adjusted", "secaucus", "second-degree-assault", "secret-service", "security-forces",
  "seeded", "seeks-players", "seidman", "seinfeld", "selig", "sen",
  "sen-john", "seng", "sept-attacks", "serb", "serbia", "serbian",
  "serbs", "serena", "serena-williams", "sergei", "sergey", "sermon",
  "servings", "sewage", "sewer", "sex", "sexual", "shaheen",
  "shampoo", "shanklin", "sharapova", "share-compared", "shares-closed", "shearson",
  "shelton", "shenandoah", "shiite", "shire", "shooter", "shooters",
  "shoppers", "shoreham", "shortstop", "shriver", "sightings", "sign-language",
  "simon-schuster", "simpson", "sinema", "singapore", "singers", "sirius",
  "slices", "smithfield", "smithkline", "smoke-free", "smoker", "smokers",
  "snakes", "sniper", "social-security", "sofa", "sofas", "solicitor",
  "somalia", "sonia-sotomayor", "sony-pictures", "sopranos", "sorenstam", "soros",
  "sotomayor", "soundtrack", "souter", "southland", "soviet", "soviets",
  "spacey", "spain", "sperm", "spices", "spieth", "spill",
  "sprewell", "squadron", "stable-condition", "stadium", "stadiums", "star-trek",
  "starwood", "state-owned", "statue", "statues", "stead", "steinbrenner",
  "stem-cell", "stem-cells", "stephens", "stern", "stiller", "stock-exchange-composite-trading",
  "stock-exchange-composite-trading-yesterday", "stockpiles", "stone", "stones", "stoudemire", "stoxx",
  "strasburg", "student-debt", "student-loan", "student-loans", "subscriber", "subscribers",
  "sudan", "sued", "sulfur", "superintendent", "supermarkets", "supply-chains",
  "supremacists", "susan-collins", "svb", "swartz", "swimming-pool", "systems-inc",
  "t's", "taiwanese", "tampa-bay", "tankers", "taylor-swift", "tb",
  "teixeira", "tel", "telecharge.com", "telehealth", "television-stations", "tendered",
  "tenet", "tenneco", "tepper", "tequila", "term-limits", "terps",
  "terrapins", "tested-positive", "testosterone", "textile", "theaters", "thefts-break-ins",
  "theme-parks", "theology", "theranos", "therapies", "therapists", "third-baseman",
  "thomson-financial", "three-bedroom", "thune", "ticketmaster", "timberwolves", "titanic",
  "toll-free", "tom-brenner", "toothpaste", "top-ranked", "top-seeded", "toshiba",
  "touche", "tpa", "tracts", "trade-deficit", "trade-representative", "traffickers",
  "trammell", "trial-lawyers", "tribunal", "trinidad", "trk", "troopers",
  "troops", "troy-ounce", "trumpeter", "tsa", "tuberculosis", "tubman",
  "tucker", "tuesday-feb", "tunes", "tupelo", "turbo", "turgeon",
  "turner", "tv-stations", "two-bedroom", "two-story", "tyler-hicks", "u-s",
  "u.s.s.r", "ual", "uber", "unarmed", "underwater", "unfunded",
  "unilever", "union-address", "univision", "unlocked", "unlocked-vehicle", "unrated",
  "unsecured", "untitled", "ups", "upshaw", "usg", "vacancy",
  "valeant", "valenti", "vanity-fair", "vann", "vaping", "vary-widely",
  "venues", "venus", "veto", "viacom", "vice-president-kamala-harris", "victorian",
  "vietnam", "vietnamese", "vinegar", "vinson", "violent-mob", "vitro",
  "vivendi", "vladimir-putin", "vocational", "volcanic", "volcano", "volcker",
  "volodymyr", "vouchers", "wagoner", "waiter", "waiters", "walkable",
  "wallet", "walz", "wan", "wang", "warner-lambert", "washington-post-15th",
  "washington-redskins", "wasserman", "waste", "watt", "weaponry", "wechat",
  "weed", "weeds", "wei", "weinstein", "wellcome", "wellpoint",
  "west-german", "west-wing", "weta", "whales", "whirlpool", "whittle",
  "whole-foods", "wiedefeld", "wildcats", "willke", "windshield", "winger",
  "witter", "wizards", "wong", "worn", "worship", "wreckage",
  "write-down", "write-off", "wrongdoing", "wu", "wyden", "wynn",
  "xerox", "xi", "xm", "yacht", "yachts", "yamane",
  "year-earlier-period", "year-eve", "yellen", "yellowstone", "yuan", "yugoslav",
  "yugoslavia", "yum", "zelensky", "zell", "zhang", "zinke")

topic <-
  topic %>%
  mutate(decision = ifelse(term %in% dropped_terms, "drop", "keep"))

length(unique(topic %>% filter(decision != "drop") %>% pull(topic)))  ## 133
table(topic$decision)                                                 ## drop / keep counts

write.csv(topic, "input_files/topics_screened_new.csv", row.names = FALSE)

