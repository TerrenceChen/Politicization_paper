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
recent_topics <- c(62, 79, 279, 334, 343)  ## dominated by very recent (post-1020) events

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
dropped_terms <- c(
  ## ---- Topic 2 - Economy ----
  "great-depression",
  # kept: "globalization"
  ## ---- Topic 4 - Religion ----
  "muhammad", "farris", "bledsoe", "falwell",
  ## ---- Topic 5 - Commodity_Market ----
  "pound", "bushel", "bellies", "stockpiles",
  ## ---- Topic 9 - Traffic ----
  "dr-gridlock", "capital-beltway", "i-66", "i-95", "northbound",
  ## ---- Topic 10 - Minority ----
  "reynolds", "wan",
  ## ---- Topic 13 - Disaster ----
  "katrina", "hurricane-katrina", "rubble", "gulf-coast", "hurricane-milton", "helene",
  "champlain", "downed", "glimmers", "forecasters", "bashar", "al-assad",
  "border-crossings", "pensacola", "fort-myers", "ashore", "sergey",
  # kept: "quake"
  ## ---- Topic 14 - Judicial ----
  "tribunal",
  ## ---- Topic 16 - Abortion ----
  "clinics", "stem-cell", "fertility", "willke", "vann", "cloning",
  "fertilization", "sperm", "stem-cells", "ivf", "embryo", "infertility",
  "vitro", "prenatal", "komen",
  # kept: "incest"
  ## ---- Topic 17 - Workers ----
  "entry-level", "interns",
  ## ---- Topic 20 - Festival ----
  "guests", "organizers", "year-eve", "venues", "black-tie", "ticketmaster",
  "hors",
  ## ---- Topic 21 - Journalism ----
  "vanity-fair", "baquet", "bradlee", "brill", "hearst", "gawker",
  "nast", "politico",
  ## ---- Topic 23 - Horticulture ----
  "weeds", "weed", "tupelo",
  ## ---- Topic 25 - Financial ----
  "lehman", "salomon", "painewebber", "bear-stearns", "csfb", "boesky",
  "nasd", "cftc", "shearson", "witter", "kidder", "quattrone",
  "salomon-smith-barney", "binance", "merc", "grasso",
  ## ---- Topic 29 - NBA ----
  "wizards", "bryant", "bullets", "all-star", "o'neal", "beal",
  "mystics", "durant", "lebron", "hornets", "kidd", "sprewell",
  "raptors", "abdul-jabbar", "timberwolves", "michael-jordan", "stoudemire", "d'antoni",
  "kobe", "garnett", "hardaway", "averaged-points", "layden", "patrick-ewing",
  ## ---- Topic 30 - Money_Markets ----
  "delivery-within-days", "general-electric", "negotiable", "prebon", "30-year-mortgage-commitments", "face-value",
  "annualized", "date-appearing", "acceptances", "major-corporations", "represent-actual-transactions", "quotations",
  "systems-inc", "30-yr", "high-grade-unsecured-notes", "yamane", "japan-switzerland-britain", "vary-widely",
  "resale", "home-loan-mortgage-corp", "labor-statistics", "10-yr", "multiples",
  ## ---- Topic 33 - TV_Entertainment ----
  "letterman", "contestants", "colbert", "leno", "seinfeld", "kimmel",
  "fey", "contestant", "fx", "west-wing", "magnum", "housewives",
  "hulu", "cw", "conan", "weta", "sopranos", "hillbilly",
  ## ---- Topic 39 - SupremeCourt ----
  "nomination", "nominee", "nominees", "alito", "gorsuch", "dissent",
  "sotomayor", "bork", "breyer", "solicitor", "antonin", "bader",
  "ideological", "rehnquist", "kagan", "judge-alito", "souter", "vacancy",
  "miers", "sonia-sotomayor", "federalist", "disqualification",
  # kept: "confirmation-hearings", "federalism"
  ## ---- Topic 46 - MiddleEastConflict ----
  "hostages", "netanyahu", "bosnia", "kosovo", "serbia", "tel",
  "abbas", "bosnian", "milosevic", "yugoslavia", "serbian", "serb",
  "serbs", "mubarak", "belgrade", "moslem", "yugoslav", "croatia",
  "balkans",
  ## ---- Topic 52 - Space ----
  "eclipse", "booster", "kong", "antenna", "volcanic", "crater",
  "milky", "star-trek", "volcano", "eruption",
  ## ---- Topic 55 - Earnings ----
  "share-compared", "year-earlier-period", "billion-yen", "stock-exchange-composite-trading", "full-year", "ended-sept",
  "thomson-financial", "kronor", "write-down", "write-off",
  # kept: "net-loss", "revenue-rose", "analysts-expectations", "profit-fell"
  ## ---- Topic 57 - Transportation ----
  "uber", "parked", "f1", "scooter", "windshield", "lyft",
  "turbo",
  ## ---- Topic 59 - ExecutiveCompensation ----
  "xerox", "stead",
  ## ---- Topic 70 - Mortgage ----
  "borrow", "sallie", "paulson", "forgiveness", "adjustable", "bankers-association",
  "delinquent", "1-year", "adj", "payday", "student-loan", "student-loans",
  ## ---- Topic 71 - Stock_market ----
  "rose-cents", "fell-cents", "index-fell", "nikkei", "intraday", "seng", "stoxx",
  # kept: "gainers", "decliners", "industrial-average", "composite-index",  "10-year-treasury-note"
  ## ---- Topic 73 - Terrorism ----
  "secret-service", "breach", "cyber", "sept-attacks", "equifax", "breaches",
  "marshals", "hackers", "tsa", "hacking", "ransomware", "hacker",
  ## ---- Topic 74 - Marine_Biology ----
  "whales", "fletcher", "pond", "spill", "mud", "darwin",
  "docks",
  ## ---- Topic 77 - Wildfires ----
  "crash", "explosion", "accidents", "crashes", "ntsb", "wreckage",
  ## ---- Topic 80 - Weather ----
  "mph", "mid", "ptcldy", "rises", "70s", "60s",
  "ptcldv", "40s", "low-tonight", "50s", "30s", "dial",
  "20s", "rcldy", "knots", "boating", "6-12", "mid-60s",
  "mid-50s", "hazy", "haze", "rockies", "5-10", "ankara",
  ## ---- Topic 81 - Senate ----
  "sen", "rep", "mcconnell", "schumer", "pelosi", "commerce-committee",
  "d-calif", "dodd", "sen-john", "hoyer", "dingell", "wyden",
  "rostenkowski", "hastert", "frist", "manchin", "joe-manchin", "ms-pelosi",
  "baucus", "proxmire", "susan-collins", "thune", "domenici", "d-w.va",
  "sinema", "cornyn", "hollings", "house-speaker-nancy-pelosi", "repeal",
  # kept: "veto", "earmarks"
  ## ---- Topic 84 - LGBTQ ----
  "sex", "morris", "sexual", "baldwin", "howe", "frye",
  "prostitutes", "prom", "prostitution", "porn", "becerra", "condoms",
  "stiller", "prostitute",
  # kept: "heterosexual"
  ## ---- Topic 85 - Movie ----
  "area-theaters", "pg-13", "hour-minutes", "coppola", "profanity", "nudity",
  "affleck", "minutes-contains", "unrated", "bourne", "pg-13-minutes", "brando",
  "goldwyn", "herrmann", "english-subtitles", "chaplin", "dargis", "gangster",
  "hepburn", "eastwood",
  ## ---- Topic 87 - Architecture ----
  "roofs", "masonry",
  ## ---- Topic 88 - Middle_East ----
  "troops", "fighters", "rebels", "civilians", "militants", "province",
  "hussein", "assad", "abu", "rebel", "somalia", "security-forces",
  "sudan", "militia", "militant", "militias", "musharraf", "afghans",
  "shiite", "airstrikes", "bombings",
  # kept: "islamist"
  ## ---- Topic 94 - Vaccine ----
  "doses", "moderna", "anthrax", "malaria", "respiratory", "tb",
  "omicron", "hospitalizations", "autism", "tested-positive", "tuberculosis", "hospitalization",
  "ebola", "covid-19-vaccine",
  ## ---- Topic 95 - Education ----
  "superintendent", "administrators", "p.s", "grader", "devos",
  # kept: "graders", "vouchers"
  ## ---- Topic 97 - Urban_development ----
  "realty", "office-buildings", "trammell", "acres-zoned", "secaucus",
  # kept: "walkable"
  ## ---- Topic 101 - NHL ----
  "playoff", "tampa-bay", "ovechkin", "edmonton", "2-0", "lundqvist",
  "3-2", "holtby", "anaheim", "backstrom", "kolzig", "esposito",
  "period-scoring", "winger", "ng", "scored-twice", "coyotes",
  ## ---- Topic 104 - Energy ----
  "gulf", "gallon", "cubic-feet", "getty", "cartel", "richfield",
  "halliburton", "tankers", "e85",
  # kept: "ethanol"
  ## ---- Topic 106 - Higher_Education ----
  "rhodes", "deaf", "recruiters", "internships", "internship", "sign-language",
  "gallaudet",
  # kept: "vocational", "hazing"
  ## ---- Topic 108 - Fashion ----
  "worn", "nike", "adidas", "lvmh",
  ## ---- Topic 109 - Museum ----
  "open-daily", "natural-history", "basel", "monday-saturday", "sackler", "o'keeffe",
  ## ---- Topic 112 - CollegeSports ----
  "terrapins", "terps", "turgeon", "huskies", "wildcats", "basketball-team",
  "calipari", "lsu", "driesell", "pitino", "friedgen", "blue-devils",
  ## ---- Topic 113 - Automotive ----
  "daimlerchrysler", "ev", "daimler", "kerkorian", "rebates", "renault",
  "trk", "wagoner",
  ## ---- Topic 118 - Television ----
  "fox-news", "cnbc", "cronkite", "moonves", "farrow", "falco",
  "csi",
  ## ---- Topic 122 - Labor ----
  "ups", "carbide", "ual", "brotherhood", "presser",
  # kept: "cost-of-living"
  ## ---- Topic 128 - Burglary ----
  "wallet", "pedestrian", "thefts-break-ins", "oct-property", "reported-missing", "information-call",
  "front-door", "males", "unlocked", "assaulted", "acquaintance", "gunpoint",
  "male-pedestrian", "area-thefts-break-ins", "among-incidents-reported", "unlocked-vehicle", "second-degree-assault", "laptop-computer",
  "prying", "richmond-hwy", "robber", "rear-door", "rear-window",
  ## ---- Topic 131 - Movie ----
  "viacom", "netflix", "theaters", "weinstein", "redstone", "iger",
  "lego", "vivendi", "diller", "dreamworks", "valenti", "mca",
  "eisner", "theme-parks", "qvc", "bluth", "mgm-ua", "comics",
  "disneyland", "wasserman", "emi", "sony-pictures",
  ## ---- Topic 132 - Music ----
  "sang", "tunes", "drummer", "bassist", "ellington", "cher",
  "pareles", "taylor-swift", "soundtrack", "trumpeter",
  ## ---- Topic 133 - Boxing ----
  "leonard", "ring", "peterson", "tucker", "holmes", "bailey",
  "frazier", "fights", "pryor", "holly", "stephens", "mayweather",
  "marquez", "funk", "promoter", "trinidad", "breland", "lyle",
  "marvis", "theranos", "duran", "roach",
  ## ---- Topic 134 - Investing ----
  "outflows", 
  # kept: "emerging-markets", "emerging-market", "junk-bonds", "fixed-income", "bull-market"
  ## ---- Topic 137 - Latin_America ----
  "simpson", "hernandez", "spain", "lopez", "jose", "miguel",
  "rica", "gomez", "alvarez", "morales", "manuel", "sanchez",
  "ramos", "noriega", "hugo", "aires", "jorge", "maduro",
  "padilla", "pedro",
  ## ---- Topic 141 - Substance Abuse ----
  "drunk", "pills", "illicit", "traffickers", "needles", "purdue",
  "legalization", "clemens", "testosterone", "cartels", "canseco", "performance-enhancing",
  ## ---- Topic 147 - Confederate ----
  "stone", "jefferson", "statue", "stones", "statues", "granite",
  "amusement", "lyon", "memorials", "monticello", "historic-preservation", "preservationists",
  "lincoln-memorial",
  ## ---- Topic 149 - Bankruptcy ----
  "creditors", "default", "debts", "bondholders", "owed", "liabilities",
  "creditor", "restructure", "unsecured", "liquidation", "repay", "robins",
  "manville", "interest-payments", "drexel", "southland", "revco", "tepper",
  "leveraged", "mf", "debt-load", "usg", "liens", "deferral",
  ## ---- Topic 150 - NuclearArms ----
  "soviet", "soviets", "soviet-union", "gorbachev", "geneva", "mx", "perle",
  "sdi", "brezhnev", "khrushchev", "mikhail-gorbachev", "u.s.s.r", "kgb",
  "weaponry", "aegis",
  # kept: "missile", "chemical-weapons"
  ## ---- Topic 152 - Golf ----
  "mickelson", "spieth", "mcilroy", "augusta", "nicklaus", "kemper",
  "els", "sorenstam", "kuchar", "mediate", "liv", "kite",
  ## ---- Topic 153 - Retail ----
  "shoppers", "federated", "safeway", "saks", "rite-aid", "neiman-marcus",
  "hawley", "bloomingdale", "lampert", "mail-order",
  # kept: "grocery", "supermarkets", "groceries", "drugstore"
  ## ---- Topic 162 - Trade ----
  "machine-tool", "million-tons", "bethlehem", "lighthizer", "maytag", "ltv",
  "whirlpool", "ec", "armco", "3m", "textile",
  # kept: "import", "imported", "trade-deficit", "trade-representative",  "supply-chains", "free-trade", "exporter"
  ## ---- Topic 165 - Healthcare ----
  "enrollment", "affordable-care-act", "obamacare", "retirees", "aca", "casualty",
  "metlife", "wellpoint", "geico", "healthcare.gov", "life-insurance", "marketplaces",
  ## ---- Topic 169 - Household ----
  "blade", "mower", "lice", "cookware",
  ## ---- Topic 170 - Wine ----
  "anheuser-busch", "coors", "kosher", "brewers", "heineken", "tequila",
  ## ---- Topic 171 - Marriage ----
  "guggenheim", "jealousy",
  ## ---- Topic 172 - Catholicism ----
  "worship", "choir", "protestant", "ministers", "mormon", "denomination",
  "sermon", "theology", "latter-day", "missionary", "messiah",
  ## ---- Topic 174 - Fiscal ----
  "unfunded", "darman",
  ## ---- Topic 176 - Railroad ----
  "hudson", "schneider", "santa-fe", "rf",
  # kept: "freight"
  ## ---- Topic 177 - Beverage ----
  "cosmetics", "unilever", "p-g", "mccormick", "whole-foods", "nestle",
  "heinz", "kraft", "avon", "conagra", "procter", "burger-king",
  "franchisees", "fragrance", "kellogg", "estee-lauder", "lauder", "coty",
  "clorox", "pillsbury", "marketer", "detergent", "fragrances", "inventors",
  "yum", "toothpaste", "bleach", "shampoo", "spices",
  ## ---- Topic 182 - Tennis ----
  "6-3", "6-4", "6-2", "chess", "becker", "6-1",
  "7-6", "shelton", "djokovic", "7-5", "mcenroe", "lendl",
  "connors", "shriver", "agassi", "federer", "venus", "seeded",
  "nadal", "sampras", "serena-williams", "graf", "evert", "pickleball",
  "top-seeded", "hingis", "sharapova", "ashe", "borg", "navratilova",
  "huber", "roddick", "arthur-ashe", "serena", "top-ranked",
  ## ---- Topic 185 - CriminalJustice ----
  "crimes", "cell", "guantanamo", "amnesty", "nonviolent", "kizer",
  # kept: "pardon"
  ## ---- Topic 186 - China ----
  "wang", "xi", "chen", "huawei", "singapore", "yuan",
  "jinping", "wong", "li", "liu", "state-owned", "geopolitical",
  "mao", "han", "malaysia", "taiwanese", "china’s", "meng",
  "wechat", "zhang", "huang", "lu", "wu", "malaysian",
  "alibaba", "hu", "wei", "jin", "quad", "shaheen",
  "mei", "guo",
  ## ---- Topic 188 - Electricity ----
  "regulatory-commission", "shoreham", "pg-e", "entergy", "lilco", "chernobyl",
  "plutonium", "toshiba", "atoms",
  ## ---- Topic 189 - Water ----
  "gallons", "rainfall", "blizzard",
  # kept: "sewer", "sewage"
  ## ---- Topic 192 - Gun_Control ----
  "turner", "brady", "shooter", "shooters", "alcohol-tobacco", "hinckley",
  "rampage", "whittle", "columbine", "remington",
  # kept: "gunman", "sniper", "opened-fire"
  ## ---- Topic 197 - Air Pollution ----
  "waste", "philip-morris", "smokers", "nicotine", "osha", "rjr",
  "liggett", "radioactive", "vaping", "ruckelshaus", "latimer", "fluoride",
  "juul", "altria", "smoker", "smoke-free", "lebow", "dumps",
  "barclay",
  ## ---- Topic 198 - Housing ----
  "median", "kb", "remodeling", 
  "lennar", "middle-income", "moderate-income", "elliman",
  ## ---- Topic 204 - Taxation ----
  "social-security", "earners", "pensions", "pension-plan", "annuity", "retirement-benefits",
  "pension-plans", "annuities",
  # kept: "depreciation", "filers", "excise"
  ## ---- Topic 206 - Corporate ----
  "mesa", "allergan", "kohlberg", "pickens", "ackman", "valeant",
  "conseco", "closing-price", "zell",
  ## ---- Topic 207 - MentalHealth ----
  "disorder",
  ## ---- Topic 208 - Cooking ----
  "vinegar", "dough", "servings", "ounces", "mustard", "slices",
  ## ---- Topic 209 - Airline ----
  "passengers", "fares", "airbus", "amr", "jetblue", "controllers",
  "baggage", "braniff", "fliers", "ryanair",
  ## ---- Topic 214 - Animal ----
  "hunting", "hunters", "hunts", "cruelty", "ants", "longnecker",
  "snakes", "sightings",
  ## ---- Topic 215 - Science ----
  "neurons", "particle", "mammals",
  ## ---- Topic 217 - Military ----
  "vietnam", "gen", "civilian", "vietnamese", "maj", "mckinney",
  "hanoi", "olds",
  # kept: "cadets", "squadron"
  ## ---- Topic 218 - Soccer ----
  "matches", "manchester", "liverpool", "premier-league", "rooney", "1-0",
  "cosmos", "dorrance", "barcelona", "olsen", "beckham", "qatar",
  "quaranta", "rfk-stadium", "lavelle",
  ## ---- Topic 219 - Classical_Music ----
  "dancer", "singers", "dances", "chorus", "choreographer", "choreographers",
  ## ---- Topic 221 - Gaming ----
  "nevada", "kenny", "andrews", "reid", "president-donald-trump", "holston",
  "atlantic-city", "union-address", "impeachment-trial", "vice-president-kamala-harris", "itt", "madison-square-garden",
  "caesars", "schaff", "doug-mills", "campaign-rally", "sands", "joint-session",
  "wynn", "joe-biden-delivers", "republican-presidential-nominee", "raskin", "reno", "insurrection",
  "mgm", "tuesday-feb", "albright", "incitement", "egging", "violent-mob",
  "mgm-grand", "moneymaker", "democratic-presidential-nomination", "harrah", "student-debt", "d-md",
  "tom-brenner",
  ## ---- Topic 222 - Baseball ----
  "football", "stadium", "athletes", "leagues", "football-league", "athlete",
  "leonsis", "stadiums", "rugby", "seeks-players", "coliseum", "selig",
  "kaepernick", "steinbrenner", "manfred", "bettman", "jerseys", "mcmullen",
  "goodell", "fehr", "lockout", "upshaw", "giamatti", "pollin",
  "collective-bargaining-agreement", "fenway",
  ## ---- Topic 224 - Culinary ----
  "cakes",
  ## ---- Topic 225 - MonetaryPolicy ----
  "yellen", "ecb", "bernanke", "ben-bernanke", "european-central-bank", "eurozone",
  "corporate-bonds", "janet-yellen", "inversely",
  ## ---- Topic 227 - Pharmaceuticals ----
  "generic", "johnson-johnson", "bayer", "glaxo", "wellcome", "lilly",
  "brand-name", "shire", "cvs", "tpa", "sanofi", "smithkline",
  "schering-plough", "bristol-myers-squibb", "medtronic", "bristol-myers", "efficacy", "warner-lambert",
  # kept: "psychedelics", "alzheimer", "mdma"
  ## ---- Topic 229 - Dining ----
  "chinatown", "bartender", "domino", "waiters", "waiter", "grubhub",
  "cocktails",
  ## ---- Topic 235 - Campaigns & Donors ----
  "charity", "charitable", "kaiser", "philanthropy", "nonprofits", "philanthropic",
  "soros", "bathgate", "messner",
  ## ---- Topic 239 - Economics ----
  "seasonally-adjusted", "new-home",
  ## ---- Topic 241 - Espionage ----
  "hayden", "beck", "petraeus", "tenet", "shanklin", "interrogation",
  "haines", "hiltzik", "teixeira",
  # kept: "counterterrorism"
  ## ---- Topic 243 - Nutrition ----
  "milligrams", "msg",
  ## ---- Topic 249 - Nature ----
  "clark", "scotch", "ranch", "shenandoah", "fraser", "blue-spruce",
  ## ---- Topic 250 - Wildlife ----
  "acres", "watt", "landowners", "zinke", "tracts", "yellowstone",
  "anchorage", "interior-department", "interior-secretary",
  # kept: "grazing", "ranchers"
  ## ---- Topic 253 - Banks ----
  "bailout", "dimon", "barclays", "mellon", "bankers-trust", "volcker",
  "ls", "jpmorgan-chase", "bcci", "geithner", "continental-illinois", "fslic",
  "insolvent", "rtc", "svb", "examiners", "j.p-morgan-chase",
  # kept: "financial-institutions", "nonperforming"
  ## ---- Topic 254 - Russia & Ukraine ----
  "putin", "nato", "zelensky", "president-vladimir-putin", "erdogan", "vladimir-putin",
  "navalny", "volodymyr", "tyler-hicks", "president-vladimir-v-putin", "sergei", "brendan-hoffman",
  "gershkovich", "counteroffensive", "baltic", "odessa", "russia’s",
  # kept: "north-atlantic-treaty"
  ## ---- Topic 258 - Criminal ----
  "plea", "attorney-office",
  # kept: "guilty-plea"
  ## ---- Topic 261 - Art ----
  "untitled", "reproductions",
  ## ---- Topic 265 - Disease ----
  "lung", "radiation", "lungs", "breast", "artery", "arteries",
  "implants", "allergies", "therapies",
  # kept: "coronary", "prostate-cancer", "angioplasty", "cardiologist"
  ## ---- Topic 270 - Baseball ----
  "nationals", "shortstop", "strasburg", "cubs", "dodgers", "royals",
  "outfielder", "astros", "hitters", "majors", "outs", "girardi",
  "marlins", "jeter", "postseason", "third-baseman", "fielder",
  ## ---- Topic 276 - Home ----
  "victorian", "baths", "antiques", "porch", "sofa", "two-story",
  "three-bedroom", "swimming-pool", "four-bedroom", "coldwell", "listing-agent", "duplex",
  "five-bedroom", "rugs", "sofas", "prewar",
  ## ---- Topic 280 - ClimateChange ----
  "fusion", "sulfur", "inflation-reduction", "nitrogen",
  ## ---- Topic 282 - Investigation ----
  "investigation", "alleged", "wrongdoing", "allegedly", "improper", "schneiderman",
  "accusers",
  # kept: "laundering", "bribes", "informant", "criminal-complaint"
  ## ---- Topic 285 - Broadway ----
  "hamlet", "telecharge.com", "239-6200", "isherwood", "krulwich", "beckett",
  ## ---- Topic 295 - Lawsuit ----
  "complaint", "alleging", "allege", "scruggs", "egbert",
  # kept: "lawsuit", "sued", "antitrust", "arbitration", "legal-fees", "trial-lawyers"
  ## ---- Topic 298 - Elections ----
  "evers",
  # kept: "electors", "term-limits"
  ## ---- Topic 304 - TravelIndustry ----
  "destinations", "accommodations", "hertz", "airbnb", "starwood",
  ## ---- Topic 306 - Homicide ----
  "robbery", "pronounced-dead", "stable-condition",
  ## ---- Topic 307 - Hospital ----
  "malpractice", "hca", "brius", "therapists", "ama",
  # kept: "nursing-home", "healthcare", "nursing-homes", "dental", "hospice", "telehealth", "paramedics", "geriatric"
  ## ---- Topic 308 - NFL ----
  "rookie", "patriots", "gibbs", "saints", "cardinals", "receiver",
  "washington-redskins", "coughlin", "gruden", "landry", "parcells", "mahomes",
  "belichick",
  ## ---- Topic 310 - Police ----
  "floyd", "nichols", "ellison", "walz", "lesher", "haskel",
  "gorman", "unarmed", "arpaio", "castile", "george-floyd",
  # kept: "homicide", "troopers", "homicides"
  ## ---- Topic 311 - Agriculture ----
  "smithfield", "rath", "citrus", "cargill",
  ## ---- Topic 313 - Mass_Transit ----
  "riders", "o'rourke", "purple-line", "wiedefeld", "greyhound",
  ## ---- Topic 317 - Racial ----
  "dees", "tubman", "ku", "rapp", "spacey", "clyburn",
  ## ---- Topic 318 - Foreign_exchange ----
  "ounce", "coins", "west-german", "coin", "troy-ounce", "profit-taking",
  ## ---- Topic 319 - CorporateFinance ----
  "industries-inc", "posner", "nasdaq-stock-market", "convertible", "previously-announced", "tenneco",
  "tendered", "shares-closed", "allegheny", "stock-exchange-composite-trading-yesterday", "composite-trading", "depositary",
  # kept: "public-offering", "quarterly-dividend"
  ## ---- Topic 320 - Telecommunications ----
  "subscribers", "t's", "communications-inc", "global-crossing", "a.t", "directv",
  "iridium", "subscriber", "echostar", "cable-wireless", "u-s", "gte",
  ## ---- Topic 321 - Literature ----
  "diary", "paperback", "barnes-noble", "simon-schuster", "nonfiction", "penguin",
  "capote", "harpercollins", "hardcover", "houghton", "librarians", "mailer",
  "nabokov", "illustrations", "bookscan", "mifflin", "demille", "farrar",
  "giroux",
  ## ---- Topic 326 - Radio ----
  "stern", "listeners", "malone", "kramer", "murdoch", "univision",
  "sirius", "kili", "television-stations", "xm", "duct", "tv-stations",
  "limbaugh", "imus", "landau", "iac",
  ## ---- Topic 328 - Native_American ----
  "india", "du", "pont", "conoco", "delhi", "modi",
  "mead", "hindu", "seagram", "kashmir",
  ## ---- Topic 331 - Immigration ----
  "customs", "passport", "mayorkas", "lópez", "el-paso", "haiti",
  "obrador", "rio-grande",
  ## ---- Topic 332 - Securities ----
  "donaldson", "aig", "mack", "kozlowski", "andersen", "heller",
  "aguirre", "vinson", "levitt", "rajaratnam", "arthur-andersen", "sac",
  "ernst-young", "peat", "pequot", "samberg", "inman", "audited",
  "touche", "swartz", "schapiro", "seidman", "gensler",
  ## ---- Topic 337 - Gender ----
  "#metoo",
  ## ---- Topic 339 - Postal_service ----
  "e-mail", "messages", "fax", "phone-number", "e-mails", "dejoy",
  "puzzles", "washington-post-15th", "crossword", "scams", "c-o", "toll-free",
  ## ---- Topic 346 - Maritime ----
  "greenberg", "titanic", "sail", "a.i.g", "carnival", "underwater",
  "creamer", "pirate",
  # kept: "coast-guard", "yacht", "sailor", "sailors", "sailed", "yachts", "sails"
  ## ---- Topic 347 - Protest ----
  "lives-matter", "extremist", "supremacists"
  # kept: "riot"
)

topic <-
  topic %>%
  mutate(decision = ifelse(term %in% dropped_terms, "drop", "keep"))

length(unique(topic %>% filter(decision != "drop") %>% pull(topic)))  ## 133
table(topic$decision)                                                 ## drop / keep counts

write.csv(topic, "Results_Files/topics_screened_new.csv", row.names = FALSE)

## Save RDS

dict_terms <-
  topic %>%
  filter(decision != "drop")

saveRDS(dict_terms, "Results_Files/dict_terms_final_0825.rds")



