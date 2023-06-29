
# Load in packages --------------------------------------------------------
library(dplyr)
library(janitor)
library(broom)
library(huxtable)
library(ggplot2)
library(ggbeeswarm)
library(wesanderson)

# Create Functions --------------------------------------------------------
# Correlation Hux Wrapper
hux_corr <- function(ct){
  if(!class(ct) == "htest"){
    print("Source must be of class 'htest'")
  } else{
    ct |> 
      broom::tidy() |> 
      mutate(
        `CI[LL,UU]` = paste0("[",conf.low |> round(2),
                             ",",conf.high |> round(2),
                             "]")
      ) |> 
      rename(
        r = estimate,
        t = statistic, 
        p = p.value, 
        df = parameter,
      ) |> 
      relocate(
        p, .after = df
      ) |>
      relocate(
        alternative, .after = last_col()
      ) |> 
      select(-conf.low,-conf.high,-method) |> 
      hux() |> 
      theme_article()
  }
}

# Read in file
cg <- read.csv("Data Analysis Caregiver Study.xlsx - Correlations.csv")

# Clean Data
cg_cln <- 
  cg |>
  rename(
    condition = Condition...Positive...1..Negative...2,
    years_as_cg = X..years.as.caregiver,
    support = Support..Do.you.feel.like.you.have.adequate.support.from.others.in.your.life..0...no..1...somewhat..2...yes,
    confidence = confidence.1.not.confident.5.very.confident,
    hd_gen = hd_genWould.you.say.that.in.general.your.health.is...,
    hd_phys = hd_physHow.many.days.during.the.past.30.days.was.your.physical.health.not.good.,
    hd_men = hd_menHow.many.days.during.the.past.30.days.was.your.mental.health.not.good.,
    hd_usual = hd_usualDuring.the.past.30.days..for.about.how.many.days.did.poor.physical.or.mental.health.keep.you.from.doing.your.usual.activities..such.as.self.care..work..or.recreation.,
    hd_pain = hd_painDuring.the.past.30.days..for.about.how.many.days.did.PAIN.make.it.hard.for.you.to.do.your.usual.activities..such.as.selfcare..work..or.recreation.,
    hd_sad = hd_sadDuring.the.past.30.days..for.about.how.many.days.have.you.felt.SAD..BLUE..or.DEPRESSED.,
    hd_worried = hd_worriedDuring.the.past.30.days..for.about.how.many.days.have.you.felt.WORRIED..TENSE..or.ANXIOUS.,
    hd_sleep = hd_sleepDuring.the.past.30.days..for.about.how.many.days.have.you.felt.you.did.NOT.get.ENOUGH.REST.or.SLEEP.,
    hd_energy = hd_energyDuring.the.past.30.days..for.about.how.many.days.have.you.felt.VERY.HEALTHY.AND.FULL.OF.ENERGY.
  ) |> 
  clean_names()


# Correlations ------------------------------------------------------------
# Years x PAOC
cor.test(cg_cln$years_as_cg,cg_cln$paoc) |> 
  hux_corr()

# Years x BSFC
cor.test(cg_cln$years_as_cg, cg_cln$bsfc) |> 
hux_corr()

# Confidence x PAOC
cor.test(cg_cln$confidence, cg_cln$paoc) |> 
  tidy() |> 
  hux() |> 
  theme_article() |> 
  set_caption("") |> 
  set_caption_pos("bottom")

# Confidence x BSFC
cor.test(cg_cln$confidence, cg_cln$bsfc) |> 
hux_corr()


# ANOVAs ------------------------------------------------------------------
# Make Support a 3 Level Factor
support <- 
  cg_cln |> 
  mutate(
    support = case_when(
      support == "0" ~ "No Support",
      support == "1" ~ "Some Support",
      support == "2" ~ "Support"
    )
  ) |> 
  select(support,bsfc,paoc)

# Level of Support and PAOC Score
lm(support$paoc~support$support) |> 
  summary()

# Level of Support and BSFC Score
lm(support$bsfc~support$support) |> 
  summary()

# Set Theme Structure
theme_set(theme_minimal())

theme_update(
  plot.background = element_rect(fill = "#fafaf5", color = "#fafaf5"),
  panel.background = element_rect(fill = NA, color = NA),
  panel.border = element_rect(fill = NA, color = NA),
)

# Plot: Support and BSFC
support |> 
  ggplot(aes(support,bsfc)) + 
  stat_summary(
    fun.data = "mean_se",
    aes(color = support)
  ) +
  geom_quasirandom() +
  labs(
    x = "\nLevel of Support from Friends and Family\n",
    y = "Burden Scale for Family Caregivers Score\n",
    color = "Degree of Support"
  ) +
  scale_color_manual(
    values = wes_palette(15)
  )

# Plot: Support and PAOC
support |> 
  ggplot(aes(support,paoc)) + 
  stat_summary(
    fun.data = "mean_se",
    aes(color = support)
  ) +
  geom_quasirandom() +
  labs(
    x = "\nLevel of Support from Friends and Family\n",
    y = "Positive Aspects of Caregiving Score\n",
    color = "Degree of Support"
  ) +
  scale_color_manual(
    values = wes_palette(15)
  )

