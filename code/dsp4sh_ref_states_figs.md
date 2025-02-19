dsp4sh_ref_states_figs
================
Katy Dynarski
2024-08-07

# Overview of DSP4SH Climate and Soil Texture

## Figure 1A - Dual-axis figure showing mean annual temperature and precipitation for each site

``` r
# Compile data
clim <- soc_pedon %>%
  distinct(project, dsp_plot_id, label, mat, map) %>%
  group_by(project) %>%
  summarize(mean_mat = mean(mat),
            mean_map = mean(map))

# Make the plot
proj_tem_prep_clim <- ggplot(clim, aes(x = project)) +
  # Add temperature points (scaled for dual y-axis)
  geom_point(aes(y = mean_mat * 40), color = "#CC5800FF", size = 4, shape = 19) + # Adjust scale factor as needed
  # Add precipitation points
  geom_point(aes(y = mean_map), color = "#1E8E99FF", size = 4, shape = 17) +
  scale_x_discrete(labels=project_labels, name="Project") + 
  # Customize y-axes
  scale_y_continuous(
    name = "Mean Annual Precipitation (mm)",
    sec.axis = sec_axis(~ . / 40, name = "Mean Annual Temperature (°C)")) + # Adjust scale factor
  theme_katy() +
  theme(axis.title.y=element_text(color="#1E8E99FF"),
        axis.title.y.right=element_text(color="#CC5800FF"),
        axis.text.x=element_text(angle=45, hjust=1),
        plot.margin = margin(l=30))

proj_tem_prep_clim
```

![](dsp4sh_ref_states_figs_files/figure-gfm/figure_1a-1.png)<!-- -->

## Figure 1B - Soil texture triangle showing mean SSURGO texture in top horizon for each project

Make the USDA soil texture triangle to overlay:

``` r
# USDA dataset comes pre-loaded in ggtern package
data(USDA)

# rename the labels to be shorter for easier printing
USDA <- USDA %>% 
  rename("clay_pct" = Clay, "sand_pct" = Sand, "silt_pct" = Silt) %>% 
  mutate(
    label = case_when(
      Label == "Clay" ~ "C",
      Label == "Sandy Clay" ~ "SC",
      Label == "Sandy Clay Loam" ~ "SCL",
      Label == "Sandy Loam" ~ "SL",
      Label == "Loamy Sand" ~ "LS",
      Label == "Sand" ~ "S",
      Label == "Clay Loam" ~ "CL",
      Label == "Silt Loam" ~ "SiL",
      Label == "Silty Clay" ~ "SiC",
      Label == "Silty Clay Loam" ~ "SiCL",
      Label == "Silt" ~ "Si",
      Label == "Loam" ~ "L",
      TRUE ~ NA_character_
    ))
USDA_text <- USDA  %>% 
  group_by(label) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
USDA_text
```

    ## # A tibble: 12 × 4
    ##    label clay_pct sand_pct silt_pct
    ##    <chr>    <dbl>    <dbl>    <dbl>
    ##  1 C       0.59     0.22     0.19  
    ##  2 CL      0.338    0.325    0.338 
    ##  3 L       0.17     0.435    0.395 
    ##  4 LS      0.0625   0.825    0.112 
    ##  5 S       0.0333   0.917    0.05  
    ##  6 SC      0.417    0.517    0.0667
    ##  7 SCL     0.275    0.575    0.15  
    ##  8 SL      0.0929   0.621    0.286 
    ##  9 Si      0.0625   0.0688   0.869 
    ## 10 SiC     0.467    0.0667   0.467 
    ## 11 SiCL    0.338    0.1      0.562 
    ## 12 SiL     0.133    0.167    0.7

Now, plot the SSURGO data on the textural triangle:

``` r
# Make a new column with both project and soil name for plotting
project_labels_df <- data.frame(project_labels) %>%
  rownames_to_column() %>%
  rename(project = rowname)

ssurgo_texture2 <- ssurgo_texture %>%
  left_join(project_labels_df, by="project") %>%
  unite("project_soil", c("project_labels", "soil"), remove=FALSE, sep=" - ")

# Plot texture with SSURGO data only, point fill color by project/soil combo
# overlay textural triangle
theme_set(theme_bw())
triangle_plot <- ggtern(ssurgo_texture2, aes(x=sand_pct, y=clay_pct, z=silt_pct, 
                                          color=project_soil)) +
  geom_polygon(data=USDA,
               aes(fill = label),
               alpha = 0.0,
               linewidth = 0.5,
               color = "black",
               show.legend = FALSE) +
  geom_text(data = USDA_text,
            aes(label = label),
            color = 'grey40',
            fontface = "bold",
            size = 5) +
  geom_point(size=4) +
  theme_showarrows() +
  labs(yarrow = "Clay (%)",
       zarrow = "Silt (%)",
       xarrow = "Sand(%)",
       x="", y="", z="",
       color = "Project")  +
  theme_clockwise() +
  scale_color_paletteer_d("rcartocolor::Safe", name="Project & Soil Series")
triangle_plot
```

![](dsp4sh_ref_states_figs_files/figure-gfm/fig_1b-1.png)<!-- -->

# Summary Figures of SOC Stocks

## Summary table of SOC stocks across projects, soils, and treatments

Mean, standard deviation, and n for SOC stocks calculated to 30 cm and
100 cm for all soils in DSP4SH projects.

``` r
soc_summary <- soc_pedon %>%
  group_by(project, soil, label) %>%
  summarize(across(soc_stock_0_30cm:soc_stock_100cm, mean_sd), n=n())
```

    ## `summarise()` has grouped output by 'project', 'soil'. You can override using
    ## the `.groups` argument.

``` r
flextable(soc_summary)
```

<img src="dsp4sh_ref_states_figs_files/figure-gfm/table_1-1.png" width="3708" />

``` r
# range in SOC values
soc_min_max <- soc_pedon %>%
  summarize(across(soc_stock_0_30cm:soc_stock_100cm, min_max), n=n())
flextable(soc_min_max)
```

<img src="dsp4sh_ref_states_figs_files/figure-gfm/table_1-2.png" width="3096" />

``` r
# which is the min?
soc_pedon %>% slice_min(soc_stock_100cm, n = 1) %>%
  select(project, label, dsp_pedon_id, soc_stock_100cm)
```

    ##        project label dsp_pedon_id soc_stock_100cm
    ## 1 TexasA&MPt-1   BAU      CVT-1-1          21.332

``` r
# which is the max?
soc_pedon %>% slice_max(soc_stock_100cm, n = 1) %>%
  select(project, label, dsp_pedon_id, soc_stock_100cm)
```

    ##           project label dsp_pedon_id soc_stock_100cm
    ## 1 UnivOfMinnesota   Ref        REF-2        372.2768

## Fig 2 - Boxplots of total SOC stocks under different management treatments

Plot boxplots of total SOC stocks under different management treatments:

``` r
soc_pedon_toplot <- soc_pedon %>%
  filter(project!="TexasA&MPt-2") %>% # Exclude Texas A&M pt 2 because data was only collected to ~20 cm, so no stocks were calculated
  mutate(label=factor(label, levels=c("BAU", "SHM", "Ref")))

# Boxplot comparing total SOC stocks (100 cm) between treatments within soil types
soc100_boxplot <- ggplot(soc_pedon_toplot, 
                         aes(x=project, y=soc_stock_100cm, fill=label)) +
  geom_boxplot() +
  labs(x="Project", y="SOC stock to 100 cm depth (Mg/ha)") +
  scale_x_discrete(labels=project_labels) +
  scale_fill_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                     breaks=c("BAU", "SHM", "Ref"), 
                    name="Management") +
  theme_katy_grid() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Boxplot comparing total SOC stocks (30 cm) between treatments within soil types
soc30_boxplot <- ggplot(soc_pedon_toplot, 
                         aes(x=project, y=soc_stock_0_30cm, fill=label)) +
  geom_boxplot() +
  labs(x="Project", y="SOC stock to 30 cm depth (Mg/ha)") +
  scale_x_discrete(labels=project_labels) +
  scale_fill_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                     breaks=c("BAU", "SHM", "Ref"), 
                    name="Management") +
  theme_katy_grid() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

soc_grid <- plot_grid(soc30_boxplot + 
                               theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()),
          soc100_boxplot + theme(legend.position="none"),
          ncol=1, labels=c("A", "B"), rel_heights=c(1, 1.4))
```

    ## Warning: Removed 2 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 18 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

``` r
soc_leg <- get_legend(soc100_boxplot)
```

    ## Warning: Removed 18 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning in get_plot_component(plot, "guide-box"): Multiple components found;
    ## returning the first one. To return all, use `return_all = TRUE`.

``` r
plot_grid(soc_grid, soc_leg, rel_widths = c(1, .23))
```

![](dsp4sh_ref_states_figs_files/figure-gfm/fig_2-1.png)<!-- -->

Takeaways:

- Significant variability in SOC stocks between sites (expected)

- Differences between treatments more apparent at 30 cm depth than 100
  cm depth

- Reference sites are significantly higher in C at some, but not all
  sites (Illinois, UConn, University of Minnesota, UTRGV)

- Differences between SHM and BAU are infrequently observed

## Boxplots of SOC stock data calculated via ESM

``` r
# ESM data contains many different ways to calculate - select only the SOC stocks calculated via standard depth increments, individual projects as reference soils, and using the minimum soil mass as a reference
esm_standard_min <- esm_all %>%
  filter(depth_increments == "standard", ref_data =="indv_project",
         method_long== "esm2_min")

# Calculate total SOC stocks
esm_standard_min_totals <- esm_standard_min %>%
  group_by(project, label, dsp_pedon_id, depth_increments) %>%
  summarize(soc_0to30 = sum(soc[apparent_depth=="0-10 cm" | apparent_depth=="10-30 cm"]),
            soc_0to100 = sum(soc))
```

    ## `summarise()` has grouped output by 'project', 'label', 'dsp_pedon_id'. You can
    ## override using the `.groups` argument.

``` r
# Boxplot comparing total SOC stocks (30 cm) between treatments within soil types
soc30_esm_boxplot <- ggplot(esm_standard_min_totals, 
                         aes(x=project, y=soc_0to30, fill=factor(label, levels=c("BAU", "SHM", "Ref")))) +
  geom_boxplot() +
  labs(x="Project", y="SOC stock to 30 cm depth (Mg/ha)") +
  scale_x_discrete(labels=project_labels) +
  scale_fill_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                     breaks=c("BAU", "SHM", "Ref"), 
                    name="Management") +
  theme_katy_grid() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Boxplot comparing total SOC stocks (100 cm) between treatments within soil types
soc100_esm_boxplot <- ggplot(esm_standard_min_totals, 
                         aes(x=project, y=soc_0to100, fill=factor(label, levels=c("BAU", "SHM", "Ref")))) +
  geom_boxplot() +
  labs(x="Project", y="SOC stock to 100 cm depth (Mg/ha)") +
  scale_x_discrete(labels=project_labels) +
  scale_fill_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                     breaks=c("BAU", "SHM", "Ref"), 
                    name="Management") +
  theme_katy_grid() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

soc_esm_grid <- plot_grid(soc30_esm_boxplot + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()),
          soc100_esm_boxplot + theme(legend.position="none"),
          ncol=1, labels=c("A", "B"), rel_heights=c(1, 1.4))
soc_esm_leg <- get_legend(soc100_esm_boxplot)
```

    ## Warning in get_plot_component(plot, "guide-box"): Multiple components found;
    ## returning the first one. To return all, use `return_all = TRUE`.

``` r
plot_grid(soc_esm_grid, soc_esm_leg, rel_widths = c(1, .23))
```

![](dsp4sh_ref_states_figs_files/figure-gfm/fig_s1-1.png)<!-- -->

## Comparison of ESM results to fixed depth results

``` r
esm_standard_min_totals_long <- esm_standard_min_totals %>%
  pivot_longer(soc_0to30:soc_0to100, names_to="depth", values_to="soc_stock") %>%
  select(-depth_increments) %>%
  mutate(depth = case_when(depth== "soc_0to30" ~ "0to30cm",
                               depth=="soc_0to100" ~ "0to100cm"))

soc_pedon_fixed <- soc_pedon %>%
  select(project, label, dsp_pedon_id, soc_stock_0_30cm, soc_stock_100cm) %>%
  pivot_longer(soc_stock_0_30cm:soc_stock_100cm,
               names_to="depth", values_to="soc_stock") %>%
    mutate(depth = case_when(depth== "soc_stock_0_30cm" ~ "0to30cm",
                               depth=="soc_stock_100cm" ~ "0to100cm"))

soc_fixed_esm <- soc_pedon_fixed %>%
  left_join(esm_standard_min_totals_long, by=c("project", "label", "dsp_pedon_id","depth"), suffix=c("_fixed", "_esm")) %>%
  pivot_longer(soc_stock_fixed:soc_stock_esm, names_to="calc_method", values_to="soc_stock") %>%
  mutate(depth = factor(depth, levels=c("0to30cm", "0to100cm"))) %>%
  filter(project!="TexasA&MPt-2")

# Make plot
ggplot(soc_fixed_esm, aes(x=label, y=soc_stock, fill=calc_method)) +
  geom_boxplot() +
  facet_grid(project ~ depth, scales="free_y", labeller=labeller(project = project_labels2, 
                                                                 depth=c("0to100cm"="0 to 100 cm", "0to30cm"="0 to 30 cm"))) +
  labs(x="Management", y="SOC Stock (Mg/ha)") +
  scale_fill_paletteer_d("nationalparkcolors::Arches", name="Calculation method",
                         labels=c("ESM (Cubic Spline)", "Fixed Depth")) +
  theme_katy()
```

    ## Warning: Removed 56 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](dsp4sh_ref_states_figs_files/figure-gfm/fig_s2-1.png)<!-- -->

Table of difference in SOC stocks calculated via fixed depth vs. ESM:

``` r
# Calculate differences
soc_stock_comparison <- soc_fixed_esm %>%
  pivot_wider(names_from="calc_method", values_from="soc_stock") %>%
  group_by(project, label, depth) %>%
  summarize(across(soc_stock_fixed:soc_stock_esm, ~round(mean(.x, na.rm=TRUE),1))) %>%
  mutate(difference = round(soc_stock_fixed - soc_stock_esm,1)) %>%
  mutate(percent_diff = round((difference / soc_stock_esm)*100,1)) %>%
  mutate(label=factor(label, levels=c("BAU", "SHM", "Ref"))) %>%
  arrange(project, label, depth)
```

    ## `summarise()` has grouped output by 'project', 'label'. You can override using
    ## the `.groups` argument.

``` r
flextable(soc_stock_comparison)
```

<img src="dsp4sh_ref_states_figs_files/figure-gfm/table_s1-1.png" width="1407" />

``` r
soc_stock_comparison_summary <- soc_stock_comparison %>%
  ungroup() %>%
  group_by(depth) %>%
  summarize(mean_diff = round(mean(difference),1),
            mean_percent_diff = round(mean(percent_diff),1))
flextable(soc_stock_comparison_summary)
```

<img src="dsp4sh_ref_states_figs_files/figure-gfm/table_s1-2.png" width="604" />

Mean difference in SOC stocks between fixed depth and ESM is 9.5 Mg/ha
(greater in fixed depth vs ESM), mean percent difference is 14.3%.

## Fig 3 - Ribbon plot of SOC stocks by depth (continuous):

``` r
# Promote horizon data to SPC
soc_spc <- soc_horizon
depths(soc_spc) <- dsp_pedon_id ~ hrzdep_t + hrzdep_b
hzdesgnname(soc_spc) <- 'hzdesg'
# promote project and label to site-level so they can be used as grouping variables
site(soc_spc) <- ~ project + label + soil

# Calculate stocks by depth increment for each project and management condition
slab_ref <- aqp::slab(subset(soc_spc, label=="Ref"),
                      fm = project ~ soc_stock_hrz,
                      slab.structure = seq(0,100,by=10)) %>%
  mutate(label="Ref")

slab_shm <- aqp::slab(subset(soc_spc, label=="SHM"),
                      fm = project ~ soc_stock_hrz,
                      slab.structure = seq(0,100,by=10)) %>%
  mutate(label="SHM")
slab_bau <- aqp::slab(subset(soc_spc, label=="BAU"),
                      fm = project ~ soc_stock_hrz,
                      slab.structure = seq(0,100,by=10)) %>%
  mutate(label="BAU")

# Put management conditions together
slab_mgmt <- bind_rows(slab_ref, slab_shm, slab_bau)

# Plot with all mgmt together
ggplot(slab_mgmt, aes(x=top, y=p.q50)) +
  geom_line(linewidth=1.2, aes(color=label)) +
  geom_ribbon(aes(ymin=p.q25, ymax=p.q75, x=top, fill=label), alpha=0.2) +
  xlim(c(100,0)) +
  coord_flip() +
  labs(title="SOC Stocks by Depth", x="Depth (cm)", y="SOC (Mg/ha)") +
  facet_wrap(~ project, scales = "free_x", labeller=labeller(project=project_labels)) +
  scale_fill_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                     breaks=c("BAU", "SHM", "Ref"), 
                    guide="none") +
  scale_color_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                     breaks=c("BAU", "SHM", "Ref"), 
                    name="Management") +
  theme_katy()
```

![](dsp4sh_ref_states_figs_files/figure-gfm/fig_3-1.png)<!-- -->

Takeaways:

- Very few projects actually showed greater SOC stocks throughout the
  soil profile (Minnesota, Texas A&M, UTRGV)
- Differences in C stocks tended to be most apparent at shallow depths
  (though see Texas A&M Pt 1 and Minnesota for exceptions where
  differences were greater deeper in the soil profile)

# Test effect of treatment on SOC stocks and concentrations with mixed linear model

## Effect of treatment on SOC stocks to 100 cm depth

``` r
# Remove NAs from data
soc100_clean <- soc_pedon %>%
  select(dsp_pedon_id, project, label, soil, soc_stock_100cm) %>%
  na.omit() 

# Random effects: project
# Fixed effects: label
soc_stock100_mixed <- lmer(soc_stock_100cm ~ label + (1|project), data = soc100_clean)
summary(soc_stock100_mixed)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: soc_stock_100cm ~ label + (1 | project)
    ##    Data: soc100_clean
    ## 
    ## REML criterion at convergence: 2484.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5271 -0.5977 -0.1282  0.4769  2.9054 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  project  (Intercept) 2668     51.65   
    ##  Residual             2316     48.12   
    ## Number of obs: 234, groups:  project, 9
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  123.423     18.070   6.830
    ## labelRef      36.910      8.221   4.490
    ## labelSHM       3.908      7.942   0.492
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) lablRf
    ## labelRef -0.173       
    ## labelSHM -0.186  0.410

``` r
# Test significance of treatment by comparing full and reduced models, use likelihood ratio test
stock100_mixed_full <- lmer(soc_stock_100cm ~ label + (1|project), data = soc100_clean, REML = FALSE)
stock100_mixed_reduced <- lmer(soc_stock_100cm ~ (1|project), data = soc100_clean, REML = FALSE)
anova(stock100_mixed_full, stock100_mixed_reduced, text="Chisq")
```

    ## Data: soc100_clean
    ## Models:
    ## stock100_mixed_reduced: soc_stock_100cm ~ (1 | project)
    ## stock100_mixed_full: soc_stock_100cm ~ label + (1 | project)
    ##                        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
    ## stock100_mixed_reduced    3 2531.0 2541.4 -1262.5   2525.0                     
    ## stock100_mixed_full       5 2513.4 2530.7 -1251.7   2503.4 21.585  2  2.056e-05
    ##                           
    ## stock100_mixed_reduced    
    ## stock100_mixed_full    ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Tukey post-hoc
soc_stock100_mixed_tukey <- glht(soc_stock100_mixed, linfct = mcp(label = 'Tukey'))
summary(soc_stock100_mixed_tukey)
```

    ## 
    ##   Simultaneous Tests for General Linear Hypotheses
    ## 
    ## Multiple Comparisons of Means: Tukey Contrasts
    ## 
    ## 
    ## Fit: lmer(formula = soc_stock_100cm ~ label + (1 | project), data = soc100_clean)
    ## 
    ## Linear Hypotheses:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## Ref - BAU == 0   36.910      8.221   4.490   <1e-04 ***
    ## SHM - BAU == 0    3.908      7.942   0.492   0.8749    
    ## SHM - Ref == 0  -33.002      8.781  -3.758   0.0005 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## (Adjusted p values reported -- single-step method)

**Results:** Full model is significantly different from reduced model -
there is a significant effect of treatment when between-project
variation is controlled for. Tukey HSD post-hoc test shows that Ref SOC
stock is significantly different from BAU and Ref groups.

## Effect of treatment on SOC stocks to 30 cm depth

``` r
# Remove NAs from data
soc30_clean <- soc_pedon %>%
  select(dsp_pedon_id, project, label, soil, soc_stock_0_30cm) %>%
  na.omit() 

# Random effects: project
# Fixed effects: label
soc_stock30_mixed <- lmer(soc_stock_0_30cm ~ label + (1|project), data = soc30_clean)
summary(soc_stock30_mixed)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: soc_stock_0_30cm ~ label + (1 | project)
    ##    Data: soc30_clean
    ## 
    ## REML criterion at convergence: 2203.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0993 -0.6676 -0.0388  0.4469  3.6310 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  project  (Intercept) 952.9    30.87   
    ##  Residual             363.1    19.06   
    ## Number of obs: 250, groups:  project, 9
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   64.218     10.488   6.123
    ## labelRef      17.995      3.217   5.594
    ## labelSHM       1.561      3.030   0.515
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) lablRf
    ## labelRef -0.116       
    ## labelSHM -0.133  0.425

``` r
# Test significance of treatment by comparing full and reduced models, use likelihood ratio test
stock30_mixed_full <- lmer(soc_stock_0_30cm ~ label + (1|project), data = soc30_clean, REML = FALSE)
stock30_mixed_reduced <- lmer(soc_stock_0_30cm ~ (1|project), data = soc30_clean, REML = FALSE)
anova(stock30_mixed_full, stock30_mixed_reduced, text="Chisq")
```

    ## Data: soc30_clean
    ## Models:
    ## stock30_mixed_reduced: soc_stock_0_30cm ~ (1 | project)
    ## stock30_mixed_full: soc_stock_0_30cm ~ label + (1 | project)
    ##                       npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
    ## stock30_mixed_reduced    3 2257.7 2268.2 -1125.8   2251.7                     
    ## stock30_mixed_full       5 2228.2 2245.8 -1109.1   2218.2 33.489  2  5.345e-08
    ##                          
    ## stock30_mixed_reduced    
    ## stock30_mixed_full    ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Full model is significantly different from reduced model - there is a significant effect of treatment when between-project variation is controlled for 

# Tukey post-hoc
soc_stock30_mixed_tukey <- glht(soc_stock30_mixed, linfct = mcp(label = 'Tukey'))
summary(soc_stock30_mixed_tukey)
```

    ## 
    ##   Simultaneous Tests for General Linear Hypotheses
    ## 
    ## Multiple Comparisons of Means: Tukey Contrasts
    ## 
    ## 
    ## Fit: lmer(formula = soc_stock_0_30cm ~ label + (1 | project), data = soc30_clean)
    ## 
    ## Linear Hypotheses:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## Ref - BAU == 0   17.995      3.217   5.594   <1e-05 ***
    ## SHM - BAU == 0    1.561      3.030   0.515    0.864    
    ## SHM - Ref == 0  -16.435      3.354  -4.899   <1e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## (Adjusted p values reported -- single-step method)

**Results:** Full model is significantly different from reduced model -
there is a significant effect of treatment when between-project
variation is controlled for. Tukey HSD post-hoc test shows that Ref SOC
stock is significantly different from BAU and Ref groups.

## Plot results of mixed linear model

``` r
# Plot predictions of 30cm SOC stocks under each treatment
pred_stock30 <- ggpredict(soc_stock30_mixed, terms = c("label"))
pred30 <- ggplot(pred_stock30, aes(x=x, y=predicted)) +
  geom_point() +
  geom_errorbar(aes(x=x, ymin=conf.low, ymax=conf.high)) +
  labs(x="Management", y="Predicted SOC stock to 30 cm (Mg/ha)") +
  theme_katy_grid()

# Plot predictions of 100cm SOC stocks under each treatment
pred_stock100 <- ggpredict(soc_stock100_mixed, terms = c("label"))

pred100 <- ggplot(pred_stock100, aes(x=x, y=predicted)) +
  geom_point() +
  geom_errorbar(aes(x=x, ymin=conf.low, ymax=conf.high)) +
  labs(x="Management", y="Predicted SOC stock to 100 cm (Mg/ha)") +
  theme_katy_grid()

plot_grid(pred30, pred100,
          ncol=1, labels=c("A", "B"))
```

![](dsp4sh_ref_states_figs_files/figure-gfm/soc_lmer_plot-1.png)<!-- -->

Takeaways:

- Though not many significant differences are detected between
  treatments within each project, mixed linear model suggests that when
  accounting for between-project variability, SOC stocks (both 30cm and
  100cm depth) are significantly higher in the Ref condition vs. SHM and
  BAU.

- Supports choice of Ref conditions as a broad concept.

# SHAPE Score Analysis

Plot SOC stocks vs SHAPE scores:

``` r
# Need to join SHAPE scores to SOC stock data
shape_soc_stock <- shape_spatial %>%
  select(dsp_pedon_id, score_mean_soc, gt_90_soc) %>%
  left_join(select(soc_pedon, dsp_pedon_id, project, soil, label, trt, lu, till, soc_stock_0_30cm, soc_stock_100cm),
            by="dsp_pedon_id")

# Details of logarithmic regression
shape_soc_lm <- lm(score_mean_soc ~ log(soc_stock_100cm), data=shape_soc_stock)
summary(shape_soc_lm)
```

    ## 
    ## Call:
    ## lm(formula = score_mean_soc ~ log(soc_stock_100cm), data = shape_soc_stock)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.72427 -0.14150  0.02464  0.15260  0.58300 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -0.90065    0.10988  -8.197 1.84e-14 ***
    ## log(soc_stock_100cm)  0.31638    0.02305  13.728  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2242 on 226 degrees of freedom
    ##   (48 observations deleted due to missingness)
    ## Multiple R-squared:  0.4547, Adjusted R-squared:  0.4523 
    ## F-statistic: 188.5 on 1 and 226 DF,  p-value: < 2.2e-16

``` r
# Plot
shape_soc_plot <- ggplot(shape_soc_stock, aes(x=soc_stock_100cm, y=score_mean_soc)) +
  geom_point(aes(colour=label)) + 
  geom_smooth(method="lm", formula=y~log(x), color="black") +
  annotate(geom="text", x=250, y=0.22,
           label=expression(atop("y = -0.90 + 0.32*log(x)", R^2~"= 0.45, p < 0.001"))) +
  labs(x="SOC stock to 100 cm depth (Mg/ha)",
       y="SHAPE Score (Peer Group Percentile)") +
  scale_colour_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                    breaks=c("BAU", "SHM", "Ref"), 
                    name="Management") +
  theme_katy()
```

Boxplots of SHAPE SOC scores:

``` r
# Join in project data
shape_spatial_proj <- shape_spatial %>%
  select(dsp_pedon_id, score_mean_soc, gt_90_soc) %>%
  left_join(select(project, dsp_plot_id, dsp_pedon_id, project, soil, label, trt, lu, till), by="dsp_pedon_id") %>%
  mutate(shape_source = "spatial")

# Boxplot showing SHAPE scores in different management categories, separated by project
shape_boxplot_all <- ggplot(shape_spatial_proj, aes(x=project, y=score_mean_soc, fill=label)) +
  geom_boxplot() +
  geom_abline(intercept=0.90, slope=0, linetype="dashed") +
  geom_abline(intercept=0.75, slope=0, linetype="dashed") +
  labs(x="Project", y="SHAPE Score (Peer Group Percentile)") +
  scale_x_discrete(labels=project_labels) +
  scale_fill_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                    breaks=c("BAU", "SHM", "Ref"), 
                    name="Management") +
  theme_katy() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Boxplot showing SHAPE scores in different management categories (no separation by project)
shape_boxplot_condensed <- ggplot(shape_spatial_proj, aes(x=factor(label, levels=c("BAU", "SHM", "Ref")), 
                                                          y=score_mean_soc, 
                      fill=factor(label, levels=c("BAU", "SHM", "Ref")))) +
  geom_boxplot() +
  geom_abline(intercept=0.90, slope=0, linetype="dashed") +
  geom_abline(intercept=0.75, slope=0, linetype="dashed") +
  labs(y="SHAPE Score (Peer Group Percentile)",
       x="Management System") +
  scale_fill_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                    breaks=c("BAU", "SHM", "Ref"), 
                    name="Management") +
  theme_katy() +
  theme(plot.title=element_text(hjust=0.5), legend.position="none")

# Panel figure of all SHAPE and consolidated SHAPE AND SOC stock
shape_grid_bottom <- plot_grid(shape_boxplot_condensed + theme(legend.position="none"),
          shape_soc_plot + theme(legend.position="none"),
          ncol=2, labels=c("B", "C"))
```

    ## Warning: Removed 13 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 48 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 48 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning in is.na(x): is.na() applied to non-(list or vector) of type
    ## 'expression'

``` r
shape_grid <- plot_grid(shape_boxplot_all + theme(legend.position="none"),
                        shape_grid_bottom,
                        ncol=1, labels=c("A", ""), rel_heights=c(1.23, 1))
```

    ## Warning: Removed 13 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

``` r
shape_leg <- get_legend(shape_boxplot_all)
```

    ## Warning: Removed 13 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning in get_plot_component(plot, "guide-box"): Multiple components found;
    ## returning the first one. To return all, use `return_all = TRUE`.

``` r
plot_grid(shape_grid, shape_leg, rel_widths = c(1, 0.23))
```

![](dsp4sh_ref_states_figs_files/figure-gfm/fig_4-1.png)<!-- -->

Overall, the SHAPE scores support the Ref/SHM/BAU groupings:

- Very few projects had median Ref SHAPE score at 90th percentile or
  above (Illinois, Oregon State, University of Minnesota, and UTRGV)

- Most projects had median Ref SHAPE score at 75th percentile or above
  (only Texas A&M projects do not hit 75th percentile - wonder if this
  is a spatial issue)

- Only one project has SHM SHAPE score at 90th percentile (UConn)

- Few projects had SHM SHAPE scores at 75th percentile (University of
  Minnesota, Oregon State, NC State is just a hair below the 75th
  percentile)

- No projects had BAU SHAPE scores at or above the 75th percentile

## SHAPE Scores and ESD-STMs

``` r
eco_sites_trt <- eco_sites %>%
  left_join(select(project, dsp_plot_id, label, trt), by="dsp_plot_id") %>%
  distinct()

eco_sites_shape <- eco_sites %>%
  left_join(shape_spatial_proj, by="dsp_plot_id")
```

Plot SHAPE scores and SOC% corresponding to different ecological states
for project that correlated sampling sites with ecological states -
UTRGV is the only project that did this.

``` r
# Compile data - need surface SOC%
utrgv_soc_surf <- surf %>%
  select(project, dsp_pedon_id, soc_pct) %>%
  filter(project=="UTRGV")

shape_soc_utrgv <- eco_sites_shape %>%
  filter(project.y=="UTRGV") %>%
  select(project.y, dsp_pedon_id, ecological_site, ecological_state, soil, label, score_mean_soc) %>%
  rename(project = project.y,
         soc_shape = score_mean_soc) %>%
  left_join(utrgv_soc_surf, by=c("dsp_pedon_id", "project")) %>%
  rename(soc_measured = soc_pct) %>%
  pivot_longer(cols=soc_shape:soc_measured,
               names_to="type", values_to="value")

# Make plot
ggplot(shape_soc_utrgv, aes(x=ecological_state, 
                          y=value, 
                          fill=ecological_state)) +
  geom_boxplot() +
  labs(x="Ecological state",
       title="UTRGV - Loamy Bottomland Ecological Site",
       y=NULL) +
  facet_wrap(vars(type), scales="free",
             strip.position = "left", 
             labeller = as_labeller(c(soc_measured = "Measured SOC (%)", soc_shape = "SOC SHAPE Score (Peer Group Percentile"))) +
  scale_fill_manual(values=c("#FED789FF", "#476F84FF")) +
  theme_katy() +
  theme(legend.position="none",
        strip.background = element_blank(),
        strip.placement = "outside")
```

![](dsp4sh_ref_states_figs_files/figure-gfm/fig_5-1.png)<!-- -->

The tricky thing here is that woodland state is not actually the
reference state for this ecological site - the reference state is
savannah!
