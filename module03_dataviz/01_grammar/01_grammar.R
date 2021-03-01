
## Basics

### set up workspace and read tutorial data
library(tidyverse)
library(viridis)
library(ggrepel)
library(scales)
library(lubridate)

vac <- read_csv("https://www.dropbox.com/s/wy0c03ib8lvds6r/country_vaccinations.csv?dl=1")

vac_top <- vac %>%
    group_by(country) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    arrange(desc(people_vaccinated_per_hundred)) %>%
    slice(1:10)

countries <- c("Israel", "Denmark", "Switzerland", "Germany", "France")
vac_subset <- vac %>%
    filter(country %in% countries)

vac_subset_last <- vac_subset %>%
    group_by(country) %>%
    filter(min_rank(desc(date)) == 1)

shapes <- 21:25
names(shapes) <- countries

### explore data
vac
vac %>%
    head()
vac %>%
    colnames()

### Base R plots
plot(vac$date, vac$total_vaccinations)
boxplot(vac$people_vaccinated ~ vac$country)
dotchart(vac_top$total_vaccinations_per_hundred, labels = vac_top$country)

boxplot
dotchart


### Dotchart ggplot2 version
ggdotchart <- function(df, x, label){
    df <- df %>%
        mutate(y = fct_reorder(!! sym(label), desc(!! sym(x))))

    p <- ggplot(df, mapping = aes(x = !! sym(x), y = y)) +
        geom_hline(aes(yintercept = country), linetype = "dashed", colour = "grey", size = 0.25) +
        geom_point(size = 2, shape = 1) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    p
}
ggdotchart(vac_top, "total_vaccinations_per_hundred", "country")

## Coordinate systems

### simple x y plot
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point()

### explicit coordinate system
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    coord_cartesian()

### zoom in
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    coord_cartesian(ylim = c(0, 1e6))

### other coordinate system
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    coord_polar()


## Aesthetics

### map shape to country
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country)) +
    geom_point()

### map colour to daily vaccinations
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, colour = daily_vaccinations_raw)) +
    geom_point()

### map size and transparency to daily vaccinations
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, size = daily_vaccinations_raw, alpha = daily_vaccinations_raw)) +
    geom_point()

### mapping vs setting aesthaetic
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, colour = daily_vaccinations_raw)) +
    geom_point()
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, colour = "daily_vaccinations_raw")) +
    geom_point()

### build main plot
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_point()


## Geometric shapes

### simple point
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point()

### aesthetics of points
df <- tibble(x = factor(0:25), y = 1)
ggplot(df, mapping = aes(x = x, y = y)) +
    geom_point(shape = 0:25, size = 4)
ggplot(df, mapping = aes(x = x, y = y)) +
    geom_point(shape = 0:25, colour = 1:26, fill = "grey", size = 4, stroke = 1)

### aesthetics of lines
df <- tibble(x = 0:5, xend = 0:5, y = 0, yend = 10)
ggplot(df, mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
    geom_segment(size = 3, linetype = 1:6)

### path vs line
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_path()
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_line()

### other geoms
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point(aes(fill = country)) +
    geom_hline(yintercept = 1e6, linetype = "dashed")

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_col(aes(fill = country))

### position adjustments
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_col(aes(fill = country), position = position_stack())
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_col(aes(fill = country), position = position_dodge())
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_col(aes(fill = country), position = position_fill())
ggplot(vac_subset, mapping = aes(x = country, y = daily_vaccinations_raw)) +
    geom_point(size = 1)
ggplot(vac_subset, mapping = aes(x = country, y = daily_vaccinations_raw)) +
    geom_point(size = 1, position = position_jitter(height = 0, width = 0.15), alpha = 0.5)


### different data for geoms
vac_subset_last <- vac_subset %>%
    group_by(country) %>%
    filter(min_rank(desc(date)) == 1)
vac_subset_last

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    geom_point(data = vac_subset_last, size = 3, shape = 17)

### build main plot
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_path() +
    geom_point(size = 2, data = vac_subset_last)


## Facets

### wrap panels
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_point() +
    facet_wrap(~country)

### panels in grid
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_point() +
    facet_grid(country ~ .)
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_point() +
    facet_grid(country ~ vaccines)

### multiple factors
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_point() +
    facet_wrap(~country+vaccines)

### build main plot
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_path() +
    geom_point(size = 2, data = vac_subset_last) +
    facet_grid(vaccines ~ .)


## Transformations

### summarising geoms
ggplot(vac_subset, mapping = aes(x = date)) +
    geom_bar()
ggplot(vac_subset, mapping = aes(x = date)) +
    geom_dotplot()
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_boxplot()

### explicit specificaion of summary stat
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    stat_summary(fun = sum)

### change geom for summary
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    stat_summary(fun = sum, geom = "bar")

### more complex summary stat
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    stat_summary(fun = median, fun.min = min, fun.max = max)

### smoothing
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    stat_smooth()

### stratify summary
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    stat_smooth(aes(group = country))

### change methods
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    stat_smooth(aes(group = country), method = "lm")


## Scales

### colour scales
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, colour = daily_vaccinations_raw)) +
    geom_point()

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, colour = daily_vaccinations_raw)) +
    geom_point() +
    scale_colour_gradient(low = "blue", high = "red")

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_point() +
    scale_colour_gradientn(colours = rainbow(100))

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, colour = country)) +
    geom_point()


### specialized colour palettes
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, colour = country)) +
    geom_point() +
    scale_color_brewer()

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, colour = country)) +
    geom_point() +
    scale_color_brewer(palette = "Set1")

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, colour = country)) +
    geom_point() +
    scale_color_brewer(palette = "Spectral")

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_point() +
    scale_colour_distiller(palette = "Spectral")

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_point() +
    scale_colour_viridis()


### manual discrete scales
shapes <- 21:25
names(shapes) <- countries

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_point() +
    scale_colour_viridis() +
    scale_shape_manual(values = shapes)


### adjusting scales
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_point() +
    scale_colour_viridis() +
    scale_shape_manual(values = shapes) +
    scale_y_continuous(trans = "log10")

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_point() +
    scale_colour_viridis() +
    scale_shape_manual(values = shapes) +
    scale_y_continuous(limits = c(0, 1e6))

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw)) +
    geom_point() +
    scale_colour_viridis() +
    scale_shape_manual(values = shapes) +
    scale_y_continuous(labels = label_number())

### build main plot
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw, fill = daily_vaccinations_raw)) +
    geom_path() +
    geom_point(size = 2, data = vac_subset_last) +
    facet_grid(vaccines ~ .) +
    scale_y_continuous(labels = label_comma()) +
    scale_colour_viridis(labels = label_comma()) +
    scale_fill_viridis(labels = label_comma()) +
    scale_shape_manual(values = shapes)


## Annotations
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point()

vac_subset_date <- vac_subset %>%
    filter(date > ymd(20210115), date < ymd(20210120))

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    geom_text(aes(label = total_vaccinations), size = 3, data = vac_subset_date)

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    geom_text_repel(aes(label = total_vaccinations), size = 2, min.segment.length = 0, data = vac_subset_date)

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    geom_label_repel(aes(label = total_vaccinations), size = 2, min.segment.length = 0, data = vac_subset_date)


### build main plot
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw, fill = daily_vaccinations_raw)) +
    geom_path() +
    geom_point(size = 2, data = vac_subset_last) +
    geom_text_repel(aes(label = country), size = 2, color = "black", direction = "y", data = vac_subset_last) +
    facet_grid(vaccines ~ .) +
    scale_y_continuous(labels = label_comma()) +
    scale_colour_viridis(labels = label_comma()) +
    scale_fill_viridis(labels = label_comma()) +
    scale_shape_manual(values = shapes)


## finishing

### themes
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    facet_grid(vaccines ~ .) +
    theme_gray()

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    facet_grid(vaccines ~ .) +
    theme_bw()

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    facet_grid(vaccines ~ .) +
    theme_classic()

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    facet_grid(vaccines ~ .) +
    theme_void()

### customize themes
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    facet_grid(vaccines ~ .) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    theme_bw() +
    facet_grid(vaccines ~ .) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), strip.background = element_rect(fill = "gainsboro", color = NA), strip.text = element_text(size = 7))

### axis labels
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations)) +
    geom_point() +
    theme_bw() +
    facet_grid(vaccines ~ .) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), strip.background = element_rect(fill = "gainsboro", color = NA), strip.text = element_text(size = 7)) +
    xlab("Date") +
    ylab("Total vaccinations")


### finalize and save main plot
ggplot(vac_subset, mapping = aes(x = date, y = total_vaccinations, shape = country, colour = daily_vaccinations_raw, fill = daily_vaccinations_raw)) +
    geom_path() +
    geom_point(size = 2, data = vac_subset_last, show.legend = FALSE) +
    geom_text_repel(aes(label = country), size = 2, color = "black", direction = "y", data = vac_subset_last) +
    facet_grid(vaccines ~ .) +
    scale_y_continuous(labels = label_comma()) +
    scale_colour_viridis(name = "Vaccinations per day", labels = label_comma()) +
    scale_fill_viridis(name = "Vaccinations per day", labels = label_comma()) +
    scale_shape_manual(values = shapes) +
    xlab("Date") +
    ylab("Total vaccinations") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), strip.background = element_rect(fill = "gainsboro", color = NA), strip.text = element_text(size = 7))
ggsave("plot.pdf")



## Exercises

# visualize the relationship between total number of vaccinations and people fully vaccinated. Map aesthaetics of choice to differentiate countries, add a line to highlight x=y, and use equl unit fixed ratio coordinate system.
# after many total vaccinations were the first german citizens fully vaccinated?
# what does the slope tell us?

ggplot(vac_subset, mapping = aes(x = total_vaccinations, y = people_fully_vaccinated)) +
    geom_point(aes(color = country), size = 1) +
    geom_line(aes(color = country)) +
    geom_abline() +
    coord_fixed()


# plot daily distibution of total number of vaccines in full vac dataset as boxplots
# which date has highest median number? (Dec 15th)
# is this surprising? can we visualize the number of observations at each date?

ggplot(vac, mapping = aes(x = date, y = total_vaccinations)) +
    geom_boxplot(mapping = aes(group = date), varwidth = TRUE) +
    coord_cartesian(ylim = c(0, 1e6))


# visualize the total number of people vaccinated per 100 inhabitants, for each day and country in the full vac dataset, using aesthetic mappings and only a single facet.
# which countries have reached the highest vaccination rates? (Seychelles, Israel, Gibraltar)
ggplot(vac, mapping = aes(x = date, y = country, fill = people_vaccinated_per_hundred)) +
    geom_raster() +
    scale_fill_viridis(option = "A")

vac_last <- vac %>%
    group_by(country) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    arrange(desc(people_vaccinated_per_hundred))

vac <- vac %>%
    mutate(country1 = fct_relevel(country, vac_last$country) %>% fct_rev) %>%
    filter(!is.na(people_vaccinated_per_hundred))

ggplot(vac, mapping = aes(x = date, y = country1, fill = people_vaccinated_per_hundred)) +
    geom_raster() +
    scale_fill_viridis(option = "A")


## recreate this plot
ggplot(vac_subset, aes(x = date, y = total_vaccinations)) +
    geom_hline(yintercept = 0) +
    geom_area(aes(fill = country), colour = "white", size = 0.25) +
    scale_fill_brewer(name = "Country", palette = "Set1") +
    scale_y_continuous(label = label_comma()) +
    xlab("Date") +
    ylab("Total vaccinations") +
    theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())
ggsave("exercise_plot_1.pdf")


## recreate this plot
ggplot(vac_subset, mapping = aes(y = country, x = daily_vaccinations)) +
    geom_violin(scale = "width", fill = "gainsboro", color = NA) +
    geom_point(aes(fill = date), colour = "white", size = 1.5, position = position_jitter(height = 0.15), shape = 21, alpha = 0.9) +
    scale_x_continuous(label = label_comma()) +
    scale_fill_viridis(name = "Date", option = "B", trans = "date") +
    xlab("Daily vaccinations") +
    ylab("Country") +
    theme_classic() +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())
ggsave("exercise_plot_2.pdf")


