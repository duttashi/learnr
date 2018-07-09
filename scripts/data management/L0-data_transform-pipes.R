# recode two-level variable

## base R

### option 1
mtcars$transmission[mtcars$am == 0] = "automatic"
mtcars$transmission[mtcars$am == 1] = "manual"

### option 2
mtcars$transmission <- 
  ifelse(mtcars$am == 0, 
         "automatic", 
         "manual")

## tidyverse

mtcars <- mtcars %>%
  mutate(
    transmission = 
      case_when(
        am == 0 ~ "automatic",
        am == 1 ~ "manual"
      )
  )

# recode multi-level variable

## base R

### option 1
mtcars$gear_char[mtcars$am == 3] = "three"
mtcars$gear_char[mtcars$am == 4] = "four"
mtcars$gear_char[mtcars$am == 5] = "five"

### option 2
mtcars$gear_char <- 
  ifelse(mtcars$gear == 3, 
         "three", 
         ifelse(mtcars$gear == 4, 
                "four", 
                "five")
  )

## tidyverse
mtcars <- mtcars %>%
  mutate(
    gear_char = 
      case_when(
        gear == 3 ~ "three",
        gear == 4 ~ "four",
        gear == 5 ~ "five"
      )
  )

# visualizing multi-level variables, coloring

## base R
mtcars$trans_color <- 
  ifelse(mtcars$transmission == "automatic", 
         "green", 
         "blue")

pdf("plots/scatter_base.pdf", width = 5, height = 3)
plot(mtcars$mpg ~ mtcars$disp, 
     col = mtcars$trans_color)
legend("topright", 
       legend = c("automatic", "manual"), 
       pch = 1, col = c("green", "blue"))
dev.off()

## tidyverse
p1 <- ggplot(mtcars, 
            aes(x = disp, y = mpg, 
                color = transmission)) +
  geom_point()
ggsave("plots/scatter_tidy.pdf", p1, width = 5, height = 3)

# visualizing multi-level variables, coloring + faceting

## base R
mtcars_cyl4 = mtcars[mtcars$cyl == 4, ]
mtcars_cyl6 = mtcars[mtcars$cyl == 6, ]
mtcars_cyl8 = mtcars[mtcars$cyl == 8, ]


pdf("plots/scatter_facet_base.pdf", width = 5, height = 2)
par(mfrow = c(1, 3))
plot(mpg ~ disp, data = mtcars_cyl4, 
     col = trans_color, main = "Cyl 4")
plot(mpg ~ disp, data = mtcars_cyl6, 
     col = trans_color, main = "Cyl 6")
plot(mpg ~ disp, data = mtcars_cyl8, 
     col = trans_color, main = "Cyl 8")
legend("topright", 
       legend = c("automatic", "manual"), 
       pch = 1, col = c("green", "blue"))
dev.off()

## tidyverse
p2 <- ggplot(mtcars, 
       aes(x = disp, y = mpg, 
           color = transmission)) +
  geom_point() +
  facet_wrap(~ cyl)
ggsave("plots/scatter_facet_tidy.pdf", p2, width = 5, height = 2)
