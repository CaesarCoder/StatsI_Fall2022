House <- read.table("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv")

saveRDS(dat, "")

ggplot(aes(x = PropertyType, y = SalePrice), # We use the aes() function to supply an x and y argument
       data = House) + # We use the `+` operator to add an additional geom
  geom_point(

plot(SalePrice ~ )



House$SalePriceK <- House$SalePrice / 1000

House$AdjSalePriceK <- House$AdjSalePrice / 1000


boxplot(AdjSalePriceK ~ NewConstruction, data = House)


boxplot(AdjSalePriceK ~ PropertyType, data = House) 


boxplot(AdjSalePriceK ~ Bedrooms, data = House)


boxplot(AdjSalePriceK ~ Bathrooms, data = House)


boxplot(AdjSalePriceK ~  , data = House)
