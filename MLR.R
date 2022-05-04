input_649 <- mtcars[,c("mpg","disp","hp","wt")]
print(head(input_649))
input_649 <- mtcars[,c("mpg","disp","hp","wt")]
model_649 <- lm(mpg~disp+hp+wt, data = input_649)
print(model_649)
cat(" The Coefficient Values","\n")
a_649 <- coef(model_649)[1]
print(a_649)

disp_649 <- coef(model_649)[2]
hp_649 <- coef(model_649)[3]
wt_649 <- coef(model_649)[4]

print(disp_649)
print(hp_649)
print(wt_649)