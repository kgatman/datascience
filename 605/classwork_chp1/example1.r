example <- read.csv("C:/hiding_path_to_file/EXAMPLE.csv")

summary(example)


# Data Exploration

# /*1.*/
# proc means data=example1 min mean median std max;
#     class bench;
#     var height;
# run;

summary(example)

# /*2.*/
# ods graphics on;
# proc sgplot data=example1;
#     scatter x=variety y=height / group=bench;
#     xaxis type=discrete;
# run;


# /*3.*/
# ods graphics on;
# proc sgplot data=example1;
#     vbox height / category=variety;
# run;

boxplot(height ~ variety, data = example, xlab = "variety",
   ylab = "height", main = "Chap1 Example")


# /*4.*/
# ods graphics on;
# proc univariate data=example1;
#     var height;
#     histogram;
# run;

hist(example$height)

# proc mixed data=example1 covtest;
#     class bench variety;
#     model height=variety /s htype=3 e;
#     random bench /s;
#     lsmeans variety /adjust=bon diff=all e;
# run;
