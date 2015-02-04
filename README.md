Inference 2

babies = read.table("babies.txt", header=TRUE)

q1:

bwt.nonsmoke = babies$bwt[babies$smoke==0]

bwt.smoke = babies$bwt[babies$smoke==1]

CIs = replicate(1000, t.test(sample(bwt.nonsmoke, 30), sample(bwt.smoke, 30))$conf.int)

mean(CIs[2,] - CIs[1,])

q2:

bwt.nonsmoke = babies$bwt[babies$smoke==0]

bwt.smoke = babies$bwt[babies$smoke==1]

popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)

mean(CIs[1,] < popdiff & CIs[2,] > popdiff)

q3 & q4:

Z-score= 1.96

q5:

bwt.nonsmoke = babies$bwt[babies$smoke==0]

bwt.smoke = babies$bwt[babies$smoke==1]

run_experiment = function(N, alpha) {

N=15

alpha=0.1

dat.ns = sample(bwt.nonsmoke, N)

dat.s = sample(bwt.smoke, N)

t.test(dat.ns, dat.s)$p.value < alpha

}

mean(replicate(1000, run_experiment(N, alpha)))

q6:

# same as q5 but rewrite alpha = 0.05

q7:

# same as q5 but rewrite alpha = 0.01

q8:

0.01*2000=20  it is 20


d = read.csv("assoctest.csv")

q9:

tab = table(d$allele, d$case)

chisq.test(tab)

q10:

tab = table(d$allele, d$case)

fisher.test(tab)
