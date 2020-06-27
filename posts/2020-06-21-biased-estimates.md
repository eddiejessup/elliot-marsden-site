---
title: The cost of biased estimates from large samples
author: Elliot Marsden
---

## Motivation

I was wondering about the relative cost of systematic bias and random noise in the context of surveys. I'm using 'survey' to refer to a situation where we are trying to infer something about a population by sampling from that population, but we aren't trying to compare multiple populations, let alone trying to infer causal relationships. The land of surveys seems like a peaceable kingdom compared to those of observational studies and causal inference: we needn't worry about correlation versus causation, confounders, auto-correlation and so on. We needn't even worry about concerns that matter in a randomised experimental context, like allocation bias, data peeking or post hoc hypothesising. We just have a population, a summary statistic we want to estimate, and we are going to go and get it.

More often than not, the summary statistic we want to know is the population mean: roughly how tall is a woman? Roughly how will a voter vote? Roughly what's the temperature? This further eases our life, because we can appeal to the central limit theorem to get an idea not only of our best guess for the mean, but also how narrowly we've constrained our beliefs. The theorem fits nicely into what I think is the underlying idea of such surveys: there is some (potentially hypothetical) process we can't practically impose: measure every woman's height, peek into the voting booth for every voter, fill every point in space with tiny wireless thermometers. So we simulate the process on a smaller scale to get a subset of the results, and use the central limit theorem to extrapolate our limited results to the result of the infeasible process.

We haven't banished all gremlins from the shady bower of 'surveying for the mean', though. The central limit theorem considers a vector of independent random variables that share a distribution. For our survey to be meaningful, we want that distribution to match that of the population we are picking from. If we aren't careful, our miniaturisation process can distort the distribution we see, such that we end up summarising some other version of the distribution that doesn't represent what we care about: we have 'systematic bias'.

One way this distortion can happen is if we don't pick samples from the population fairly: instead of using a uniformly reduced subset of all points in space for our thermometer placement, we might put them close to the ground, where they are easier to install and maintain. We have 'sampling bias'. We will skew our distribution, and end up summarising the statistics of another distribution that's easier to get hold of, but not what's ultimately good for us.

Another way this distortion can happen is if there is some transformation of our sample that we wouldn't apply in our idealised process. In our thermometer example, once we have selected a subset of points of space at which to measure the temperature, we must transform the state of the thermometer into a number, to which we can apply statistics. We want this transformation to be extensionally equal to a more definitional transformation like 'return the average kinetic energy of each particle in the thermometer', but hopefully much easier to apply. Providing such an easy transformation is the essential job of a thermometer; example instructions might read 'return the nearest number to the surface of the red liquid'. If our transformation from thermometer to number differs from the idealised transformation, our survey will have systematic bias, in the form of 'measurement bias'. Again, we are still producing an unbiased estimate of _some_ distribution, it just isn't the distribution we care about.

I have the impression that of these two causes of distortion, sampling bias gets more attention, at least in surveys whose population consists of people. However, I wanted to investigate the effect of measurement bias in such contexts. I'm specifically interested in surveys of behaviour where we feel good about having access to lots of data. In some sense, bigger data eventually fixes sampling bias for any finite population: as you sample more people, eventually you approach the population size, and there isn't such a thing as a biased sample of the whole population. This is unrealistic, but I think the intuition informs our perspective of large data sets: the data probably has sampling bias, but it's such a big sample it must be representative of _some_ broad group, because there isn't really _any_ casually collected data set of ten million people whose member are all unusual in quite the same way.

I think this leads to an understandable intuition that whether you are dominated by systematic bias or random noise, more data will improve estimates at any margin, as long as you don't wilfully increase bias. However, it seems likely that the marginal cost of acquiring an extra data point increases quite rapidly, for a constant amount of bias: getting an accurately measured, representative sample of ten thousand people might be more than proportionately costly than getting a sample of the same quality with one hundred people. When bias dominates random noise as the limit on uncertainty, might _decreasing_ the sample size let us _increase_ the accuracy of our estimates, for the same cost?

## Investigation

Let's consider two competing protocols, with the common aim of estimating the mean of some distribution. We will assume we have a population expressing some quantity $x$ that is distributed normally, with mean $\mu_0$ and standard deviation $\sigma$.

Our first protocol samples directly from this distribution (i.e. there is no sampling bias), and makes an honest measurement of the quantity (i.e. there is also no measurement bias). We will take $n_u$ ($u$ for 'unbiased') samples, which we can imagine is a small number, perhaps since the cost of this protocol scales badly.

Our second protocol also samples directly from this distribution (i.e. there is no sampling bias), but makes a systematic additive transformation of the true value of the quantity, with magnitude $b$: it has some measurement bias. We will take $n_b$ ($b$ for 'biased') samples, which we can imagine is larger than $n_u$; otherwise the competition isn't much fun.

If we care about getting a good estimate of the population mean, for fixed $b$ and $n_b$, what is the smallest $n_u$ where we should prefer the unbiased protocol to the biased protocol?

In the unbiased protocol, the central limit theorem tells us that when we take our $n_u$ samples, and compute the mean over our sample, we should expect to end up with a random variable whose distribution $p(x)$ is the normal distribution with mean $\mu_0$, and variance $\sigma^2 / n_u$. This random variable is what we will use to summarise our protocol, and the equivalent variable for the biased protocol.

To measure the quality of a protocol, let's consider the root-mean-square deviation of the associated random variable defined above:

$$\text{rms}_p := \sqrt{\int p(x) (x - \mu_0)^2 \, dx} := \sqrt{A}$$

$$A = \int p(x) x^2 - 2 x \mu_0 p(x) + \mu_0^2 p(x) \, dx$$

We can recognise the second quantity as a constant multiple of the expected value of $p(x)$, and the third as a constant multiple of the integrated distribution, which is $1$ by definition. The first quantity is the second _non-central_ moment about $\mu_0$ (as opposed to the more common second _central_ moment, also known as the variance, which is centred on the expected value). A bit of mundane maths tells us that the second moment of a normal distribution with parameters $\sigma_p$ and $\mu_p$, centred on some point $c$ is $\sigma_p^2 + (\mu_p - c)^2$. Using these substitutions and cancelling some terms simplifies our expression to,

$$A = \sigma_p^2 + (\mu_p - \mu_0)^2$$

We know these values for the unbiased and biased protocols, so we can conclude that,

$$\text{rms}_u = \sqrt{\sigma^2 / n_u} \, , \quad \text{rms}_b = \sqrt{\sigma^2 / n_b + b^2}$$

We should switch our preferred protocol around the point where the uncertainties arising from each protocol are equal, so requiring $\text{rms}_u = \text{rms}_b$ and rearranging,


$$\boxed{n_u = \frac{1}{1 / n_b + (b / \sigma)^2}}$$

To recap, this formula tells us that if we have a normally distributed population with standard deviation $\sigma$, and we are given two options for generating data: an unbiased sample of size $n_u$, or a sample of size $n_b$ with a constant added measurement bias $b$, we should prefer the unbiased sample when its size is greater than the right-hand side above.

This is a bit abstract, so let's have an example. Let's assume we are surveying the height of men. The height of men has a standard deviation of around 7.6 centimetre. Let's say we could get a sample of 1,000 heights through an online survey, where we tell the respondents how to measure their height. Maybe each respondent over-reports their height by 1 centimetre, maybe because they like the idea of being taller, or due to some physical awkwardness from measuring one's own height. So we have $n_b = 10^3$, and $b = 2$. Maybe we are considering another option where a third party would measure and report the result, and this produces a practically unbiased estimate. This method may be more costly per respondent. How big would the third-party-reported protocol sample have to be to produce an equally good estimate as the self-reported protocol sample?

$$n_u = \frac{1}{1 / 10^3 + (1 / 7.6)^2} = \frac{1}{0.001 + 0.017} \simeq 56$$

Around fifty expensively obtained samples may cost less than a survey reaching one thousand people.

We can generalise a bit by dividing by $n_b$, to obtain the _share_ of the biased sample size we would need, if we had an unbiased measurement:

$$\boxed{\frac{n_u}{n_b} = \frac{1}{1 + n_b (b / \sigma)^2}}$$

For example, if we had 1000 samples with a bias on the scale of 5% of the population standard deviation, we could do as well with 28% of the data, if we had no bias.

## Extension

A fair objection to the above line of reasoning is that we don't know that an alternative protocol will have exactly zero measurement bias. We can account for this by introducing an extra, uncorrelated additive bias that is present in both protocols. This increases the uncertainty for both protocols, but when we equate their errors in the same way as above, the common bias term cancels out, such that the formula reduces again to the one presented. We can therefore extend the argument to apply to situations where we are considering a change in our procedure to remove one marginal source of measurement bias, even when it is potentially one source among many.

## Conclusion

In many scenarios we might pursue more data without question, to reduce our uncertainty of a population mean. I think the above logic motivates a view where we consider bias and noise as functions of our sample size and investment cost, and make decisions to minimise uncertainty over that space. It is difficult in many situations to quantify how much measurement bias is up for grabs by changing our sampling procedure, but we can usually put bounds based on heuristic reasoning. If we assume there are some mundane variable costs involved in gathering extra data points, it may be worthwhile to consider how much payoff we could get, for a similar investment cost, by instead trying to reduce systematic errors. We might find that where we assumed we were in a noise-limited regime, in fact our uncertainty is limited by bias.
