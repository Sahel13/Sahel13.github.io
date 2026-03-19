---
title: Sahel Iqbal
description: Academic portfolio website of Sahel Iqbal.
---

<div class="profile">
  <img alt="Profile picture." src="images/profile-pic.png" />
  <h1>Sahel Iqbal</h1>
</div>

I'm Sahel Iqbal, a third-year PhD student at Aalto University, Finland, where I
work with [Simo Särkkä](https://users.aalto.fi/~ssarkka/). My main
research focus is on developing accurate and efficient Monte Carlo algorithms
for reinforcement learning and Bayesian experimental design (BED). Recently, I
have also developed an interest in how similar algorithms can be used for
inference-time alignment of diffusion and [large language models](posts/steering-llms-with-smc.html).

For details regarding my research, see my [Google
Scholar](https://scholar.google.com/citations?user=KP7mJUgAAAAJ&hl=en) profile.
I can be contacted on [X](https://x.com/sahel_iqbal) and at my email
sahel[dot]iqbal[at]aalto[dot]fi.

Outside work, my time is mostly taken up by reading, lifting weights, and
writing JAX code. The projects that I'm actively working on are available on
[GitHub](https://github.com/Sahel13).

## Recent Posts

$partial("templates/post-list.html")$

<p class="all-articles-link">
  <a href="/blog.html">All articles</a>
</p>

## Recent News

$for(recentNews)$
- **$date$**: $body$
$endfor$
