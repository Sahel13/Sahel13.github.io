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
inference-time alignment of diffusion and large language models.

For academic details, see my [resume](/files/cv_sahel_iqbal.pdf) and my [Google
Scholar](https://scholar.google.com/citations?user=KP7mJUgAAAAJ&hl=en) profile.
I can be contacted on [X](https://x.com/sahel_iqbal) and at my email
sahel[dot]iqbal[at]aalto[dot]fi.

Outside work, my time is mostly taken up by reading, lifting weights, and
writing JAX code. The projects that I'm actively working on are available on
[GitHub](https://github.com/Sahel13).

## Recent News

- **2025-07**: If you're attending [MCM
  2025](https://ccbatiit.github.io/mcm2025/), my coauthor [Adrien
  Corenflos](https://adriencorenflos.github.io/) will be giving a talk on our
  joint work on BED.
- **2025-06**: Gave a talk on using particle filters for amortized BED at the
  _[Accelerating statistical inference and experimental design with machine
  learning workshop](https://www.newton.ac.uk/event/rclw03/)_ at the Isaac Newton
  Institute for Mathematical Sciences.
- **2024-12**: Presented a poster at the [Bayesian Decision-making and
  Uncertainty workshop](https://gp-seminar-series.github.io/neurips-2024/) at
  NeurIPS 2024 in Vancouver.

## Posts

$for(recentPosts)$

<div class="post-item">
  <p class="post-date">$date$</p>
  <p class="post-description">
    <span class="post-title"><a href="$url$">$title$</a></span> ---
    $description$
  </p>
</div>

$endfor$

## Featured Publications

**Sahel Iqbal**, Hany Abdulsamad, Sara Pérez-Vieites, Simo Särkkä, Adrien Corenflos (2024). _Recursive Nested Filtering for Efficient Amortized Bayesian Experimental Design_. NeurIPS workshop on Bayesian Decision-making and Uncertainty. [arXiv](https://arxiv.org/abs/2409.05354). [Code](https://github.com/Sahel13/InsideOutNPF.jl).

**Sahel Iqbal**, Adrien Corenflos, Simo Särkkä, Hany Abdulsamad (2024). _Nesting Particle Filters for Experimental Design in Dynamical Systems_. ICML. [arXiv](https://arxiv.org/abs/2402.07868). [Code](https://github.com/Sahel13/InsideOutSMC.jl).
