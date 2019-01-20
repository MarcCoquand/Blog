---
title: Vietnam Ramblings, Week One
author: Marc Coquand
date: 2019-01-19
---

Arriving in Vietnam is a breath of hot, dirty air. Mixed with the beautiful
fauna is a smog so dense it fills up your lungs and makes you wonder how many
years you give up by being here. It is the first country I have been to where
the people seem to love foreigners, which is refreshing and makes me happy. 

The city is filled with a refreshing entrepreneurial spirit. Everyone seems to
be doing freelance gigs, run a store or something else.  Maybe it is the
recovery from the wars, making this generation responsible for reviving the city
and bringing vietnamese economy back. Maybe it is something exclusive for Ho Chi
Minh City. I do not know. 

As far as development goes. I finally have the blog up and running. I made a
sweet animation for the home page and also finalized the styling to achieve a
nice mixture of modern and classical while keeping it very minimal. 

I noticed that Github Pages is a nightmare to work with. For some reason if you
want a personal webpage, I.E. a webpage like marccoquand.github.io, you have to
publish it on the `master` branch starting from the root directory. If you
publish repository specific web pages you get the options to publish it on a
specific branch named `gh-pages` or the `/docs` folder. I am using a Haskell
library called sitepipe to generate this Blog, and it only provides the option
to generate a folder named dist with the website in it. So I figured that my
best bet would be to have a `dev` branch and then use submodules to push the
website content to the master branch. Sadly Sitepipe flushes the entire
directory before generating the website which means it also clears the submodule
metadata. So after running out of options I just decided to switch to netlify
where I could select which directory on which branch I wanted to use as my
website. So much better... Truly I do not understand why Github makes it so
complicated.


