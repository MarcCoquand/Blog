---
date: 2019-10-10
title: Command Hook Pattern for React
author: Marc Coquand
---

When comparing React hooks and Redux, the researcher wonders why one should pick
Redux. I have personally been asked multiple times why Redux should be chosen
over Hooks. According to the article [do react hooks replace
Redux](https://medium.com/javascript-scene/do-react-hooks-replace-redux-210bab340672)

Googling around, they find one key being the powerful Redux middleware
ecosystem. Indeed, access to side effect handling libraries such as Redux-sagas
allows us to keep logic pure and moving side effects to the outwards of the
program.

This posts goes through a pattern for the useReducer hook which replaces the
useEffect hook with a Command hook. The hook is inspired by Cmds in Elm, and
aims to solve the handling of effects. The Command Hook's strengths are:

- Easy to plug into hooks
- Effects in components trivial to unit tests
- Effects are easier to integration test

This comes at the price of:

- Extra infrastructure
- Boilerplate adding effects
