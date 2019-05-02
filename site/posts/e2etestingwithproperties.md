---
title: Rapid E2E-testing with property-based integration tests.
author: Marc Coquand
date: 2019-05-02
---

To ensure that our program works correctly there are three layers that need to
check: the integration layer, unit layer and entire application, so called
End2End-test. Common wisdom is that the higher up you move on these layers the
more expensive it gets, so unit tests are cheap, integration tests more
expensive and E2E-tests most expensive. Thus the recommendation is that you
write mostly unit tests, then less integration tests and even less
End2End-tests. This is called the testing pyramid.

Testing environments can be difficult. But using property-based testing we can
find more obscure concurrency bugs easier. When we do integration tests for
environments using property based testing we create a symbolic representation
that we test against the effectful representation. We then assert that after a
random set of actions the symbolic and the effectful representation have the
same properties. This means that afterwards when we have asserted they behave
the same we can swap one for the other, ensuring that in practice our mock is
the same as the actual implementation.

We use End2End-test to give us confidence that the program works as it should
from start to finish. They also allow us to make sure that we have completed the
objectives in the specification. In practice, these tests are difficult since we
need to spin up mocked environment with fake services running in the background.
This means that it can be tempting to just skip these tests all together.

However does it really have to be this way? If the integration tests have
ensured that the symbolic and actual implementation have the same properties, we
can actually substitute one for the other. This means that if every environment
has been integration tested, the End2End-tests can be ran using the symbolic
environment. We do not need to worry about strange concurrency bugs happening,
or to clarify, it is not the goal of the End2End-test anymore. That
responsibility is in the integration test.

So knowing that, say we have a web application with a CRUD REST api and a client
using react/redux . We want to test the client side and ensure it's correct and
meets the specification. We have two borders that talk to the real world, the
REST api and react that is displaying html to the browser. So to cover the borders we have
to do two integration tests.

To test the CRUD api, we hide the CRUD api behind an interface (inversion of
control). Afterwards we instance that interface also with a symbolic
representation, using a hashmap as our store. By generating a list of actions
done, we check that if the prequisities for the CRUD api and the hashmap are the
same, after performing the action they have the same property. For example if we
have performed insert item with `itemId` then `get(itemId)` should return `item`
for both the symbolic and CRUD api.

Next we need to integration test the html code consisting of a view function. A
view function is defined as `view : State -> Html Action`. We can create a
symbolic representation of this by creating a function which takes a state and
returns a list of actions, `viewSymbolic : State -> [Actions]`. Now the property
we need to test is that `forall State s. actions(view(s)) === viewSymbolic(s)`
where `actions : Html Action -> [Action]`, it extracts all the actions that view
can dispatch.

Now we can test the entire application. If we use the TEA architecture we have
one big state and a sum type for our actions. We have a tested interface for the
REST API that we can substitute with the symbolic representation to use with the
reducer. Now it's easy to test actions on the reducer and ensure whatever
properties we want to hold (for example the user can't login and then reach
login page). If we want, we can also create tests for user stories, like given
the list of actions `buy=[GOTOLOGIN, SENDLOGINDETAILS, ADDTOCART,FILLINDETAILS, BUY]` and an initial state `s` if we reduce it we should end up with a goal
state `s'` and at each step should the action be a member of
`viewSymbolic(currentState)`.

Doing this we can create really rapid E2E-tests that don't need an entire
environment. We will also quicker find concurrency bug and their source by
testing at the borders of our applications rather than testing the entire thing.
The E2E-test works more as a way for us to ensure that the specification is
fulfilled.
