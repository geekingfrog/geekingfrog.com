---
title: Breadcrumbs with ember
tags: coffeescript, ember
status: published
---

So today I refactored some part of our application and I redid the breadcrumbs part. All routes are stored in `App.Router.router.currentHandlerInfos`. So by using this, I could compute the needed informations and put that in a controller:

```
App.BreadcrumbsController = Ember.Controller.extend(
  needs: "application"

  breadContent: Ember.computed ->
    routes = App.get("Router.router.currentHandlerInfos")
    segments = _.map(_.tail(routes), (route) ->
      if route.isDynamic and route.context
        name = route.context.get("id")
        context = route.context
      else
        dotIndex = _.lastIndexOf(route.name, '.')+1
        name = Ember.String.capitalize(route.name.slice(dotIndex))

      return {
        name: name
        routePath: route.name
        context: context
      }
    )
    return segments
  .property("controllers.application.currentPath")
)
```

The _.tail is used to get rid of the "application" route which is always here. Then for the route name, I only take what is after the last dot, so a route: 'user.edit' will appear as 'edit'. Note the dependency on ApplicationController.currentPath. This make sure the property is updated whenever the route changes.

The template look like this:

```
<ul class="breadcrumb">
  <li>{{#linkTo 'index'}}<i class="icon-home"></i>Home{{/linkTo}}</li>
  {{#each segment in breadContent}}
  <li>
  <span class="divider">&gt;</span>
  <a href="javascript://" {{action gotoRoute segment}} >{{segment.name}}</a>
  </li>
  {{/each}}
</ul>


```

And we also need to add the gotoRoute in the controller:

```
  gotoRoute: (routeOpt)->
    if routeOpt.context
      @transitionToRoute(routeOpt.routePath, routeOpt.context)
    else
      @transitionToRoute(routeOpt.routePath)
```

This action is here because ember's linkTo helper cannot get a route with a variable name.
Finally, wherever you need the breadcrumbs, call it with `{{ render breadcrumbs }}`

And that's it, now you have easy breadcrumbs, which links to the right part of the app. Enjoy