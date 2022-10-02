---
title: Rails tutorial
status: published
tags:
- rails
---

When I was doing this <a href="http://ruby.railstutorial.org/chapters/">exellent tutorial</a> about rails, I got some problems the exercise 8.5 (ie : use session in place of cookies), the <a href="http://ruby.railstutorial.org/chapters/updating-showing-and-deleting-users?version=3.2#code:sign_in_helper">listing 9.6</a> doesn't work anymore. I had to search how to use session inside the utilities.rb file. Just replacing the word <em>cookies</em> by <em>session</em> does not work, got :

`undefined method 'session' for nil:NilClass`

The right code is here :

```
# Sign in when not using Capybara as well
post sessions_path, :session => { remember_token: user.remember_token }
```