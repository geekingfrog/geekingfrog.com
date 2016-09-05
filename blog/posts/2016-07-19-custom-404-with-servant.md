---
title: Custom 404 with servant
tags: haskell, servant
status: published
---

I'm rebuilding my website + blog using [servant](https://hackage.haskell.org/package/servant). It's mostly for educational purposes since there exist loads of great solutions for my needs.

Servant is meant to create API from datatypes. It leverage http content negotation mechanism to render a haskell type to the correct representation.

The servant doc is pretty good, but one thing I couldn't find is how to do a custom 404 page. The idea is to use `Raw` to plug a custom WAI application and put it as the last handler of the api. The tricky part is then to construct the response using only warp's primitives.

```haskell
type websiteAPI =
  -- some servant endpoints
  :<|> Servant.Raw


websiteImplementation =
  -- handler implementation
  :<|> custom404


-- Application is a synonym for
-- Network.Wai.Request -> (Network.Wai.Response -> IO ResponseReceived) -> ResponseReceived
custom404 :: Application
custom404 _ sendResponse = sendResponse $ responseLBS status404
                             [("Content-Type", "text/html; charset=UTF-8")]
(renderMarkup Errors.notFound)
```

Where `Errors.notFound` is a simple view of type `Text.Blaze.Html5.Markup`.
For example:

```haskell
notFound = docTypeHtml $ body $ h1 "not found"
```

This doesn't do correct content negotiation though. In my case it's not an issue. Otherwise a regular servant handler with the possible content type would be better, although I don't know how to do a default content-type.