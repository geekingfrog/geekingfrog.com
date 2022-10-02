---
title: Clojurescript, Re-frame and react interop
status: draft
tags:
- clojure
- clojurescript
- web
---

So I wanted to try a bit clojurescript and I ended up spending some time
attempting a couple of things. Here's a quick summary, if only to speed things
up next time.

# Leiningen template and vim

`lein new re-frame dat-project +garden`
This will gives a pretty good base. The `+garden` is an extra to get css through [garden](https://github.com/noprompt/garden), which is a clojure way to generate css.

To get vim with [fireplace](https://github.com/tpope/vim-fireplace) working properly, it needs [piggieback](https://github.com/nrepl/piggieback). Some adjustments for `project.clj`

```clojure
;; Add the following two dependencies under [:profiles :dev :dependencies]
[cider/cider-nrepl "0.24.0"]
[cider/piggieback "0.4.2"]

;; Add the piggieback wrapper in shadow-cljs nrepl middlewares under
;; :shadow-cljs :nrepl :middleware
:middleware [cider.piggieback/wrap-cljs-repl]
```

And then, within vim: `:Connect nrepl://localhost:<nrepl-port>` followed by
`:Piggieback :app`. It's a bit fiddly but works reasonably well.

To run the whole thing:
`lein dev` in a terminal, and `lein garden auto` to watch the css as well.

The result as really amazing. Super quick feedback loop, with everything which just works™.


# Interop with react
By default, the leiningen template will output a file `src/cljs/deps.cljs`, which only contains `{:npm-dev-deps [[shadow-cljs "2.8.109"]]}`. The `lein-shadow` plugin will only look at this file to generate a `package.json` file. So when starting `lein dev`, `package.json` will be overriden, and extra js deps won't be found.

I wanted to play around [framer motion](https://www.framer.com/motion/) which is a react library to define animations and transitions in a declarative way. Add the following to `src/cljs/deps.cljs`: `:npm-deps [[framer-motion "1.10.3"]]`.

To create a component with the correct react class:

```clojure
;; inside src/cljs/dat_project/views.cljs
(:require [["framer-motion" :as fm]])

(defn main-panel []
  [:div {:class "example-container"}
   [:> fm/motion.div {:animate {:scale 2} :transition {:duration 3}}]
   ]
  )
```

The `:>` is a shorthand for `(reagent.core/adapt-react-class fm/motion.div)`.

And the corresponding css in `src/clj/dat_project/css.clj`

```clojure
(ns dat-project.css
  (:require [garden.def :refer [defstyles]]
            [garden.selectors :refer [defclass]]))

(defclass example-container)

(defstyles screen
  [:body {:color "black"}]
  [:h1 {:color "steelblue"}]
  [example-container [:div {:width "150px"
                            :height "150px"
                            :border-radius "50%"
                            :background "red"}]]
  )
```

This will display a bright red circle, growing to twice its initial size. The
dot syntax to access the native javascript component tripped me up. The
shadow-cljs documentation mentionned other way to get that, but none worked.


# Sources

* [github issue](https://github.com/tpope/vim-fireplace/issues/322#issuecomment-417461929) to get fireplace working with cljs files.
* [shadow-cljs npm interop](https://shadow-cljs.github.io/docs/UsersGuide.html#_using_npm_packages)
* [reagent react interop doc](https://github.com/reagent-project/reagent/blob/master/doc/InteropWithReact.md)
* [reagent tutorial](https://purelyfunctional.tv/guide/reagent/)
