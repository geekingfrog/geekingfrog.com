---
title: Contextual logging in clojure with timbre
tags: clojure, logging
status: published
---

# Contextual logging?

Often, and especially when things go wrong, it is very useful to trace what happened for a given request, job or unit of work. However, under any kind of load, different requests will be processed concurrently, and so the logs will be interleaved.

For example, let's consider the following function:

```clojure
(ns core
  (:require [taoensso.timbre :as t]))

(defn do-work [request-id]
  (t/info "start working for request-id" request-id)
  ;; do-stuff
  (t/info "done working on request" request-id))
```

This works, but it gets very cumbersome to have to manually thread through all the logging contextual info. And it's a prohibitive effort to do such refactoring on a large codebase.

# vars and bindings to the rescue

Logging is typically one of the very few places where having some global state (or thread local) makes sense and is unlikely to haunt you later. So let's use a global var to store the logging context and create a wrapper macro to automatically use that.

```clojure
;; let's hardcode that for now
(def *log-ctx* {:request-id "123456"})

(defmacro log-with-context
  [level line & args]
  `(if (empty? *log-ctx*)
     (t/log! ~level :p ~args ~line)
     (t/log! ~level :p [*log-ctx* ~@args] ~line)))

(defmacro info [& args] `(log-with-context :info ~{:?line (:line (meta &form))} ~@args))
```
The `&form` is a [special form](https://clojure.org/reference/macros#_special_variables) available in macro, and the whole `~{:?line (:line (meta &form))}` is copied from [the source code of timbre](https://github.com/ptaoussanis/timbre/blob/master/src/taoensso/timbre.cljc#L780) and gives you the line number of the call site.


And this gives us a log like:

```
2022-06-29T12:47:24.069Z arch-desktop INFO [core:27] - {:request-id "123456"} start working for request-id
2022-06-29T12:47:24.070Z arch-desktop INFO [core:28] - {:request-id "123456"} done working on request
```

# Making the context dynamic and scoped

So far this is promising, we can automatically inject some context into every log call, but this is hardcoded, let's fix that.

```clojure
(def ^:dynamic *log-ctx* {})
;;   ☝ allow override of the var

(defmacro with-log-context
  "given a map of key-value, execute the body with the log context
  augmented by the given context. At the end of the execution, the
  original context is restored."
  [ctx & body]
  `(with-bindings {#'*log-ctx* (merge *log-ctx* ~ctx)}
     (do ~@body)))
```

And now, we can use this useful macro as such:

```clojure
(defn do-work [& _args]
  (info "no context")
  (with-log-context {:request-id "123456"}
    (info "starting work on a new request")
    (with-log-context {:job-id "job-foobared"}
      (info "doing work on job"))
    (info "finished work on request"))
  (info "final log call with no context"))
```

and this gives:

```
2022-06-29T13:00:09.611Z arch-desktop INFO [core:31] - no context
2022-06-29T13:00:09.612Z arch-desktop INFO [core:33] - {:request-id "123456"} starting work on a new request
2022-06-29T13:00:09.613Z arch-desktop INFO [core:35] - {:request-id "123456", :job-id "job-foobared"} doing work on job
2022-06-29T13:00:09.613Z arch-desktop INFO [core:36] - {:request-id "123456"} finished work on request
2022-06-29T13:00:09.613Z arch-desktop INFO [core:37] - final log call with no context
```

# What about threads?

[`with-bindings`](https://clojuredocs.org/clojure.core/with-bindings) uses thread-local storage, and clojure's vars have a property called [binding conveyance](https://clojure.org/reference/vars#conveyance), meaning a bunch of functions like `future` and `pmap` will automatically forward the logging context to other threads.

For example, the following example works beautifully:

```clojure
(defn multi-threaded []
  (with-log-context {:request-id "123456"}
    (pmap #(with-log-context {:thread-id (.getId (Thread/currentThread))}
             (info "doing work in a thread")
             %)
          (range 3))))
```

```
2022-06-29T13:06:54.167Z arch-desktop INFO [core:51] - {:request-id "123456", :thread-id 248} doing work in a thread
2022-06-29T13:06:54.167Z arch-desktop INFO [core:51] - {:request-id "123456", :thread-id 249} doing work in a thread
2022-06-29T13:06:54.167Z arch-desktop INFO [core:51] - {:request-id "123456", :thread-id 246} doing work in a thread
```

This, however, does _not_ work for `core.async` where there are concurrent execution sharing the same thread context.


# Conclusion

With a few lines of code and a couple of wrapper, one can easily add logging information in a scoped manner. Stick these macros in a namespace, and gradually replace your call to `timbre/info` and the likes, and voilà!
