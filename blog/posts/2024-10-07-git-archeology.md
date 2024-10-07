---
title: Git archeology
status: published
tags:
  - git
  - TIL
---

Some small tips that makes digging through big git(hub) repos easier.

# Evolution of a function

This one is coming directly from [this article](https://nerderati.com/tracing-the-evolution-of-a-python-function-with-git-log/) to display `git log` but only the relevant commits for a given function.

This requires a little bit of config though, in `~/.gitattributes`:

```
*.ex diff=elixir
*.exs diff=elixir
*.py diff=python
```


# Which github PR introduced a change?

At home and work, I'm using github a lot. And one useful thing is to find
which pull request introduced a specific change.

So I have a scrappy script to do exactly that:

* find the commit hash for the relevant line with [git fugitive](https://github.com/tpope/vim-fugitive) or whatever
  your IDE gives you.

* `gith <sha>` then gives something like `https://github.com/beyond-all-reason/teiserver/pull/424`

Here's the script. It does a basic attempt at figuring out the correct url for the repo.
I'm using [babashka](https://babashka.org/) because why not, better than bash anyway.

```clojure
#!/usr/bin/env bb
;; vim: ft=clojure

(require '[babashka.process :as p]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[babashka.cli :as cli])

(def cli-specs
  {:spec
   {:remote {:alias :r}
    :branch {:alias :b}}
   :exec-args {:remote "origin" :branch "master"}})

(defn get-remote-url [remote-name]
  (let [url (:out (p/shell {:out :string} "git config --get" (str "remote." remote-name ".url")))
        url (subs url 0 (- (count url) 5))] ;; remove the trailing .git
    (if (str/starts-with? url "git@github.com:")
      (str "https://github.com/" (subs url (count "git@github.com:")))
      url)))

(let [{:keys [args opts]} (cli/parse-args *command-line-args* cli-specs)
      remote-url (get-remote-url (:remote opts))
      sha (first args)
      stream (p/process {:err :inherit
                         :shutdown p/destroy-tree}
                        "git" "log" (str sha ".." (:branch opts)) "--ancestry-path")
      line-stream (-> stream :out io/reader line-seq)
      last-merge (->> line-stream
                      (map #(re-matches #"\s*Merge pull request #(\d+) from.*" %1))
                      (filter some?)
                      last
                      second)]
  (println (str remote-url "/pull/" last-merge)))
```
