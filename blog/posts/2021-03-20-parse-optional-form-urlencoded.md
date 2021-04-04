---
title: Parse optional values in form-urlencoded
tags: rust
status: published
---

# The setup

I'm doing a bit of rust things here and there, and I'm currently looking into building a really simple web form to upload stuff. I hit a problem when I wanted to retrieve an optional value from the html form.

A super basic project will do:

```sh
cargo new sentinel-form
cd sentinel-form

# cargo-utils crate gives us `cargo add`
cargo add serde --features derive
cargo add serde_urlencoded
```


The form is the simplest thing you can imagine:

```html
<div>
  <p>Max size:</p>
  <input type="radio" name="max-size" value="1" id="max-size-1MB">
  <label for="max-size-1MB">1 MB</label>
  <input type="radio" name="max-size" value="10" id="max-size-10MB" checked>
  <label for="max-size-10MB">10 MB</label>
</div>
```

I want to map that to a struct:

```rust
#[derive(Debug, Deserialize)]
struct Form {
  #[serde(rename = "max-size")]
  max_size: u64
}
```

Here's a basic program to convert to and from `xxx-url-form-urlencoded`.

```rust
use serde::{Deserialize, Serialize};

fn main() {
    let form = Form { max_size: 10 };
    let ser = serde_urlencoded::to_string(&form).unwrap();
    println!("url-encoded form: {}", ser);
    let decoded: Result<Form, _> = serde_urlencoded::from_str("max-size=12");
    println!("decoded form: {:?}", decoded);
}

#[derive(Debug, Deserialize, Serialize)]
struct Form {
    #[serde(rename = "max-size")]
    max_size: u64,
}
```

And when we run it, we get:
```
url-encoded form: max-size=10
decoded form: Ok(Form { max_size: 12 })
```

# Optional values

Now, I want to have the ability to not specify a maximum size, in this case, a simple solution is to turn the `max_size` field into an `Option<u64>`.

The updated `main.rs` is now:

```rust
use serde::{Deserialize, Serialize};

fn main() {
    let form = Form { max_size: Some(10) };
    let ser = serde_urlencoded::to_string(&form).unwrap();
    println!("url-encoded form: {}", ser);
    let decoded: Result<Form, _> = serde_urlencoded::from_str("max-size=12");
    println!("decoded form: {:?}", decoded);

    // New !
    let decoded_none: Result<Form, _> = serde_urlencoded::from_str("");
    println!("decoded none form: {:?}", decoded_none);
}

#[derive(Debug, Deserialize, Serialize)]
struct Form {
    #[serde(rename = "max-size")]
    // new !
    max_size: Option<u64>,
}
```

And it gives us what we want:
```
url-encoded form: max-size=10
decoded form: Ok(Form { max_size: Some(12) })
decoded none form: Ok(Form { max_size: None })
```

# Dealing with sentinel values

Now, the problem is that the html form needs to give the user a button for that. It's easy enough:

```html
<div>
  <p>Max size:</p>
  <input type="radio" name="max-size" value="1" id="max-size-1MB">
  <label for="max-size-1MB">1 MB</label>
  <input type="radio" name="max-size" value="10" id="max-size-10MB" checked>
  <label for="max-size-10MB">10 MB</label>
  <!-- NEW ! -->
  <input type="radio" name="max-size" value="None" id="max-size-unlimited">
  <label for="max-size-unlimited">Unlimited</label>
</div>
```

However, when the user selects this new option, the string we get looks like: `max-size=None`, and if we try to deserialize it like before, we get an error:

```rust
// in main()
let decoded: Result<Form, _> = serde_urlencoded::from_str("max-size=None");
println!("decoded form: {:?}", decoded);
```

```sh
decoded form: Err(Error("invalid digit found in string"))
```

So we need a custom deserializer function, which will treat the sentinel value `"None"` as the rust `None` value. That is, we want a function like so:

```rust
fn deserialize_sentinel<'de, T, D>(deserializer: D) -> Result<Option<T>, D:Error>
where
  D: Deserializer<'de>
{
  todo!()
}
```

Now, we can leverage serde untagged enums to do the heavy lifting for us. First we define an enum where each variant will be attempted in order:

```rust
#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
enum Maybe<U> {
    Just(Option<U>),
    Nothing(String),
}
```

And then we can use this enum in the `deserialize_sentinel` function:

```rust
fn deserialize_sentinel<'de, T, D>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    // deserialize using the enum
    let value: Maybe<T> = Deserialize::deserialize(deserializer)?;

    match value {
        Maybe::Just(x) => Ok(x),
        Maybe::Nothing(raw) => {
            // handle the sentinel value
            if raw == "None" {
                Ok(None)
            } else {
                Err(serde::de::Error::custom(format!(
                    "Unexpected string {}",
                    raw
                )))
            }
        }
    }
}
```

Now just have to ask serde to use this function to deserialize our `max_size` field:

```rust
#[derive(Debug, Deserialize, Serialize)]
struct Form {
    #[serde(rename = "max-size", deserialize_with = "deserialize_sentinel")]
    max_size: Option<u64>,
}
```

And the `main()`:

```rust
fn main() {
    let decoded: Result<Form, _> = serde_urlencoded::from_str("max-size=None");
    println!("decoded form: {:?}", decoded);
}
```

gives: `decoded form: Ok(Form { max_size: None })`. Hooray !

Just to check:

```rust
    let decoded2: Result<Form, _> = serde_urlencoded::from_str("max-size=10");
    println!("decoded form: {:?}", decoded2);
```
gives `decoded form: Err(Error("Unexpected string 10"))` /o\ oops.


# The bugs
So something is amiss, we can decode `None` with the sentinel value, but the regular ones can't be deserialize anymore.
For non self-describing format like a query string or a url encoded form, the string `max-size=12` can be deserialized as String or a number. It seems it's [a serde limitation](https://github.com/nox/serde_urlencoded/issues/33) when using `serde(flatten)` or `serde(untagged)`. Thankfully, there is [a workaround](https://docs.rs/serde_qs/0.8.3/serde_qs/index.html#flatten-workaround).

First, we need to specify a custom deserializing function for the variants of the enum:

```rust
fn from_optional_str<'de, D, S>(deserializer: D) -> Result<Option<S>, D::Error>
where
    D: serde::Deserializer<'de>,
    S: std::str::FromStr,
{
    let s: Option<&str> = Deserialize::deserialize(deserializer)?;
    match s {
        Some(s) => S::from_str(&s)
            .map(Some)
            // D::Error::custom requires to import serde::de::Error
            .map_err(|_| D::Error::custom("could not parse string")),
        None => Ok(None),
    }
}
```

And we annotate the relevant variant in `Maybe` to use this new function:
```rust
    // new derive !
    #[serde(deserialize_with = "from_optional_str")]
    Just(Option<U>),
```

We also need to add a new trait bound to the function `deserialize_sentinel`: `T: std::str::FromStr` otherwise rustc rightly complains that it can't deserialize into a `Maybe<T>`.

And now, with the following code:
```rust
fn main() {
    let decoded: Result<Form, _> = serde_urlencoded::from_str("max-size=None");
    println!("decoded form: {:?}", decoded);

    let decoded2: Result<Form, _> = serde_urlencoded::from_str("max-size=10");
    println!("decoded form: {:?}", decoded2);
}
```

we get:
```
decoded form: Ok(Form { max_size: None })
decoded form: Ok(Form { max_size: Some(10) })
```

# One last default

With this example and an html form, it won't happen, but if we get an empty string as input? That means the field `max-size` is missing, and it's legitimate to want to default to `None`. However, with the current setup, what we get is: `decoded form: Err(Error("missing field 'max-size'"))`.

The fix is simple, even though it took me a while to understand what was going on. We want a default for the struct's field:

```rust
#[derive(Debug, Deserialize, Serialize)]
struct Form {
    #[serde(
        rename = "max-size",
        deserialize_with = "deserialize_sentinel",
        // new !
        default
    )]
    max_size: Option<u64>,
}
```

And with this last fix we get:
```rust
    let decoded3: Result<Form, _> = serde_urlencoded::from_str("");
    println!("decoded form: {:?}", decoded3);
```
`decoded form: Ok(Form { max_size: None })`
\o/


# Conclusion
That was waaay more tricky than I expected. At least I learned a lot about serde in the process, and this solution seems general enough that I can bring it over to any other project if I need it.

I'm also realising that doing web stuff with html forms in rust is nowhere near as nice as in other language, the ecosystem simply isn't there yet, contrary to what [are we web yet](https://www.arewewebyet.org/) pretends. For APIs it's probable better, but handling html forms is a pain.


---
Thanks to Chouhartem for his proofreading.
