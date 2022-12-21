---
title: Debug symbols for openjdk8 on archlinux
tags: linux, java
status: draft
---

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.

* list item
* another item
* moaaaar

blah

```rust
// a simple struct to hold a post, without parsing the markdown
// basically, simply parse the header before feeding it to content pipeline
struct Post<'input> {
    title: &'input str,
    tags: Vec<&'input str>,
    status: PostStatus,
    raw_content: &'input str,
}


impl<'a> Post<'a> {
}
```

and some python

```python
def numsum(n):
    ''' The recursive sum of all digits in a number
        unit a single character is obtained'''
    res = sum([int(i) for i in str(n)])
    if res < 10: return res
    else : return numsum(res)

for n in range(1,101):
    response = 'Fizz'*(numsum(n) in [3,6,9]) + \
                   'Buzz'*(str(n)[-1] in ['5','0'])\
                or n
    print(response)
```

```
echo "this language shall not be named"
exit 0
```

## voilà
done
