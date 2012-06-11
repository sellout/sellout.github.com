---
    layout: post
    publish: false
    title: Third-party Method Combinations
    tags: method-combinations
---

Talk about using `ENSURE-GENERIC-FUNCTION` to change the method combination for an existing function.
* need to make sure that the new combination is compatible with the old (or that no incompatible methods are ever included in the applicable set – which may require using `DELETE-METHOD`).
* `WRAP-EXISTING-METHOD-COMBINATION` can help with some cases, but there are definitely other compatible combinations