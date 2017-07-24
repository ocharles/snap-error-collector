# 1.1.4

## Other Changes

* Increase the upper bound of `base`.

---

# 1.1.3

## Other Changes

* Increase the upper bound of `time` to allow < 1.9

---

# 1.1.2

* Increase upper bound on `base`, `async`, `snap`, `time` and `transformers`.
* `lifted-base` is now used instead of `MonadCatchIO-transformers`, as it is
  actively maintained.

---

# 1.1.1

* Increase upper bound on `base`, `snap`, `time`, `transformers`.

---

# 1.1.0

* Add an upper bound on the amount of errors collected. This means that even
  when things go horribly wrong in other code, you at least won't run out of
  memory by collecting all the exceptions! Thanks to @gregorycollins for
  this advice.

---

# 1.0.0

* Initial version.
