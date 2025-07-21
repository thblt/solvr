# Solvr: circular spreadsheets

Solvr is a simple language to express relationship between numeric
variables, then solve a full system from assigning values to arbitrary
variables.  As a simple example, you can initialize solvr with the
following definitions:

``` solvr
@rounding 0
flour;
water = .5 * flour;
salt = 0.02 * flour;
butter = .75 * flour;
```

These are the proportions for a puff pastry dough.  Now, say you have
500g butter, you can state:

``` solvr
> butter = 500
```

and solvr will reply:

```
flour = 666; water = 325; salt = 13;
```
