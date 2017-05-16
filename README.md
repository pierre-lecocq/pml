# pml - parse my log

Parse a nginx log file and display statistics

## Requirements

- `sbcl` : A [Common Lisp compiler](http://www.sbcl.org/)
- `quicklisp` : A [library manager for Common Lisp](https://www.quicklisp.org/beta/#installation)

## Run

```
./pml.lisp [--logfile FILE] [--format FORMAT] [METRICS] [FILTERS]
```

## Supported metrics

- `--ip` : group by remote address
- `--path`: group by HTTP request path
- `--verb`: group by HTTP verb
- `--status`: group by HTTP status code
- `--agent`: group by user agent

## Supported output formats

- `json`
- `csv`
- `txt` (default)

## Supported filters

- `start` : a start date
- `end` : an end date

## TODO

- [ ] Format function call (txt is always used now)
- [ ] System definition
- [ ] Configurable log format

## Sample output

```
$> ./pml.lisp --logfile sample/big.log --format txt --verb --status --ip --path --start "2017-01-01" --end "2017-02-15"

 * By verb
GET     916     91.6%
POST	84      8.4%

 * By status
200     562     56.2%
304     304     30.4%
404     101     10.1%
302     33      3.3%

 * By remote address
10.1.2.3	603     60.3%
10.1.2.4	310     31.0%
10.1.2.5	60      6.0%
10.1.2.6	27      2.7%

 * By path

/               31      3.1%
/products       31      3.1%
/products/25	31      3.1%
/products/2     30      3.0%
/about          30      3.0%
/contact        30      3.0%
/categories/36	30	    3.0%
/products/16	30	    3.0%
/categories/11	29	    2.9%
/products/48	29	    2.9%
/categories/85	29	    2.9%
/categories/45	29	    2.9%
/tags           29	    2.9%
/tags/7         19	    1.9%
/tags/14        17	    1.7%

[... skip ...]
```

## License

`pml` is released under the [MIT License](LICENSE).
