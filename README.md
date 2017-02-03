# pml - parse my log

Parse a nginx log file and display statistics

## Run

```
./pml.lisp /path/to/logfile metric1 metric2 ...
```

## Supported metrics

- `ip` : group by remote address
- `path`: group by HTTP request path
- `verb`: group by HTTP verb
- `status`: group by HTTP status code
- `agent`: group by user agent

## Sample output

```
$> ./pml.lisp sample/big.log verb status ip path

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
