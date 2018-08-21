PLAN
====

Brainf\*ck
----------

* &gt;
* <
* &#43;
* &#45;
* .
* ,
* [
* ]

構文木
------

	data ParseTree
		= PtrInc
		| PtrDec
		| ValInc
		| ValDec
		| PutCh
		| GetCh
		| Loop ParseTree
