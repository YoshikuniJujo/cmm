TODO
====

コードの設計
------------

* プロジェクト名: brainf\_cmm
* Brainf\*ckのコンパイラを作成する
* 中間言語としてC--を使用
* パーサには[パッケージparsers](
	http://hackage.haskell.org/package/parsers
	)を使用
* 外部プロセス呼び出しを利用してghcを呼び出して、
	C--より先の処理は行う
	+ /etc/brainf\_cmm/brainf\_cmm.confでghcを設定できるようにする
		- たとえば`stack ghc --'を変わりに指定できる
		- また/etc以下ではなくホームディレクトリ下の設定ファイルも
			使用可能とする
			* ~/brainf\_cmm/brainf\_cmm.conf
			* ホームディレクトリ下の設定ファイルが優先される
			* どちらもなければghcとする
* パースされた結果は[]の入れ子が樹構造として表される
* それを、幅優先探索的なコードによって、平坦にする
	+ 「ブロック名」の(無限)リストを引数にとり、
		その名前をプレースホルダーにする
* 生成されるC--ファイルは以下のようになる
	+ ブロックbf\_mainからはじまる
	+ []内のコードはブロックで表現する
		- 内部のくりかえしはjumpで表現する
		- 参照しているメモリの値が0になったらreturnでもどる
	+ []外から[]内のコードをcallで呼び出す
* メモリ領域は30000要素のバイトの配列とする
* 「現在参照しているメモリ」を示すポインタはレジスタに保存する
* C--のブロックを呼び出すCの関数mainを自動生成する
	+ できればアセンブラを表現するADTを使用
	+ 変えるのは呼び出すC--のブロック名くらいではあるが

準備など
--------

* jumpやcallの動作をチェックする
	+ レジスタやメモリの、アセンブリとの対応を調べて、まとめる
	+ アセンブリにコンパイルしてチェックする
* 安全なレジスタをチェックする
	+ つまり、jumpやcallで使用されないということ