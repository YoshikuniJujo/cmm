メモ
====

* とりあえずHello, world!をコマンドghcでコンパイル、リンクして、実行してみる
* ghcパッケージを使った.cmmのコンパイルを試す
	+ まずは.sまで
		- .sができたらghc(または、できたらgcc)でリンクしてみる
	+ call\_cmm.cとのリンクもソース内でやってみる
* .cmmファイルから構文木を生成してみる
* 構文木から.sファイルを作成してみる
* 構文木をインクリメンタルに生成しながら、.sファイルにコンパイルしてみる

やったこと
----------

* パッケージghcを使って、.cmmを.sにコンパイルした

課題
----

* initSysToolsやinitLlvmTargetsの引数になるtop directoryというものについて
	+ その役割や位置づけについて直観的に理解したい
	+ コマンドghcについて
		- オプションとしてどのようにあたえられるか
			* minusb (-Bオプション)
		- オプションになかったとき、どのように設定されるか
			* findTopDirについて
				+ どのような意味あいを持つか

得られた知見
------------

### findTopDirの重複の解消

2018.7.20のコミットでinitSysToolsとinitLlvmTargetsから
findTopDirの呼び出しが削除され、同機能が3回呼び出されていたという重複が
解消された。

バージョン8.7.20180720からということになるのかな。

### findTopDir

指定されている(Just path)ならば、normalise pathをかえす。
指定されていない(Nothing)ならば、(Linuxならば、)getExecutablePathをして、
それが含まれるディレクトリのひとつうえのディレクトリ下のディレクトリlibをかえす。

getExecutablePathは/proc/self/exeをreadSymbolicLinkすることで、
実行中のexeファイルのパスを入手する。

コード等へのリンク
------------------

1. [ghc: commit: Avoid redundant invocation of `findTopDir'](
https://github.com/ghc/ghc/commit/f64f06bebddd1dbfc6568f36fa1f91f758fa22f1)
