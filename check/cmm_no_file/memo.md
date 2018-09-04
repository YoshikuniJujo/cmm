メモ
====

* 構文木から.sファイルを作成してみる
* 構文木をlazyに生成してみる
* 構文木をインクリメンタルに生成しながら、.sファイルにコンパイルしてみる

やったこと
----------

* パッケージghcを使って、.cmmを.sにコンパイルした
* call\_cmm.cと合わせてリンクもした
* .cmmをパースして構文木をoutputした

課題
----

得られた知見
------------

### top directory

-Bで設定されるか、または
実行可能ファイルを含むディレクトリのひとつうえのディレクトリ下のディレクトリlib。
ファイルsettingsをはじめ、さまざまな設定ファイルと、
(多分)インストールされたパッケージを含む。
パッケージには直接、置かれているものと、package.conf.d以下にメタデータだけ
置いてあるものとがあるようだ。

### findTopDirの重複の解消

2018.7.20のコミットでinitSysToolsとinitLlvmTargetsから
findTopDirの呼び出しが削除され、同機能が3回呼び出されていたという重複が
解消された。

バージョン8.7.20180720からということになるのかな。

[ghc: commit: Avoid redundant invocation of `findTopDir'](
https://github.com/ghc/ghc/commit/f64f06bebddd1dbfc6568f36fa1f91f758fa22f1)

### findTopDir

指定されている(Just path)ならば、normalise pathをかえす。
指定されていない(Nothing)ならば、(Linuxならば、)getExecutablePathをして、
それが含まれるディレクトリのひとつうえのディレクトリ下のディレクトリlibをかえす。

getExecutablePathは/proc/self/exeをreadSymbolicLinkすることで、
実行中のexeファイルのパスを入手する。

### -Bオプション

-BオプションでtopDirを設定できる。

[ghc/Main.hs -Bオプション](
https://github.com/ghc/ghc/blame/d1514e8f0e146e7b917bbb05465f875a5de4b2a4/ghc/Main.hs#L102)

hscCompileCmmFile
-----------------

```hs
hscCompileCmmFile :: HscEnv -> FilePath -> FilePath -> IO ()
hscCompileCmmFile hsc_env filename output_filename = runHsc hsc_env $ do
    let dflags = hsc_dflags hsc_env
    cmm <- ioMsgMaybe $ parseCmmFile dflags filename
    liftIO $ do
        us <- mkSplitUniqSupply 'S'
        let initTopSRT = initUs_ us emptySRT
        dumpIfSet_dyn dflags Opt_D_dump_cmm_verbose "Parsed Cmm" (ppr cmm)
        (_, cmmgroup) <- cmmPipeline hsc_env initTopSRT cmm
        rawCmms <- cmmToRawCmm dflags (Stream.yield cmmgroup)
        let -- Make up a module name to give the NCG. We can't pass bottom here
            -- lest we reproduce #11784.
            mod_name = mkModuleName $ "Cmm$" ++ FilePath.takeFileName filename
            cmm_mod = mkModule (thisPackage dflags) mod_name
        _ <- codeOutput dflags cmm_mod output_filename no_loc NoStubs [] []
             rawCmms
        return ()
  where
    no_loc = ModLocation{ ml_hs_file  = Just filename,
                          ml_hi_file  = panic "hscCompileCmmFile: no hi file",
                          ml_obj_file = panic "hscCompileCmmFile: no obj file" }
```
