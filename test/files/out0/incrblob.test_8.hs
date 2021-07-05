Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "blobs"
                }
            , alias = Nothing
            }
        , columns = Just
            ( "k" :|
                [ "v"
                , "i"
                ]
            )
        , insert = InsertSelect
            ( SelectStatement
                { commonTableExpressions = Nothing
                , select = CompoundSelect
                    ( SelectCore'Values
                        ( Values
                            (
                                ( Expression'LiteralValue
                                    ( Blob "010203040506070809" ) :|
                                    [ Expression'LiteralValue
                                        ( String "hello" )
                                    , Expression'LiteralValue
                                        ( String "world" )
                                    ]
                                ) :| []
                            )
                        )
                    )
                , orderBy = Nothing
                , limit = Nothing
                }
            ) Nothing
        , returning = Nothing
        }
    )