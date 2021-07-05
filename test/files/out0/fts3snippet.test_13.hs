Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "ft"
                }
            , alias = Nothing
            }
        , columns = Just
            ( "docid" :|
                [ "x"
                , "y"
                ]
            )
        , insert = InsertSelect
            ( SelectStatement
                { commonTableExpressions = Nothing
                , select = CompoundSelect
                    ( SelectCore'Values
                        ( Values
                            (
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "docid"
                                        }
                                    ) :|
                                    [ Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "v1"
                                            }
                                        )
                                    , Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "v2"
                                            }
                                        )
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