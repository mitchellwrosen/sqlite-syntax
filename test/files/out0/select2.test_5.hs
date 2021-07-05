Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "tbl2"
                }
            , alias = Nothing
            }
        , columns = Nothing
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
                                        , value = "i"
                                        }
                                    ) :|
                                    [ Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "i2"
                                            }
                                        )
                                    , Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "i3"
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