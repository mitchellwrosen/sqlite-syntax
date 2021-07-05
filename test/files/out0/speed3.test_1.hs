Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Just "main"
                , value = "t1"
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
                                        , value = "ii"
                                        }
                                    ) :|
                                    [ Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "text"
                                            }
                                        )
                                    , Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "ii"
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