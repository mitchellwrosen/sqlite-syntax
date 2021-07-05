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
        , columns = Nothing
        , insert = InsertSelect
            ( SelectStatement
                { commonTableExpressions = Nothing
                , select = CompoundSelect
                    ( SelectCore'Values
                        ( Values
                            (
                                ( Expression'Concatenate
                                    ( Expression'Concatenate
                                        ( Expression'LiteralValue
                                            ( String "one" )
                                        )
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "commas"
                                                }
                                            )
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( String "two" )
                                    ) :| []
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